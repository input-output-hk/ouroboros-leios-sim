{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module PraosProtocol.BlockFetch where

import Control.Concurrent.Class.MonadSTM (
  MonadSTM (
    STM,
    TVar,
    atomically,
    modifyTVar',
    newTVar,
    readTVar,
    retry,
    writeTVar
  ),
 )
import Control.Monad (forM, forever, guard, replicateM, (<=<))
import Data.Bifunctor (second)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Network.TypedProtocol (
  Agency (ClientAgency, NobodyAgency, ServerAgency),
  IsPipelined (NonPipelined),
  Protocol (..),
  StateTokenI (..),
 )
import qualified Network.TypedProtocol.Peer.Client as TC
import qualified Network.TypedProtocol.Peer.Server as TS
import Ouroboros.Network.AnchoredFragment (AnchoredFragment, anchorPoint)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.Block as OAPI

import Data.Maybe (fromMaybe)
import Ouroboros.Network.Mock.ConcreteBlock (BlockHeader (..), hashBody)
import PraosProtocol.Types (
  Block (..),
  BlockBody,
  Blocks,
  Chain (..),
  ChainProducerState (..),
  ReadOnly (..),
  blockPrevPoint,
  fromAnchoredFragment,
  headPoint,
  headerPoint,
  initChainProducerState,
  intersectChains,
  readReadOnlyTVar,
  selectChain,
  switchFork,
  toAnchoredFragment,
 )

type BlockId = OAPI.HeaderHash Block
type Point = OAPI.Point Block

data BlockFetchState
  = StIdle
  | StBusy
  | StStreaming
  | StDone

data SingBlockFetchState (st :: BlockFetchState) where
  SingStIdle :: SingBlockFetchState StIdle
  SingStBusy :: SingBlockFetchState StBusy
  SingStStreaming :: SingBlockFetchState StStreaming
  SingStDone :: SingBlockFetchState StDone

instance Protocol BlockFetchState where
  data Message BlockFetchState from to where
    MsgRequestRange :: Point -> Point -> Message BlockFetchState StIdle StBusy
    MsgNoBlocks :: Message BlockFetchState StBusy StIdle
    MsgStartBatch :: Message BlockFetchState StBusy StStreaming
    MsgBlock :: BlockBody -> Message BlockFetchState StStreaming StStreaming
    MsgBatchDone :: Message BlockFetchState StStreaming StIdle
    MsgClientDone :: Message BlockFetchState StIdle StDone
  type StateAgency StIdle = ClientAgency
  type StateAgency StBusy = ServerAgency
  type StateAgency StStreaming = ServerAgency
  type StateAgency StDone = NobodyAgency
  type StateToken = SingBlockFetchState

instance StateTokenI StIdle where stateToken = SingStIdle
instance StateTokenI StBusy where stateToken = SingStBusy
instance StateTokenI StStreaming where stateToken = SingStStreaming
instance StateTokenI StDone where stateToken = SingStDone

--------------------------------
--- BlockFetch Server
--------------------------------

data BlockFetchProducerState m = BlockFetchProducerState
  { blocksVar :: ReadOnly (TVar m Blocks)
  }

resolveRange :: MonadSTM m => BlockFetchProducerState m -> Point -> Point -> STM m (Maybe [BlockBody])
resolveRange st start end = do
  blocks <- readReadOnlyTVar st.blocksVar
  let resolveRangeAcc :: [BlockBody] -> Point -> Maybe [BlockBody]
      resolveRangeAcc acc p | start == p = Just acc
      resolveRangeAcc _acc OAPI.GenesisPoint = Nothing
      resolveRangeAcc acc p@(OAPI.BlockPoint pSlot pHash)
        | OAPI.pointSlot start > OAPI.pointSlot p = Nothing
        | otherwise = do
            Block{..} <- Map.lookup pHash blocks
            guard $ OAPI.blockSlot blockHeader == pSlot
            resolveRangeAcc (blockBody : acc) =<< blockPrevPoint blocks blockHeader
  return $ reverse <$> resolveRangeAcc [] end

blockFetchProducer ::
  forall m.
  MonadSTM m =>
  BlockFetchProducerState m ->
  TS.Server BlockFetchState NonPipelined StIdle m ()
blockFetchProducer st = idle
 where
  idle :: TS.Server BlockFetchState NonPipelined StIdle m ()
  idle = TS.Await $ \case
    MsgRequestRange start end -> TS.Effect $ atomically $ do
      mblocks <- resolveRange st start end
      case mblocks of
        Nothing -> return $ TS.Yield MsgNoBlocks idle
        Just blocks -> return $ TS.Yield MsgStartBatch (streaming blocks)
    MsgClientDone -> TS.Done ()

  streaming :: [BlockBody] -> TS.Server BlockFetchState NonPipelined StStreaming m ()
  streaming [] = TS.Yield MsgBatchDone idle
  streaming (block : blocks) = TS.Yield (MsgBlock block) (streaming blocks)

--------------------------------
--- BlockFetch Client
--------------------------------

newtype BlockRequest
  = BlockRequest {blockRequestFragments :: [AnchoredFragment BlockHeader]}
  deriving (Show)
  deriving newtype (Semigroup) -- TODO: we could merge the fragments.

fragmentRange :: AnchoredFragment BlockHeader -> (Point, Point)
fragmentRange fr = (OAPI.castPoint $ AF.lastPoint fr, OAPI.castPoint $ AF.headPoint fr)

blockRequestPoints :: BlockRequest -> [Point]
blockRequestPoints (BlockRequest frs) = concatMap (map headerPoint . AF.toOldestFirst) $ frs

data BlockFetchConsumerState m = BlockFetchConsumerState
  { blockRequestVar :: TVar m BlockRequest
  , addFetchedBlock :: Block -> m ()
  , removeInFlight :: [Point] -> m ()
  }

blockFetchConsumer ::
  forall m.
  MonadSTM m =>
  BlockFetchConsumerState m ->
  TC.Client BlockFetchState NonPipelined StIdle m ()
blockFetchConsumer st = idle
 where
  -- does not support preemption of in-flight requests.
  blockRequest :: STM m (AnchoredFragment BlockHeader)
  blockRequest = do
    BlockRequest rs <- readTVar st.blockRequestVar
    case rs of
      [] -> retry
      (r : rs') -> do
        writeTVar st.blockRequestVar (BlockRequest rs')
        return r

  idle :: TC.Client BlockFetchState NonPipelined StIdle m ()
  idle = TC.Effect $ atomically $ do
    fr <- blockRequest
    let range@(start, end) = fragmentRange fr
    return $ TC.Yield (MsgRequestRange start end) $ busy range fr

  busy :: (Point, Point) -> AnchoredFragment BlockHeader -> TC.Client BlockFetchState NonPipelined StBusy m ()
  busy range fr = TC.Await $ \case
    MsgNoBlocks -> TC.Effect $ do
      -- TODO: controller might just ask this peer again.
      st.removeInFlight (blockRequestPoints $ BlockRequest [fr])
      return idle
    MsgStartBatch -> streaming range $ AF.toOldestFirst fr

  streaming :: (Point, Point) -> [BlockHeader] -> TC.Client BlockFetchState NonPipelined StStreaming m ()
  streaming range headers = TC.Await $ \msg ->
    case (msg, headers) of
      (MsgBatchDone, []) -> idle
      (MsgBlock block, header : headers') -> TC.Effect $ do
        ifValidBlockBody
          header
          block
          ( do
              st.addFetchedBlock (Block header block)
              return (streaming range headers')
          )
          (error "blockFetchConsumer: invalid block") -- TODO
      (MsgBatchDone, _ : _) -> TC.Effect $ error "TooFewBlocks" -- TODO?
      (MsgBlock _, []) -> TC.Effect $ error "TooManyBlocks" -- TODO?
  ifValidBlockBody hdr bdy t f = do
    -- TODO: threadDelay
    if headerBodyHash hdr == hashBody bdy
      then t
      else f

--------------------------------------------
---- BlockFetch controller
--------------------------------------------

longestChainSelection ::
  forall block header m.
  ( OAPI.HasHeader block
  , OAPI.HasHeader header
  , OAPI.HeaderHash block ~ OAPI.HeaderHash header
  , MonadSTM m
  ) =>
  [(PeerId, ReadOnly (TVar m (Chain header)))] ->
  ReadOnly (TVar m (ChainProducerState block)) ->
  (block -> header) ->
  STM m (Maybe (PeerId, AnchoredFragment header))
longestChainSelection candidateChainVars cpsVar getHeader = do
  candidateChains <- mapM (\(pId, v) -> (pId,) <$> readReadOnlyTVar v) candidateChainVars
  cps <- readReadOnlyTVar cpsVar
  let
    chain = fmap getHeader cps.chainState
    aux (mpId, c1) (pId, c2) =
      let c = selectChain c1 c2
       in if headPoint c == headPoint c1
            then (mpId, c1)
            else (Just pId, c2)
    -- using foldl' since @selectChain@ is left biased
    (selectedPeer, chain') = List.foldl' aux (Nothing, chain) candidateChains
  return $ do
    peerId <- selectedPeer
    point <- intersectChains chain chain'
    let suffix =
          snd . fromMaybe (error "longestChainSelection: intersect not on chain") $
            AF.splitAfterPoint (toAnchoredFragment chain') point
    pure (peerId, suffix)

-- | Note:
--    * a block is added to RequestVar and InFlightVar at the same time.
--    * the block is removed from blockRequestVar when the consumer starts fetching
--      the corresponding fragment.
--    * the block is removed from blocksInFlightVar when it reaches the
--      "ChainDB" i.e. blockBodiesVar, or the consumer encountered a
--      problem when fetching it. TODO!
data PeerStatus m = PeerStatus
  { blockRequestVar :: TVar m BlockRequest
  , blocksInFlightVar :: TVar m (Set Point)
  , peerChainVar :: ReadOnly (TVar m (Chain BlockHeader))
  }

type PeerId = Int

data BlockFetchControllerState m = BlockFetchControllerState
  { blocksVar :: TVar m Blocks
  , selectedChainVar :: TVar m (Maybe MissingBlocksChain)
  , peers :: Map PeerId (PeerStatus m)
  , cpsVar :: TVar m (ChainProducerState Block)
  }

-- NOTE: The ChainProducerState is initalized with no followers.
initBlockFetchControllerState :: MonadSTM m => [PeerId] -> m (BlockFetchControllerState m)
initBlockFetchControllerState peerIds = atomically $ do
  blocksVar <- newTVar Map.empty
  selectedChainVar <- newTVar Nothing
  peers <- Map.fromList . zip peerIds <$> replicateM (length peerIds) initPeerStatus
  cpsVar <- newTVar $ initChainProducerState Genesis
  return BlockFetchControllerState{..}
 where
  initPeerStatus = do
    blockRequestVar <- newTVar (BlockRequest [])
    blocksInFlightVar <- newTVar Set.empty
    peerChainVar <- ReadOnly <$> newTVar Genesis
    return PeerStatus{..}

blockFetchController :: forall m. MonadSTM m => BlockFetchControllerState m -> m ()
blockFetchController st@BlockFetchControllerState{..} = forever (atomically makeRequests)
 where
  makeRequests :: STM m ()
  makeRequests = do
    let peerChainVars = (map . second) (.peerChainVar) $ Map.toList peers
    (peerId, fr) <- maybe retry pure =<< longestChainSelection peerChainVars (ReadOnly cpsVar) blockHeader
    e <- initMissingBlocksChain <$> readTVar blocksVar <*> (chainState <$> readTVar cpsVar) <*> pure fr
    updateChains st e
    whenMissing e $ \_missingChain -> do
      -- TODO: filterFetched could be reusing the missingChain suffix.
      req <- filterInFlight <=< filterFetched $ fr
      addRequest peerId req

  filterFetched :: AnchoredFragment BlockHeader -> STM m BlockRequest
  filterFetched fr = do
    blocks <- readTVar blocksVar
    pure $ filterBR ((`Map.notMember` blocks) . OAPI.blockHash) (BlockRequest [fr])

  filterBR :: (BlockHeader -> Bool) -> BlockRequest -> BlockRequest
  filterBR p = BlockRequest . concatMap (AF.filter p) . (.blockRequestFragments)

  filterInFlight :: BlockRequest -> STM m BlockRequest
  filterInFlight br = do
    in_flights <- forM (Map.elems peers) $ \peer -> do
      readTVar peer.blocksInFlightVar
    pure $ List.foldl' (flip $ \s -> filterBR ((`Set.notMember` s) . headerPoint)) br in_flights

  addRequest :: PeerId -> BlockRequest -> STM m ()
  addRequest _pId (BlockRequest []) = retry
  addRequest pId br = do
    case Map.lookup pId peers of
      Nothing -> error "addRequest: no such peer"
      Just PeerStatus{..} -> do
        modifyTVar' blocksInFlightVar (`Set.union` Set.fromList (blockRequestPoints br))
        modifyTVar' blockRequestVar (<> br)

------------------------------------------------------
---- MissingBlocksChain
------------------------------------------------------

-- | invariants:
--    1. prefix starts at Genesis, and the tip of the prefix is the anchor of the suffix.
--    2. the suffix has one `Left header` element at the start.
data MissingBlocksChain = MissingBlocksChain
  { prefix :: AnchoredFragment Block
  , suffix :: AnchoredFragment BlockOrHeader
  }

newtype BlockOrHeader = BlockOrHeader {unBlockOrHeader :: Either BlockHeader Block}

type instance OAPI.HeaderHash BlockOrHeader = OAPI.HeaderHash BlockHeader

instance OAPI.StandardHash BlockOrHeader

instance OAPI.HasHeader BlockOrHeader where
  getHeaderFields (BlockOrHeader x) =
    either
      (OAPI.castHeaderFields . OAPI.getHeaderFields)
      (OAPI.castHeaderFields . OAPI.getHeaderFields)
      x

fillInBlocks :: Blocks -> MissingBlocksChain -> Either (Chain Block) MissingBlocksChain
fillInBlocks blocks MissingBlocksChain{..} =
  case addToChain prefix (AF.mapAnchoredFragment blkLookup suffix) of
    (prefix', Just suffix') -> Right $ MissingBlocksChain prefix' suffix'
    (prefix', Nothing) ->
      Left $
        fromMaybe (error "fillInBlocks: prefix not from genesis") $
          fromAnchoredFragment prefix'
 where
  blkLookup :: BlockOrHeader -> BlockOrHeader
  blkLookup x@(BlockOrHeader e) = case e of
    Right _ -> x
    Left hdr -> maybe x (BlockOrHeader . Right) . Map.lookup (OAPI.blockHash hdr) $ blocks
  addToChain c (AF.Empty _) = (c, Nothing)
  addToChain c af@(x AF.:< af') = case x of
    BlockOrHeader (Right blk) -> addToChain (c AF.:> blk) af'
    BlockOrHeader (Left _) -> (c, Just af)

initMissingBlocksChain ::
  Blocks ->
  Chain Block ->
  AnchoredFragment BlockHeader ->
  Either (Chain Block) MissingBlocksChain
initMissingBlocksChain blocks c fr =
  fillInBlocks blocks $
    MissingBlocksChain prefix (AF.mapAnchoredFragment (BlockOrHeader . Left) fr)
 where
  pt :: Point
  pt = OAPI.castPoint $ anchorPoint fr
  prefix = case AF.splitAfterPoint (toAnchoredFragment c) pt of
    Just (before, _) -> before
    Nothing -> error "initMissingBlocksChain: anchor of fragment not on chain"

whenMissing ::
  Monad m =>
  Either (Chain Block) MissingBlocksChain ->
  (MissingBlocksChain -> m ()) ->
  m ()
whenMissing (Left _) _ = return ()
whenMissing (Right m) k = k m

updateChains ::
  forall m.
  MonadSTM m =>
  BlockFetchControllerState m ->
  Either (Chain Block) MissingBlocksChain ->
  STM m ()
updateChains BlockFetchControllerState{..} e =
  case e of
    Left fullChain -> do
      writeTVar selectedChainVar Nothing
      modifyTVar' cpsVar (switchFork fullChain)
    Right missingChain -> do
      writeTVar selectedChainVar (Just missingChain)

-----------------------------------------------------------
---- Methods for blockFetchConsumer and blockFetchProducer
-----------------------------------------------------------

removeInFlight :: MonadSTM m => BlockFetchControllerState m -> PeerId -> [Point] -> STM m ()
removeInFlight BlockFetchControllerState{..} pId points = do
  let peer = fromMaybe (error "missing peer") $ Map.lookup pId peers
  modifyTVar' peer.blocksInFlightVar (\s -> List.foldl' (flip Set.delete) s points)

-- | Like @addBlock@ but also removes block from PeerId's in-flight set.
addFetchedBlock :: MonadSTM m => BlockFetchControllerState m -> PeerId -> Block -> STM m ()
addFetchedBlock st pId blk = do
  removeInFlight st pId [OAPI.blockPoint blk]
  addBlock st blk

-- | Adds validated block to the state.
--   * adds block to blocksVar
--   * fillBlocks on selectedChain, and @updateChains@
addBlock :: MonadSTM m => BlockFetchControllerState m -> Block -> STM m ()
addBlock st@BlockFetchControllerState{..} blk = do
  modifyTVar' blocksVar (Map.insert (OAPI.blockHash blk) blk)

  selected <- readTVar selectedChainVar
  case selected of
    Nothing -> return () -- I suppose we do not need this block anymore.
    Just missingChain -> do
      updateChains st =<< fillInBlocks <$> readTVar blocksVar <*> pure missingChain
