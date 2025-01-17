{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PraosProtocol.Common (
  Anchor (..),
  AnchoredFragment,
  Chain (..),
  FullTip (..),
  fullTip,
  Blocks,
  toBlocks,
  headerPoint,
  blockPrevPoint,
  setFollowerPoint,
  blockBodyColor,
  blockHeaderColor,
  blockHeaderColorAsBody,
  module ConcreteBlock,
  module ProducerState,
  SlotConfig (..),
  slotTime,
  slotConfigFromNow,
  PraosNodeEvent (..),
  PraosConfig (..),
  MessageSize (..),
  kilobytes,
  module TimeCompat,
  defaultPraosConfig,
  defaultPraosConfig',
  CPUTask (..),
  hashToColor,
) where

import ChanTCP (Bytes, MessageSize (..))
import Control.Exception (assert)
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import Ouroboros.Network.Mock.ProducerState as ProducerState
import PraosProtocol.Common.AnchoredFragment (Anchor (..), AnchoredFragment)
import PraosProtocol.Common.Chain (Chain (..), foldChain, pointOnChain)
import PraosProtocol.Common.ConcreteBlock as ConcreteBlock
import SimTCPLinks (kilobytes)
import SimTypes (CPUTask (..))
import System.Random (mkStdGen, uniform)
import TimeCompat

--------------------------------
--- Common types
--------------------------------

instance MessageSize BlockBody where
  messageSizeBytes b = b.bodyMessageSize

instance MessageSize BlockHeader where
  messageSizeBytes h = h.headerMessageSize

instance MessageSize SlotNo where
  messageSizeBytes _ = 8

-- TODO: Refactor to provide sizes for basic types.
instance MessageSize (Tip block) where
  messageSizeBytes _ = {- slot no -} 8 + {- hash -} 32 + {- block no -} 8

instance MessageSize (Point block) where
  messageSizeBytes _ = {- hash -} 32 + {- slot no -} 8

data FullTip
  = -- | The tip is genesis
    FullTipGenesis
  | -- | The tip is not genesis
    FullTip BlockHeader
  deriving (Show)

fullTip :: Chain (Block body) -> FullTip
fullTip Genesis = FullTipGenesis
fullTip (_ :> blk) = FullTip (blockHeader blk)

type Blocks body = Map (HeaderHash (Block body)) (Block body)

toBlocks :: Chain (Block body) -> Blocks body
toBlocks = foldChain (\blocks block -> Map.insert (headerHash . blockHeader $ block) block blocks) Map.empty

headerPoint :: BlockHeader -> Point (Block body)
headerPoint = castPoint . blockPoint

blockPrevPoint :: (IsBody body, HasFullHeader b, HeaderHash b ~ HeaderHash (Block body)) => Blocks body -> b -> Maybe (Point (Block body))
blockPrevPoint blks header = case blockPrevHash header of
  GenesisHash -> pure GenesisPoint
  BlockHash hash -> castPoint . blockPoint <$> Map.lookup hash blks

setFollowerPoint :: forall body. IsBody body => FollowerId -> Point (Block body) -> ChainProducerState (Block body) -> ChainProducerState (Block body)
setFollowerPoint followerId point st@ChainProducerState{..} =
  assert (pointOnChain point chainState) $
    st{chainFollowers = Map.adjust setFollowerPoint' followerId chainFollowers}
 where
  setFollowerPoint' :: FollowerState (Block body) -> FollowerState (Block body)
  setFollowerPoint' followerState = followerState{followerPoint = point}

data SlotConfig = SlotConfig {start :: UTCTime, duration :: NominalDiffTime}

slotTime :: SlotConfig -> SlotNo -> UTCTime
slotTime SlotConfig{start, duration} sl = (fromIntegral (unSlotNo sl) * duration) `addUTCTime` start

slotConfigFromNow :: MonadTime m => m SlotConfig
slotConfigFromNow = do
  start <- getCurrentTime
  return $ SlotConfig{start, duration = 1}

blockBodyColor :: IsBody body => body -> (Double, Double, Double)
blockBodyColor = hashToColor . coerce . hashBody

blockHeaderColor :: BlockHeader -> (Double, Double, Double)
blockHeaderColor = hashToColor . coerce . blockHash

blockHeaderColorAsBody :: BlockHeader -> (Double, Double, Double)
blockHeaderColorAsBody = hashToColor . coerce . headerBodyHash

hashToColor :: Int -> (Double, Double, Double)
hashToColor hash = (fromIntegral r / 256, fromIntegral g / 256, fromIntegral b / 256)
 where
  r, g, b :: Word8
  ((r, g, b), _) = uniform (mkStdGen hash)

data PraosNodeEvent body
  = PraosNodeEventGenerate (Block body)
  | PraosNodeEventReceived (Block body)
  | PraosNodeEventEnterState (Block body)
  | PraosNodeEventNewTip (Chain (Block body))
  | PraosNodeEventCPU CPUTask
  deriving (Show)

data PraosConfig body = PraosConfig
  { slotConfig :: !SlotConfig
  , blockValidationDelay :: !(Block body -> DiffTime)
  , headerValidationDelay :: !(BlockHeader -> DiffTime)
  , blockGenerationDelay :: !(Block body -> DiffTime)
  , headerSize :: !Bytes
  , bodySize :: !(body -> Bytes)
  , bodyMaxSize :: !Bytes
  }

defaultPraosConfig' :: SlotConfig -> PraosConfig body
defaultPraosConfig' slotConfig =
  PraosConfig
    { slotConfig
    , blockValidationDelay = const 0.1
    , headerValidationDelay = const 0.005
    , blockGenerationDelay = const 0
    , headerSize = kilobytes 1
    , bodySize = const $ kilobytes 95
    , bodyMaxSize = kilobytes 96
    }

defaultPraosConfig :: MonadTime m => m (PraosConfig body)
defaultPraosConfig = defaultPraosConfig' <$> slotConfigFromNow
