{-# LANGUAGE FlexibleInstances #-}

module PraosProtocol.SimBlockFetch where

import Chan (Chan)
import ChanDriver (ProtocolMessage)
import ChanTCP
import Control.Concurrent.Class.MonadSTM (MonadSTM (..))
import Control.Monad.Class.MonadAsync (
  MonadAsync (concurrently_),
 )
import Control.Monad.IOSim as IOSim (IOSim, runSimTrace)
import Control.Tracer as Tracer (
  Contravariant (contramap),
  Tracer,
  traceWith,
 )
import qualified Data.ByteString as BS
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import PraosProtocol.BlockFetch
import PraosProtocol.Common hiding (Point)
import PraosProtocol.Common.Chain (Chain (..))
import SimTCPLinks
import SimTypes

type BlockFetchTrace = [(Time, BlockFetchEvent)]

data BlockFetchEvent
  = -- | Declare the nodes and links
    BlockFetchEventSetup
      !WorldShape
      !(Map NodeId Point) -- nodes and locations
      !(Set (NodeId, NodeId)) -- links between nodes
  | -- | An event at a node
    BlockFetchEventNode (LabelNode BlockFetchNodeEvent)
  | -- | An event on a tcp link between two nodes
    BlockFetchEventTcp (LabelLink (TcpEvent (ProtocolMessage BlockFetchState)))
  deriving (Show)

data BlockFetchNodeEvent = BlockFetchNodeEvent
  deriving (Show)

exampleTrace1 :: BlockFetchTrace
exampleTrace1 = traceRelayLink1 $ mkTcpConnProps 0.1 1000000

traceRelayLink1 ::
  TcpConnProps ->
  --- PacketGenerationPattern ->
  BlockFetchTrace
traceRelayLink1 tcpprops =
  selectTimedEvents $
    runSimTrace $ do
      traceWith tracer $
        BlockFetchEventSetup
          WorldShape
            { worldDimensions = (500, 500)
            , worldIsCylinder = False
            }
          ( Map.fromList
              [ (NodeId 0, Point 50 100)
              , (NodeId 1, Point 450 100)
              ]
          )
          ( Set.fromList
              [(NodeId 0, NodeId 1), (NodeId 1, NodeId 0)]
          )
      (inChan, outChan) <- newConnectionTCP (linkTracer na nb) tcpprops
      concurrently_
        (nodeA outChan)
        (nodeB inChan)
      return ()
 where
  -- Soon-To-Be-Shared Chain
  bchain = mkChainSimple $ replicate 10 (BlockBody $ BS.replicate 100 0)

  -- Block-Fetch Controller & Consumer
  nodeA :: (MonadAsync m, MonadSTM m) => Chan m (ProtocolMessage BlockFetchState) -> m ()
  nodeA chan = do
    peerChainVar <- newTVarIO (blockHeader <$> bchain)
    st <- newBlockFetchControllerState Genesis >>= addPeer (asReadOnly peerChainVar) <&> fst
    concurrently_
      ( blockFetchController st
      )
      ( runBlockFetchConsumer chan $
          initBlockFetchConsumerStateForPeerId 1 st
      )
  -- Block-Fetch Producer
  nodeB chan = do
    st <- BlockFetchProducerState . asReadOnly <$> newTVarIO (toBlocks bchain)
    runBlockFetchProducer chan st

  (na, nb) = (NodeId 0, NodeId 1)

  tracer :: Tracer (IOSim s) BlockFetchEvent
  tracer = simTracer

  linkTracer ::
    NodeId ->
    NodeId ->
    Tracer
      (IOSim s)
      (LabelTcpDir (TcpEvent (ProtocolMessage BlockFetchState)))
  linkTracer nfrom nto =
    contramap (BlockFetchEventTcp . labelDirToLabelLink nfrom nto) tracer