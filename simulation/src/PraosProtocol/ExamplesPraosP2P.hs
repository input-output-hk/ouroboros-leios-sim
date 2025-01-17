{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module PraosProtocol.ExamplesPraosP2P where

import ChanDriver
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.Coerce (coerce)
import Data.Functor.Contravariant (Contravariant (contramap))
import qualified Data.IntMap.Strict as IMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Generics
import Network.TypedProtocol
import P2P (P2PTopography (p2pNodes), P2PTopographyCharacteristics (..), genArbitraryP2PTopography)
import PraosProtocol.BlockFetch
import PraosProtocol.BlockGeneration (PacketGenerationPattern (..))
import PraosProtocol.Common
import qualified PraosProtocol.Common.Chain as Chain
import PraosProtocol.PraosNode
import PraosProtocol.SimPraos
import PraosProtocol.SimPraosP2P
import PraosProtocol.VizSimPraos (ChainsMap, DiffusionLatencyMap, PraosVizConfig' (blockFetchMessageColor), accumChains, accumDiffusionLatency, examplesPraosSimVizConfig, praosSimVizModel)
import PraosProtocol.VizSimPraosP2P
import Sample
import SimTCPLinks (mkTcpConnProps)
import SimTypes
import System.FilePath
import System.Random (StdGen, mkStdGen)
import Viz

example1 :: StdGen -> DiffTime -> P2PTopography -> Visualization
example1 rng0 blockInterval p2pTopography =
  Viz (praosSimVizModel (example1Trace rng0 blockInterval p2pTopography)) $
    LayoutAbove
      [ layoutLabelTime
      , LayoutBeside
          [ LayoutReqSize 1200 1000 $
              Layout $
                praosP2PSimVizRender config
          , LayoutBeside
              [ LayoutAbove
                  [ LayoutReqSize 350 300 $
                      Layout $
                        chartDiffusionLatency config
                  , LayoutReqSize 350 300 $
                      Layout $
                        chartDiffusionImperfection
                          p2pTopography
                          0.1
                          (96 / 1000)
                          config
                  ]
              , LayoutAbove
                  [ LayoutReqSize 350 300 $
                      Layout chartBandwidth
                  , LayoutReqSize 350 300 $
                      Layout chartLinkUtilisation
                  ]
              ]
          ]
      ]

data DiffusionEntry = DiffusionEntry
  { hash :: Int
  , node_id :: Int
  , created :: DiffTime
  , arrivals :: [DiffTime]
  }
  deriving (Generic, ToJSON, FromJSON)

data LatencyPerStake = LatencyPerStake
  { hash :: Int
  , latencies :: [(Maybe DiffTime, Double)]
  }
  deriving (Generic, ToJSON, FromJSON)

data DiffusionData = DiffusionData
  { topography :: P2PTopographyCharacteristics
  , topography_details :: P2PTopography
  , entries :: [DiffusionEntry]
  , latency_per_stake :: [LatencyPerStake]
  , stable_chain_hashes :: [Int]
  , cpuTasks :: Map.Map NodeId [(DiffTime, CPUTask)]
  }
  deriving (Generic, ToJSON, FromJSON)

diffusionEntryToLatencyPerStake :: Int -> DiffusionEntry -> LatencyPerStake
diffusionEntryToLatencyPerStake nnodes DiffusionEntry{..} =
  LatencyPerStake
    { hash
    , latencies = bin $ diffusionLatencyPerStakeFraction nnodes (Time created) (map Time arrivals)
    }
 where
  bins = [0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.8, 0.9, 0.92, 0.94, 0.96, 0.98, 1]
  bin xs = map (\b -> (,b) $ fst <$> listToMaybe (dropWhile (\(_, x) -> x < b) xs)) bins

data DiffusionLatencyState = DiffusionLatencyState
  { chains :: !ChainsMap
  , diffusions :: !DiffusionLatencyMap
  , cpuTasks :: !(Map.Map NodeId [(DiffTime, CPUTask)])
  }

diffusionSampleModel :: P2PTopographyCharacteristics -> P2PTopography -> FilePath -> SampleModel PraosEvent DiffusionLatencyState
diffusionSampleModel p2pTopographyCharacteristics p2pTopography fp = SampleModel initState accum render
 where
  initState = DiffusionLatencyState IMap.empty Map.empty Map.empty
  accum t e DiffusionLatencyState{..} =
    DiffusionLatencyState
      { chains = accumChains t e chains
      , diffusions = accumDiffusionLatency t e diffusions
      , cpuTasks = accumCPUTasks t e cpuTasks
      }
  nnodes = p2pNumNodes p2pTopographyCharacteristics
  render DiffusionLatencyState{..} = do
    let stable_chain = fromMaybe Genesis $ do
          guard $ not $ IMap.null chains
          pure $ List.foldl1' aux (IMap.elems chains)
        aux c1 c2 = fromMaybe Genesis $ do
          p <- Chain.intersectChains c1 c2
          Chain.rollback p c1
    let stable_chain_hashes = coerce $ map blockHash $ Chain.toNewestFirst stable_chain
    let entries =
          [ DiffusionEntry
            { hash = coerce hash'
            , node_id = coerce i
            , created = coerce t
            , arrivals = coerce ts
            }
          | (hash', (_, i, t, ts)) <- Map.toList diffusions
          ]
    let diffusionData =
          DiffusionData
            { topography = p2pTopographyCharacteristics
            , topography_details = p2pTopography
            , entries
            , latency_per_stake = map (diffusionEntryToLatencyPerStake nnodes) entries
            , stable_chain_hashes
            , cpuTasks
            }

    encodeFile fp diffusionData
    putStrLn $ "Diffusion data written to " ++ fp

    let arrived98 = unzip [(l.hash, d) | l <- diffusionData.latency_per_stake, (Just d, p) <- l.latencies, p == 0.98]
    let missing = filter (not . (`elem` fst arrived98)) diffusionData.stable_chain_hashes
    putStrLn $ "Number of blocks that reached 98% stake: " ++ show (length $ fst arrived98)
    putStrLn $ "with a maximum diffusion latency: " ++ show (maximum $ snd arrived98)
    putStrLn $ "Blocks in longest common prefix that did not reach 98% stake: " ++ show missing

accumCPUTasks :: Time -> PraosEvent -> Map.Map NodeId [(DiffTime, CPUTask)] -> Map.Map NodeId [(DiffTime, CPUTask)]
accumCPUTasks (Time t) (PraosEventNode (LabelNode nId (PraosNodeEventCPU task))) = Map.insertWith (++) nId [(t, task)]
accumCPUTasks _ _ = id

-- | Diffusion example with 1000 nodes.
example1000Diffusion ::
  -- | number of close links
  Int ->
  -- | number of random links
  Int ->
  -- | when to stop simulation.
  Time ->
  -- | file to write data to.
  FilePath ->
  IO ()
example1000Diffusion clinks rlinks stop fp =
  runSampleModel traceFile logEvent (diffusionSampleModel p2pTopographyCharacteristics p2pTopography fp) stop $
    example1Trace rng 20 p2pTopography
 where
  asString x = x :: String
  logEvent (PraosEventNode (LabelNode nid (PraosNodeEventGenerate blk))) =
    Just $
      object ["nid" .= nid, "tag" .= asString "generated", "hash" .= show (coerce @_ @Int (blockHash blk))]
  logEvent (PraosEventNode (LabelNode nid (PraosNodeEventReceived blk))) =
    Just $
      object ["nid" .= nid, "tag" .= asString "received", "hash" .= show (coerce @_ @Int (blockHash blk))]
  logEvent (PraosEventNode (LabelNode nid (PraosNodeEventEnterState blk))) =
    Just $
      object ["nid" .= nid, "tag" .= asString "enterstate", "hash" .= show (coerce @_ @Int (blockHash blk))]
  logEvent (PraosEventNode (LabelNode nid (PraosNodeEventCPU task))) =
    Just $
      object ["nid" .= nid, "tag" .= asString "cpu", "task" .= task]
  logEvent _ = Nothing
  traceFile = dropExtension fp <.> "log"
  rng = mkStdGen 42
  p2pTopography = genArbitraryP2PTopography p2pTopographyCharacteristics rng
  p2pTopographyCharacteristics =
    P2PTopographyCharacteristics
      { p2pWorld =
          World
            { worldDimensions = (0.600, 0.300)
            , worldShape = Cylinder
            }
      , p2pNumNodes = 1000
      , p2pNodeLinksClose = clinks
      , p2pNodeLinksRandom = rlinks
      }

example1Trace :: StdGen -> DiffTime -> P2P.P2PTopography -> PraosTrace
example1Trace rng0 blockInterval p2pTopography =
  tracePraosP2P
    rng0
    p2pTopography
    (\latency -> mkTcpConnProps latency (kilobytes 1000))
    ( \slotConfig nid rng ->
        PraosNodeConfig
          { blockGeneration =
              PoissonGenerationPattern
                rng
                -- average seconds between blocks:
                (realToFrac blockInterval * fromIntegral p2pNumNodes)
          , slotConfig
          , praosConfig = defaultPraosConfig{blockFrequencyPerSlot = 1 / realToFrac blockInterval}
          , blockMarker = BS8.pack $ show nid ++ ": "
          , chain = Genesis
          }
    )
 where
  p2pNumNodes = Map.size $ p2pNodes p2pTopography

example2 :: Visualization
example2 =
  --  slowmoVisualization 0.2 $
  Viz (pairVizModel model1 model2) $
    LayoutAbove
      [ layoutLabel 18 "Flat vs cylindrical world topology"
      , LayoutReqSize 0 40 $
          layoutLabel 10 $
            "Left side is a flat rectangular world.\n"
              ++ "Right is a cylindrical world, i.e. the east and "
              ++ "west edges are connected."
      , layoutLabelTime
      , LayoutBeside
          [ contramap fst
              <$> LayoutAbove
                [ LayoutReqSize 900 600 $
                    Layout $
                      praosP2PSimVizRender config
                , LayoutBeside
                    [ LayoutReqSize 350 300 $
                        Layout $
                          chartDiffusionLatency config
                    , LayoutReqSize 350 300 $
                        Layout
                          chartLinkUtilisation
                    ]
                ]
          , contramap snd
              <$> LayoutAbove
                [ LayoutReqSize 900 600 $
                    Layout $
                      praosP2PSimVizRender config
                , LayoutBeside
                    [ LayoutReqSize 350 300 $
                        Layout $
                          chartDiffusionLatency config
                    , LayoutReqSize 350 300 $
                        Layout
                          chartLinkUtilisation
                    ]
                ]
          ]
      ]
 where
  model1 =
    model
      p2pTopographyCharacteristicsCommon
        { p2pWorld =
            p2pWorld
              { worldShape = Rectangle
              }
        }
  model2 = model p2pTopographyCharacteristicsCommon

  model p2pTopographyCharacteristics =
    praosSimVizModel trace
   where
    trace =
      tracePraosP2P
        rng0
        p2pTopography
        (\latency -> mkTcpConnProps latency (kilobytes 1000))
        ( \slotConfig nid rng ->
            PraosNodeConfig
              { blockGeneration =
                  PoissonGenerationPattern
                    rng
                    -- average seconds between blocks:
                    (5 * fromIntegral p2pNumNodes)
              , praosConfig = defaultPraosConfig
              , slotConfig
              , chain = Genesis
              , blockMarker = BS8.pack $ show nid ++ ": "
              }
        )
    p2pTopography =
      genArbitraryP2PTopography p2pTopographyCharacteristics rng0

  rng0 = mkStdGen 4 -- TODO: make a param
  p2pNumNodes = 100
  p2pWorld =
    World
      { worldDimensions = (0.600, 0.300)
      , worldShape = Cylinder
      }
  p2pTopographyCharacteristicsCommon =
    P2PTopographyCharacteristics
      { p2pWorld
      , p2pNumNodes
      , p2pNodeLinksClose = 5
      , p2pNodeLinksRandom = 5
      }

config :: PraosP2PSimVizConfig
config =
  PraosP2PSimVizConfig
    { nodeMessageColor = testNodeMessageColor
    , ptclMessageColor = testPtclMessageColor
    }
 where
  testPtclMessageColor ::
    PraosMessage BlockBody ->
    Maybe (Double, Double, Double)
  testPtclMessageColor msg =
    case msg of
      PraosMessage (Right bmsg@(ProtocolMessage (SomeMessage MsgBlock{}))) ->
        Just (blockFetchMessageColor examplesPraosSimVizConfig bmsg)
      _ -> Nothing

  testNodeMessageColor :: BlockHeader -> (Double, Double, Double)
  testNodeMessageColor = blockHeaderColorAsBody
