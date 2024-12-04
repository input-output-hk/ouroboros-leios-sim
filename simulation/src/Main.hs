{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Maybe (fromMaybe)
import qualified ExamplesRelay
import qualified ExamplesRelayP2P
import qualified ExamplesTCP
import qualified LeiosProtocol.VizSimTestRelay as VizSimTestRelay
import Options.Applicative
import qualified PraosProtocol.ExamplesPraosP2P as VizPraosP2P
import qualified PraosProtocol.VizSimBlockFetch as VizBlockFetch
import qualified PraosProtocol.VizSimChainSync as VizChainSync
import qualified PraosProtocol.VizSimPraos as VizPraos
import TimeCompat (DiffTime)
import Topology (readP2PTopography, readSimpleTopologyFromBenchTopologyAndLatency, writeSimpleTopology)
import Viz

main :: IO ()
main = do
  customExecParser parserPrefs options >>= \case
    CliCommand CliOptions{..} ->
      case cliCommand of
        CliConvertBenchTopology{..} -> do
          simpleTopology <- readSimpleTopologyFromBenchTopologyAndLatency inputBenchTopology inputBenchLatencies
          writeSimpleTopology outputSimpleTopology simpleTopology
    VizCommand opt@VizOptions{..} -> do
      viz <- vizOptionsToViz opt
      case vizOutputFramesDir of
        Nothing ->
          let gtkVizConfig =
                defaultGtkVizConfig
                  { gtkVizCpuRendering = vizCpuRendering
                  , gtkVizResolution = vizSize
                  }
           in vizualise gtkVizConfig viz
        Just outputFramesDir ->
          let animVizConfig =
                defaultAnimVizConfig
                  { animVizFrameFiles = \n -> outputFramesDir ++ "/frame-" ++ show n ++ ".png"
                  , animVizDuration = fromMaybe 60 vizOutputSeconds
                  , animVizStartTime = fromMaybe 0 vizOutputStartTime
                  , animVizResolution = vizSize
                  }
           in writeAnimationFrames animVizConfig viz

parserPrefs :: ParserPrefs
parserPrefs =
  prefs . mconcat $
    [ showHelpOnEmpty
    , helpShowGlobals
    ]

options :: ParserInfo Options
options = info (parserOptions <**> helper) mempty

data Options
  = VizCommand VizOptions
  | CliCommand CliOptions

parserOptions :: Parser Options
parserOptions = VizCommand <$> parserVizOptions <|> CliCommand <$> parserCliOptions

newtype CliOptions = CliOptions
  { cliCommand :: CliCommand
  }

data CliCommand
  = CliConvertBenchTopology {inputBenchTopology :: FilePath, inputBenchLatencies :: FilePath, outputSimpleTopology :: FilePath}

parserCliOptions :: Parser CliOptions
parserCliOptions =
  CliOptions
    <$> parserCliCommand

parserCliCommand :: Parser CliCommand
parserCliCommand =
  subparser . mconcat $
    [ commandGroup "Available utility commands:"
    , command "convert-bench-topology" . info (parserCliConvertBenchTopology <**> helper) $
        progDesc "Convert the benchmark topology files to a simple topology file."
    ]

parserCliConvertBenchTopology :: Parser CliCommand
parserCliConvertBenchTopology =
  CliConvertBenchTopology
    <$> strOption
      ( long "input-bench-topology"
          <> short 't'
          <> metavar "FILE"
          <> help "The input topology file."
      )
    <*> strOption
      ( long "input-bench-latencies"
          <> short 'l'
          <> metavar "FILE"
          <> help "The input latencies database."
      )
    <*> strOption
      ( long "output-simple-topology"
          <> short 'o'
          <> metavar "FILE"
          <> help "The output topology file."
      )

data VizOptions = VizOptions
  { vizCommand :: VizCommand
  , vizOutputFramesDir :: Maybe FilePath
  , vizOutputSeconds :: Maybe Int
  , vizOutputStartTime :: Maybe Int
  , vizCpuRendering :: Bool
  , vizSize :: Maybe VizSize
  }

parserVizOptions :: Parser VizOptions
parserVizOptions =
  VizOptions
    <$> parserVizCommand
    <*> optional
      ( strOption
          ( long "frames-dir"
              <> metavar "DIR"
              <> help "Output animation frames to directory"
          )
      )
    <*> optional
      ( option
          auto
          ( long "seconds"
              <> metavar "SEC"
              <> help "Output N seconds of animation"
          )
      )
    <*> optional
      ( option
          auto
          ( long "skip-seconds"
              <> metavar "SEC"
              <> help "Skip the first N seconds of animation"
          )
      )
    <*> switch
      ( long "cpu-render"
          <> help "Use CPU-based client side Cairo rendering"
      )
    <*> optional vizSizeOptions

data VizCommand
  = VizTCP1
  | VizTCP2
  | VizTCP3
  | VizRelay1
  | VizRelay2
  | VizP2P1
  | VizP2P2
  | VizPCS1
  | VizPBF1
  | VizPraos1
  | VizPraosP2P1 {seed :: Int, blockInterval :: DiffTime, maybeTopologyFile :: Maybe FilePath}
  | VizPraosP2P2
  | VizRelayTest1
  | VizRelayTest2
  | VizRelayTest3

parserVizCommand :: Parser VizCommand
parserVizCommand =
  subparser . mconcat $
    [ commandGroup "Available visualisations:"
    , command "tcp-1" . info (pure VizTCP1) $
        progDesc ""
    , command "tcp-2" . info (pure VizTCP2) $
        progDesc ""
    , command "tcp-3" . info (pure VizTCP3) $
        progDesc ""
    , command "relay-1" . info (pure VizRelay1) $
        progDesc ""
    , command "relay-2" . info (pure VizRelay2) $
        progDesc ""
    , command "p2p-1" . info (pure VizP2P1) $
        progDesc ""
    , command "p2p-2" . info (pure VizP2P2) $
        progDesc ""
    , command "pcs-1" . info (pure VizPCS1) $
        progDesc
          "A simulation of two nodes running Praos chain-sync."
    , command "pbf-1" . info (pure VizPBF1) $
        progDesc
          "A simulation of two nodes running Praos block-fetch."
    , command "praos-1" . info (pure VizPraos1) $
        progDesc
          "A simulation of two nodes running Praos consensus."
    , command "praos-p2p-1" . info (parserPraosP2P1 <**> helper) $
        progDesc
          "A simulation of 100 nodes running Praos consensus."
    , command "praos-p2p-2" . info (pure VizPraosP2P2) $
        progDesc
          "A simulation of 100 nodes running Praos consensus, \
          \comparing a cylindrical world to a flat world."
    , command "relay-test-1" . info (pure VizRelayTest1) $
        progDesc ""
    , command "relay-test-2" . info (pure VizRelayTest2) $
        progDesc ""
    , command "relay-test-3" . info (pure VizRelayTest3) $
        progDesc ""
    ]

parserPraosP2P1 :: Parser VizCommand
parserPraosP2P1 =
  VizPraosP2P1
    <$> option
      auto
      ( long "seed"
          <> metavar "NUMBER"
          <> help "The seed for the random number generator."
          <> value 0
      )
    <*> option
      (fmap (fromIntegral @Int) auto)
      ( long "block-interval"
          <> metavar "NUMBER"
          <> help "The interval at which blocks are generated."
          <> value 5
      )
    <*> optional
      ( option
          str
          ( long "topology"
              <> metavar "FILE"
              <> help "The file describing the network topology."
          )
      )

vizOptionsToViz :: VizOptions -> IO Vizualisation
vizOptionsToViz VizOptions{..} = case vizCommand of
  VizTCP1 -> pure ExamplesTCP.example1
  VizTCP2 -> pure ExamplesTCP.example2
  VizTCP3 -> pure ExamplesTCP.example3
  VizRelay1 -> pure ExamplesRelay.example1
  VizRelay2 -> pure ExamplesRelay.example2
  VizP2P1 -> pure ExamplesRelayP2P.example1
  VizP2P2 -> pure ExamplesRelayP2P.example2
  VizPCS1 -> pure VizChainSync.example1
  VizPBF1 -> pure VizBlockFetch.example1
  VizPraos1 -> pure VizPraos.example1
  VizPraosP2P1{..} -> do
    let worldDimensions = (1200, 1000)
    maybeP2PTopography <- traverse (readP2PTopography worldDimensions) maybeTopologyFile
    pure $ VizPraosP2P.example1 seed blockInterval maybeP2PTopography
  VizPraosP2P2 -> pure VizPraosP2P.example2
  VizRelayTest1 -> pure VizSimTestRelay.example1
  VizRelayTest2 -> pure VizSimTestRelay.example2
  VizRelayTest3 -> pure VizSimTestRelay.example3

type VizSize = (Int, Int)

vizSizeOptions :: Parser VizSize
vizSizeOptions =
  flag'
    (1280, 720)
    ( long "720p"
        <> help "Use 720p resolution"
    )
    <|> flag'
      (1920, 1080)
      ( long "1080p"
          <> help "Use 1080p resolution"
      )
    <|> option
      auto
      ( long "resolution"
          <> metavar "(W,H)"
          <> help "Use a specific resolution"
      )
