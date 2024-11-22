{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
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
import Viz

main :: IO ()
main = do
  VizOptions{..} <- execParser (info (helper <*> vizOptions) mempty)
  let viz = vizCommandToViz vizCommand
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

data VizOptions = VizOptions
  { vizCommand :: VizCommand
  , vizOutputFramesDir :: Maybe FilePath
  , vizOutputSeconds :: Maybe Int
  , vizOutputStartTime :: Maybe Int
  , vizCpuRendering :: Bool
  , vizSize :: Maybe VizSize
  }

vizOptions :: Parser VizOptions
vizOptions =
  VizOptions
    <$> vizCommands
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
  | VizPraosP2P1
  | VizPraosP2P2
  | VizRelayTest1
  | VizRelayTest2
  | VizRelayTest3

vizCommands :: Parser VizCommand
vizCommands =
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
    , command "praos-p2p-1" . info (pure VizPraosP2P1) $
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

vizCommandToViz :: VizCommand -> Vizualisation
vizCommandToViz = \case
  VizTCP1 -> ExamplesTCP.example1
  VizTCP2 -> ExamplesTCP.example2
  VizTCP3 -> ExamplesTCP.example3
  VizRelay1 -> ExamplesRelay.example1
  VizRelay2 -> ExamplesRelay.example2
  VizP2P1 -> ExamplesRelayP2P.example1
  VizP2P2 -> ExamplesRelayP2P.example2
  VizPCS1 -> VizChainSync.example1
  VizPBF1 -> VizBlockFetch.example1
  VizPraos1 -> VizPraos.example1
  VizPraosP2P1 -> VizPraosP2P.example1
  VizPraosP2P2 -> VizPraosP2P.example2
  VizRelayTest1 -> VizSimTestRelay.example1
  VizRelayTest2 -> VizSimTestRelay.example2
  VizRelayTest3 -> VizSimTestRelay.example3

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
