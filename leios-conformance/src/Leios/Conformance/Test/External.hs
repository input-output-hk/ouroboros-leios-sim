{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Leios.Conformance.Test.External where

import Control.Monad.IO.Class ()
import Control.Monad.State (
  MonadState (get),
  MonadTrans (lift),
  StateT,
  modify,
 )
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Default (Default (def))
import GHC.Generics (Generic)
import System.IO (Handle)
import Test.QuickCheck (
  Blind (Blind),
  Property,
  counterexample,
  ioProperty,
  noShrinking,
 )
import Test.QuickCheck.Extras (runPropertyStateT)
import Test.QuickCheck.Monadic (monadicIO, monitor)
import Test.QuickCheck.StateModel (
  Actions,
  Realized,
  RunModel (perform, postcondition),
  runActions
 )
import Prelude hiding (round)

import Leios.Conformance.Model
import Leios.Conformance.Test

data NodeRequest
  = Initialize
  | NewSlot
      { newIBs :: [InputBlock]
      , newEBs :: [EndorserBlock]
      }
  | Stop
  deriving (Eq, Generic, Show)

instance FromJSON NodeRequest
instance ToJSON NodeRequest

data NodeResponse
  = NodeResponse
      { diffuseIBs :: [InputBlock]
      , diffuseEBs :: [EndorserBlock]
      }
  | Failed
      { failure :: String
      }
  | Stopped
  deriving (Eq, Generic, Show)

instance Default NodeResponse where
  def = NodeResponse mempty mempty

instance FromJSON NodeResponse
instance ToJSON NodeResponse

data RunState = RunState
  { hReader :: Handle
  , hWriter :: Handle
  , unfetchedIBs :: [InputBlock]
  , unfetchedEBs :: [EndorserBlock]
  }

callSUT :: RunState -> NodeRequest -> IO NodeResponse
callSUT RunState{hReader, hWriter} req =
  do LBS8.hPutStrLn hWriter $ A.encode req
     either Failed id . A.eitherDecode' . LBS.fromStrict <$> BS8.hGetLine hReader

type Runtime = StateT RunState IO

instance Realized IO ([InputBlock], [EndorserBlock]) ~ ([InputBlock], [EndorserBlock]) => RunModel NetworkModel Runtime where

  perform net@NetworkModel{nodeModel = NodeModel{..}} (Step a) _ = case a of
    Tick -> do
      rs@RunState{..} <- get
      modify $ \rs' -> rs' {unfetchedIBs = mempty, unfetchedEBs = mempty}
      lift
        ( callSUT
            rs
            NewSlot
              { newIBs = unfetchedIBs
              , newEBs = unfetchedEBs
              }
        )
        >>= \case
          NodeResponse{..} -> pure (diffuseIBs, diffuseEBs)
          _ -> pure (mempty, mempty) -- FIXME: The state model should define an error type.
    NewIB ib -> do
      modify $ \rs -> rs{unfetchedIBs = unfetchedIBs rs ++ pure ib}
      pure (mempty, mempty)
    NewEB eb -> do
      modify $ \rs -> rs{unfetchedEBs = unfetchedEBs rs ++ pure eb}
      pure (mempty, mempty)

  postcondition (net@NetworkModel{nodeModel = s}, NetworkModel{nodeModel = s'}) (Step a) _ (ibs, ebs) = do
    pure False -- FIXME: just fail for now

prop_node :: Handle -> Handle -> Blind (Actions NetworkModel) -> Property
prop_node hReader hWriter (Blind as) = noShrinking $
  ioProperty $ do
    let unfetchedIBs = mempty
        unfetchedEBs = mempty
    callSUT
      RunState{..}
      Initialize
      >>= \case
        NodeResponse{} ->
          pure $
            monadicIO $ do
              monitor $ counterexample "-- Actions:"
              monitor $ counterexample "do"
              _ <- runPropertyStateT (runActions @_ @Runtime as) RunState{..}
              pure True
        _ -> pure $ monadicIO $ pure False
