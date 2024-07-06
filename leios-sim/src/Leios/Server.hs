{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Leios.Server where

import Control.Concurrent.Class.MonadSTM (MonadSTM (modifyTVar), TVar, atomically, newBroadcastTChanIO, writeTChan, newTVarIO, stateTVar, readTVarIO, writeTVar)
import Control.Concurrent.Class.MonadSTM.TChan (TChan, dupTChan, readTChan)
import Control.Concurrent.Class.MonadSTM.TQueue
import Control.Monad (forever)
import Control.Monad.Class.MonadAsync (race_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as Text
import Control.Concurrent (threadDelay)
import Control.Exception (handle, SomeException, throw)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.WebSockets as WS
import qualified Web.Scotty as Sc
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- FIXME: import explicitly
import Leios.Model (Parameters (..), BitsPerSecond (..), NumberOfSlots (..), NumberOfSlices (..), BitsPerSecond (..), NumberOfBits (..), IBFrequency (..), EBFrequency (..), ShouldContinue(..))
import qualified Leios.Model as Model
import Leios.Trace (mkTracer)
import Network.HTTP.Types.Status (badRequest400)

--------------------------------------------------------------------------------
-- Server state
--------------------------------------------------------------------------------

-- FIXME: before deploying we should use other types, like random hashes or UUIDs
type SessionId = Int

nextId :: MonadSTM m => ServerState m -> m SessionId
nextId ServerState { nextIdTVar } =
  atomically $ stateTVar nextIdTVar (\i -> (i, i+1))

-- | Create a new session with the given parameters. Return the id of the new session.
newSession :: MonadSTM m =>
  ServerState m
  -> TVar m Parameters
  -> TVar m ShouldContinue
  -> m SessionId
newSession state paramsTVar continueTVar = do
  sid <- nextId state
  let clientState = ClientState {
        paramsTVar = paramsTVar, continueTVar = continueTVar
        }
  atomically $ modifyTVar (sessionsTVar state) (Map.insert sid clientState) -- We could assert the session does not exist in the map.
  pure sid

lookupParams :: MonadSTM m => SessionId -> ServerState m -> m (Maybe Parameters)
lookupParams sid serverState = do
  mParamsTVar <- lookupParamsTVar sid serverState
  maybe (pure Nothing) (fmap Just . readTVarIO) $ mParamsTVar

lookupParamsTVar :: MonadSTM m => SessionId -> ServerState m -> m (Maybe (TVar m Parameters))
lookupParamsTVar sid serverState =
  fmap (fmap paramsTVar) $ lookupClientState sid serverState

lookupClientState :: MonadSTM m => SessionId -> ServerState m -> m (Maybe (ClientState m))
lookupClientState sid ServerState { sessionsTVar } = do
  sessions <- readTVarIO sessionsTVar
  pure $ Map.lookup sid sessions

lookupContinueTVar :: MonadSTM m => SessionId -> ServerState m
  -> m (Maybe (TVar m ShouldContinue))
lookupContinueTVar sid serverState  = do
  fmap (fmap continueTVar) $ lookupClientState sid serverState

data ServerState m  = ServerState {
  sessionsTVar :: TVar m (Map SessionId (ClientState m)),
  nextIdTVar :: TVar m Int
  }

data ClientState m = ClientState {
  paramsTVar :: TVar m Parameters,
  continueTVar :: TVar m ShouldContinue
  }

mkServerState :: (Monad m, MonadSTM m) => m (ServerState m)
mkServerState = do
  sessionsTVar <- newTVarIO mempty
  nextIdTVar <- newTVarIO 0
  pure $ ServerState { sessionsTVar = sessionsTVar, nextIdTVar = nextIdTVar }

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

runServer :: IO ()
runServer = do
  let port = 8080
  let settings = Warp.setPort port Warp.defaultSettings
  serverState <- mkServerState
  sapp <- scottyApp serverState
  Warp.runSettings
    settings
    (WS.websocketsOr
      WS.defaultConnectionOptions
      (wsapp serverState)
      sapp
    )

feedClient :: MonadSTM m => TQueue m Value -> TChan m Value -> m ()
feedClient input output = forever $ do
  atomically $ do
    readTQueue input >>= writeTChan output

scottyApp :: ServerState IO -> IO Wai.Application
scottyApp serverState =
  Sc.scottyApp $ do
    Sc.middleware logStdoutDev

    Sc.get "/" $
      Sc.redirect "/index.html"

    Sc.get "/index.html" $
      Sc.file "index.html"

    Sc.get "/index.js" $
      Sc.file "index.js"

    Sc.get "/leios.css" $
      Sc.file "leios.css"

    Sc.get "/api/parameters" $ do
      sid <- Sc.queryParam "sessionId"
      mParams <- liftIO $ lookupParams sid serverState
      case mParams of
        Nothing -> Sc.status badRequest400
        Just params -> Sc.json params

    Sc.post "/api/start-simulation" $ do
      -- TODO: starting the simulation twice will only set the
      -- parameters, but it will not restart it. We might want to change this.
      sid <- Sc.queryParam "sessionId"
      params <- Sc.jsonData
      mClientState <- liftIO $ lookupClientState sid serverState
      case mClientState of
        Nothing -> Sc.status badRequest400
        Just clientState -> do
          liftIO $ atomically $ writeTVar (paramsTVar clientState) params
          liftIO $ atomically $ writeTVar (continueTVar clientState) Continue
      liftIO $ putStrLn $ show params
      pure ()

    Sc.post "/api/stop-simulation" $ do
      sid <- Sc.queryParam "sessionId"
      mContinueTVar <- liftIO $ lookupContinueTVar sid serverState
      case mContinueTVar of
        Nothing -> Sc.status badRequest400
        Just continueTVar ->
          liftIO $ atomically $ writeTVar continueTVar Stop

    Sc.post "/api/node-bandwidth" $ do
      -- bps <- Sc.jsonData
      -- TODO: modify parameters
      (id :: SessionId) <- Sc.queryParam "sessionId"
      liftIO $ print id
      pure ()
      -- liftIO $
      --   atomically $
      --     modifyTVar params (\p -> p{nodeBandwidth = BitsPerSecond bps})

    Sc.post "/api/lambda" $ do
      λ <- Sc.jsonData
      sid <- Sc.queryParam "sessionId"
      -- TODO: modify parameters
      mParamsTVar <- liftIO $ lookupParamsTVar sid serverState
      case mParamsTVar of
        Nothing -> Sc.status badRequest400
        Just paramsTVar ->
          liftIO $ atomically $ modifyTVar paramsTVar (\p -> p{λ})

wsapp :: ServerState IO -> WS.ServerApp
wsapp serverState pending = do
  conn <- WS.acceptRequest pending

  paramsTVar <- newTVarIO defaultParams
  continueTVar <- newTVarIO Stop
  sid <- newSession serverState paramsTVar continueTVar
  -- For now we send the session ID it this way. We can make this more robust if needed.
  WS.sendTextData conn $  "{ \"tag\": \"SessionId\", \"sessionId\": "
                       <>  Text.pack (show sid)
                       <> " }"

  WS.withPingThread conn 30 (pure ()) $ do
    eventQueue <- newTQueueIO
    clientChannel <- newBroadcastTChanIO
    clientQueue <- atomically $ dupTChan clientChannel

    -- raceAll could be moved to some `Utils` package if we want to use it here.
    handle cleanup $
        Model.raceAll
          [ feedClient eventQueue clientChannel
          , Model.run (mkTracer eventQueue) paramsTVar continueTVar
          , forever $ do
              msg <- atomically $ readTChan clientQueue
              WS.sendTextData conn $ decodeUtf8 $ encode msg
          ]
  where
    cleanup :: SomeException -> IO ()
    cleanup e = putStrLn "TODO: perform cleanup." >> throw e

    defaultParams =
        Parameters
          { _L = NumberOfSlots 4
          , λ = NumberOfSlices 3
          , nodeBandwidth = BitsPerSecond 1000
          , ibSize = NumberOfBits 300
          , f_I = IBFrequency 5
          , f_E = EBFrequency 1
          , initialSeed = 22595838
          }
