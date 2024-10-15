{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Model for network diffusion as defined in the Leios paper (appendix A)
module Leios.Diffusion where

import Control.Monad (forM_, forever)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)

newtype Timestamp = Timestamp Integer
  deriving (Show, Eq, Ord)

newtype HeaderId = HeaderId (Slot, Peer)
  deriving (Show, Eq, Ord)

newtype Peer = Peer Integer
  deriving (Show, Eq, Ord)

newtype Slot = Slot Integer
  deriving (Show, Eq, Ord)

newtype Proof = Proof ByteString
  deriving (Show, Eq, Ord)

newtype Hash = Hash ByteString
  deriving (Show, Eq, Ord)

hash :: ByteString -> Hash
hash = Hash

newtype Signature = Signature ByteString
  deriving (Show, Eq, Ord)

-- from p.8
data Header = Header
  { slot :: !Slot
  , peer :: !Peer
  , lotteryProof :: !Proof
  , bodyHash :: !Hash
  , signature :: !Signature
  }
  deriving (Show, Eq, Ord)

msgID :: Header -> HeaderId
msgID Header{slot, peer} = HeaderId (slot, peer)

newtype Body = Body ByteString
  deriving (Show, Eq, Ord)

match :: Header -> Body -> Bool
match Header{bodyHash} (Body body) = bodyHash == hash body

valid :: Header -> Body -> Bool
valid hdr@Header{signature, lotteryProof, peer} (Body body) =
  verifyProof lotteryProof (msgID hdr)
    && verifySignature peer signature body

verifySignature :: Peer -> Signature -> ByteString -> Bool
verifySignature (Peer pid) (Signature bytes) body =
  encodeUtf8 (pack $ show (pid, body)) == bytes

verifyProof :: Proof -> HeaderId -> Bool
verifyProof (Proof bytes) (HeaderId (s, p)) =
  encodeUtf8 (pack $ show (s, p)) == bytes

data Diffused h = Diffused
  { diffusionTime :: !Timestamp
  , peer :: !(Set.Set Peer)
  , content :: !h
  }
  deriving (Show, Eq, Ord)

data NetworkState = NetworkState
  { headers :: !(Set.Set (Diffused Header))
  , bodies :: !(Set.Set (Diffused (Header, Body)))
  , prefHeader :: !(Map.Map Peer (Map.Map HeaderId Header))
  }
  deriving (Show)

runNode :: Network IO -> IO ()
runNode network = forever $ diffuseHeaders network

diffuseHeaders :: Network IO -> IO ()
diffuseHeaders network = do
  headers <- fetchHeaders network
  forM_ (filter verifyHeader headers) $ \header -> do
    diffuseHeader network header

verifyHeader :: Header -> Bool
verifyHeader = const True

data Network (m :: Type -> Type) = Network
  { fetchHeaders :: !(m [Header])
  , diffuseHeader :: !(Header -> m ())
  }
