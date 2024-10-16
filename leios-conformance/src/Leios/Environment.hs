module Leios.Environment where

import Control.Concurrent.STM (TChan, TVar)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Leios.Types (
  Body,
  Header (..),
  HeaderId,
  Peer (..),
  Slot (..),
  SlotRate,
  Timestamp,
  hash,
  mkProof,
  mkSignature,
 )
import Test.QuickCheck (Arbitrary (..), Gen, choose, sized, vectorOf)

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

data Network (m :: Type -> Type) = Network
  { fetchHeaders :: !(m [Header])
  , diffuseHeader :: !(Header -> m ())
  }

mkNetwork :: TVar NetworkState -> Network IO
mkNetwork stateRef =
  Network
    { fetchHeaders = atomically $ modifyTVar stateRef $ \state ->
        let headers = Set.map content (headers state)
         in (state, headers)
    , diffuseHeader = \_ -> pure ()
    }

-- * Generators

generateValidHeaders :: SlotRate -> Gen [Header]
generateValidHeaders f = sized $ \n ->
  catMaybes <$> mapM (generateValidHeaderAt f) [1 .. fromIntegral n]

generateValidHeaderAt :: SlotRate -> Integer -> Gen (Maybe Header)
generateValidHeaderAt f s = do
  p <- choose (0, 1)
  if p < f
    then do
      let slot = Slot s
      peer <- Peer <$> arbitrary
      let lotteryProof = mkProof slot peer
      body <- genBody
      let bodyHash = hash body
      let signature = mkSignature peer body
      pure $ Just Header{slot, peer, lotteryProof, bodyHash, signature}
    else pure Nothing

genBody :: Gen ByteString
genBody = BS.pack <$> vectorOf 16 arbitrary
