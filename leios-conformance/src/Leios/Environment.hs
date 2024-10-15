module Leios.Environment where

import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Leios.Types (Body, Header, HeaderId, Peer, Timestamp)

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
