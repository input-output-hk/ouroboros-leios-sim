module Leios.Types where

import Data.ByteString (ByteString)
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
