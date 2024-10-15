module Leios.DiffusionSpec where

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  prop "a node diffuses all valid received headers" prop_nodeDiffusesAllValidReceivedHeaders
