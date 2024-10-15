module Leios.DiffusionSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec =
  prop "a node diffuses all valid received headers" prop_nodeDiffusesAllValidReceivedHeaders

prop_nodeDiffusesAllValidReceivedHeaders :: Property
prop_nodeDiffusesAllValidReceivedHeaders =
  forAll
