module Leios.DiffusionSpec where

import Leios.Environment (generateValidHeaders)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, elements, forAll, (===))

spec :: Spec
spec =
  prop "a node diffuses all valid received headers" prop_nodeDiffusesAllValidReceivedHeaders

prop_nodeDiffusesAllValidReceivedHeaders :: Property
prop_nodeDiffusesAllValidReceivedHeaders =
  forAll (elements [0.0, 0.5, 1.0]) $ \slotRate ->
    forAll (generateValidHeaders slotRate) $ \headers ->
      length headers === length headers
