-- | Model for network diffusion as defined in the Leios paper (appendix A)
module Leios.Diffusion where

import Control.Monad (forM_, forever)
import Leios.Environment (Network (..))
import Leios.Types (Header)

runNode :: Network IO -> IO ()
runNode network = forever $ diffuseHeaders network

diffuseHeaders :: Network IO -> IO ()
diffuseHeaders network = do
  headers <- fetchHeaders network
  forM_ (filter verifyHeader headers) $ \header -> do
    diffuseHeader network header

verifyHeader :: Header -> Bool
verifyHeader = const True
