{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Bandit.Algorithm.UCB where

import Bandit.Algorithm.Types
import qualified Data.Vector as V
import GHC.Generics (Generic)

newtype UCB = UCB
  { ucbC :: Double
  }
  deriving (Show, Eq, Generic)

newUCB :: Double -> UCB
newUCB c = UCB (max 0 c)

selectArmUCB :: UCB -> V.Vector ArmStats -> Int -> Int
selectArmUCB UCB {..} stats _ =
  let totalCounts = fromIntegral $ V.sum (V.map armCount stats)
      ucbValues = V.imap (computeUCB totalCounts) stats
   in V.maxIndex ucbValues
  where
    computeUCB :: Double -> Int -> ArmStats -> Double
    computeUCB totalN idx ArmStats {..} =
      if armCount == 0
        then 1e10
        else armExpectation + ucbC * sqrt (2 * log (max 1 totalN) / fromIntegral armCount)

instance BanditAlgorithm UCB where
  initAlgorithm _ = newUCB
  selectArm = selectArmUCB
  algorithmName _ = "UCB"
