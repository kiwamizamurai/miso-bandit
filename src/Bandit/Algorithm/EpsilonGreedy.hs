{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Bandit.Algorithm.EpsilonGreedy where

import Bandit.Algorithm.Types
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.Random (StdGen, mkStdGen, randomR)

newtype EpsilonGreedy = EpsilonGreedy
  { epsilon :: Double
  }
  deriving (Show, Eq, Generic)

newEpsilonGreedy :: Double -> EpsilonGreedy
newEpsilonGreedy eps = EpsilonGreedy (max 0 (min 1 eps))

-- | Uses seed for deterministic randomness in pure context
selectArmEpsilonGreedy :: EpsilonGreedy -> V.Vector ArmStats -> Int -> Int
selectArmEpsilonGreedy EpsilonGreedy {..} stats seed =
  let gen = mkStdGen seed
      (randVal, gen') = randomR (0.0, 1.0) gen :: (Double, StdGen)
      nArms = V.length stats
   in if nArms == 0
        then 0
        else
          if randVal < epsilon
            then
              let (armIdx, _) = randomR (0, nArms - 1) gen'
               in armIdx
            else
              let expectations = V.map armExpectation stats
               in if V.null expectations || V.all (== 0) expectations
                    then fst $ randomR (0, nArms - 1) gen'
                    else V.maxIndex expectations

instance BanditAlgorithm EpsilonGreedy where
  initAlgorithm _ = newEpsilonGreedy
  selectArm = selectArmEpsilonGreedy
  algorithmName _ = "ε-Greedy"
