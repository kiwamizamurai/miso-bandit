{-# LANGUAGE DeriveGeneric #-}

module Bandit.Model where

import Bandit.Algorithm.Types (ArmStats, initArmStats)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

data AlgorithmResult = AlgorithmResult
  { algArmStats :: Vector ArmStats,
    algChosenArms :: Vector Int,
    algRewards :: Vector Double,
    algCumulativeRewards :: Vector Double
  }
  deriving (Show, Eq, Generic)

data Model = Model
  { arms :: Vector Double,
    epsilonGreedyResult :: AlgorithmResult,
    ucbResult :: AlgorithmResult,
    thompsonResult :: AlgorithmResult,
    algorithmParams :: AlgorithmParams,
    isRunning :: Bool,
    currentTrial :: Int,
    totalTrials :: Int,
    showResults :: Bool,
    randomSeed :: Int
  }
  deriving (Show, Eq, Generic)

data AlgorithmParams = AlgorithmParams
  { epsilon :: Double,
    ucbC :: Double
  }
  deriving (Show, Eq, Generic)

initAlgorithmResult :: Int -> AlgorithmResult
initAlgorithmResult nArms =
  AlgorithmResult
    { algArmStats = initArmStats nArms,
      algChosenArms = V.empty,
      algRewards = V.empty,
      algCumulativeRewards = V.empty
    }

initialModel :: Model
initialModel =
  let armProbs = V.fromList [0.1, 0.2, 0.3, 0.4, 0.5]
      nArms = V.length armProbs
   in Model
        { arms = armProbs,
          epsilonGreedyResult = initAlgorithmResult nArms,
          ucbResult = initAlgorithmResult nArms,
          thompsonResult = initAlgorithmResult nArms,
          algorithmParams = AlgorithmParams 0.15 2.0,
          isRunning = False,
          currentTrial = 0,
          totalTrials = 1000,
          showResults = False,
          randomSeed = 42
        }
