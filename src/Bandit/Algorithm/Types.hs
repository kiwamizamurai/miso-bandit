{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}

module Bandit.Algorithm.Types where

import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

data Arm = Arm
  { armId :: Int,
    armProbability :: Double
  }
  deriving (Show, Eq, Generic)

data ArmStats = ArmStats
  { armCount :: Int,
    armRewards :: Double,
    armExpectation :: Double
  }
  deriving (Show, Eq, Generic)

data SelectionResult = SelectionResult
  { selectedArm :: Int,
    reward :: Double
  }
  deriving (Show, Eq, Generic)

class BanditAlgorithm a where
  initAlgorithm :: Int -> Double -> a
  selectArm :: a -> Vector ArmStats -> Int -> Int
  algorithmName :: a -> String

initArmStats :: Int -> Vector ArmStats
initArmStats n = V.replicate n $ ArmStats 0 0.0 0.0

updateArmStats :: Vector ArmStats -> Int -> Double -> Vector ArmStats
updateArmStats stats armIdx reward =
  let currentStats = stats V.! armIdx
      newCount = armCount currentStats + 1
      newRewards = armRewards currentStats + reward
      newExpectation = newRewards / fromIntegral newCount
      newStats = ArmStats newCount newRewards newExpectation
   in stats V.// [(armIdx, newStats)]
