{-# LANGUAGE DeriveGeneric #-}

module Bandit.Actions where

import GHC.Generics (Generic)

data AlgorithmStepResult = AlgorithmStepResult
  { stepArm :: Int,
    stepReward :: Double
  }
  deriving (Show, Eq, Generic)

data Action
  = NoOp
  | Start
  | Stop
  | Reset
  | Step
  | RunStep
  | UpdateAllAlgorithms AlgorithmStepResult AlgorithmStepResult AlgorithmStepResult
  | SetEpsilon Double
  | SetUCBC Double
  | SetTrials Int
  | ToggleResults
  | SetArmProbability Int Double
  deriving (Show, Eq, Generic)
