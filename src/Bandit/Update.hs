{-# LANGUAGE RecordWildCards #-}

module Bandit.Update where

import Bandit.Actions
import qualified Bandit.Algorithm.EpsilonGreedy as EG
import qualified Bandit.Algorithm.ThompsonSampling as TS
import Bandit.Algorithm.Types hiding (updateArmStats)
import qualified Bandit.Algorithm.UCB as UCB
import Bandit.Model
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Vector (Vector, (//))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Miso
import Miso.Effect
import System.Random

updateModel :: Action -> Transition Model Action
updateModel NoOp = pure ()
updateModel Start = do
  model <- get
  put model {isRunning = True}
  scheduleIO (pure RunStep)
updateModel Stop = do
  model <- get
  put model {isRunning = False}
  pure ()
updateModel Reset = do
  put initialModel
  pure ()
updateModel Step = do
  model <- get
  when (not (isRunning model) && currentTrial model < totalTrials model) $
    scheduleIO (pure RunStep)
updateModel RunStep = do
  model@Model {..} <- get
  if currentTrial >= totalTrials
    then do
      put model {isRunning = False}
      pure ()
    else do
      let seedEG = randomSeed + currentTrial * 3
          seedUCB = randomSeed + currentTrial * 3 + 1
          seedTS = randomSeed + currentTrial * 3 + 2

          armEG =
            EG.selectArmEpsilonGreedy
              (EG.newEpsilonGreedy (epsilon algorithmParams))
              (algArmStats epsilonGreedyResult)
              seedEG
          safeArmEG = max 0 (min (V.length arms - 1) armEG)

          armUCB =
            UCB.selectArmUCB
              (UCB.newUCB (ucbC algorithmParams))
              (algArmStats ucbResult)
              seedUCB
          safeArmUCB = max 0 (min (V.length arms - 1) armUCB)

          armTS =
            TS.selectArmThompsonSampling
              TS.newThompsonSampling
              (algArmStats thompsonResult)
              seedTS
          safeArmTS = max 0 (min (V.length arms - 1) armTS)

          newModel = model {currentTrial = currentTrial + 1, randomSeed = randomSeed + 10}

      put newModel
      scheduleIO $ do
        rewardEG <- liftIO $ getReward (arms V.! safeArmEG)
        rewardUCB <- liftIO $ getReward (arms V.! safeArmUCB)
        rewardTS <- liftIO $ getReward (arms V.! safeArmTS)

        return $
          UpdateAllAlgorithms
            (AlgorithmStepResult safeArmEG rewardEG)
            (AlgorithmStepResult safeArmUCB rewardUCB)
            (AlgorithmStepResult safeArmTS rewardTS)
updateModel (UpdateAllAlgorithms egStepResult ucbStepResult tsStepResult) = do
  model@Model {..} <- get

  let egStats = updateArmStats (stepArm egStepResult) (stepReward egStepResult) (algArmStats epsilonGreedyResult)
      egNewResult =
        epsilonGreedyResult
          { algArmStats = egStats,
            algChosenArms = V.snoc (algChosenArms epsilonGreedyResult) (stepArm egStepResult),
            algRewards = V.snoc (algRewards epsilonGreedyResult) (stepReward egStepResult),
            algCumulativeRewards =
              V.snoc
                (algCumulativeRewards epsilonGreedyResult)
                ( if V.null (algCumulativeRewards epsilonGreedyResult)
                    then stepReward egStepResult
                    else V.last (algCumulativeRewards epsilonGreedyResult) + stepReward egStepResult
                )
          }

  let ucbStats = updateArmStats (stepArm ucbStepResult) (stepReward ucbStepResult) (algArmStats ucbResult)
      ucbNewResult =
        ucbResult
          { algArmStats = ucbStats,
            algChosenArms = V.snoc (algChosenArms ucbResult) (stepArm ucbStepResult),
            algRewards = V.snoc (algRewards ucbResult) (stepReward ucbStepResult),
            algCumulativeRewards =
              V.snoc
                (algCumulativeRewards ucbResult)
                ( if V.null (algCumulativeRewards ucbResult)
                    then stepReward ucbStepResult
                    else V.last (algCumulativeRewards ucbResult) + stepReward ucbStepResult
                )
          }

  let tsStats = updateArmStats (stepArm tsStepResult) (stepReward tsStepResult) (algArmStats thompsonResult)
      tsNewResult =
        thompsonResult
          { algArmStats = tsStats,
            algChosenArms = V.snoc (algChosenArms thompsonResult) (stepArm tsStepResult),
            algRewards = V.snoc (algRewards thompsonResult) (stepReward tsStepResult),
            algCumulativeRewards =
              V.snoc
                (algCumulativeRewards thompsonResult)
                ( if V.null (algCumulativeRewards thompsonResult)
                    then stepReward tsStepResult
                    else V.last (algCumulativeRewards thompsonResult) + stepReward tsStepResult
                )
          }

  let updatedModel =
        model
          { epsilonGreedyResult = egNewResult,
            ucbResult = ucbNewResult,
            thompsonResult = tsNewResult
          }

  put updatedModel

  let Model {isRunning = running, currentTrial = trial, totalTrials = total} = updatedModel
  if running && trial < total
    then scheduleIO (pure RunStep)
    else
      when (trial >= total) $
        put updatedModel {isRunning = False}
updateModel (SetEpsilon eps) = do
  model@Model {..} <- get
  put model {algorithmParams = algorithmParams {epsilon = max 0 (min 1 eps)}}
  pure ()
updateModel (SetUCBC c) = do
  model@Model {..} <- get
  put model {algorithmParams = algorithmParams {ucbC = max 0.1 (min 5 c)}}
  pure ()
updateModel (SetTrials n) = do
  model <- get
  put model {totalTrials = max 1 n}
  pure ()
updateModel ToggleResults = do
  model <- get
  put model {showResults = not (showResults model)}
  pure ()
updateModel (SetArmProbability armIdx prob) = do
  model@Model {..} <- get
  let clampedProb = max 0 (min 1 prob)
      newArms = arms // [(armIdx, clampedProb)]
      resetStats = V.replicate (V.length arms) (ArmStats 0 0 0)
  put
    model
      { arms = newArms,
        epsilonGreedyResult = epsilonGreedyResult {algArmStats = resetStats},
        ucbResult = ucbResult {algArmStats = resetStats},
        thompsonResult = thompsonResult {algArmStats = resetStats},
        currentTrial = 0,
        isRunning = False
      }
  pure ()

updateArmStats :: Int -> Double -> Vector ArmStats -> Vector ArmStats
updateArmStats armIdx reward stats =
  V.modify
    ( \v -> do
        let oldStats = stats V.! armIdx
            newCount = armCount oldStats + 1
            newSum = armRewards oldStats + reward
            newMean = newSum / fromIntegral newCount
        MV.write v armIdx $ ArmStats newCount newSum newMean
    )
    stats

getReward :: Double -> IO Double
getReward prob = do
  r <- randomRIO (0.0, 1.0)
  return $ if r < prob then 1.0 else 0.0
