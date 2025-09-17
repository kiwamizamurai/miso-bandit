{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bandit where

import Control.Monad
import Data.IORef
import qualified Data.Vector as V
import System.Random

newtype BernoulliArm = BernoulliArm
  { armProbability :: Double
  }
  deriving (Show)

getReward :: BernoulliArm -> IO Double
getReward BernoulliArm {..} = do
  r <- randomRIO (0, 1)
  return $ if r <= armProbability then 1.0 else 0.0

data BanditState = BanditState
  { counts :: V.Vector Double,
    rewards :: V.Vector Double,
    expectations :: V.Vector Double
  }
  deriving (Show)

initBanditState :: Int -> BanditState
initBanditState nArms =
  BanditState
    { counts = V.replicate nArms 0,
      rewards = V.replicate nArms 0,
      expectations = V.replicate nArms 0
    }

data EpsilonGreedy = EpsilonGreedy
  { epsilon :: Double,
    state :: IORef BanditState
  }

initEpsilonGreedy :: Double -> Int -> IO EpsilonGreedy
initEpsilonGreedy eps nArms = do
  stateRef <- newIORef (initBanditState nArms)
  return $ EpsilonGreedy eps stateRef

selectArmEpsilonGreedy :: EpsilonGreedy -> IO Int
selectArmEpsilonGreedy EpsilonGreedy {..} = do
  r <- randomRIO (0, 1)
  BanditState {..} <- readIORef state
  if r > epsilon
    then return $ V.maxIndex expectations
    else randomRIO (0, V.length rewards - 1)

updateEpsilonGreedy :: EpsilonGreedy -> Int -> Double -> IO ()
updateEpsilonGreedy EpsilonGreedy {..} chosenArm reward = do
  BanditState {..} <- readIORef state
  let newCounts = counts V.// [(chosenArm, counts V.! chosenArm + 1)]
      newRewards = rewards V.// [(chosenArm, rewards V.! chosenArm + reward)]
      sumSelectArm = newCounts V.! chosenArm
      sumReward = newRewards V.! chosenArm
      expectation = sumReward / sumSelectArm
      newExpectations = expectations V.// [(chosenArm, expectation)]
  writeIORef state $ BanditState newCounts newRewards newExpectations

data UCB = UCB
  { ucbEpsilon :: Double,
    ucbState :: IORef BanditState
  }

initUCB :: Double -> Int -> IO UCB
initUCB eps nArms = do
  stateRef <- newIORef (initBanditState nArms)
  return $ UCB eps stateRef

selectArmUCB :: UCB -> IO Int
selectArmUCB UCB {..} = do
  r <- randomRIO (0, 1)
  BanditState {..} <- readIORef ucbState
  if r > ucbEpsilon
    then return $ V.maxIndex expectations
    else do
      let totalCounts = V.sum counts
          upperBounds = V.map (\c -> if c == 0 then 1e10 else sqrt (2 * log totalCounts / c)) counts
          ucbValues = V.zipWith (+) expectations upperBounds
      return $ V.maxIndex ucbValues

updateUCB :: UCB -> Int -> Double -> IO ()
updateUCB UCB {..} chosenArm reward = do
  BanditState {..} <- readIORef ucbState
  let newCounts = counts V.// [(chosenArm, counts V.! chosenArm + 1)]
      newRewards = rewards V.// [(chosenArm, rewards V.! chosenArm + reward)]
      sumSelectArm = newCounts V.! chosenArm
      sumReward = newRewards V.! chosenArm
      expectation = sumReward / sumSelectArm
      newExpectations = expectations V.// [(chosenArm, expectation)]
  writeIORef ucbState $ BanditState newCounts newRewards newExpectations

data ThompsonSampling = ThompsonSampling
  { tsAlpha :: IORef (V.Vector Double),
    tsBeta :: IORef (V.Vector Double)
  }

initThompsonSampling :: Int -> IO ThompsonSampling
initThompsonSampling nArms = do
  alphaRef <- newIORef (V.replicate nArms 1)
  betaRef <- newIORef (V.replicate nArms 1)
  return $ ThompsonSampling alphaRef betaRef

sampleBeta :: Double -> Double -> IO Double
sampleBeta alpha beta = do
  x <- sampleGamma alpha
  y <- sampleGamma beta
  return $ x / (x + y)
  where
    sampleGamma :: Double -> IO Double
    sampleGamma shape = do
      if shape >= 1
        then do
          u <- randomRIO (0, 1)
          v <- randomRIO (0, 1)
          let x = -log u
          return $ x * (v ** (1 / shape))
        else do
          u <- randomRIO (0, 1)
          return $ u ** (1 / shape)

selectArmThompsonSampling :: ThompsonSampling -> IO Int
selectArmThompsonSampling ThompsonSampling {..} = do
  alphas <- readIORef tsAlpha
  betas <- readIORef tsBeta
  samples <- V.generateM (V.length alphas) $ \i ->
    sampleBeta (alphas V.! i) (betas V.! i)
  return $ V.maxIndex samples

updateThompsonSampling :: ThompsonSampling -> Int -> Double -> IO ()
updateThompsonSampling ThompsonSampling {..} chosenArm reward = do
  alphas <- readIORef tsAlpha
  betas <- readIORef tsBeta
  if reward > 0
    then writeIORef tsAlpha $ alphas V.// [(chosenArm, alphas V.! chosenArm + 1)]
    else writeIORef tsBeta $ betas V.// [(chosenArm, betas V.! chosenArm + 1)]

class BanditAlgorithm a where
  selectArm :: a -> IO Int
  update :: a -> Int -> Double -> IO ()

instance BanditAlgorithm EpsilonGreedy where
  selectArm = selectArmEpsilonGreedy
  update = updateEpsilonGreedy

instance BanditAlgorithm UCB where
  selectArm = selectArmUCB
  update = updateUCB

instance BanditAlgorithm ThompsonSampling where
  selectArm = selectArmThompsonSampling
  update = updateThompsonSampling

data SimulationResults = SimulationResults
  { chosenArms :: V.Vector Int,
    cumulativeRewards :: V.Vector Double,
    finalState :: BanditState
  }
  deriving (Show)

demonstrate :: (BanditAlgorithm algo) => algo -> V.Vector BernoulliArm -> Int -> IO SimulationResults
demonstrate algo arms trials = do
  chosenArmsRef <- newIORef V.empty
  cumulativeRewardsRef <- newIORef V.empty
  cumReward <- newIORef 0.0

  forM_ [0 .. trials - 1] $ \t -> do
    chosenArm <- selectArm algo
    reward <- getReward (arms V.! chosenArm)

    modifyIORef chosenArmsRef (`V.snoc` chosenArm)

    if t == 0
      then do
        writeIORef cumReward reward
        modifyIORef cumulativeRewardsRef (`V.snoc` reward)
      else do
        prevCum <- readIORef cumReward
        let newCum = prevCum + reward
        writeIORef cumReward newCum
        modifyIORef cumulativeRewardsRef (`V.snoc` newCum)

    update algo chosenArm reward

    when (t `mod` 1000 == 0) $ do
      putStrLn $ "Trial " ++ show t ++ "/" ++ show trials

  finalChosenArms <- readIORef chosenArmsRef
  finalCumulativeRewards <- readIORef cumulativeRewardsRef

  let getFinalState = return $ initBanditState (V.length arms)
  SimulationResults finalChosenArms finalCumulativeRewards <$> getFinalState

createArms :: [Double] -> V.Vector BernoulliArm
createArms probs = V.fromList $ map BernoulliArm probs
