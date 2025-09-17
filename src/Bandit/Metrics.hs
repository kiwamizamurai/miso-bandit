module Bandit.Metrics where

import Bandit.Algorithm.Types
import Bandit.Model
import Data.Vector (Vector)
import qualified Data.Vector as V

cumulativeRegret :: Vector Double -> AlgorithmResult -> Double
cumulativeRegret arms result =
  let bestArmProb = V.maximum arms
   in V.sum $ V.map (\chosen -> bestArmProb - (arms V.! chosen)) (algChosenArms result)

-- Simple regret is the difference between the true best arm reward and
-- the true reward of the arm the algorithm would select based on its estimates
simpleRegret :: Vector Double -> AlgorithmResult -> Double
simpleRegret arms result =
  if V.null (algArmStats result) || V.all (\s -> armCount s == 0) (algArmStats result)
    then 0
    else
      let bestArmProb = V.maximum arms
          bestArmIndex = V.maxIndex arms
          empiricalBest = V.maxIndex $ V.map armExpectation (algArmStats result)
          empiricalBestProb = arms V.! empiricalBest
          bestArmEstimate = armExpectation (algArmStats result V.! bestArmIndex)
       in if empiricalBest == bestArmIndex
            then abs (bestArmProb - bestArmEstimate)
            else bestArmProb - empiricalBestProb

perActionRegret :: Vector Double -> AlgorithmResult -> Vector Double
perActionRegret arms result =
  V.map
    ( \(i, stats) ->
        let trueProb = arms V.! i
            bestProb = V.maximum arms
         in (bestProb - trueProb) * fromIntegral (armCount stats)
    )
    (V.indexed $ algArmStats result)

bestArmRate :: Vector Double -> AlgorithmResult -> Int -> Double
bestArmRate arms result windowSize =
  let bestArm = V.maxIndex arms
      recentChoices = V.take windowSize $ V.reverse (algChosenArms result)
      bestCount = V.length $ V.filter (== bestArm) recentChoices
      total = min windowSize (V.length $ algChosenArms result)
   in if total > 0
        then fromIntegral bestCount / fromIntegral total * 100
        else 0

confidenceInterval :: Double -> ArmStats -> (Double, Double)
confidenceInterval confidence stats =
  let mean = armExpectation stats
      n = fromIntegral (armCount stats)
      z
        | confidence >= 0.99 = 2.576
        | confidence >= 0.95 = 1.96
        | confidence >= 0.90 = 1.645
        | otherwise = 1.28
      stderr =
        if n > 0
          then sqrt (mean * (1 - mean) / n)
          else 0.5
      margin = z * stderr
   in if armCount stats == 0
        then (0, 1)
        else (max 0 (mean - margin), min 1 (mean + margin))

empiricalDivergence :: Vector Double -> AlgorithmResult -> Double
empiricalDivergence arms result =
  let klDiv p q =
        if p > 0 && q > 0
          then p * log (p / q) + (1 - p) * log ((1 - p) / (1 - q))
          else 0
   in V.sum $
        V.zipWith
          klDiv
          (V.map armExpectation $ algArmStats result)
          arms

explorationRatio :: AlgorithmResult -> Double
explorationRatio result =
  let totalPulls = V.sum $ V.map armCount (algArmStats result)
      maxPulls = V.maximum $ V.map armCount (algArmStats result)
   in if totalPulls > 0
        then 1.0 - (fromIntegral maxPulls / fromIntegral totalPulls)
        else 0

rewardVariance :: AlgorithmResult -> Double
rewardVariance result =
  let rewards = algRewards result
      n = fromIntegral $ V.length rewards
      mean = V.sum rewards / n
      variance = V.sum (V.map (\r -> (r - mean) ^ 2) rewards) / n
   in if n > 0 then variance else 0

-- Sample complexity - trials needed to identify best arm
sampleComplexity :: Vector Double -> AlgorithmResult -> Double -> Int
sampleComplexity arms result delta =
  let bestArm = V.maxIndex arms
      empiricalBest = V.maxIndex $ V.map armExpectation (algArmStats result)
      bestArmProb = armExpectation $ algArmStats result V.! bestArm
      secondBestProb =
        V.maximum $
          V.imap (\i p -> if i == bestArm then 0 else p) $
            V.map armExpectation (algArmStats result)
   in if bestArm == empiricalBest && bestArmProb - secondBestProb > delta
        then V.sum $ V.map armCount (algArmStats result)
        else maxBound

epsilonGreedyTheoreticalBound :: Vector Double -> Int -> Double -> Double
epsilonGreedyTheoreticalBound arms t epsilon =
  let bestProb = V.maximum arms
      gaps = V.map (bestProb -) arms
      suboptimalGaps = V.filter (> 0) gaps
   in epsilon * fromIntegral t * V.sum suboptimalGaps

ucbTheoreticalBound :: Vector Double -> Int -> Double
ucbTheoreticalBound arms t =
  let bestProb = V.maximum arms
      gaps = V.map (bestProb -) arms
      suboptimalGaps = V.filter (> 0) gaps
      bound gap =
        if gap > 0
          then 8 * log (fromIntegral t) / gap
          else 0
   in V.sum $ V.map bound suboptimalGaps

thompsonTheoreticalBound :: Vector Double -> Int -> Double
thompsonTheoreticalBound arms t =
  let bestProb = V.maximum arms
      gaps = V.map (bestProb -) arms
      suboptimalGaps = V.filter (> 0) gaps
      klDiv p q
        | p > 0 && q > 0 && p /= q = p * log (p / q) + (1 - p) * log ((1 - p) / (1 - q))
        | p == q = 0
        | otherwise = 1e10
      bound gap =
        if gap > 0
          then log (fromIntegral t) / klDiv bestProb (bestProb - gap)
          else 0
   in V.sum $ V.map bound suboptimalGaps

actualRegret :: Vector Double -> AlgorithmResult -> Double
actualRegret arms result =
  let bestProb = V.maximum arms
      optimalReward = bestProb * fromIntegral (V.length $ algChosenArms result)
      actualReward =
        if V.null (algCumulativeRewards result)
          then 0
          else V.last (algCumulativeRewards result)
   in optimalReward - actualReward
