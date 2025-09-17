{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Bandit.Algorithm.ThompsonSampling where

import Bandit.Algorithm.Types
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.Random (StdGen, mkStdGen, randomR)

data ThompsonSampling = ThompsonSampling
  { priorAlpha :: Double,
    priorBeta :: Double
  }
  deriving (Show, Eq, Generic)

newThompsonSampling :: ThompsonSampling
newThompsonSampling = ThompsonSampling 1.0 1.0

-- Beta(α, β) = Gamma(α) / (Gamma(α) + Gamma(β))
sampleBeta :: Double -> Double -> StdGen -> (Double, StdGen)
sampleBeta alpha beta gen =
  let -- Use Jöhnk's method for Beta distribution when α, β ≤ 1
      -- Otherwise use Gamma relationship
      (sample, gen') =
        if alpha <= 1 && beta <= 1
          then johnkBeta alpha beta gen
          else gammaMethodBeta alpha beta gen
   in (sample, gen')

johnkBeta :: Double -> Double -> StdGen -> (Double, StdGen)
johnkBeta alpha beta gen =
  let loop g =
        let (u, g1) = randomR (0.0, 1.0) g
            (v, g2) = randomR (0.0, 1.0) g1
            x = u ** (1.0 / alpha)
            y = v ** (1.0 / beta)
         in if x + y <= 1.0
              then (x / (x + y), g2)
              else loop g2
   in loop gen

gammaMethodBeta :: Double -> Double -> StdGen -> (Double, StdGen)
gammaMethodBeta alpha beta gen =
  let (x, gen1) = sampleGamma alpha gen
      (y, gen2) = sampleGamma beta gen1
   in (x / (x + y), gen2)

sampleGamma :: Double -> StdGen -> (Double, StdGen)
sampleGamma shape gen
  | shape < 1 =
      let (u, gen1) = randomR (0.0, 1.0) gen
          (g, gen2) = sampleGamma (shape + 1) gen1
       in (g * (u ** (1.0 / shape)), gen2)
  | otherwise =
      let d = shape - 1.0 / 3.0
          c = 1.0 / sqrt (9.0 * d)
          loop g =
            let (z, g1) = normalSample g
                v = (1.0 + c * z) ** 3
                (u, g2) = randomR (0.0, 1.0) g1
             in if z > -1.0 / c && log u < 0.5 * z * z + d - d * v + d * log v
                  then (d * v, g2)
                  else loop g2
       in loop gen

normalSample :: StdGen -> (Double, StdGen)
normalSample gen =
  let (u1, gen1) = randomR (0.0, 1.0) gen
      (u2, gen2) = randomR (0.0, 1.0) gen1
      r = sqrt (-2.0 * log u1)
      theta = 2.0 * pi * u2
   in (r * cos theta, gen2)

selectArmThompsonSampling :: ThompsonSampling -> V.Vector ArmStats -> Int -> Int
selectArmThompsonSampling ThompsonSampling {..} stats seed =
  let gen = mkStdGen seed
      samples = V.imap (sampleArm gen) stats
   in V.maxIndex (V.map fst samples)
  where
    sampleArm :: StdGen -> Int -> ArmStats -> (Double, StdGen)
    sampleArm g idx ArmStats {..} =
      let successes =
            if armCount > 0
              then max 1 (armRewards + priorAlpha)
              else priorAlpha
          failures =
            if armCount > 0
              then max 1 (fromIntegral armCount - armRewards + priorBeta)
              else priorBeta
          genForArm = mkStdGen (seed * 100 + idx)
       in sampleBeta successes failures genForArm

instance BanditAlgorithm ThompsonSampling where
  initAlgorithm _ _ = newThompsonSampling
  selectArm = selectArmThompsonSampling
  algorithmName _ = "Thompson Sampling"
