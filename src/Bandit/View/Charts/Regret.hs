{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bandit.View.Charts.Regret where

import Bandit.Actions (Action)
import Bandit.Algorithm.Types (ArmStats (..))
import Bandit.Metrics
import Bandit.Model
import Bandit.View.Charts.Common
import qualified Data.Vector as V
import Miso
import Miso.Html
import Miso.Html.Property (class_)
import Miso.String (ms)
import Miso.Svg hiding (text)
import qualified Miso.Svg as Svg
import Miso.Svg.Property
import Text.Printf (printf)

cumulativeRegretChart :: Model -> View Model Action
cumulativeRegretChart Model {..} =
  let dims = largeDimensions

      egRegrets = calculateRegretOverTime arms epsilonGreedyResult
      ucbRegrets = calculateRegretOverTime arms ucbResult
      tsRegrets = calculateRegretOverTime arms thompsonResult

      maxRegret =
        maximum $
          map
            (\r -> if V.null r then 0 else V.last r)
            [egRegrets, ucbRegrets, tsRegrets]
   in div_
        [class_ "chart-wrapper"]
        [ h3_ [] [text "Cumulative Regret"],
          svgContainer
            dims
            [ drawAxes dims,
              drawYAxisLabels dims $ map (\i -> (fromIntegral i / 4, printf "%.0f" (maxRegret * fromIntegral i / 4))) [0 .. 4],
              drawXAxisLabels dims $
                if currentTrial > 0
                  then map (\i -> (fromIntegral i / 4, show $ i * currentTrial `div` 4)) [0 .. 4]
                  else [(0, "0"), (1, "1000")],
              drawRegretLine dims egRegrets maxRegret currentTrial (getAlgorithmColor "epsilon"),
              drawRegretLine dims ucbRegrets maxRegret currentTrial (getAlgorithmColor "ucb"),
              drawRegretLine dims tsRegrets maxRegret currentTrial (getAlgorithmColor "thompson"),
              drawLegend
                dims
                [ (getAlgorithmColor "epsilon", "ε-Greedy"),
                  (getAlgorithmColor "ucb", "UCB"),
                  (getAlgorithmColor "thompson", "Thompson")
                ]
            ]
        ]

calculateRegretOverTime :: V.Vector Double -> AlgorithmResult -> V.Vector Double
calculateRegretOverTime arms result =
  let bestProb = V.maximum arms
      chosenArms = algChosenArms result
   in V.scanl1' (+) $ V.map (\chosen -> bestProb - (arms V.! chosen)) chosenArms

drawRegretLine :: ChartDimensions -> V.Vector Double -> Double -> Int -> MisoString -> View Model Action
drawRegretLine dims regrets maxRegret maxTrials color =
  if V.null regrets
    then g_ [] []
    else
      let points =
            V.imap
              ( \i r ->
                  let x = fromIntegral i / fromIntegral (max 1 maxTrials)
                      y = if maxRegret > 0 then r / maxRegret else 0
                   in (x, y)
              )
              regrets
       in drawLinePath dims points color "2"

simpleRegretChart :: Model -> View Model Action
simpleRegretChart Model {..} =
  let dims = defaultDimensions

      egSimple = calculateSimpleRegretOverTime arms epsilonGreedyResult
      ucbSimple = calculateSimpleRegretOverTime arms ucbResult
      tsSimple = calculateSimpleRegretOverTime arms thompsonResult

      maxSimple = 1.0
   in div_
        [class_ "chart-wrapper"]
        [ h3_ [] [text "Simple Regret"],
          svgContainer
            dims
            [ drawAxes dims,
              drawYAxisLabels dims [(0, "0"), (0.25, "0.25"), (0.5, "0.5"), (0.75, "0.75"), (1, "1.0")],
              drawXAxisLabels dims $
                if currentTrial > 0
                  then map (\i -> (fromIntegral i / 4, show $ i * currentTrial `div` 4)) [0 .. 4]
                  else [(0, "0"), (1, "1000")],
              drawSimpleRegretLine dims egSimple currentTrial (getAlgorithmColor "epsilon"),
              drawSimpleRegretLine dims ucbSimple currentTrial (getAlgorithmColor "ucb"),
              drawSimpleRegretLine dims tsSimple currentTrial (getAlgorithmColor "thompson"),
              drawLegend
                dims
                [ (getAlgorithmColor "epsilon", "ε-Greedy"),
                  (getAlgorithmColor "ucb", "UCB"),
                  (getAlgorithmColor "thompson", "Thompson")
                ]
            ]
        ]

calculateSimpleRegretOverTime :: V.Vector Double -> AlgorithmResult -> V.Vector Double
calculateSimpleRegretOverTime arms result =
  let bestProb = V.maximum arms
      stats = algArmStats result
      progressiveStats =
        V.scanl1' updateStatsVector
          $ V.map
            ( \(arm, reward) ->
                V.generate (V.length arms) $ \i ->
                  if i == arm then ArmStats 1 reward reward else ArmStats 0 0 0
            )
          $ V.zip (algChosenArms result) (algRewards result)
   in V.map
        ( \s ->
            let empiricalBest = V.maxIndex $ V.map armExpectation s
                empiricalProb = arms V.! empiricalBest
             in bestProb - empiricalProb
        )
        progressiveStats
  where
    updateStatsVector = V.zipWith addStats
    addStats (ArmStats c1 s1 _) (ArmStats c2 s2 _) =
      let count = c1 + c2
          sum = s1 + s2
       in ArmStats count sum (if count > 0 then sum / fromIntegral count else 0)

drawSimpleRegretLine :: ChartDimensions -> V.Vector Double -> Int -> MisoString -> View Model Action
drawSimpleRegretLine dims regrets maxTrials color =
  if V.null regrets
    then g_ [] []
    else
      let points =
            V.imap
              ( \i r ->
                  let x = fromIntegral i / fromIntegral (max 1 maxTrials)
                      y = r -- Already in 0-1 range
                   in (x, y)
              )
              regrets
       in drawLinePath dims points color "2"

regretComparisonChart :: Model -> View Model Action
regretComparisonChart Model {..} =
  let dims = defaultDimensions

      egRegret = cumulativeRegret arms epsilonGreedyResult
      ucbRegret = cumulativeRegret arms ucbResult
      tsRegret = cumulativeRegret arms thompsonResult

      maxRegret = maximum [egRegret, ucbRegret, tsRegret, 1]
   in div_
        [class_ "chart-wrapper"]
        [ h3_ [] [text "Final Regret Comparison"],
          svgContainer
            dims
            [ drawAxes dims,
              drawYAxisLabels dims $ map (\i -> (fromIntegral i / 4, printf "%.0f" (maxRegret * fromIntegral i / 4))) [0 .. 4],
              drawBars
                dims
                ( V.fromList
                    [(0, egRegret / maxRegret), (1, ucbRegret / maxRegret), (2, tsRegret / maxRegret)]
                )
                "#666",
              drawXAxisLabels dims [(0.166, "ε-Greedy"), (0.5, "UCB"), (0.833, "Thompson")]
            ]
        ]
