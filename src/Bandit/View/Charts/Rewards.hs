{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bandit.View.Charts.Rewards where

import Bandit.Actions (Action)
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

cumulativeRewardsChart :: Model -> View Model Action
cumulativeRewardsChart Model {..} =
  let dims = largeDimensions
      maxTrials = currentTrial

      egRewards = algCumulativeRewards epsilonGreedyResult
      ucbRewards = algCumulativeRewards ucbResult
      tsRewards = algCumulativeRewards thompsonResult

      maxReward =
        maximum $
          map
            (\r -> if V.null r then 0 else V.last r)
            [egRewards, ucbRewards, tsRewards]
   in div_
        [class_ "chart-wrapper"]
        [ h3_ [] [text "Cumulative Rewards"],
          svgContainer
            dims
            [ drawAxes dims,
              drawYAxisLabels dims $ map (\i -> (fromIntegral i / 4, printf "%.0f" (maxReward * fromIntegral i / 4))) [0 .. 4],
              drawXAxisLabels dims $
                if maxTrials > 0
                  then map (\i -> (fromIntegral i / 4, show $ i * maxTrials `div` 4)) [0 .. 4]
                  else [(0, "0"), (1, "1000")],
              drawRewardLine dims egRewards maxReward maxTrials (getAlgorithmColor "epsilon"),
              drawRewardLine dims ucbRewards maxReward maxTrials (getAlgorithmColor "ucb"),
              drawRewardLine dims tsRewards maxReward maxTrials (getAlgorithmColor "thompson"),
              drawLegend
                dims
                [ (getAlgorithmColor "epsilon", "ε-Greedy"),
                  (getAlgorithmColor "ucb", "UCB"),
                  (getAlgorithmColor "thompson", "Thompson")
                ]
            ]
        ]

drawRewardLine :: ChartDimensions -> V.Vector Double -> Double -> Int -> MisoString -> View Model Action
drawRewardLine dims@ChartDimensions {..} rewards maxReward maxTrials color =
  if V.null rewards
    then g_ [] []
    else
      let innerWidth = fromIntegral $ chartWidth - 2 * chartPadding
          innerHeight = fromIntegral $ chartHeight - 2 * chartPadding
          points =
            V.imap
              ( \i r ->
                  let x = fromIntegral i / fromIntegral (max 1 maxTrials)
                      y = if maxReward > 0 then r / maxReward else 0
                   in (x, y)
              )
              rewards
       in drawLinePath dims points color "2"

averageRewardChart :: Model -> View Model Action
averageRewardChart Model {..} =
  let dims = defaultDimensions

      egAvg = calculateAverageRewards (algCumulativeRewards epsilonGreedyResult)
      ucbAvg = calculateAverageRewards (algCumulativeRewards ucbResult)
      tsAvg = calculateAverageRewards (algCumulativeRewards thompsonResult)

      maxAvg = V.maximum arms
   in div_
        [class_ "chart-wrapper"]
        [ h3_ [] [text "Average Reward"],
          svgContainer
            dims
            [ drawAxes dims,
              drawYAxisLabels dims [(0, "0"), (0.5, "0.5"), (1, "1.0")],
              drawXAxisLabels dims $
                if currentTrial > 0
                  then map (\i -> (fromIntegral i / 4, show $ i * currentTrial `div` 4)) [0 .. 4]
                  else [(0, "0"), (1, "1000")],
              drawAverageLine dims egAvg currentTrial (getAlgorithmColor "epsilon"),
              drawAverageLine dims ucbAvg currentTrial (getAlgorithmColor "ucb"),
              drawAverageLine dims tsAvg currentTrial (getAlgorithmColor "thompson"),
              drawOptimalLine dims maxAvg,
              drawLegend
                dims
                [ (getAlgorithmColor "epsilon", "ε-Greedy"),
                  (getAlgorithmColor "ucb", "UCB"),
                  (getAlgorithmColor "thompson", "Thompson"),
                  ("#FF0000", "Optimal")
                ]
            ]
        ]

calculateAverageRewards :: V.Vector Double -> V.Vector Double
calculateAverageRewards =
  V.imap (\i c -> if i == 0 then c else c / fromIntegral (i + 1))

drawAverageLine :: ChartDimensions -> V.Vector Double -> Int -> MisoString -> View Model Action
drawAverageLine dims@ChartDimensions {..} averages maxTrials color =
  if V.null averages
    then g_ [] []
    else
      let innerWidth = fromIntegral $ chartWidth - 2 * chartPadding
          innerHeight = fromIntegral $ chartHeight - 2 * chartPadding
          points =
            V.imap
              ( \i avg ->
                  let x = fromIntegral i / fromIntegral (max 1 maxTrials)
                      y = avg -- Already in 0-1 range
                   in (x, y)
              )
              averages
       in drawLinePath dims points color "2"

drawOptimalLine :: ChartDimensions -> Double -> View Model Action
drawOptimalLine ChartDimensions {..} optimal =
  let y = chartHeight - chartPadding - round (optimal * fromIntegral (chartHeight - 2 * chartPadding))
   in line_
        [ x1_ (ms chartPadding),
          y1_ (ms y),
          x2_ (ms $ chartWidth - chartPadding),
          y2_ (ms y),
          stroke_ "#FF0000",
          strokeWidth_ "1",
          strokeDasharray_ "5,5"
        ]
