{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bandit.View.Charts.Arms where

import Bandit.Actions (Action)
import Bandit.Algorithm.Types (ArmStats (..))
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

calculateConfidenceInterval :: ArmStats -> (Double, Double, Double)
calculateConfidenceInterval ArmStats {..} =
  if armCount == 0
    then (0, 0, 0)
    else
      let mean = armExpectation
          -- For Bernoulli: variance = p(1-p), std error = sqrt(p(1-p)/n)
          variance = mean * (1 - mean)
          stdError = sqrt (variance / fromIntegral armCount)
          -- 95% confidence interval (z = 1.96)
          margin = 1.96 * stdError
          lower = max 0 (mean - margin)
          upper = min 1 (mean + margin)
       in (lower, mean, upper)

calculateUCBBound :: Double -> Int -> ArmStats -> Double
calculateUCBBound ucbC totalTrials ArmStats {..} =
  if armCount == 0
    then 1.0
    else
      let totalN = fromIntegral totalTrials
          ucbBonus = ucbC * sqrt (log (max 1 totalN) / fromIntegral armCount)
       in min 1 (armExpectation + ucbBonus)

armConfidenceIntervalsChart :: Model -> View Model Action
armConfidenceIntervalsChart Model {..} =
  let dims = largeDimensions
      nArms = V.length arms
   in div_
        [class_ "chart-wrapper"]
        [ h3_ [] [text "Arm Confidence Intervals"],
          svgContainer
            dims
            [ drawAxes dims,
              drawYAxisLabels dims [(0, "0"), (0.25, "0.25"), (0.5, "0.5"), (0.75, "0.75"), (1, "1.0")],
              drawXAxisLabels dims $ map (\i -> (fromIntegral i / fromIntegral (nArms - 1), "Arm " ++ show i)) [0 .. nArms - 1],
              drawTrueValues dims arms,
              drawConfidenceIntervalBars dims arms epsilonGreedyResult ucbResult thompsonResult algorithmParams currentTrial,
              drawLegend
                dims
                [ ("#FF0000", "True Value"),
                  (getAlgorithmColor "epsilon", "ε-Greedy"),
                  (getAlgorithmColor "ucb", "UCB"),
                  (getAlgorithmColor "thompson", "Thompson")
                ]
            ]
        ]

drawTrueValues :: ChartDimensions -> V.Vector Double -> View Model Action
drawTrueValues ChartDimensions {..} arms =
  g_ [] $
    V.toList $
      V.imap drawTrueLine arms
  where
    innerWidth = chartWidth - 2 * chartPadding
    innerHeight = chartHeight - 2 * chartPadding
    nArms = V.length arms
    groupWidth = if nArms > 0 then innerWidth `div` nArms else 60

    drawTrueLine idx value =
      let xCenter = chartPadding + idx * groupWidth + groupWidth `div` 2
          y = chartHeight - chartPadding - round (value * fromIntegral innerHeight)
       in line_
            [ x1_ (ms $ xCenter - groupWidth `div` 3),
              y1_ (ms y),
              x2_ (ms $ xCenter + groupWidth `div` 3),
              y2_ (ms y),
              stroke_ "#FF0000",
              strokeWidth_ "2",
              strokeDasharray_ "5,2"
            ]

drawConfidenceIntervalBars :: ChartDimensions -> V.Vector Double -> AlgorithmResult -> AlgorithmResult -> AlgorithmResult -> AlgorithmParams -> Int -> View Model Action
drawConfidenceIntervalBars ChartDimensions {..} arms egResult ucbResult tsResult params trials =
  g_ [] $
    concat $
      V.toList $
        V.imap drawArmGroup arms
  where
    innerWidth = chartWidth - 2 * chartPadding
    innerHeight = chartHeight - 2 * chartPadding
    nArms = V.length arms
    groupWidth = if nArms > 0 then innerWidth `div` nArms else 60
    barWidth = groupWidth `div` 5

    drawArmGroup idx _ =
      let xBase = chartPadding + idx * groupWidth + groupWidth `div` 4

          egStats = algArmStats egResult V.! idx
          (egLower, egMean, egUpper) = calculateConfidenceInterval egStats

          ucbStats = algArmStats ucbResult V.! idx
          (ucbLower, ucbMean, ucbUpper) = calculateConfidenceInterval ucbStats
          ucbBound = calculateUCBBound (ucbC params) trials ucbStats

          tsStats = algArmStats tsResult V.! idx
          (tsLower, tsMean, tsUpper) = calculateConfidenceInterval tsStats
       in [ -- Epsilon-Greedy
            drawBarWithCI (xBase - barWidth) egMean egLower egUpper (getAlgorithmColor "epsilon"),
            -- UCB (with upper bound)
            drawBarWithUCB xBase ucbMean ucbLower ucbUpper ucbBound (getAlgorithmColor "ucb"),
            -- Thompson Sampling
            drawBarWithCI (xBase + barWidth) tsMean tsLower tsUpper (getAlgorithmColor "thompson")
          ]

    drawBarWithCI :: Int -> Double -> Double -> Double -> MisoString -> View model action
    drawBarWithCI x mean lower upper color =
      g_
        []
        [ -- Error bar
          line_
            [ x1_ (ms $ x + barWidth `div` 2),
              y1_ (ms $ chartHeight - chartPadding - round (upper * fromIntegral innerHeight :: Double)),
              x2_ (ms $ x + barWidth `div` 2),
              y2_ (ms $ chartHeight - chartPadding - round (lower * fromIntegral innerHeight :: Double)),
              stroke_ color,
              strokeWidth_ "2"
            ],
          -- Upper cap
          line_
            [ x1_ (ms $ x + barWidth `div` 2 - 4),
              y1_ (ms $ chartHeight - chartPadding - round (upper * fromIntegral innerHeight :: Double)),
              x2_ (ms $ x + barWidth `div` 2 + 4),
              y2_ (ms $ chartHeight - chartPadding - round (upper * fromIntegral innerHeight :: Double)),
              stroke_ color,
              strokeWidth_ "2"
            ],
          -- Lower cap
          line_
            [ x1_ (ms $ x + barWidth `div` 2 - 4),
              y1_ (ms $ chartHeight - chartPadding - round (lower * fromIntegral innerHeight :: Double)),
              x2_ (ms $ x + barWidth `div` 2 + 4),
              y2_ (ms $ chartHeight - chartPadding - round (lower * fromIntegral innerHeight :: Double)),
              stroke_ color,
              strokeWidth_ "2"
            ],
          -- Mean point
          circle_
            [ cx_ (ms $ x + barWidth `div` 2),
              cy_ (ms $ chartHeight - chartPadding - round (mean * fromIntegral innerHeight :: Double)),
              r_ "3",
              fill_ color,
              stroke_ "white",
              strokeWidth_ "1"
            ]
        ]

    drawBarWithUCB :: Int -> Double -> Double -> Double -> Double -> MisoString -> View model action
    drawBarWithUCB x mean lower upper ucbVal color =
      g_
        []
        [ drawBarWithCI x mean lower upper color,
          -- UCB upper bound marker
          circle_
            [ cx_ (ms $ x + barWidth `div` 2),
              cy_ (ms $ chartHeight - chartPadding - round (ucbVal * fromIntegral innerHeight :: Double)),
              r_ "3",
              fill_ "#FF9800",
              stroke_ color,
              strokeWidth_ "1"
            ]
        ]

armSelectionChart :: Model -> View Model Action
armSelectionChart Model {..} =
  let dims = largeDimensions
      nArms = V.length arms

      egCounts = countArmSelections nArms (algChosenArms epsilonGreedyResult)
      ucbCounts = countArmSelections nArms (algChosenArms ucbResult)
      tsCounts = countArmSelections nArms (algChosenArms thompsonResult)

      maxCount = maximum $ concatMap V.toList [egCounts, ucbCounts, tsCounts]
   in div_
        [class_ "chart-wrapper"]
        [ h3_ [] [text "Arm Selection Frequency"],
          svgContainer
            dims
            [ drawAxes dims,
              drawYAxisLabels dims $ map (\i -> (fromIntegral i / 4, printf "%.0f" (maxCount * fromIntegral i / 4))) [0 .. 4],
              drawXAxisLabels dims $ map (\i -> (fromIntegral i / fromIntegral (nArms - 1), "Arm " ++ show i)) [0 .. nArms - 1],
              drawGroupedBars dims egCounts ucbCounts tsCounts maxCount,
              drawLegend
                dims
                [ (getAlgorithmColor "epsilon", "ε-Greedy"),
                  (getAlgorithmColor "ucb", "UCB"),
                  (getAlgorithmColor "thompson", "Thompson")
                ]
            ]
        ]

countArmSelections :: Int -> V.Vector Int -> V.Vector Double
countArmSelections nArms selections =
  V.generate nArms $ \arm ->
    fromIntegral $ V.length $ V.filter (== arm) selections

drawGroupedBars :: ChartDimensions -> V.Vector Double -> V.Vector Double -> V.Vector Double -> Double -> View Model Action
drawGroupedBars ChartDimensions {..} egCounts ucbCounts tsCounts maxCount =
  g_ [] $
    concat $
      zipWith3 drawGroup [0 ..] (V.toList egCounts) (zip (V.toList ucbCounts) (V.toList tsCounts))
  where
    innerWidth = chartWidth - 2 * chartPadding
    innerHeight = chartHeight - 2 * chartPadding
    nGroups = V.length egCounts
    groupWidth = if nGroups > 0 then innerWidth `div` nGroups else 60
    barWidth = groupWidth `div` 4

    drawGroup idx eg (ucb, ts) =
      let xBase = chartPadding + idx * groupWidth + groupWidth `div` 4
          scale v = if maxCount > 0 then v / maxCount else 0
       in [ drawBar xBase (scale eg) (getAlgorithmColor "epsilon"),
            drawBar (xBase + barWidth) (scale ucb) (getAlgorithmColor "ucb"),
            drawBar (xBase + 2 * barWidth) (scale ts) (getAlgorithmColor "thompson")
          ]

    drawBar x value color =
      let barHeight = round (value * fromIntegral innerHeight)
          y = chartHeight - chartPadding - barHeight
       in rect_
            [ x_ (ms x),
              y_ (ms y),
              width_ (ms $ barWidth - 2),
              height_ (ms barHeight),
              fill_ color,
              opacity_ "0.8"
            ]

bestArmConvergenceChart :: Model -> View Model Action
bestArmConvergenceChart Model {..} =
  let dims = defaultDimensions
      bestArm = V.maxIndex arms
      windowSize = 100

      egRates = calculateBestArmRate bestArm windowSize (algChosenArms epsilonGreedyResult)
      ucbRates = calculateBestArmRate bestArm windowSize (algChosenArms ucbResult)
      tsRates = calculateBestArmRate bestArm windowSize (algChosenArms thompsonResult)
   in div_
        [class_ "chart-wrapper"]
        [ h3_ [] [text "Best Arm Convergence"],
          svgContainer
            dims
            [ drawAxes dims,
              drawYAxisLabels dims [(0, "0%"), (0.25, "25%"), (0.5, "50%"), (0.75, "75%"), (1, "100%")],
              drawXAxisLabels dims $
                if currentTrial > 0
                  then map (\i -> (fromIntegral i / 4, show $ i * currentTrial `div` 4)) [0 .. 4]
                  else [(0, "0"), (1, "1000")],
              drawConvergenceLine dims egRates currentTrial (getAlgorithmColor "epsilon"),
              drawConvergenceLine dims ucbRates currentTrial (getAlgorithmColor "ucb"),
              drawConvergenceLine dims tsRates currentTrial (getAlgorithmColor "thompson"),
              drawLegend
                dims
                [ (getAlgorithmColor "epsilon", "ε-Greedy"),
                  (getAlgorithmColor "ucb", "UCB"),
                  (getAlgorithmColor "thompson", "Thompson")
                ]
            ]
        ]

calculateBestArmRate :: Int -> Int -> V.Vector Int -> V.Vector Double
calculateBestArmRate bestArm windowSize selections =
  V.imap
    ( \i _ ->
        let start = max 0 (i - windowSize + 1)
            window = V.slice start (min windowSize (i + 1)) selections
            bestCount = V.length $ V.filter (== bestArm) window
            total = V.length window
         in if total > 0
              then fromIntegral bestCount / fromIntegral total
              else 0
    )
    selections

drawConvergenceLine :: ChartDimensions -> V.Vector Double -> Int -> MisoString -> View Model Action
drawConvergenceLine dims rates maxTrials color =
  if V.null rates
    then g_ [] []
    else
      let points =
            V.imap
              ( \i rate ->
                  let x = fromIntegral i / fromIntegral (max 1 maxTrials)
                      y = rate -- Already in 0-1 range
                   in (x, y)
              )
              rates
       in drawLinePath dims points color "2"
