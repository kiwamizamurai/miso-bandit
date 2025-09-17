{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bandit.View.Charts.Common where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Miso
import Miso.Html
import Miso.String (MisoString, ms)
import Miso.Svg hiding (text)
import qualified Miso.Svg as Svg
import Miso.Svg.Property
import Text.Printf (printf)

data ChartDimensions = ChartDimensions
  { chartWidth :: Int,
    chartHeight :: Int,
    chartPadding :: Int
  }
  deriving (Show, Eq)

defaultDimensions :: ChartDimensions
defaultDimensions = ChartDimensions 280 180 25

largeDimensions :: ChartDimensions
largeDimensions = ChartDimensions 280 180 25

drawAxes :: ChartDimensions -> View model action
drawAxes ChartDimensions {..} =
  g_
    []
    [ -- Y-axis
      line_
        [ x1_ (ms chartPadding),
          y1_ (ms chartPadding),
          x2_ (ms chartPadding),
          y2_ (ms $ chartHeight - chartPadding),
          stroke_ "black",
          strokeWidth_ "1"
        ],
      -- X-axis
      line_
        [ x1_ (ms chartPadding),
          y1_ (ms $ chartHeight - chartPadding),
          x2_ (ms $ chartWidth - chartPadding),
          y2_ (ms $ chartHeight - chartPadding),
          stroke_ "black",
          strokeWidth_ "1"
        ]
    ]

drawYAxisLabels :: ChartDimensions -> [(Double, String)] -> View model action
drawYAxisLabels ChartDimensions {..} labels =
  g_ [] $
    concatMap drawLabel labels
  where
    innerHeight = chartHeight - 2 * chartPadding
    drawLabel (value, label) =
      let yPos = chartHeight - chartPadding - round (value * fromIntegral innerHeight)
       in [ Svg.text_
              [ x_ (ms $ chartPadding - 5),
                y_ (ms yPos),
                textAnchor_ "end",
                fontSize_ "10"
              ]
              [text $ ms label],
            line_
              [ x1_ (ms $ chartPadding - 3),
                y1_ (ms yPos),
                x2_ (ms chartPadding),
                y2_ (ms yPos),
                stroke_ "gray",
                strokeWidth_ "0.5",
                strokeDasharray_ "2,2"
              ]
          ]

drawXAxisLabels :: ChartDimensions -> [(Double, String)] -> View model action
drawXAxisLabels ChartDimensions {..} labels =
  g_ [] $
    map drawLabel labels
  where
    innerWidth = chartWidth - 2 * chartPadding
    drawLabel (value, label) =
      let xPos = chartPadding + round (value * fromIntegral innerWidth)
       in Svg.text_
            [ x_ (ms xPos),
              y_ (ms $ chartHeight - chartPadding + 15),
              textAnchor_ "middle",
              fontSize_ "10"
            ]
            [text $ ms label]

svgContainer :: ChartDimensions -> [View model action] -> View model action
svgContainer ChartDimensions {..} =
  svg_
    [ width_ (ms chartWidth),
      height_ (ms chartHeight),
      viewBox_ (ms $ "0 0 " ++ show chartWidth ++ " " ++ show chartHeight)
    ]

drawLinePath :: ChartDimensions -> Vector (Double, Double) -> MisoString -> MisoString -> View model action
drawLinePath ChartDimensions {..} points color width =
  let innerWidth = fromIntegral $ chartWidth - 2 * chartPadding
      innerHeight = fromIntegral $ chartHeight - 2 * chartPadding
      pathData =
        if V.null points
          then ""
          else
            "M "
              <> V.foldl1
                (\acc pt -> acc <> " L " <> pt)
                ( V.map
                    ( \(x, y) ->
                        ms (chartPadding + round (x * innerWidth))
                          <> ","
                          <> ms (chartHeight - chartPadding - round (y * innerHeight))
                    )
                    points
                )
   in Svg.path_
        [ d_ pathData,
          stroke_ color,
          strokeWidth_ width,
          fill_ "none"
        ]

drawBars :: ChartDimensions -> Vector (Int, Double) -> MisoString -> View model action
drawBars ChartDimensions {..} values color =
  g_ [] $
    V.toList $
      V.map drawBar values
  where
    innerWidth = chartWidth - 2 * chartPadding
    innerHeight = chartHeight - 2 * chartPadding
    nBars = V.length values
    barWidth = if nBars > 0 then innerWidth `div` nBars else 20

    drawBar (idx, value) =
      let x = chartPadding + idx * barWidth + barWidth `div` 4
          barHeight = round (value * fromIntegral innerHeight)
          y = chartHeight - chartPadding - barHeight
       in rect_
            [ x_ (ms x),
              y_ (ms y),
              width_ (ms $ barWidth `div` 2),
              height_ (ms barHeight),
              fill_ color,
              opacity_ "0.8"
            ]

drawLegend :: ChartDimensions -> [(MisoString, MisoString)] -> View model action
drawLegend ChartDimensions {..} items =
  g_ [] $
    zipWith drawItem [0 ..] items
  where
    drawItem idx (color, label) =
      g_
        []
        [ rect_
            [ x_ (ms $ chartWidth - 80),
              y_ (ms $ 5 + idx * 15),
              width_ "10",
              height_ "8",
              fill_ color
            ],
          Svg.text_
            [ x_ (ms $ chartWidth - 65),
              y_ (ms $ 11 + idx * 15),
              fontSize_ "10"
            ]
            [text label]
        ]

calculateScale :: Vector Double -> (Double, Double)
calculateScale values =
  if V.null values
    then (0, 1)
    else (V.minimum values, V.maximum values)

normalizeValues :: Vector Double -> Vector Double
normalizeValues values =
  let (minVal, maxVal) = calculateScale values
      range = maxVal - minVal
   in if range == 0
        then V.map (const 0.5) values
        else V.map (\v -> (v - minVal) / range) values

algorithmColors :: [(String, MisoString)]
algorithmColors =
  [ ("epsilon", "#4CAF50"), -- Green for ε-Greedy
    ("ucb", "#2196F3"), -- Blue for UCB
    ("thompson", "#FF9800") -- Orange for Thompson Sampling
  ]

getAlgorithmColor :: String -> MisoString
getAlgorithmColor "epsilon" = "#4CAF50"
getAlgorithmColor "ucb" = "#2196F3"
getAlgorithmColor "thompson" = "#FF9800"
getAlgorithmColor _ = "#666666"
