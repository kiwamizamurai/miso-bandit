{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bandit.View.Statistics where

import Bandit.Actions (Action)
import Bandit.Algorithm.Types (ArmStats (..))
import Bandit.Metrics
import Bandit.Model
import qualified Data.Vector as V
import Miso
import Miso.Html
import Miso.Html.Property (class_)
import Miso.String (MisoString, ms)
import Text.Printf (printf)

statisticsPanel :: Model -> View Model Action
statisticsPanel model@Model {..} =
  if currentTrial > 0
    then
      div_
        [class_ "statistics-panel"]
        [algorithmComparisonTable model]
    else div_ [] []

algorithmMetrics :: String -> AlgorithmResult -> V.Vector Double -> Model -> View Model Action
algorithmMetrics name result armsVec Model {..} =
  div_
    [class_ "metric-card"]
    [ h4_ [] [text $ ms name],
      metricRow "Total Reward" $
        printf "%.0f" (if V.null (algCumulativeRewards result) then 0 else V.last (algCumulativeRewards result)),
      metricRow "Average Reward" $
        printf
          "%.3f"
          ( if currentTrial > 0 && not (V.null (algCumulativeRewards result))
              then V.last (algCumulativeRewards result) / fromIntegral currentTrial
              else 0 :: Double
          ),
      metricRow "Best Arm Rate" $
        printf "%.1f%%" (bestArmRate armsVec result 100),
      metricRow "Cumulative Regret" $
        printf "%.2f" (cumulativeRegret armsVec result),
      metricRow "Simple Regret" $
        printf "%.4f" (simpleRegret armsVec result),
      metricRow "Exploration Ratio" $
        printf "%.2f%%" (explorationRatio result * 100)
    ]

metricRow :: String -> String -> View Model Action
metricRow label value =
  div_
    [class_ "metric-row"]
    [ span_ [class_ "metric-label"] [text $ ms label <> ": "],
      span_ [class_ "metric-value"] [text $ ms value]
    ]

algorithmComparisonTable :: Model -> View Model Action
algorithmComparisonTable Model {..} =
  if currentTrial > 0
    then
      table_
        [class_ "comparison-table"]
        [ thead_
            []
            [ tr_
                []
                [ th_ [] [text "Metric"],
                  th_ [] [text "ε-Greedy"],
                  th_ [] [text "UCB"],
                  th_ [] [text "Thompson"]
                ]
            ],
          tbody_
            []
            [ compareRow
                "Total Reward"
                (getLastReward epsilonGreedyResult)
                (getLastReward ucbResult)
                (getLastReward thompsonResult),
              compareRowPct
                "Best Arm Rate %"
                (bestArmRate arms epsilonGreedyResult 100)
                (bestArmRate arms ucbResult 100)
                (bestArmRate arms thompsonResult 100),
              compareRow
                "Cumulative Regret"
                (cumulativeRegret arms epsilonGreedyResult)
                (cumulativeRegret arms ucbResult)
                (cumulativeRegret arms thompsonResult),
              compareRow
                "Simple Regret"
                (simpleRegret arms epsilonGreedyResult)
                (simpleRegret arms ucbResult)
                (simpleRegret arms thompsonResult),
              compareRowPct
                "Exploration Ratio %"
                (explorationRatio epsilonGreedyResult * 100)
                (explorationRatio ucbResult * 100)
                (explorationRatio thompsonResult * 100)
            ]
        ]
    else div_ [] []
  where
    getLastReward result = if V.null (algCumulativeRewards result) then 0 else V.last (algCumulativeRewards result)
    compareRow label eg ucb ts =
      tr_
        []
        [ td_ [] [text $ ms label],
          td_ [] [text $ ms (printf "%.2f" eg :: String)],
          td_ [] [text $ ms (printf "%.2f" ucb :: String)],
          td_ [] [text $ ms (printf "%.2f" ts :: String)]
        ]
    compareRowPct label eg ucb ts =
      tr_
        []
        [ td_ [] [text $ ms label],
          td_ [] [text $ ms (printf "%.1f" eg :: String)],
          td_ [] [text $ ms (printf "%.1f" ucb :: String)],
          td_ [] [text $ ms (printf "%.1f" ts :: String)]
        ]
