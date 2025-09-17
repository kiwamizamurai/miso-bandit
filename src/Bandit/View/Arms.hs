{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bandit.View.Arms where

import Bandit.Actions (Action)
import Bandit.Algorithm.Types (ArmStats (..))
import Bandit.Metrics (confidenceInterval)
import Bandit.Model
import qualified Data.Vector as V
import Miso
import Miso.Html
import Miso.Html.Property (class_)
import Miso.String (MisoString, ms)
import Miso.Svg hiding (height_, width_)
import qualified Miso.Svg as Svg
import Miso.Svg.Property
import Text.Printf (printf)

armVisualization :: Model -> View Model Action
armVisualization Model {..} =
  div_
    [class_ "arms-container"]
    [ armProbabilitiesDisplay arms
    ]

algorithmSection :: String -> AlgorithmResult -> V.Vector Double -> View Model Action
algorithmSection name result arms =
  div_
    [class_ "algorithm-section"]
    [ h4_ [] [text $ ms name],
      div_ [class_ "arms-stats-grid"] $
        V.toList $
          V.imap (\i prob -> armStatsDisplay i prob (algArmStats result V.! i)) arms
    ]

armStatsDisplay :: Int -> Double -> ArmStats -> View Model Action
armStatsDisplay idx trueProb stats =
  let count = armCount stats
      expectation = armExpectation stats
      (ciLower, ciUpper) = confidenceInterval 0.95 stats
   in div_
        [class_ "arm-stat-card"]
        [ div_ [class_ "arm-stat-header"] [text $ "Arm " <> ms idx],
          div_
            [class_ "arm-stat-body"]
            [ statLine "Pulls" (show count),
              statLine "Est. p" (printf "%.3f" expectation),
              statLine "CI" (printf "[%.3f, %.3f]" ciLower ciUpper)
            ]
        ]

statLine :: String -> String -> View Model Action
statLine label value =
  div_
    [class_ "stat-line"]
    [ span_ [class_ "stat-label"] [text $ ms label <> ":"],
      span_ [class_ "stat-value"] [text $ ms value]
    ]

armProbabilitiesDisplay :: V.Vector Double -> View Model Action
armProbabilitiesDisplay arms =
  div_
    [class_ "arm-probabilities"]
    [ text "Arms: ",
      span_ [] $
        V.toList $
          V.imap
            ( \i p ->
                span_
                  []
                  [text $ ms (printf "[%d]=%.3f " i p :: String)]
            )
            arms,
      text $ " (Best: Arm " <> ms (V.maxIndex arms) <> ")"
    ]
