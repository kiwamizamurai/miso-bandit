{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bandit.View.Charts where

import Bandit.Actions (Action)
import Bandit.Model
import Bandit.View.Charts.Arms
import Bandit.View.Charts.Regret
import Bandit.View.Charts.Rewards
import Miso
import Miso.Html
import Miso.Html.Property (class_)

renderCharts :: Model -> View Model Action
renderCharts model@Model {..} =
  if currentTrial > 0
    then
      div_
        [class_ "charts-container"]
        [ div_
            [class_ "charts-grid-3x2"]
            [ -- Row 1, Column 1
              cumulativeRewardsChart model,
              -- Row 1, Column 2
              cumulativeRegretChart model,
              -- Row 2, Column 1
              armConfidenceIntervalsChart model,
              -- Row 2, Column 2
              averageRewardChart model,
              -- Row 3, Column 1
              simpleRegretChart model,
              -- Row 3, Column 2
              bestArmConvergenceChart model
            ]
        ]
    else div_ [] []
