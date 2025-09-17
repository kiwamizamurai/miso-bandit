{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bandit.View.Controls where

import Bandit.Actions
import Bandit.Model
import qualified Data.Vector as V
import Miso
import Miso.Html
import Miso.Html.Property (class_, max_, min_, step_, type_, value_)
import Miso.String (MisoString, ms)
import Text.Printf (printf)

controlPanel :: Model -> View Model Action
controlPanel Model {..} =
  div_
    [class_ "control-panel"]
    [ armProbabilityControls arms,
      parameterControls Model {..},
      simulationControls Model {..}
    ]

armProbabilityControls :: V.Vector Double -> View Model Action
armProbabilityControls arms =
  div_
    [class_ "arm-probability-controls"]
    [ div_ [class_ "arm-sliders"] $
        V.toList $
          V.imap
            ( \i prob ->
                div_
                  [class_ "arm-control"]
                  [ label_ [] [text $ ms $ "Arm " ++ show i ++ ": "],
                    input_
                      [ type_ "number",
                        min_ "0",
                        max_ "1",
                        step_ "0.01",
                        value_ (ms (printf "%.2f" prob :: String)),
                        onInput (SetArmProbability i . read . show),
                        class_ "arm-input"
                      ]
                  ]
            )
            arms
    ]

parameterControls :: Model -> View Model Action
parameterControls Model {..} =
  div_
    [class_ "parameter-controls"]
    [ div_
        [class_ "control-group epsilon-control"]
        [ label_ [] [text "ε: "],
          input_
            [ type_ "number",
              min_ "0",
              max_ "1",
              step_ "0.01",
              value_ (ms (printf "%.2f" (epsilon algorithmParams) :: String)),
              onInput (SetEpsilon . read . show),
              class_ "param-input"
            ]
        ],
      div_
        [class_ "control-group ucb-control"]
        [ label_ [] [text "c: "],
          input_
            [ type_ "number",
              min_ "0.1",
              max_ "5",
              step_ "0.1",
              value_ (ms (printf "%.1f" (ucbC algorithmParams) :: String)),
              onInput (SetUCBC . read . show),
              class_ "param-input"
            ]
        ]
    ]

simulationControls :: Model -> View Model Action
simulationControls Model {..} =
  div_
    [class_ "simulation-controls"]
    [ div_
        [class_ "button-group main-controls"]
        [ button_
            [ onClick Start,
              class_ $ if isRunning then "btn btn-disabled" else "btn btn-primary"
            ]
            [text "Start"],
          button_
            [ onClick Stop,
              class_ $ if not isRunning then "btn btn-disabled" else "btn btn-warning"
            ]
            [text "Stop"],
          button_
            [ onClick Reset,
              class_ "btn btn-danger"
            ]
            [text "Reset"]
        ],
      div_
        [class_ "trial-control"]
        [ label_ [] [text "Trials: "],
          input_
            [ type_ "number",
              value_ (ms totalTrials),
              min_ "10",
              max_ "10000",
              step_ "100",
              onInput (SetTrials . read . show),
              class_ "trial-input"
            ],
          span_
            [class_ "trial-info"]
            [text $ ms (printf " %d/%d" currentTrial totalTrials :: String)]
        ]
    ]
