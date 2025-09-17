{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bandit.View where

import Bandit.Actions
import Bandit.Model
import Bandit.View.Arms (armVisualization)
import Bandit.View.Charts (renderCharts)
import Bandit.View.Controls (controlPanel)
import Bandit.View.Statistics (statisticsPanel)
import Miso
import Miso.Html
import Miso.Html.Property (class_)
import Miso.String (MisoString, ms)

viewModel :: Model -> View Model Action
viewModel model =
  div_
    [class_ "app-container"]
    [ header model,
      div_
        [class_ "controls-section"]
        [ controlPanel model
        ],
      div_
        [class_ "info-section"]
        [ armVisualization model,
          statisticsPanel model
        ],
      div_
        [class_ "charts-section"]
        [ renderCharts model
        ],
      cssStyles
    ]

header :: Model -> View Model Action
header Model {..} =
  div_
    [class_ "header"]
    [ h1_ [] [text "Multi-Armed Bandit Simulator"]
    ]

cssStyles :: View Model Action
cssStyles =
  node
    HTML
    "style"
    []
    [ text $
        ms $
          unlines
            [ "body { font-family: -apple-system, sans-serif; background: white; margin: 0; padding: 10px; }",
              ".app-container { max-width: 1400px; margin: 0 auto; }",
              ".header { background: #333; color: white; padding: 10px; margin-bottom: 15px; }",
              ".header h1 { margin: 0; font-size: 1.2em; }",
              -- Main content layout
              ".controls-section { margin-bottom: 20px; }",
              ".info-section { margin-bottom: 20px; }",
              -- Control panel styles
              ".control-panel { padding: 5px 0; display: flex; align-items: center; flex-wrap: wrap; gap: 10px; }",
              ".control-group { display: inline-block; margin: 0 15px; }",
              ".control-group label { display: inline; margin-right: 5px; font-weight: 600; color: #333; font-size: 13px; }",
              ".arm-probability-controls { display: inline-flex; align-items: center; gap: 10px; border-right: 1px solid #ddd; padding-right: 15px; margin-right: 15px; }",
              ".arm-sliders { display: flex; gap: 10px; }",
              ".arm-control { display: flex; align-items: center; gap: 5px; }",
              ".arm-control label { margin: 0; font-size: 12px; }",
              ".arm-input { width: 60px; padding: 2px 5px; border: 1px solid #ddd; font-size: 12px; }",
              ".algorithm-select { width: 100%; padding: 10px; border: 2px solid #e0e0e0; border-radius: 6px; font-size: 16px; background: white; }",
              ".parameter-controls { display: inline-block; margin: 0 20px; }",
              ".param-input { width: 60px; padding: 2px 5px; border: 1px solid #ddd; font-size: 12px; margin-left: 5px; }",
              -- Button styles
              ".button-group { display: inline-flex; gap: 10px; margin: 0 20px; }",
              ".btn { padding: 5px 15px; border: none; border-radius: 4px; font-size: 12px; font-weight: 600; cursor: pointer; }",
              ".btn-primary { background: #333; color: white; }",
              ".btn-primary:hover { background: #555; }",
              ".btn-warning { background: #666; color: white; }",
              ".btn-warning:hover { background: #888; }",
              ".btn-danger { background: #999; color: white; }",
              ".btn-danger:hover { background: #aaa; }",
              ".btn-info { background: #666; color: white; }",
              ".btn-secondary { background: #999; color: white; }",
              ".btn-disabled { background: #ccc; cursor: not-allowed; opacity: 0.6; }",
              ".simulation-controls { display: inline-block; }",
              ".trial-control { display: inline-flex; align-items: center; gap: 5px; margin: 0 20px; }",
              ".trial-input { width: 80px; padding: 2px 5px; border: 1px solid #ddd; font-size: 12px; }",
              ".trial-info { color: #666; font-size: 12px; }",
              -- Arms visualization styles
              ".arms-container { background: white; padding: 25px; border-radius: 12px; box-shadow: 0 4px 15px rgba(0,0,0,0.08); }",
              ".arms-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px; margin-top: 20px; }",
              ".arm-card { background: #f9f9f9; padding: 20px; border-radius: 10px; border: 2px solid #e0e0e0; transition: all 0.3s ease; }",
              ".arm-card:hover { transform: translateY(-3px); box-shadow: 0 6px 20px rgba(0,0,0,0.1); }",
              ".best-arm { background: linear-gradient(135deg, #fff3cd 0%, #ffe8a1 100%); border-color: #ffc107; }",
              ".arm-header { font-weight: bold; font-size: 18px; margin-bottom: 15px; color: #333; display: flex; align-items: center; gap: 10px; }",
              ".best-badge { background: #ffc107; color: #856404; padding: 2px 8px; border-radius: 12px; font-size: 12px; }",
              ".arm-stats { margin: 10px 0; }",
              ".stat-row { display: flex; justify-content: space-between; margin: 8px 0; font-size: 14px; }",
              ".stat-label { color: #666; font-weight: 500; }",
              ".stat-value { color: #333; font-weight: 600; }",
              ".selection-bar-container { margin-top: 15px; }",
              ".selection-bar-background { width: 100%; height: 8px; background: #e0e0e0; border-radius: 4px; overflow: hidden; }",
              ".selection-bar-fill { height: 100%; background: #4CAF50; transition: width 0.3s ease; }",
              ".arm-mini-chart { margin-top: 10px; border: 1px solid #e0e0e0; border-radius: 4px; }",
              ".arm-comparison-chart { margin-top: 20px; }",
              -- Statistics panel styles
              ".statistics-panel { background: #f5f5f5; padding: 15px; border: 1px solid #ddd; }",
              ".stats-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin-top: 20px; }",
              ".metric-card { background: #f9f9f9; padding: 20px; border-radius: 10px; border-left: 4px solid #2196F3; }",
              ".metric-card h4 { margin: 0 0 15px 0; color: #333; font-size: 16px; }",
              ".metric-row { display: flex; justify-content: space-between; margin: 10px 0; font-size: 14px; }",
              ".metric-label { color: #666; font-weight: 500; }",
              ".metric-value { color: #333; font-weight: bold; }",
              ".regret-metrics { border-left-color: #F44336; }",
              ".theoretical-bounds { border-left-color: #FF9800; }",
              ".exploration-metrics { border-left-color: #9C27B0; }",
              ".per-action-regret { margin-top: 15px; padding-top: 15px; border-top: 1px solid #e0e0e0; }",
              ".regret-list { display: flex; flex-wrap: wrap; gap: 10px; margin-top: 10px; }",
              ".regret-item { background: white; padding: 5px 10px; border-radius: 15px; font-size: 12px; }",
              -- Charts styles
              ".charts-section { margin-top: 20px; }",
              ".charts-container { }",
              ".charts-grid-3x2 { display: grid; grid-template-columns: repeat(2, 1fr); gap: 10px; }",
              ".chart-wrapper { background: #f9f9f9; padding: 10px; border: 1px solid #ddd; }",
              ".chart-wrapper h3 { margin: 0 0 10px 0; color: #333; font-size: 14px; }",
              ".chart-wrapper svg { display: block; margin: 0 auto; }",
              -- Confidence intervals
              ".confidence-intervals { margin-top: 20px; }",
              ".confidence-intervals h4 { font-size: 14px; margin-bottom: 10px; }",
              ".ci-table { width: 100%; border-collapse: collapse; font-size: 12px; }",
              ".ci-table th { background: #f0f0f0; padding: 5px; text-align: left; font-weight: 600; }",
              ".ci-table td { padding: 5px; border-bottom: 1px solid #eee; }",
              ".ci-table tr:hover { background: #f9f9f9; }",
              -- Comparison table
              ".comparison-table { width: 100%; border-collapse: collapse; }",
              ".comparison-table th { background: #333; color: white; padding: 8px; text-align: left; font-size: 13px; }",
              ".comparison-table td { padding: 8px; border-bottom: 1px solid #ddd; font-size: 13px; }",
              ".comparison-table tr:hover { background: #f0f0f0; }",
              -- Responsive design
              "@media (max-width: 1200px) { .main-content { grid-template-columns: 1fr; } }",
              "@media (max-width: 768px) { .charts-grid { grid-template-columns: 1fr; } .stats-grid { grid-template-columns: 1fr; } }"
            ]
    ]
