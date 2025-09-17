{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bandit.Actions (Action (..))
import Bandit.Model (Model, initialModel)
import Bandit.Update (updateModel)
import Bandit.View (viewModel)
import Miso

#if !defined(__GHCJS__) && !defined(wasm32_HOST_ARCH)
import Language.Javascript.JSaddle.Warp as JSaddle
import Control.Monad.IO.Class (liftIO)
#endif

#ifdef wasm32_HOST_ARCH
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
#if !defined(__GHCJS__) && !defined(wasm32_HOST_ARCH)
main = JSaddle.run 8080 $ do
  liftIO $ putStrLn "Running on http://localhost:8080"
  startApp app
#else
main = run (startApp app)
#endif

app :: App Model Action
app = component initialModel updateModel viewModel

