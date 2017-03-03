{-# LANGUAGE OverloadedStrings #-}

module Player.Worker.Style where

import Settings.Types
import Common.CssClass
import Common.CommonClasses

import Clay
import Control.Monad

activeWorkerClass :: CssClass
activeWorkerClass = CssClass "active-worker"
workerAnimationClass :: CssClass
workerAnimationClass = CssClass "worker-animation"

colorClass :: PlayerColor -> CssClass
colorClass PlayerRed = CssClass "worker-red"
colorClass PlayerBlue = CssClass "worker-blue"
colorClass PlayerCyan = CssClass "worker-cyan"
colorClass PlayerGreen = CssClass "worker-green"
colorClass PlayerWhite = CssClass "worker-white"
colorClass PlayerOrange = CssClass "worker-orange"
colorClass PlayerBlack = CssClass "worker-black"

colorUrl :: PlayerColor -> BackgroundImage
colorUrl PlayerRed = url "data/worker_red.svg"
colorUrl PlayerBlue = url "data/worker_blue.svg"
colorUrl PlayerCyan = url "data/worker_cyan.svg"
colorUrl PlayerGreen = url "data/worker_green.svg"
colorUrl PlayerWhite = url "data/worker_white.svg"
colorUrl PlayerOrange = url "data/worker_yellow.svg"
colorUrl PlayerBlack = url "data/worker_black.svg"

colorGlowingUrl :: PlayerColor -> BackgroundImage
colorGlowingUrl PlayerRed = url "data/worker_red_glowing.svg"
colorGlowingUrl PlayerBlue = url "data/worker_blue_glowing.svg"
colorGlowingUrl PlayerCyan = url "data/worker_cyan_glowing.svg"
colorGlowingUrl PlayerGreen = url "data/worker_green_glowing.svg"
colorGlowingUrl PlayerWhite = url "data/worker_white_glowing.svg"
colorGlowingUrl PlayerOrange = url "data/worker_yellow_glowing.svg"
colorGlowingUrl PlayerBlack = url "data/worker_black_glowing.svg"

workerStyle :: Css
workerStyle = do
  let workerFade = do
        opacity 0
        width (px 0)
        margin (px 0) (px 0) (px 0) (px 0)
        transitions [("opacity", sec 0.5, easeInOut, sec 0), ("width", sec 0.5, easeInOut, sec 0.5), ("margin", sec 0.5, easeInOut, sec 0.5)]
      workerCommon = do
        opacity 1
        width (em 3.7)
        margin (em 0.2) (em 0.2) (em 0.2) (em 0.2)
        transitions [("width", sec 0.5, easeInOut, sec 0), ("margin", sec 0.5, easeInOut, sec 0), ("opacity", sec 0.5, easeInOut, sec 0.5)]
        display inlineBlock
        height (em 3.7)
        backgroundSize contain
        backgroundRepeat noRepeat
  keyframes "worker-kf" [
      (0, opacity 0),
      (100, opacity 1)]
  forM_ allPlayerColors $ \clr -> do
    star # classSelector (colorClass clr) ? do
      background $ colorUrl clr
      workerCommon
    star # classSelector (colorClass clr) # classSelector activeWorkerClass ? do
      background $ colorGlowingUrl clr
      workerCommon
    star # classSelector (colorClass clr) # classSelector fadeClass ? workerFade
  star # classSelector workerAnimationClass ? do
    animationName "worker-kf"
    animationDuration (sec 1)
    animationIterationCount (iterationCount 1)
