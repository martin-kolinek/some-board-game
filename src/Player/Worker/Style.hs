{-# LANGUAGE OverloadedStrings #-}

module Player.Worker.Style where

import Settings.Types
import Common.CssClass

import Clay
import qualified Clay.Flexbox as F
import Control.Monad

activeWorkerClass :: CssClass
activeWorkerClass = CssClass "active-worker"
strengthClass :: CssClass
strengthClass = CssClass "strength"

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
  let workerCommon = do
        cursor pointer
        F.flex 0 0 auto
        width (em 3.5)
        display inlineBlock
        height (em 3.5)
        textAlign end
        backgroundSize contain
        backgroundRepeat noRepeat
  star # classSelector strengthClass ? do
    backgroundColor "#848484"
    borderColor black
    fontWeight bold
    borderWidth (px 1)
    borderStyle solid
    width (em 1.4)
    height (em 1.3)
    display inlineBlock
    textAlign center
    paddingTop (em 0.1)
    margin (px 4) (px 4) (px 4) (px 4)
    borderRadius (em 0.7) (em 0.7) (em 0.7) (em 0.7)
  forM_ allPlayerColors $ \clr -> do
    star # classSelector (colorClass clr) ? do
      background $ colorUrl clr
      workerCommon
    star # classSelector (colorClass clr) # classSelector activeWorkerClass ? do
      background $ colorGlowingUrl clr
      workerCommon
