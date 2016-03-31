{-# LANGUAGE OverloadedStrings #-}

module Board.Worker.Style where

import Common.CssClass hiding (fadeClass)
import qualified Common.CssClass as C

import Clay

workerClass = CssClass "worker"
activeWorkerClass = CssClass "active-worker"
fadeClass = C.fadeClass

workerStyle = do
  let workerFade = do
        opacity 0
        width (px 0)
        margin (px 0) (px 0) (px 0) (px 0)
        transitions [("opacity", sec 0.5, easeInOut, sec 0), ("width", sec 0.5, easeInOut, sec 0.5), ("margin", sec 0.5, easeInOut, sec 0.5)]
      workerCommon = do
        opacity 1
        width (em 4)
        margin (em 0.2) (em 0.2) (em 0.2) (em 0.2)
        transitions [("width", sec 0.5, easeInOut, sec 0), ("margin", sec 0.5, easeInOut, sec 0), ("opacity", sec 0.5, easeInOut, sec 0.5)]
        display inlineBlock
        height $ em 4
        backgroundSize contain
        backgroundRepeat noRepeat
        animationName "worker-kf"
        animationDuration (sec 1)
        animationIterationCount (iterationCount 1)
  keyframes "worker-kf" [
      (0, opacity 0),
      (100, opacity 1)]
  star # classSelector workerClass ? do
    background $ url "data/worker.svg"
    workerCommon
  star # classSelector activeWorkerClass ? do
    background (url "data/worker_glowing.svg")
    workerCommon
  star # classSelector workerClass # classSelector fadeClass ? workerFade
  star # classSelector activeWorkerClass # classSelector fadeClass ? workerFade
