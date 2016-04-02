{-# LANGUAGE OverloadedStrings #-}

module Board.Style where

import Common.CssClass
import Board.Player.Style
import Board.Worker.Style
import Prelude hiding (repeat)

import Clay

cardWrapperClass = CssClass "cardWrapper"
cardClass = CssClass "card"
workplacesClass = CssClass "workplaces"

boardStyle = do
  playerStyle
  workerStyle
  star # classSelector cardWrapperClass ? do
    width $ em 8
    display inlineBlock
    position relative
    margin (em 0.5) (em 0.5) (em 0.5) (em 0.5)
  star # classSelector cardWrapperClass # after ? do
    paddingTop $ pct 143.28
    display block
    content $ stringContent ""
  star # classSelector cardClass ? do
    position absolute
    top $ px 0
    bottom $ px 0
    left $ px 0
    right $ px 0
    padding (em 1) (em 1) (em 1) (em 1)
    background $ url "data/card.svg"
    backgroundSize contain
    backgroundRepeat noRepeat
  star # classSelector workplacesClass ? do
    width (pct 60)
    backgroundImage $ url "data/cardbackground.png"
    backgroundRepeat repeat
    borderRadius (px 10) (px 10) (px 10) (px 10)
    borderColor "#1c0d00"
    borderWidth (px 4)
    borderStyle solid
