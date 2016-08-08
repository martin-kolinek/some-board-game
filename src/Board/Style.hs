{-# LANGUAGE OverloadedStrings #-}

module Board.Style where

import Common.CssClass
import Board.Player.Style
import Board.Worker.Style
import Board.Settings.Style
import Prelude hiding (repeat)

import Clay
import qualified Clay.Flexbox as F

cardWrapperClass = CssClass "cardWrapper"
cardClass = CssClass "card"
workplacesClass = CssClass "workplaces"

boardStyle = do
  playerStyle
  workerStyle
  boardSettingsStyle
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
    F.flex 4 1 (pct 30)
    F.order 5
    margin (em 2) (em 2) (em 2) (em 2)
    minWidth (em 13)
    backgroundImage $ url "data/cardbackground.png"
    backgroundRepeat repeat
    borderRadius (px 15) (px 15) (px 15) (px 15)
    borderColor "#27231a"
    borderWidth (px 4)
    borderStyle solid
    minHeight (pct 90)
