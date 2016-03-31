{-# LANGUAGE OverloadedStrings #-}

module Board.Style where

import Common.CssClass
import Board.Player.Style
import Board.Worker.Style

import Clay

cardWrapperClass = CssClass "cardWrapper"
cardClass = CssClass "card"

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
