{-# LANGUAGE OverloadedStrings #-}
module Player.Resources.Style where

import Common.CssClass

import Clay as C
import Data.Monoid ((<>))
import Data.Text as T

resourceHolderClass :: CssClass
resourceHolderClass = CssClass "resource-holder"

resourceIconClass :: CssClass
resourceIconClass = CssClass "resource-icon"

resourceLineClass :: Common.CssClass.CssClass
resourceLineClass = CssClass "resource-line"

woodIconClass :: Common.CssClass.CssClass
woodIconClass = CssClass "wood-icon"
stoneIconClass :: Common.CssClass.CssClass
stoneIconClass = CssClass "stone-icon"
goldIconClass :: Common.CssClass.CssClass
goldIconClass = CssClass "gold-icon"
ironIconClass :: Common.CssClass.CssClass
ironIconClass = CssClass "iron-icon"
wheatIconClass :: Common.CssClass.CssClass
wheatIconClass = CssClass "wheat-icon"
potatoIconClass :: Common.CssClass.CssClass
potatoIconClass = CssClass "potato-icon"
moneyIconClass :: Common.CssClass.CssClass
moneyIconClass = CssClass "money-icon"
foodIconClass :: Common.CssClass.CssClass
foodIconClass = CssClass "food-icon"

resourcesStyle :: Css
resourcesStyle = do
  star # classSelector resourceHolderClass ? do
    backgroundColor "#dddddd"
    maxWidth (em 12)
  star # classSelector resourceIconClass ? do
    width (em 2.5)
    height (em 1.2)
    marginTop auto
    marginBottom auto
    display inlineBlock
    backgroundSize contain
    backgroundRepeat noRepeat
    backgroundPosition $ placed sideCenter sideCenter
    marginRight (em 0.3)
  star # classSelector resourceLineClass ? do
    verticalAlign middle
    lineHeight (em 1.2)
    paddingTop (em 0.3)
    paddingLeft (em 0.3)
  resCls woodIconClass "wood"
  resCls stoneIconClass "stone"
  resCls goldIconClass "gold"
  resCls ironIconClass "iron"
  resCls wheatIconClass "wheat"
  resCls potatoIconClass "potato"
  resCls moneyIconClass "money"
  resCls foodIconClass "food"

resCls :: CssClass -> Text -> Css
resCls cls iconName = star # classSelector cls ? do backgroundImage (url ("data/" <> iconName <> ".svg"))

