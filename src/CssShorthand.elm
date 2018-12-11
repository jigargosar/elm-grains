module CssShorthand exposing (..)

import Css exposing (num, px)

aic =
    Css.alignItems Css.center

fg1 = Css.flexGrow <| num 1

wpx pVal= Css.width <| px pVal

