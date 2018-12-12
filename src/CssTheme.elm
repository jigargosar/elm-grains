module CssTheme exposing
    ( black80
    , blackAlpha
    , pageWidth
    , space2
    , space4
    , space8
    , white
    )

import Css exposing (hex, px, rem)
import CssShorthand as CS


black80 =
    Css.rgba 0 0 0 0.8


blackAlpha alpha =
    Css.rgba 0 0 0 alpha


space2 =
    px 8


space4 =
    px 16


space8 =
    px 32


white =
    hex "#fff"


pageWidth =
    CS.w_xs
