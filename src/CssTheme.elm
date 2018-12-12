module CssTheme exposing
    ( black80
    , blackAlpha
    , contentWidth
    , space2
    , space4
    , white
    )

import Css exposing (hex, px)


black80 =
    Css.rgba 0 0 0 0.8


blackAlpha alpha =
    Css.rgba 0 0 0 alpha


space2 =
    px 8


space4 =
    px 16


white =
    hex "#fff"


contentWidth =
    px 500
