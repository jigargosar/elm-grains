module CssTheme exposing
    ( black80
    , blackAlpha
    , contentWidth
    , space1
    , space2
    , white
    )

import Css exposing (hex, px, rem)


black80 =
    Css.rgba 0 0 0 0.8


blackAlpha alpha =
    Css.rgba 0 0 0 alpha


space1 =
    rem 0.5


space2 =
    rem 1


white =
    hex "#fff"


contentWidth =
    px 500
