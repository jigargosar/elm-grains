module CssTheme exposing
    ( black80
    , blackAlpha
    , fontFamily
    , pageWidth
    , primaryColor
    , space1
    , space2
    , space4
    , space8
    , textColor
    , white
    )

import Css exposing (hex, px, rem)
import CssShorthand as CS
import MaterialColor


black80 =
    Css.rgba 0 0 0 0.8


black20 =
    Css.rgba 0 0 0 0.8


blackAlpha alpha =
    Css.rgba 0 0 0 alpha


space1 =
    px 4


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


textColor =
    CS.fromMaterialColor MaterialColor.grey800


fontFamily =
    Css.property "font-family"
        """-apple-system, system-ui, BlinkMacSystemFont, "Segoe UI",
                   Roboto, "Helvetica Neue", sans-serif;"""


primaryColor =
    CS.dodgerBlue
