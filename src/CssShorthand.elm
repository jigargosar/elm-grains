module CssShorthand exposing (aic, fg1, p, pointer, wpx)

import Css exposing (num, px)


aic =
    Css.alignItems Css.center


fg1 =
    Css.flexGrow <| num 1


wpx pVal =
    Css.width <| px pVal


p =
    Css.padding


p2 =
    Css.padding2


pointer =
    Css.cursor Css.pointer