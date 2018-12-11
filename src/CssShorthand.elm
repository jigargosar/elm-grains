module CssShorthand exposing
    ( abs
    , aic
    , fg1
    , fixed
    , fs0
    , p
    , pointer
    , rel
    , sticky
    , wpx
    )

import Css exposing (num, px)


fs0 =
    Css.flexShrink <| num 0


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


abs =
    Css.position Css.absolute


rel =
    Css.position Css.relative


sticky =
    Css.position Css.sticky


fixed =
    Css.position Css.fixed
