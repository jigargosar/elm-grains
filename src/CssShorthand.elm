module CssShorthand exposing
    ( abs
    , absFill
    , abs__fill
    , aic
    , fg1
    , fixed
    , fs0
    , jcc
    , overflowScroll
    , p
    , pointer
    , rel
    , sticky
    , wpx
    )

import BasicsX exposing (callWith)
import Css exposing (num, px, zero)


overflowScroll =
    Css.overflow Css.scroll


fs0 =
    Css.flexShrink <| num 0


aic =
    Css.alignItems Css.center


jcc =
    Css.justifyContent Css.center


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


abs__fill =
    [ Css.left, Css.right, Css.top, Css.bottom ]
        |> List.map (callWith <| px 0)
        |> Css.batch


absFill =
    Css.batch [ abs, abs__fill ]


rel =
    Css.position Css.relative


sticky =
    Css.position Css.sticky


fixed =
    Css.position Css.fixed
