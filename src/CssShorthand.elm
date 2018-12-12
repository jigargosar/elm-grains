module CssShorthand exposing
    ( abs
    , absFill
    , abs__fill
    , aic
    , fixed
    , flexGrow1
    , fs0
    , jcc
    , maxHeight100VH
    , maxWidth100VW
    , min100V
    , minHeight100VH
    , minWidth100VW
    , overflowScroll
    , p
    , pointer
    , rel
    , sticky
    , w100
    , wpx
    )

import BasicsX exposing (callWith)
import Css exposing (num, pct, px, vh, vw, zero)


overflowScroll =
    Css.overflow Css.scroll


w100 =
    Css.width <| pct 100


min100V =
    Css.batch [ minWidth100VW, minHeight100VH ]


maxWidth100VW =
    Css.maxWidth <| vw 100


minWidth100VW =
    Css.minWidth <| vw 100


maxHeight100VH =
    Css.maxHeight <| vh 100


minHeight100VH =
    Css.minHeight <| vh 100


fs0 =
    Css.flexShrink <| num 0


aic =
    Css.alignItems Css.center


jcc =
    Css.justifyContent Css.center


flexGrow1 =
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
