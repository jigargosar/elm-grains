module CssShorthand exposing
    ( absFill
    , abs__fill
    , absolute
    , aic
    , asc
    , br_pill
    , ellipsis
    , fixed
    , flex11Auto
    , flexGrow1
    , fs0
    , jcc
    , maxHeight100VH
    , max_w_full
    , max_w_screen
    , max_w_sm
    , max_w_xs
    , min100V
    , minWidth100VW
    , min_h_screen
    , overflowScroll
    , p
    , p2
    , pointer
    , relative
    , sticky
    , w_full
    , w_sm
    , w_xs
    , wpx
    )

import BasicsX exposing (callWith)
import Css exposing (num, pct, px, rem, vh, vw, zero)


flex11Auto =
    Css.batch
        [ Css.flexShrink <| num 1
        , Css.flexGrow <| num 1
        , Css.flexBasis Css.auto
        ]


ellipsis =
    Css.batch
        [ Css.display Css.inlineBlock
        , Css.overflow Css.hidden
        , Css.textOverflow Css.ellipsis
        , Css.whiteSpace Css.noWrap
        ]


overflowScroll =
    Css.overflow Css.scroll


full =
    pct 100


w_full =
    Css.width full


max_w_full =
    Css.maxWidth full


br_pill =
    Css.borderRadius <| px 9999


sm =
    rem 30


w_sm =
    Css.width sm


max_w_sm =
    Css.maxWidth <| rem 30


xs =
    rem 20


w_xs =
    Css.width xs


max_w_xs =
    Css.maxWidth xs


min100V =
    Css.batch [ minWidth100VW, min_h_screen ]


max_w_screen =
    Css.maxWidth <| vw 100


minWidth100VW =
    Css.minWidth <| vw 100


maxHeight100VH =
    Css.maxHeight <| vh 100


min_h_screen =
    Css.minHeight <| vh 100


fs0 =
    Css.flexShrink <| num 0


aic =
    Css.alignItems Css.center


asc =
    Css.alignSelf Css.center


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


absolute =
    Css.position Css.absolute


abs__fill =
    [ Css.left, Css.right, Css.top, Css.bottom ]
        |> List.map (callWith <| px 0)
        |> Css.batch


absFill =
    Css.batch [ Css.position Css.absolute, abs__fill ]


relative =
    Css.position Css.relative


sticky =
    Css.position Css.sticky


fixed =
    Css.position Css.fixed
