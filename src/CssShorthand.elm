module CssShorthand exposing
    ( absFill
    , absolute
    , bgBlack20
    , br_pill
    , ellipsis
    , fixed
    , flex11Auto
    , flexGrow0
    , flexGrow1
    , flexShrink0
    , itemsCenter
    , justifyCenter
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
    , row
    , rowCC
    , selfCenter
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


black80 =
    Css.rgba 0 0 0 0.8


black20 =
    Css.rgba 0 0 0 0.2


blackAlpha alpha =
    Css.rgba 0 0 0 alpha



--bgBlack80 =
--    Css.backgroundColor black80


bgBlack20 =
    Css.backgroundColor black20


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


flexShrink0 =
    Css.flexShrink <| num 0


itemsCenter =
    Css.alignItems Css.center


selfCenter =
    Css.alignSelf Css.center


justifyCenter =
    Css.justifyContent Css.center


flexGrow1 =
    Css.flexGrow <| num 1


flexGrow0 =
    Css.flexGrow <| num 0


rowCC =
    Css.batch [ row, itemsCenter, justifyCenter ]


row =
    Css.batch [ Css.displayFlex, Css.flexDirection Css.row ]


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


absFill =
    [ Css.left, Css.right, Css.top, Css.bottom ]
        |> List.map (callWith <| px 0)
        |> Css.batch


relative =
    Css.position Css.relative


sticky =
    Css.position Css.sticky


fixed =
    Css.position Css.fixed
