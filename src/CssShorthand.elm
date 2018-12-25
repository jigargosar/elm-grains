module CssShorthand exposing
    ( absolute
    , batchIf
    , bgBlack20
    , bgWhite
    , black20
    , black80
    , blackAlpha
    , bold
    , br_pill
    , debug
    , dodgerBlue
    , ellipsis
    , fixed
    , flex11Auto
    , flexGrow0
    , flexGrow1
    , flexShrink0
    , fromMaterialColor
    , h_full
    , h_screen
    , inlineRow
    , itemsCenter
    , justifyCenter
    , ma0
    , max_h_screen
    , max_w_full
    , max_w_screen
    , max_w_sm
    , max_w_xs
    , min100V
    , min_h_full
    , min_h_screen
    , min_w_screen
    , move
    , overflowScroll
    , p2
    , pa
    , pa0
    , pointer
    , posFill
    , pv
    , relative
    , row
    , rowCC
    , selfCenter
    , sticky
    , styleIf
    , uppercase
    , w_full
    , w_screen
    , w_sm
    , w_xs
    , wpx
    )

import BasicsX exposing (callWith)
import Css exposing (num, pct, px, rem, vh, vw, zero)
import MaterialColor


fromMaterialColor { red, green, blue } =
    Css.rgb red green blue


noStyle =
    Css.batch []


dodgerBlue =
    Css.hsla 210 1 0.56 1


styleIf bool style =
    if bool then
        style

    else
        noStyle


batchIf bool styles =
    if bool then
        Css.batch styles

    else
        noStyle


bold =
    Css.fontWeight Css.bold


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


bgWhite =
    Css.backgroundColor <| Css.hex "fff"


overflowScroll =
    Css.overflow Css.scroll


full =
    pct 100


w_full =
    Css.width full


h_full =
    Css.height full


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
    Css.batch [ min_w_screen, min_h_screen ]


max_w_screen =
    Css.maxWidth <| vw 100


min_w_screen =
    Css.minWidth <| vw 100


max_h_screen =
    Css.maxHeight <| vh 100


min_h_screen =
    Css.minHeight <| vh 100


min_h_full =
    Css.minHeight <| pct 100


w_screen =
    Css.width <| vw 100


h_screen =
    Css.height <| vh 100


debug =
    Css.boxShadow4
        (px 0)
        (px 0)
        (px 50)
        (fromMaterialColor MaterialColor.grey500)


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


inlineRow =
    Css.batch [ Css.display Css.inlineFlex, Css.flexDirection Css.row ]


wpx pVal =
    Css.width <| px pVal


pa =
    Css.padding


p2 =
    Css.padding2


pa0 =
    Css.padding Css.zero


ma0 =
    Css.margin Css.zero


pv unit =
    Css.batch [ Css.paddingTop unit, Css.paddingBottom unit ]


pointer =
    Css.cursor Css.pointer


move =
    Css.cursor Css.move


uppercase =
    Css.textTransform Css.uppercase


absolute =
    Css.position Css.absolute


posFill =
    [ Css.left, Css.right, Css.top, Css.bottom ]
        |> List.map (callWith <| px 0)
        |> Css.batch


relative =
    Css.position Css.relative


sticky =
    Css.position Css.sticky


fixed =
    Css.position Css.fixed
