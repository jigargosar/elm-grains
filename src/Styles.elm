module Styles exposing
    ( borderButton
    , borderButtonStyles
    , flatButton
    , flatButtonStyles
    , global
    , globalForSelector
    , globalStylesForSelector
    )

import Css exposing (px, rem)
import Css.Global
import CssShorthand as CS
import CssTheme
import MaterialColor


globalStylesForSelector selector =
    selector
        [ CS.pa0
        , CS.ma0
        , CS.min_h_screen
        , CS.min_w_screen
        , CS.row
        , Css.boxSizing Css.borderBox
        , Css.property "font-size" "16px"
        , Css.color CssTheme.textColor
        , CssTheme.fontFamily
        , Css.Global.descendants
            [ Css.Global.button
                [ Css.property "font-size" "inherit"
                , Css.property "font-family" "inherit"
                ]
            ]
        ]


globalForSelector selector =
    Css.Global.global [ globalStylesForSelector selector ]


global =
    Css.Global.global [ globalStylesForSelector Css.Global.body ]


borderButtonStyles =
    [ Css.border3 (px 2) Css.solid CssTheme.primaryColor
    , CS.row
    , CS.p2 (rem 0.25) (rem 0.5)
    , Css.borderRadius (rem 0.25)
    , Css.borderRadius Css.zero
    , CS.uppercase
    , CS.pointer
    , Css.boxShadow4
        (px 1)
        (px 1)
        (px 2)
        CS.black20
    , Css.active
        [ Css.boxShadow5 Css.inset
            (px 1)
            (px 1)
            (px 2)
            CS.black20
        ]
    ]


flatButtonStyles =
    [ CS.row
    , CS.p2 (rem 0.25) (rem 0.5)
    , Css.borderWidth Css.zero
    , Css.color CS.dodgerBlue
    , CS.pointer
    ]


flatButton =
    Css.batch flatButtonStyles


borderButton =
    Css.batch borderButtonStyles
