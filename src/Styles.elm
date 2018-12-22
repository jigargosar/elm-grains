module Styles exposing
    ( borderButton
    , borderButtonStyles
    , flatButton
    , flatButtonStyles
    , global
    )

import Css exposing (px, rem)
import Css.Global
import CssShorthand as CS
import CssTheme
import MaterialColor


globalStyles =
    [ Css.Global.id "css-container"
        [ Css.boxSizing Css.borderBox
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
    ]


global =
    Css.Global.global globalStyles


borderButtonStyles =
    [ Css.border3 (px 2) Css.solid CssTheme.primaryColor
    , CS.row
    , CS.p2 (rem 0.25) (rem 0.5)
    , Css.borderRadius (rem 0.25)
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
