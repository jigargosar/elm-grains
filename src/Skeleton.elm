module Skeleton exposing (view)

import BasicsX exposing (defaultEmptyStringTo)
import Css exposing (num, pct, px, vh, vw)
import CssLayout exposing (flexCol, flexColIC)
import CssShorthand as CS
import CssTheme exposing (space2)
import EventX
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as SA exposing (..)
import Html.Styled.Events as SE exposing (onBlur, onClick, onFocus, onInput, onSubmit)
import Msg


view { onKeyDownPD, children } =
    flexColIC
        [ CS.fg1
        , CS.abs
        ]
        [ id "base-layer"
        , class "sans-serif absolute--fill"
        , SA.fromUnstyled <| EventX.onFocusIn <| Msg.BaseLayerFocusInChanged True
        , SA.fromUnstyled <| EventX.onFocusOut <| Msg.BaseLayerFocusInChanged False
        , tabindex -1
        , SA.fromUnstyled <| EventX.onKeyDownPD onKeyDownPD
        ]
        [ flexCol
            [ CS.fg1
            , CS.wpx 500
            , Css.maxWidth <| vh 100
            , CS.rel
            ]
            [ class "ba b--light-gray" ]
            children
        ]
