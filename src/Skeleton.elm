module Skeleton exposing (view)

import BasicsX exposing (defaultEmptyStringTo)
import Css exposing (num, pct, px, vh, vw)
import CssEvents
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
        , CS.absFill
        , Css.overflow Css.scroll
        ]
        [ id "base-layer"
        , class "sans-serif"
        , CssEvents.onFocusIn <| Msg.BaseLayerFocusInChanged True
        , CssEvents.onFocusOut <| Msg.BaseLayerFocusInChanged False
        , tabindex -1
        , SA.fromUnstyled <| EventX.onKeyDownPD onKeyDownPD
        ]
        [ flexCol
            [ CS.fg1
            , Css.width CssTheme.contentWidth
            , Css.maxWidth <| vw 100
            ]
            [ class "ba b--light-gray" ]
            children
        ]
