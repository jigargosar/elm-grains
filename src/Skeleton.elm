module Skeleton exposing (view)

import BasicsX exposing (defaultEmptyStringTo)
import Css exposing (num, pct, px)
import CssLayout exposing (flexCol, flexColIC)
import CssShorthand as CS
import CssTheme exposing (space2)
import EventX
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as SA exposing (..)
import Html.Styled.Events as SE exposing (onBlur, onClick, onFocus, onInput, onSubmit)
import Msg


view { onKeyDownPD, children } =
    flexCol
        [ CS.fg1
        , Css.minWidth <| pct 100
        , Css.height <| pct 100
        ]
        [ id "base-layer"
        , class "sans-serif"
        , SA.fromUnstyled <| EventX.onFocusIn <| Msg.BaseLayerFocusInChanged True
        , SA.fromUnstyled <| EventX.onFocusOut <| Msg.BaseLayerFocusInChanged False
        , tabindex -1
        , SA.fromUnstyled <| EventX.onKeyDownPD onKeyDownPD
        ]
        [ flexColIC [ CS.fg1 ]
            []
            [ flexCol [ CS.wpx 400, CS.fg1, CS.rel ]
                [ class "ba b--light-gray" ]
                children
            ]
        ]
