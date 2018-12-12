module Skeleton exposing (view)

import BasicsX exposing (defaultEmptyStringTo)
import Css exposing (num, pct, px, vh, vw)
import CssLayout exposing (flexCol, flexColIC)
import CssShorthand as CS
import CssTheme exposing (space2)
import EventX
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as SA
import Html.Styled.Events as SE
import Msg
import StyledEvents as SE


view { onKeyDownPD, children } =
    flexCol [ CS.minHeight100VH ]
        [ SA.id "base-layer"
        , SA.class "container grid-xs"
        , SE.onFocusIn <| Msg.BaseLayerFocusInChanged True
        , SE.onFocusOut <| Msg.BaseLayerFocusInChanged False
        , SA.tabindex -1
        , SE.onKeyDownPD onKeyDownPD
        ]
        [ flexCol []
            [ SA.class "columns ba b--light-gray" ]
            children
        ]
