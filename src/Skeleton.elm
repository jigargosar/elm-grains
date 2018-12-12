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
    flexCol
        [ CS.min_h_screen
        , CS.w_full
        , CS.max_w_xs
        , CS.relative
        , Css.margin Css.auto
        , Css.outline Css.none
        ]
        [ SA.id "base-layer"
        , SE.onFocusIn <| Msg.BaseLayerFocusInChanged True
        , SE.onFocusOut <| Msg.BaseLayerFocusInChanged False
        , SA.tabindex -1
        , SE.onKeyDownPD onKeyDownPD
        ]
        [ flexCol []
            [ SA.class "ba b--light-gray" ]
            children
        ]
