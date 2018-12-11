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
    flexColIC
        [ CS.fg1
        , CS.absFill
        , Css.overflow Css.scroll
        ]
        [ SA.id "base-layer"
        , SA.class "sans-serif"
        , SE.onFocusIn <| Msg.BaseLayerFocusInChanged True
        , SE.onFocusOut <| Msg.BaseLayerFocusInChanged False
        , SA.tabindex -1
        , SE.onKeyDownPD onKeyDownPD
        ]
        [ flexCol
            [ CS.fg1
            , Css.width CssTheme.contentWidth
            , Css.maxWidth <| vw 100
            ]
            [ SA.class "ba b--light-gray" ]
            children
        ]
