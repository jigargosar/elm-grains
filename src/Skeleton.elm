module Skeleton exposing (view)

import BasicsX exposing (defaultEmptyStringTo)
import Css exposing (num, pct, px, vh, vw)
import CssLayout exposing (flexCol)
import CssShorthand as CS
import CssTheme exposing (space2)
import EventX
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as SA
import Html.Styled.Events as SE
import StyledEvents as SE


view { children } =
    flexCol
        [ CS.min_h_screen
        , CssTheme.pageWidth
        , CS.max_w_screen
        , Css.margin Css.auto
        , Css.outline Css.none
        ]
        []
        [ flexCol
            []
            [ SA.class "ba b--light-gray" ]
            children
        ]
