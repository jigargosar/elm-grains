module Skeleton exposing (notFoundView, viewChildren)

import BasicsX exposing (defaultEmptyStringTo)
import Css exposing (num, pct, px)
import CssLayout exposing (flexCol, flexColIC)
import CssShorthand as CS
import CssTheme exposing (space2)
import Html.Styled exposing (Html, button, div, styled, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)


viewChildren children =
    flexColIC [ CS.fg1 ]
        []
        [ flexCol [ CS.wpx 400, CS.fg1, CS.p space2 ]
            [ class "ba b--light-gray" ]
            children
        ]


notFoundView =
    viewChildren [ div [] [ text "404 page not found" ] ]
