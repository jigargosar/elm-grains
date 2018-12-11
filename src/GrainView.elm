module GrainView exposing (grainDisplayTitle, view, viewGrain, viewNotFound)

import BasicsX exposing (defaultEmptyStringTo, unwrapMaybe)
import Browser.Dom
import Css exposing (num, pct, px)
import CssLayout exposing (flexCol, flexColIC)
import CssShorthand as CS
import CssTheme exposing (space2)
import Grain exposing (Grain)
import Html.Styled exposing (Html, button, div, styled, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)
import Msg exposing (Msg)
import Skeleton
import Task exposing (Task)


view maybeGrain =
    Skeleton.viewChildren
        [ unwrapMaybe viewNotFound viewGrain maybeGrain
        ]


viewNotFound =
    div [] [ text "Not Found" ]


grainDisplayTitle =
    Grain.title >> defaultEmptyStringTo "<empty>"


viewGrain grain =
    let
        title =
            grainDisplayTitle grain
    in
    flexCol [ Css.padding space2 ] [] [ text title ]
