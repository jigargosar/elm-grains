module GrainView exposing (autoFocusId, view)

import BasicsX exposing (defaultEmptyStringTo, unwrapMaybe)
import Browser.Dom
import Css exposing (num, pct, px, zero)
import CssLayout exposing (flexCol, flexColIC)
import CssShorthand as CS
import CssTheme exposing (space2)
import Grain exposing (Grain)
import Html.Styled exposing (Html, button, div, input, styled, text, textarea)
import Html.Styled.Attributes exposing (autofocus, class, id, placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import Msg exposing (Msg)
import Skeleton
import Task exposing (Task)


autoFocusId =
    "grain-view-input"


view maybeGrain =
    [ unwrapMaybe viewNotFound viewGrain maybeGrain
    ]


viewNotFound =
    div [] [ text "Grain Not Found" ]


viewGrain grain =
    styled textarea
        [ Css.displayFlex
        , CS.flexGrow1
        , Css.resize Css.none
        , CS.p space2
        , Css.borderWidth zero
        ]
        [ id autoFocusId
        , placeholder "Start Typing..."
        , value <| Grain.content grain
        , autofocus True
        , onInput <| Msg.GrainContentChanged grain
        ]
        []
