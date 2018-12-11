module GrainView exposing (view)

import BasicsX exposing (defaultEmptyStringTo, unwrapMaybe)
import Browser.Dom
import Css exposing (num, pct, px, zero)
import CssLayout exposing (flexCol, flexColIC)
import CssShorthand as CS
import CssTheme exposing (space2)
import Grain exposing (Grain)
import Html.Styled exposing (Html, button, div, input, styled, text, textarea)
import Html.Styled.Attributes exposing (autofocus, class, placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import Msg exposing (Msg)
import Skeleton
import Task exposing (Task)


view maybeGrain =
    [ unwrapMaybe viewNotFound viewGrain maybeGrain
    ]


viewNotFound =
    div [] [ text "Grain Not Found" ]


viewGrain grain =
    let
        title =
            Grain.title grain
    in
    styled textarea
        [ Css.displayFlex
        , CS.fg1
        , Css.resize Css.none
        , CS.p space2
        , Css.borderWidth zero
        ]
        [ placeholder "Start Typing..."
        , value title
        , autofocus True
        , onInput <| Msg.GrainTitleChanged (Grain.id grain)
        ]
        []
