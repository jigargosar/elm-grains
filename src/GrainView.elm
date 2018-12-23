module GrainView exposing (GrainView, autoFocusId, view)

import BasicsX exposing (defaultEmptyStringTo, unwrapMaybe)
import Browser.Dom
import Css exposing (num, pct, px, zero)
import CssLayout exposing (flexCol)
import CssShorthand as CS
import CssTheme exposing (space2)
import Grain exposing (Grain)
import Html.Styled exposing (Html, button, div, input, styled, text, textarea)
import Html.Styled.Attributes exposing (autofocus, class, id, placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import Skeleton
import Task exposing (Task)


autoFocusId =
    "grain-view-input"


type alias GrainView msg =
    Maybe
        { contentChangedMsg : String -> msg
        , content : String
        }


view vm =
    [ unwrapMaybe viewNotFound viewGrain vm
    ]


viewNotFound =
    div [] [ text "Grain Not Found" ]


viewGrain vm =
    styled textarea
        [ Css.displayFlex
        , CS.flexGrow1
        , Css.resize Css.none
        , CS.pa space2
        , Css.borderWidth zero
        ]
        [ id autoFocusId
        , placeholder "Start Typing..."
        , value vm.content
        , autofocus True
        , onInput vm.contentChangedMsg
        ]
        []
