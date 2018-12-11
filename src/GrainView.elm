module GrainView exposing (..)
import BasicsX exposing (defaultEmptyStringTo)
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
import Task exposing (Task)


import CssLayout exposing (flexColIC)


view grain =
    flexColIC [ CS.fg1 ]
            []
            [ flexCol [ CS.wpx 400, CS.fg1, CS.p space2 ]
                [ class "ba b--light-gray" ]
                [ button [ onClick Msg.AddNewClicked ] [ text "add new empty" ]
                , viewGrain grain
                ]
            ]

grainDisplayTitle =
    Grain.title >> defaultEmptyStringTo "<empty>"

viewGrain grain =
    let
        title =
          grainDisplayTitle grain

    in

    flexCol [ Css.padding space2 ] [] [ text title ]
