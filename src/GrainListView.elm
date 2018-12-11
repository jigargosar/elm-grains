module GrainListView exposing
    ( GrainListView
    , focusGrain
    , view
    )

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
import Route
import Skeleton
import Task exposing (Task)


grainDomId : Grain -> String
grainDomId =
    Grain.toDomIdWithPrefix "grain-list-item--"


focusGrain : Grain -> Task Browser.Dom.Error ()
focusGrain grain =
    Browser.Dom.focus (grainDomId grain)


type alias GrainListView =
    { grainList : List Grain }


view : GrainListView -> List (Html Msg)
view { grainList } =
    [ button [ onClick Msg.AddNewClicked ] [ text "add new empty" ]
    , viewGrainList grainList
    ]


grainDisplayTitle =
    Grain.title >> defaultEmptyStringTo "<empty>"


viewGrainList list =
    let
        viewTitle title g =
            flexCol
                [ Css.padding space2
                , CS.pointer
                ]
                [ onClick <| Msg.routeToGrain g ]
                [ text title ]

        viewItem g =
            let
                title =
                    grainDisplayTitle g
            in
            viewTitle title g
    in
    flexCol [] [] (List.map viewItem list)
