module GrainListView exposing
    ( GrainListView
    , focusGrain
    , view
    )

import BasicsX exposing (defaultEmptyStringTo)
import Browser.Dom
import Css exposing (num, pct, px)
import CssLayout exposing (flexCol)
import CssTheme exposing (space2)
import Grain exposing (Grain)
import Html.Styled exposing (Html, div, styled, text)
import Html.Styled.Attributes exposing (class)
import Msg exposing (Msg)
import Task exposing (Task)


grainDomId : Grain -> String
grainDomId =
    Grain.toDomIdWithPrefix "grain-list-item--"


focusGrain : Grain -> Task Browser.Dom.Error ()
focusGrain grain =
    Browser.Dom.focus (grainDomId grain)


type alias GrainListView =
    { grainList : List Grain }


view : GrainListView -> Html Msg
view { grainList } =
    styled div
        [ Css.flexGrow <| num 1 ]
        [ class "flex flex-column items-center" ]
        [ styled div
            [ Css.width <| px 400
            , Css.flexGrow <| num 1
            ]
            [ class "flex flex-column pv3 ba b--light-gray" ]
            [ viewGrainList grainList ]
        ]


grainDisplayTitle =
    Grain.title >> defaultEmptyStringTo "<empty>"


viewGrainList list =
    let
        viewTitle title =
            flexCol [ Css.padding space2 ] [] [ text title ]

        viewItem g =
            let
                title =
                    grainDisplayTitle g
            in
            viewTitle title
    in
    flexCol [] [] (List.map viewItem list)
