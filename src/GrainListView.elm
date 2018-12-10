module GrainListView exposing (GrainListView, view, viewGrainList)

import Css exposing (px)
import CssLayout exposing (flexCol)
import Grain exposing (Grain)
import Html.Styled exposing (Html, div, styled, text)
import Html.Styled.Attributes exposing (class)
import Msg exposing (Msg)


type alias GrainListView =
    { grainList : List Grain }


view : GrainListView -> Html Msg
view { grainList } =
    styled div
        []
        [ class "flex flex-column items-center" ]
        [ styled div
            [ Css.width <| px 400 ]
            [ class "flex flex-column pv3" ]
            [ viewGrainList grainList ]
        ]


viewGrainList list =
    flexCol []
        []
        (List.map
            (\g ->
                let
                    title =
                        Grain.title g
                in
                flexCol [] [] [ text title ]
            )
            list
        )
