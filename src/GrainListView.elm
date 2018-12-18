module GrainListView exposing
    ( GrainListView
    , grainDomId
    , view
    )

import BasicsX exposing (defaultEmptyStringTo, ter)
import Browser.Dom
import Css exposing (num, pct, px, rem, vh, vw, zero)
import CssElements
import CssIcons
import CssLayout exposing (flexCol, flexColIC, flexRow, flexRowIC)
import CssShorthand as CS
import CssTheme exposing (black80, blackAlpha, space2, space4, white)
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


type alias GrainListView =
    { grains : List Grain, deleted : List Grain }


view : GrainListView -> List (Html Msg)
view { grains, deleted } =
    [ flexCol [ Css.marginBottom <| rem 3 ]
        []
        [ flexCol
            [ CS.p space2
            , CS.flexGrow0
            ]
            []
            (viewGrainItems grains)
        , flexCol
            [ CS.p space2
            , CS.flexGrow0
            ]
            []
            (viewGrainItems deleted)
        ]
    , viewFab
    ]


viewFab =
    flexRow
        [ CS.fixed
        , Css.bottom <| space4
        , CssTheme.pageWidth
        , CS.max_w_full
        , Css.justifyContent Css.flexEnd
        , Css.pointerEvents Css.none
        ]
        []
        [ CssElements.iconBtnWithStyles
            [ CS.relative
            , Css.right <| space4
            , Css.pointerEventsAll
            , Css.backgroundColor black80
            , Css.color white
            , CS.br_pill
            , Css.boxShadow4 (px 1) (px 1) (px 8) (blackAlpha 0.5)
            , CS.p space2
            ]
            [ onClick Msg.CreateAndAddNewGrain ]
            [ CssIcons.viewColorWhite CssIcons.add ]
        ]


grainDisplayTitle =
    Grain.titleOrEmpty >> defaultEmptyStringTo "<empty>"


viewGrainItems list =
    let
        viewTitle title g =
            styled div
                [ CS.p2 space2 zero
                , CS.pointer
                , CS.flex11Auto
                , CS.ellipsis
                ]
                [ onClick <| Msg.routeToGrain g ]
                [ text title ]

        viewDelete g =
            let
                deleted =
                    Grain.deleted g

                action =
                    ter deleted Msg.RestoreGrain Msg.DeleteGrain <|
                        g

                icon =
                    ter deleted CssIcons.restore CssIcons.delete
            in
            CssElements.iconBtnWithStyles [ CS.asc ]
                [ onClick action
                ]
                [ CssIcons.view icon
                ]

        viewItem g =
            let
                title =
                    grainDisplayTitle g

                deleted =
                    Grain.deleted g

                opacityValue =
                    ter deleted 0.7 1
            in
            styled div
                [ Css.displayFlex
                , Css.flexDirection Css.row
                , Css.maxWidth <| pct 100
                , Css.opacity <| num opacityValue
                ]
                []
                [ viewTitle title g
                , viewDelete g
                ]
    in
    List.map viewItem list
