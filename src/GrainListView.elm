module GrainListView exposing
    ( GrainListView
    , grainDomId
    , view
    )

import BasicsX exposing (defaultEmptyStringTo)
import Browser.Dom
import Css exposing (num, pct, px, rem, vh, vw, zero)
import CssElements
import CssIcons
import CssLayout exposing (flexCol, flexColIC, flexRow, flexRowIC)
import CssShorthand as CS
import CssTheme exposing (black80, blackAlpha, space1, space2, white)
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
    { grainList : List Grain }


view : GrainListView -> List (Html Msg)
view { grainList } =
    [ viewGrainList grainList
    , viewFab
    ]


viewFab =
    flexRow
        [ CS.fixed
        , Css.width <| Css.calc CssTheme.contentWidth Css.minus space2
        , Css.bottom <| space2
        , Css.justifyContent Css.flexEnd
        , Css.pointerEvents Css.none
        ]
        []
        [ CssElements.iconBtnWithStyles
            [ CS.rel
            , Css.pointerEventsAll
            , Css.right <| space2
            , Css.backgroundColor black80
            , Css.color white
            , Css.borderRadius <| px 9999
            , Css.boxShadow4 (px 1) (px 1) (px 8) (blackAlpha 0.5)
            , CS.p space1
            ]
            [ onClick Msg.addNewGrainClicked ]
            [ CssIcons.viewColorWhite CssIcons.add ]
        ]


grainDisplayTitle =
    Grain.title >> defaultEmptyStringTo "<empty>"


viewGrainList list =
    let
        viewTitle title g =
            flexRow
                [ Css.padding space1
                , CS.pointer
                , CS.flexGrow1
                ]
                [ onClick <| Msg.routeToGrain g ]
                [ text title ]

        viewDelete g =
            CssElements.iconBtnEl
                [ onClick <| Msg.deleteGrain g
                ]
                [ CssIcons.view CssIcons.delete
                ]

        viewItem g =
            let
                title =
                    grainDisplayTitle g
            in
            flexRowIC []
                []
                [ viewTitle title g
                , viewDelete g
                ]
    in
    flexCol
        [ CS.p space1
        , CS.flexGrow1
        , Css.marginBottom <| rem 3
        ]
        []
        (List.map viewItem list)
