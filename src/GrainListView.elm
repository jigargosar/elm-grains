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
    { grainList : List Grain }


view : GrainListView -> List (Html Msg)
view { grainList } =
    [ viewGrainList grainList
    , viewFab
    ]


viewFab =
    flexRow
        [ CS.fixed
        , Css.width <| px 500
        , Css.maxWidth <| vw 100
        , Css.height <| vh 100
        , Css.alignItems Css.flexEnd
        , Css.justifyContent Css.flexEnd
        ]
        []
        [ CssElements.iconBtnWithStyles
            [ Css.backgroundColor black80
            , Css.color white
            , Css.borderRadius <| px 9999
            , Css.boxShadow4 (px 1) (px 1) (px 8) (blackAlpha 0.5)
            , CS.p space2
            ]
            [ onClick Msg.AddNewClicked ]
            [ CssIcons.viewColorWhite CssIcons.add ]
        ]


grainDisplayTitle =
    Grain.title >> defaultEmptyStringTo "<empty>"


viewGrainList list =
    let
        viewTitle title g =
            flexRow
                [ Css.padding space2
                , CS.pointer
                , CS.fg1
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
        [ CS.p space2
        , CS.fg1
        ]
        []
        (List.map viewItem list)
