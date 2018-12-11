module GrainListView exposing
    ( GrainListView
    , focusGrain
    , view
    )

import BasicsX exposing (defaultEmptyStringTo)
import Browser.Dom
import Css exposing (num, pct, px)
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


focusGrain : Grain -> Task Browser.Dom.Error ()
focusGrain grain =
    Browser.Dom.focus (grainDomId grain)


type alias GrainListView =
    { grainList : List Grain }


view : GrainListView -> List (Html Msg)
view { grainList } =
    [ button [ onClick Msg.AddNewClicked ] [ text "add new empty" ]
    , viewGrainList grainList
    , viewFab
    ]


viewFab =
    CssElements.iconBtnWithStyles
        [ CS.abs
        , Css.bottom space4
        , Css.right space4
        , Css.backgroundColor black80
        , Css.color white
        , Css.borderRadius <| px 9999
        , Css.boxShadow4 (px 1) (px 1) (px 8) (blackAlpha 0.5)
        , CS.p space2
        ]
        [ onClick Msg.AddNewClicked ]
        [ CssIcons.viewColorWhite CssIcons.add ]


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
    flexCol [] [] (List.map viewItem list)
