module GrainListView exposing
    ( GrainListView
    , grainDomId
    , inlineGrainEditInputDomId
    , view
    )

import BasicsX exposing (callWith, defaultEmptyStringTo, ifElse, ter)
import Browser.Dom
import Css exposing (num, pct, px, rem, vh, vw, zero)
import CssAttrX exposing (attrIf)
import CssElements
import CssEventX
import CssHtml
import CssIcons
import CssLayout exposing (flexCol, flexRow)
import CssShorthand as CS
import CssTheme exposing (black80, blackAlpha, space1, space2, space4, white)
import Grain exposing (Grain)
import HotKey
import Html.Styled exposing (Html, button, div, input, styled, text, textarea)
import Html.Styled.Attributes exposing (autocomplete, class, css, id, value)
import Html.Styled.Events exposing (onClick, onInput)
import InlineEditGrain exposing (InlineEditGrain)
import Maybe.Extra as Maybe
import Msg exposing (Msg)
import Route
import Skeleton
import Task exposing (Task)


grainDomId : Grain -> String
grainDomId =
    Grain.toDomIdWithPrefix "grain-list-item--"


inlineGrainEditInputDomId =
    Grain.toDomIdWithPrefix "grain-list-item-edit-input--"


type alias GrainListView =
    { grains : List Grain
    , getChildren : Grain -> List Grain
    , inlineEditGrain : InlineEditGrain
    }


view : GrainListView -> List (Html Msg)
view { grains, inlineEditGrain, getChildren } =
    [ CssHtml.keyedDiv
        [ css
            [ CS.pa space2
            , Css.marginBottom <| rem 3
            ]
        ]
        (viewGrainItems getChildren inlineEditGrain 0 grains)
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
            , CS.pa space2
            ]
            [ onClick Msg.CreateAndAddNewGrain ]
            [ CssIcons.viewColorWhite CssIcons.add ]
        ]


grainDisplayTitle =
    Grain.titleOrEmpty >> defaultEmptyStringTo "<empty>"


viewGrainItems getChildren inlineEditGrain level list =
    let
        viewTitle g =
            let
                title =
                    grainDisplayTitle g

                canEdit =
                    Grain.deleted g |> not
            in
            styled div
                [ CS.p2 space2 zero
                , CS.styleIf canEdit CS.pointer
                , CS.flex11Auto
                , CS.ellipsis
                ]
                --                [ onClick <| Msg.routeToGrain g ]
                [ attrIf canEdit (onClick <| Msg.InlineEditGrain g) ]
                [ text title ]

        viewRightMenu g =
            CssElements.iconBtnWithStyles [ CS.selfCenter ]
                [ onClick (Msg.GrainMoreClicked g)
                ]
                [ CssIcons.view CssIcons.moreHoriz
                ]

        viewDisplayItem g =
            let
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
                [ viewTitle g
                , viewRightMenu g
                ]

        viewEditingItem content g =
            let
                bindings =
                    [ ( HotKey.enter, ( Msg.InlineEditGrainSubmit, True ) )
                    ]
            in
            styled div
                [ Css.displayFlex
                , Css.flexDirection Css.row
                , Css.maxWidth <| pct 100
                , CS.pv space2
                ]
                []
                [ textarea
                    [ id <| inlineGrainEditInputDomId g
                    , value <| content
                    , onInput <| Msg.InlineEditGrainContentChanged g
                    , CssEventX.onKeyDownPD <|
                        HotKey.bindEachToMsg bindings
                    , autocomplete False
                    , css
                        [ CS.w_full
                        , Css.borderWidth zero
                        , Css.borderBottom3 (px 1.5) Css.solid CS.black20
                        , CS.p2 space1 space2
                        , Css.focus
                            [ Css.outline Css.none
                            , Css.borderBottomColor CS.black80
                            ]
                        ]
                    ]
                    []
                ]

        viewItem g =
            InlineEditGrain.maybeContentFor g inlineEditGrain
                |> Maybe.unwrap viewDisplayItem viewEditingItem
                |> callWith g

        viewKeyedItem currentLevel g =
            ( grainDomId g, viewItem g )
                :: List.concatMap (viewKeyedItem (currentLevel + 1))
                    (getChildren g)
    in
    List.concatMap (viewKeyedItem level) list



--viewDelete g =
--            let
--                deleted =
--                    Grain.deleted g
--
--                action =
--                    ter deleted Msg.RestoreGrain Msg.DeleteGrain <|
--                        g
--
--                icon =
--                    ter deleted CssIcons.restore CssIcons.delete
--            in
--            CssElements.iconBtnWithStyles [ CS.selfCenter ]
--                [ onClick action
--                ]
--                [ CssIcons.view icon
--                ]
