module GrainListView exposing
    ( GrainListView
    , grainDomId
    , inlineGrainEditInputDomId
    , view
    )

import BasicsX exposing (callWith, callWith2, defaultEmptyStringTo, ifElse, ter)
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


type alias NodeModel =
    { grain : Grain
    , level : Int
    , maybeEditContent : Maybe String
    }


type Node
    = Node NodeModel Forest


type alias Forest =
    List Node


nodeModel (Node model children) =
    model


nodeChildren (Node model children) =
    children


nodeGrain =
    nodeModel >> .grain


nodeLevel =
    nodeModel >> .level


maybeNodeEditContent =
    nodeModel >> .maybeEditContent


nodeDomId =
    nodeGrain >> grainDomId


nodeGid =
    nodeGrain >> Grain.id


nodeDeleted =
    nodeGrain >> Grain.deleted


canEditNodeContent =
    nodeDeleted >> not


nodeTitle =
    nodeGrain >> Grain.titleOrEmpty


nodeInlineEditInputId =
    nodeGrain >> inlineGrainEditInputDomId


nodeDragMsg =
    nodeGrain >> Msg.DragGrain


nodeInlineEditMsg =
    nodeGrain >> Msg.InlineEditGrain


nodeMoreClicked =
    nodeGrain >> Msg.GrainMoreClicked


nodeInlineEditInputContentChanged =
    nodeGrain >> Msg.InlineEditGrainContentChanged


view : GrainListView -> List (Html Msg)
view { grains, inlineEditGrain, getChildren } =
    let
        createNode : Int -> Grain -> Node
        createNode level g =
            let
                newNodeModel : NodeModel
                newNodeModel =
                    { grain = g
                    , level = level
                    , maybeEditContent =
                        InlineEditGrain.maybeContentFor g inlineEditGrain
                    }

                children : Forest
                children =
                    getChildren g |> List.map (createNode (level + 1))
            in
            Node newNodeModel children

        forest : Forest
        forest =
            List.map (createNode 0) grains
    in
    [ CssHtml.keyedDiv
        [ css
            [ CS.pa space2
            , Css.marginBottom <| rem 3
            ]
        ]
        (viewGrainItems forest)
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


viewGrainItems : Forest -> List ( String, Html Msg )
viewGrainItems forest =
    let
        viewItem node =
            maybeNodeEditContent node
                |> Maybe.unwrap viewDisplayItem viewEditingItem
                |> callWith node

        viewKeyedItem node =
            ( nodeDomId node, viewItem node )
                :: List.concatMap viewKeyedItem
                    (nodeChildren node)
    in
    List.concatMap viewKeyedItem forest


viewTitle node =
    let
        title =
            nodeTitle node

        canEdit =
            canEditNodeContent node
    in
    styled div
        [ CS.pa space2
        , CS.styleIf canEdit CS.pointer
        , CS.flex11Auto
        , CS.ellipsis
        ]
        [ attrIf canEdit (onClick <| nodeInlineEditMsg node) ]
        [ text title ]


viewRightMenu node =
    CssElements.iconBtnWithStyles [ CS.selfCenter ]
        [ onClick (nodeMoreClicked node)
        ]
        [ CssIcons.view CssIcons.moreHoriz
        ]


viewDragHandle node =
    CssElements.iconBtnWithStyles [ CS.selfCenter, CS.move ]
        [ onClick (nodeDragMsg node)
        ]
        [ CssIcons.view CssIcons.dragHandle
        ]


viewDisplayItem node =
    let
        level =
            nodeLevel node |> toFloat

        deleted =
            nodeDeleted node

        opacityValue =
            ter deleted 0.7 1
    in
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.maxWidth <| pct 100
        , Css.opacity <| num opacityValue
        , Css.paddingLeft <| px (level * 16)
        ]
        []
        [ {- viewDragHandle node
             ,
          -}
          viewTitle node
        , viewRightMenu node
        ]


viewEditingItem content node =
    let
        bindings =
            [ ( HotKey.enter, ( Msg.InlineEditGrainSubmit, True ) )
            ]

        level =
            nodeLevel node |> toFloat
    in
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.maxWidth <| pct 100
        , CS.pv space2
        , Css.paddingLeft <| px (level * 16)
        ]
        []
        [ textarea
            [ id <| nodeInlineEditInputId node
            , value <| content
            , onInput <| nodeInlineEditInputContentChanged node
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
