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
import Route
import Skeleton
import Task exposing (Task)


grainDomId : Grain -> String
grainDomId =
    Grain.toDomIdWithPrefix "grain-list-item--"


inlineGrainEditInputDomId =
    Grain.toDomIdWithPrefix "grain-list-item-edit-input--"


type alias GrainMessages msg =
    { grainMoreClicked : Grain -> msg
    , inlineEditGrain : Grain -> msg
    , dragGrain : Grain -> msg
    , inlineEditGrainContentChanged : Grain -> String -> msg
    , inlineEditSubmit : Grain -> msg
    }


type alias GrainListView msg =
    { grains : List Grain
    , getChildren : Grain -> List Grain
    , inlineEditGrain : InlineEditGrain
    , addFabClicked : msg
    , grainMsg : GrainMessages msg
    }


type alias NodeModel msg =
    { grain : Grain
    , domId : String
    , level : Int
    , maybeEditContent : Maybe String
    , grainMsg : GrainMessages msg
    , moreClickedMsg : msg
    }


type Node msg
    = Node (NodeModel msg) (Forest msg)


type alias Forest msg =
    List (Node msg)


nodeModel (Node model _) =
    model


nodeChildren (Node _ children) =
    children


nodeGrain =
    nodeModel >> .grain


nodeDeleted =
    nodeGrain >> Grain.deleted


canEditNodeContent =
    nodeDeleted >> not


nodeTitle =
    nodeGrain >> Grain.titleOrEmpty


nodeInlineEditInputId =
    nodeGrain >> inlineGrainEditInputDomId


nodeGrainMsg : (GrainMessages msg -> Grain -> a) -> Node msg -> a
nodeGrainMsg fn (Node model _) =
    fn model.grainMsg model.grain


nodeDragMsg =
    nodeGrainMsg .dragGrain


nodeInlineEditMsg =
    nodeGrainMsg .inlineEditGrain


nodeInlineEditGrainContentChanged =
    nodeGrainMsg .inlineEditGrainContentChanged


nodeInlineEditSubmit =
    nodeGrainMsg .inlineEditSubmit


view : GrainListView msg -> List (Html msg)
view { grains, inlineEditGrain, getChildren, addFabClicked, grainMsg } =
    let
        createNode : Int -> Grain -> Node msg
        createNode level g =
            let
                newNodeModel : NodeModel msg
                newNodeModel =
                    { grain = g
                    , domId = grainDomId g
                    , level = level
                    , maybeEditContent =
                        InlineEditGrain.maybeContentFor g inlineEditGrain
                    , grainMsg = grainMsg
                    , moreClickedMsg = grainMsg.grainMoreClicked g
                    }

                children : Forest msg
                children =
                    getChildren g |> List.map (createNode (level + 1))
            in
            Node newNodeModel children

        forest : Forest msg
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
    , viewFab addFabClicked
    ]


viewFab addFabClicked =
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
            [ onClick addFabClicked ]
            [ CssIcons.viewColorWhite CssIcons.add ]
        ]


grainDisplayTitle =
    Grain.titleOrEmpty >> defaultEmptyStringTo "<empty>"


viewGrainItems : Forest msg -> List ( String, Html msg )
viewGrainItems forest =
    let
        viewKeyedItem node =
            let
                nModel =
                    nodeModel node

                nChildren =
                    nodeChildren node
            in
            ( nModel.domId
            , nModel.maybeEditContent
                |> Maybe.unwrap (viewDisplayItem nModel) (viewEditingItem nModel)
                |> callWith node
            )
                :: List.concatMap viewKeyedItem nChildren
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


viewRightMenu nModel =
    CssElements.iconBtnWithStyles [ CS.selfCenter ]
        [ onClick nModel.moreClickedMsg
        ]
        [ CssIcons.view CssIcons.moreHoriz
        ]


viewDragHandle node =
    CssElements.iconBtnWithStyles [ CS.selfCenter, CS.move ]
        [ onClick (nodeDragMsg node)
        ]
        [ CssIcons.view CssIcons.dragHandle
        ]


viewDisplayItem nModel node =
    let
        level =
            nModel.level |> toFloat

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
        , viewRightMenu nModel
        ]


viewEditingItem nModel content node =
    let
        bindings =
            [ ( HotKey.enter, ( nodeInlineEditSubmit node, True ) )
            ]

        level =
            nModel.level |> toFloat
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
            , onInput <| nodeInlineEditGrainContentChanged node
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
