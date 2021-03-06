module GrainListView exposing
    ( GrainListView
    , contentInputDomId
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
import EventX
import Grain exposing (Grain)
import GrainId exposing (GrainId)
import GrainZipper__ exposing (GrainTree)
import HotKey
import Html.Styled exposing (Html, button, div, input, styled, text, textarea)
import Html.Styled.Attributes exposing (autocomplete, class, css, id, rows, tabindex, value)
import Html.Styled.Events exposing (onBlur, onClick, onFocus, onInput)
import InlineEditGrain exposing (InlineEditGrain)
import Json.Decode exposing (Decoder)
import Maybe.Extra as Maybe
import Route
import Skeleton
import Task exposing (Task)


grainDomId : GrainId -> String
grainDomId =
    GrainId.toDomIdWithPrefix "grain-list-item--"


inlineGrainEditInputDomId =
    Grain.toDomIdWithPrefix editInputPrefix


editInputPrefix =
    "grain-list-item-edit-input--"


contentInputDomId =
    GrainId.toDomIdWithPrefix editInputPrefix


type alias GrainMessages msg =
    { grainMoreClicked : GrainId -> msg
    , grainTitleClicked : GrainId -> msg
    , dragGrain : GrainId -> msg
    , keyDownCustom : GrainId -> EventX.CustomDecoder msg
    , inlineEditGrainContentChanged : GrainId -> String -> msg
    , inlineEditSubmit : GrainId -> msg
    , inlineEditKeyDownCustom : GrainId -> EventX.CustomDecoder msg
    , inlineEditFocusChanged : GrainId -> Bool -> msg
    , grainFocus : GrainId -> Bool -> msg
    }


type alias GrainListView msg =
    { tree : GrainTree
    , grains : List Grain
    , getChildren : Grain -> List Grain
    , inlineEditGrain : InlineEditGrain
    , addFabClicked : msg
    , grainMsg : GrainMessages msg
    }


type alias NodeModel msg =
    { domId : String
    , title : String
    , level : Float
    , maybeEditContent : Maybe String
    , moreClickedMsg : msg
    , keyDownCustom : EventX.CustomDecoder msg
    , grainTitleClicked : msg
    , grainFocus : Bool -> msg
    , inlineEditContentChangedMsg : String -> msg
    , inlineEditSubmit : msg
    , inlineEditKeyDownCustom : EventX.CustomDecoder msg
    , inlineEditInputId : String
    , inlineEditFocusChanged : Bool -> msg
    , canEdit : Bool
    , deleted : Bool
    }


type Node msg
    = Node (NodeModel msg) (Forest msg)


type alias Forest msg =
    List (Node msg)


createNode : GrainListView msg -> Float -> Grain -> Node msg
createNode vm level g =
    let
        { grainMsg, inlineEditGrain, getChildren } =
            vm

        newNodeModel : NodeModel msg
        newNodeModel =
            let
                gid =
                    Grain.id g
            in
            { domId = grainDomId gid
            , title = Grain.titleOrEmpty g
            , deleted = Grain.deleted g
            , canEdit = Grain.deleted g |> not
            , level = level
            , moreClickedMsg = grainMsg.grainMoreClicked gid
            , maybeEditContent =
                InlineEditGrain.maybeContentFor gid inlineEditGrain
            , keyDownCustom = grainMsg.keyDownCustom gid
            , grainFocus = grainMsg.grainFocus gid
            , grainTitleClicked = grainMsg.grainTitleClicked gid
            , inlineEditContentChangedMsg =
                grainMsg.inlineEditGrainContentChanged gid
            , inlineEditSubmit = grainMsg.inlineEditSubmit gid
            , inlineEditKeyDownCustom = grainMsg.inlineEditKeyDownCustom gid
            , inlineEditInputId = inlineGrainEditInputDomId g
            , inlineEditFocusChanged = grainMsg.inlineEditFocusChanged gid
            }

        children : Forest msg
        children =
            getChildren g |> List.map (createNode vm (level + 1))
    in
    Node newNodeModel children


view : GrainListView msg -> List (Html msg)
view vm =
    let
        forest : Forest msg
        forest =
            List.map (createNode vm 0) vm.grains
    in
    [ CssHtml.keyedDiv
        [ css
            [ CS.pa space2
            , Css.marginBottom <| rem 3
            ]
        ]
        (viewGrainItems forest)
    , viewFab vm.addFabClicked
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
                (Node nModel children) =
                    node
            in
            ( nModel.domId
            , nModel.maybeEditContent
                |> Maybe.unwrap (viewDisplayItem nModel) (viewEditingItem nModel)
                |> callWith node
            )
                :: List.concatMap viewKeyedItem children
    in
    List.concatMap viewKeyedItem forest


viewTitle : NodeModel msg -> Node msg -> Html msg
viewTitle nModel node =
    let
        title =
            nModel.title

        canEdit =
            nModel.canEdit
    in
    styled div
        [ CS.pa space2
        , CS.styleIf canEdit CS.pointer
        , CS.flex11Auto
        , CS.ellipsis
        ]
        [ attrIf canEdit (onClick <| nModel.grainTitleClicked) ]
        [ text title ]


viewRightMenu nModel =
    CssElements.iconBtnWithStyles [ CS.selfCenter ]
        [ onClick nModel.moreClickedMsg
        ]
        [ CssIcons.view CssIcons.moreHoriz
        ]



--viewDragHandle node =
--    CssElements.iconBtnWithStyles [ CS.selfCenter, CS.move ]
--        [ onClick (nodeDragMsg node)
--        ]
--        [ CssIcons.view CssIcons.dragHandle
--        ]
--


viewDisplayItem : NodeModel msg -> Node msg -> Html msg
viewDisplayItem nModel node =
    let
        level =
            nModel.level

        deleted =
            nModel.deleted

        opacityValue =
            ter deleted 0.7 1
    in
    div
        [ id nModel.domId
        , tabindex 0
        , CssEventX.onKeyDownCustom nModel.keyDownCustom
        , onFocus <| nModel.grainFocus True
        , onBlur <| nModel.grainFocus False
        , css
            [ CS.row
            , CS.max_w_full
            , Css.opacity <| num opacityValue
            , Css.paddingLeft <| px (level * 16)
            ]
        ]
        [ {- viewDragHandle node
             ,
          -}
          viewTitle nModel node
        , viewRightMenu nModel
        ]


viewEditingItem : NodeModel msg -> String -> Node msg -> Html msg
viewEditingItem nModel content node =
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.maxWidth <| pct 100
        , CS.pv space2
        , Css.paddingLeft <| px (nModel.level * 16)
        ]
        []
        [ textarea
            [ id <| nModel.inlineEditInputId
            , value <| content
            , onInput <| nModel.inlineEditContentChangedMsg
            , onFocus (nModel.inlineEditFocusChanged True)
            , onBlur (nModel.inlineEditFocusChanged False)
            , CssEventX.onKeyDownCustom nModel.inlineEditKeyDownCustom
            , autocomplete False
            , rows 1
            , css
                [ CS.w_full
                , Css.maxHeight <| vh 50
                , Css.resize Css.none
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
