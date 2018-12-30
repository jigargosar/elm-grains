module GrainTreeView exposing
    ( GrainTreeView
    , contentInputDomId
    , grainDomId
    , view
    )

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
import GrainZipper exposing (GrainTree)
import Html.Styled
    exposing
        ( Html
        , button
        , div
        , input
        , styled
        , text
        , textarea
        )
import Html.Styled.Attributes exposing (autocomplete, class, css, id, rows, tabindex, value)
import Html.Styled.Events
    exposing
        ( onBlur
        , onClick
        , onDoubleClick
        , onFocus
        , onInput
        )
import Tree exposing (Tree)


grainDomId : GrainId -> String
grainDomId =
    GrainId.toDomIdWithPrefix "grain-list-item--"


inlineGrainEditInputDomId =
    Grain.toDomIdWithPrefix editInputPrefix


editInputPrefix =
    "grain-list-item-edit-input--"


contentInputDomId =
    GrainId.toDomIdWithPrefix editInputPrefix


type alias DisplayModel =
    { title : String
    , domId : String
    , gid : GrainId
    }


type alias EditModel =
    { content : String
    , domId : String
    , gid : GrainId
    }


type Node
    = DisplayNode DisplayModel
    | EditNode EditModel


grainToNode : Grain -> Node
grainToNode grain =
    let
        gid =
            Grain.id grain
    in
    DisplayNode
        { title = Grain.titleOrEmpty grain
        , domId = grainDomId gid
        , gid = gid
        }


type alias GrainTreeView msg =
    { keyDownCustom : GrainId -> EventX.CustomDecoder msg
    , routeTo : GrainId -> msg
    , grainTree : GrainTree
    , editGid : Maybe GrainId
    }


view : GrainTreeView msg -> List (Html msg)
view config =
    let
        grainTree =
            config.grainTree

        nodeTree =
            Tree.map grainToNode grainTree
    in
    viewNodeTree config 0 nodeTree


viewNodeTree : GrainTreeView msg -> Float -> Tree Node -> List (Html msg)
viewNodeTree config level tree =
    case Tree.label tree of
        DisplayNode node ->
            let
                nodeStyles =
                    if level == 0 then
                        [ CS.pv1, CS.ph2, CS.bold ]

                    else
                        [ CS.pa1
                        , Css.paddingLeft <| px <| 4 + (level * 32)
                        ]
            in
            div
                [ id node.domId
                , tabindex 0
                , CssEventX.onKeyDownCustom
                    (config.keyDownCustom node.gid)
                , css nodeStyles
                , onDoubleClick (config.routeTo node.gid)
                ]
                [ text node.title ]
                :: viewForest config (level + 1) tree


viewForest config level tree =
    tree |> Tree.children >> List.concatMap (viewNodeTree config level)
