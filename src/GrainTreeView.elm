module GrainTreeView exposing (view)

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
import Grain
import GrainId exposing (GrainId)
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
import Html.Styled.Events exposing (onBlur, onClick, onFocus, onInput)
import Tree


type alias NodeModel =
    { title : String
    , domId : String
    }


type Node
    = DisplayNode NodeModel


grainDomId : GrainId -> String
grainDomId =
    GrainId.toDomIdWithPrefix "grain-list-item--"


grainToNode grain =
    let
        gid =
            Grain.id grain
    in
    { title = Grain.titleOrEmpty grain
    , domId = grainDomId gid
    , gid = gid
    }


type alias Config msg =
    { keyDownCustom : GrainId -> EventX.CustomDecoder msg
    }


view config grainTree =
    let
        nodeTree =
            Tree.map grainToNode grainTree
    in
    viewTree config 0 nodeTree


viewForest config level tree =
    tree |> Tree.children >> List.concatMap (viewTree config level)


viewTree config level tree =
    let
        node =
            Tree.label tree

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
        , CssEventX.onKeyDownCustom (config.keyDownCustom node.gid)
        , css nodeStyles
        ]
        [ text node.title ]
        :: viewForest config (level + 1) tree
