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


type alias GrainTreeView msg =
    { keyDownCustom : GrainId -> EventX.CustomDecoder msg
    , routeTo : GrainId -> msg
    , grainTree : GrainTree
    , onContentChanged : GrainId -> String -> msg
    , editGid : Maybe GrainId
    }


view : GrainTreeView msg -> List (Html msg)
view config =
    let
        grainTree =
            config.grainTree
    in
    viewNodeTree config 0 grainTree


viewNodeTree : GrainTreeView msg -> Float -> GrainTree -> List (Html msg)
viewNodeTree config level tree =
    let
        nodeStyles =
            if level == 0 then
                [ CS.pv1, CS.ph2, CS.bold ]

            else
                [ CS.pa1
                , Css.paddingLeft <| px <| 4 + (level * 32)
                ]

        grain =
            Tree.label tree

        gid =
            Grain.id grain

        title =
            Grain.titleOrEmpty grain
    in
    div
        [ id (grainDomId gid)
        , tabindex 0
        , CssEventX.onKeyDownCustom
            (config.keyDownCustom gid)
        , css nodeStyles
        , onDoubleClick (config.routeTo gid)
        ]
        [ text title ]
        :: viewForest config (level + 1) tree


viewForest config level tree =
    tree |> Tree.children >> List.concatMap (viewNodeTree config level)
