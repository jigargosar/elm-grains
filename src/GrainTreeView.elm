module GrainTreeView exposing
    ( GrainTreeView
    , contentInputDomId
    , grainDomId
    , view
    )

import BasicsX exposing (..)
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
import Maybe.Extra as Maybe
import Tree exposing (Tree)


grainDomId : GrainId -> String
grainDomId =
    GrainId.toDomIdWithPrefix "grain-tree-item--"


contentInputDomId : GrainId -> String
contentInputDomId =
    GrainId.toDomIdWithPrefix "grain-tree-item-content-input--"


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

        isEditing =
            Maybe.unwrap False (eqs gid) config.editGid

        grainView =
            if isEditing then
                textarea
                    [ id (contentInputDomId gid)
                    , tabindex 0
                    , CssEventX.onKeyDownCustom (config.keyDownCustom gid)
                    , css nodeStyles
                    , value (Grain.content grain)
                    , onInput (config.onContentChanged gid)
                    ]
                    []

            else
                div
                    [ id (grainDomId gid)
                    , tabindex 0
                    , CssEventX.onKeyDownCustom (config.keyDownCustom gid)
                    , css nodeStyles
                    , onDoubleClick (config.routeTo gid)
                    ]
                    [ text (Grain.titleOrEmpty grain) ]
    in
    grainView
        :: viewForest config (level + 1) tree


viewForest config level tree =
    tree |> Tree.children >> List.concatMap (viewNodeTree config level)
