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


type alias Node =
    { title : String
    }


grainToNode grain =
    { title = Grain.titleOrEmpty grain
    }


view grainTree =
    let
        nodeTree =
            Tree.map grainToNode grainTree
    in
    viewRootTree nodeTree


viewRootTree tree =
    let
        title =
            Tree.label tree |> .title
    in
    div [ css [ CS.pv1, CS.ph2, CS.bold ] ] [ text title ]
        :: viewForest 1 tree


simpleIndentedStringEl level string =
    div
        [ css
            [ CS.pa1
            , Css.paddingLeft <| px <| 4 + (level * 32)
            ]
        ]
        [ text string ]


viewForest level tree =
    tree |> Tree.children >> List.concatMap (viewTree level)


viewTree level tree =
    let
        title =
            Tree.label tree |> .title
    in
    simpleIndentedStringEl level title
        :: viewForest (level + 1) tree
