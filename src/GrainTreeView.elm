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


simplePaddedContainer =
    div [ css [ CS.pa2 ] ]


view tree =
    let
        rootGrain =
            Tree.label tree

        rootForest =
            Tree.children tree
    in
    [ simplePaddedContainer
        [ viewRootGrain rootGrain
        , viewForest 1 rootForest
        ]
    ]


simpleStringEl string =
    div [ css [ CS.pa1 ] ] [ text string ]


simpleIndentedStringEl level string =
    div
        [ css
            [ CS.pa1
            , Css.paddingLeft <| px <| level * 8
            ]
        ]
        [ text string ]


viewRootGrain grain =
    let
        title =
            Grain.titleOrEmpty grain
    in
    simpleStringEl title


viewForest level grainForest =
    simpleIndentedStringEl level "forest"
