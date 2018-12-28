module DatGui exposing (Field, boolean, integer, view)

import BasicsX exposing (ter)
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


type Field
    = Integer Int
    | Boolean Bool


type LabelField
    = LabelField String Field


integer name val =
    LabelField name (Integer val)


boolean name val =
    LabelField name (Boolean val)


view : List LabelField -> Html msg
view entries =
    div [ css [ CS.fixed, Css.right <| px 0 ] ]
        (div [] [ text "GUI" ]
            :: List.map viewLabelField entries
        )


viewLabelField : LabelField -> Html msg
viewLabelField (LabelField title field) =
    let
        fieldView =
            case field of
                Integer int ->
                    viewInt int

                Boolean bool ->
                    viewBool bool

        labelView =
            div [] [ text title ]
    in
    div [ css [ CS.row ] ]
        [ labelView
        , fieldView
        ]


viewInt int =
    div [] [ text (String.fromInt int) ]


viewBool bool =
    div [] [ text <| ter bool "True" "False" ]
