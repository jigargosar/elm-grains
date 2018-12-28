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
import CssTheme
    exposing
        ( black80
        , blackAlpha
        , space1
        , space2
        , space4
        , white
        )
import EventX
import Html.Styled
    exposing
        ( Html
        , button
        , div
        , dt
        , input
        , styled
        , text
        , textarea
        )
import Html.Styled.Attributes
    exposing
        ( autocomplete
        , class
        , css
        , id
        , rows
        , tabindex
        , value
        )
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
    div
        [ css
            [ CS.fixed
            , Css.right <| rem 1
            , Css.fontSize <| px 12
            , Css.minWidth <| rem 8
            , CS.bgBlack80
            , CS.colorWhite
            ]
        ]
        (List.map viewLabelField entries
            ++ [ div
                    [ css
                        [ CS.rowCC
                        ]
                    ]
                    [ text "Close Controls" ]
               ]
        )


viewLabelField : LabelField -> Html msg
viewLabelField (LabelField title field) =
    let
        fieldView =
            let
                children =
                    case field of
                        Integer int ->
                            [ text (String.fromInt int) ]

                        Boolean bool ->
                            [ text <| ter bool "True" "False" ]
            in
            div
                [ css
                    [ Css.display Css.tableCell
                    , Css.padding <| px 2
                    ]
                ]
                children

        labelView =
            div
                [ css
                    [ Css.display Css.tableCell
                    , Css.padding <| px 2
                    ]
                ]
                [ text title ]
    in
    div [ css [ Css.display Css.tableRow ] ]
        [ labelView
        , fieldView
        ]
