module Cards exposing (initialCardModel, initialMenuModel, main)

import Css exposing (hex, px, rem)
import Css.Global
import CssShorthand as CS
import Html.Styled exposing (button, div, text)
import Html.Styled.Attributes exposing (css, id, tabindex)
import Main as App
import UiCards exposing (card, cardError, deck, show)



-- This is your application's main module


initialMenuModel =
    {}


initialCardModel =
    {}


mockUpdate message model =
    ( model, Cmd.none )


globalStyles =
    Css.Global.global
        [ Css.Global.id "css-container"
            [ Css.boxSizing Css.borderBox
            , Css.property "font-size" "16px"
            , Css.property "color" "rgba (0,0,0,0.8)"
            , Css.property "font-family"
                """-apple-system, system-ui, BlinkMacSystemFont, "Segoe UI",
               Roboto, "Helvetica Neue", sans-serif;"""
            , Css.Global.descendants
                [ Css.Global.button
                    [ Css.property "font-size" "inherit"
                    , Css.property "font-family" "inherit"
                    ]
                ]
            ]
        ]


cssContainer el =
    Html.Styled.toUnstyled <|
        div [ id "css-container" ] [ globalStyles, el ]


borderButtonStyleList =
    [ Css.border2 (px 2) Css.solid

    -- , Css.property "border-color" "dodgerblue"
    --    , Css.property "color" "#555"
    , Css.color <| hex "#555"
    , Css.borderColor (Css.hsla 210 1 0.56 0.6)
    , Css.borderColor (Css.hsla 210 1 0.56 1)
    , Css.display Css.inlineFlex
    , Css.flexDirection Css.row
    , CS.p2 (rem 0.25) (rem 0.5)

    --    , Css.outline Css.none
    , Css.borderRadius (rem 0.25)
    , Css.boxShadow4
        (px 1)
        (px 1)
        (px 2)
        CS.black20
    , Css.focus
        [ Css.borderColor (Css.hsla 210 1 0.56 1)
        , Css.color <| hex "#000"
        ]
    , Css.active
        [ Css.boxShadow5 Css.inset
            (px 1)
            (px 1)
            (px 2)
            CS.black20
        ]
    , CS.uppercase
    ]


buttonStyleList =
    [ Css.border2 (px 2) Css.solid

    -- , Css.property "border-color" "dodgerblue"
    --    , Css.property "color" "#555"
    , Css.color <| hex "#555"
    , Css.borderColor CS.dodgerBlue
    , CS.inlineRow
    , CS.p2 (rem 0.25) (rem 0.5)

    --    , Css.outline Css.none
    , Css.borderRadius (rem 0.25)
    , Css.boxShadow4
        (px 1)
        (px 1)
        (px 2)
        CS.black20
    , Css.focus
        [ Css.borderColor (Css.hsla 210 1 0.56 1)
        , Css.color <| hex "#000"
        ]
    , Css.active
        [ Css.boxShadow5 Css.inset
            (px 1)
            (px 1)
            (px 2)
            CS.black20
        ]
    , CS.uppercase
    ]


buttonStyle =
    Css.batch


borderButtonStyle =
    Css.batch borderButtonStyleList


main =
    show mockUpdate
        [ deck "Basic elements"
            [ card "button" initialMenuModel <|
                \_ ->
                    Html.Styled.toUnstyled <|
                        div [ css [] ] [ text "I should be a button" ]
            , card "button" initialMenuModel <|
                \_ ->
                    cssContainer <|
                        button
                            [ css [ borderButtonStyle ]
                            , tabindex 0
                            ]
                            [ text "Click Me!" ]
            ]
        , deck "Menu elements"
            [ card "Menu button" initialMenuModel <|
                \_ ->
                    Html.Styled.toUnstyled <|
                        div [ css [] ] [ text "I should be a menu button" ]
            , card "Menu button" initialMenuModel <|
                \_ ->
                    Html.Styled.toUnstyled <|
                        div
                            [ css
                                [ Css.border2 (px 2) Css.solid
                                , Css.property "border-color" "lightblue"
                                , Css.display Css.inlineFlex
                                , Css.flexDirection Css.row
                                , CS.p2 (rem 0.25) (rem 0.5)
                                ]
                            ]
                            [ text "I should be a menu button" ]
            , card "Menu panel" initialMenuModel <|
                \_ ->
                    cardError "This is a test"
            ]
        , deck "Card elements"
            [ card "Card" initialCardModel <|
                \_ ->
                    cardError "This is a test"
            , card "Error test" initialCardModel <|
                \_ ->
                    cardError "This is a test"
            ]
        ]
