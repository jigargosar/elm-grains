module Cards exposing (initialCardModel, initialMenuModel, main)

import Css exposing (hex, px, rem)
import Css.Global
import CssShorthand as CS
import CssTheme
import Html.Styled exposing (button, div, text)
import Html.Styled.Attributes exposing (css, id, tabindex)
import Main as App
import MaterialColor
import Styles
import UiCards exposing (card, cardError, deck, show)



-- This is your application's main module


initialMenuModel =
    {}


initialCardModel =
    {}


mockUpdate message model =
    ( model, Cmd.none )


cssContainer el =
    Html.Styled.toUnstyled <|
        div [ id "css-container" ] [ Styles.global, el ]


main =
    show mockUpdate
        [ deck "Button"
            [ card "Flat" initialMenuModel <|
                \_ ->
                    cssContainer <|
                        button [ css [ Styles.flatButton ] ]
                            [ text "Flat Button" ]
            , card "Border" initialMenuModel <|
                \_ ->
                    cssContainer <|
                        button
                            [ css [ Styles.borderButton ]
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
