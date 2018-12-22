module Cards exposing (initialCardModel, initialMenuModel, main)

import Css exposing (hex, px, rem)
import Css.Global
import CssShorthand as CS
import Html.Styled exposing (button, div, text)
import Html.Styled.Attributes exposing (css, id, tabindex)
import Main as App
import MaterialColor
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

            --            , Css.property "color" "rgba (0,0,0,0.8)"
            , Css.color <| fromMaterialColor MaterialColor.grey800
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
    [ Css.border3 (px 2) Css.solid CS.dodgerBlue
    , CS.row
    , CS.p2 (rem 0.25) (rem 0.5)
    , Css.borderRadius (rem 0.25)
    , CS.uppercase
    , CS.pointer
    , Css.boxShadow4
        (px 1)
        (px 1)
        (px 2)
        CS.black20
    , Css.active
        [ Css.boxShadow5 Css.inset
            (px 1)
            (px 1)
            (px 2)
            CS.black20
        ]
    ]


fromMaterialColor { red, green, blue } =
    Css.rgb red green blue


flatButtonStyleList =
    [ CS.row
    , CS.p2 (rem 0.25) (rem 0.5)
    , Css.borderWidth Css.zero
    , Css.color CS.dodgerBlue
    , CS.pointer
    ]


flatButtonStyle =
    Css.batch flatButtonStyleList


borderButtonStyle =
    Css.batch borderButtonStyleList


main =
    show mockUpdate
        [ deck "Basic elements"
            [ card "button" initialMenuModel <|
                \_ ->
                    cssContainer <|
                        button [ css [ flatButtonStyle ] ]
                            [ text "Flat Button" ]
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
