module Cards exposing (main)

import Browser
import Css exposing (hex, px, rem)
import Css.Global
import CssShorthand as CS
import CssTheme
import Html.Styled exposing (button, div, text)
import Html.Styled.Attributes exposing (css, id, tabindex)
import MaterialColor
import Return
import Styles


viewFlatButton1 =
    button [ css [ Styles.flatButton ] ]
        [ text "Flat Button" ]


viewBorderButton1 =
    button
        [ css [ Styles.borderButton ]
        ]
        [ text "Click Me!" ]


viewDocument _ =
    { title = "Beautiful UI in ELM"
    , body =
        [ Styles.global, viewPage ]
            |> List.map Html.Styled.toUnstyled
    }


viewPage =
    div [] [ viewFlatButton1, viewBorderButton1 ]


main : Program () () ()
main =
    Browser.document
        { init = \_ -> Return.singleton ()
        , view = viewDocument
        , update = \_ -> Return.singleton
        , subscriptions = \_ -> Sub.none
        }
