module Cards exposing (main)

import Browser
import Css exposing (hex, px, rem)
import Css.Global
import CssShorthand as CS
import CssTheme
import Html.Styled exposing (button, div, text)
import Html.Styled.Attributes exposing (css, id, tabindex)
import Main as App
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


view _ =
    { title = "Beautiful UI in ELM"
    , body =
        [ Styles.global, viewFlatButton1, viewBorderButton1 ]
            |> List.map Html.Styled.toUnstyled
    }


main : Program () () ()
main =
    Browser.document
        { init = \_ -> Return.singleton ()
        , view = view
        , update = \_ -> Return.singleton
        , subscriptions = \_ -> Sub.none
        }
