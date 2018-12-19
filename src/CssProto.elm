module CssProto exposing (modal)

import Css
import CssElements
import CssLayout as CL
import CssShorthand as CS
import CssTheme exposing (space2)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes as SA exposing (class, css)
import Html.Styled.Events as SE


type alias ModalView msg =
    { content : Html msg
    , onDismiss : msg
    }


modal { content, onDismiss } =
    div
        [ css [ CS.fixed, CS.posFill, CS.rowCC, CS.bgBlack20 ]
        ]
        [ div
            [ css [ CS.absolute, CS.posFill ]
            , SE.onClick onDismiss
            ]
            []
        , div
            [ css
                [ Css.minWidth <| Css.rem 15
                , CS.absolute
                , CS.bgWhite
                , CS.pa space2
                ]
            ]
            content
        ]
