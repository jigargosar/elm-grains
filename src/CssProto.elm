module CssProto exposing (modal)

import Css
import CssElements
import CssLayout as CL
import CssShorthand as CS
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes as SA exposing (class, css)
import Html.Styled.Events as SE


type alias ModalView msg =
    { content : Html msg
    , onDismiss : msg
    }


modal { content, onDismiss } =
    div
        [ css [ CS.fixed, CS.absFill, CS.rowCC, CS.bgBlack20 ]
        , class "bg-black-20"
        ]
        [ CssElements.modelBackdropEl [ SE.onClick onDismiss ] []
        , CssElements.modelContentEl [ SA.css [ Css.minWidth <| Css.rem 20 ] ]
            content
        ]
