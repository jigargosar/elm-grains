module CssProto exposing (modal)

import Css
import CssElements
import CssLayout as CL
import Html.Styled exposing (Html)
import Html.Styled.Attributes as SA
import Html.Styled.Events as SE


type alias ModalView msg =
    { content : Html msg
    , onDismiss : msg
    }


modal { content, onDismiss } =
    CssElements.modelWrapperEl []
        [ CssElements.modelBackdropEl [ SE.onClick onDismiss ] []
        , CssElements.modelContentEl [ SA.css [ Css.minWidth <| Css.rem 20 ] ]
            content
        ]
