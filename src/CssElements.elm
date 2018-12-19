module CssElements exposing
    ( iconBtnEl
    , iconBtnWithStyles
    , modelBackdropEl
    , modelContentEl
    , modelWrapperEl
    )

import Css exposing (num)
import Html.Styled exposing (button, div, styled)
import Html.Styled.Attributes exposing (class)


withClass class_ attrs =
    div (class class_ :: attrs)


modelWrapperEl =
    withClass "bg-black-20 fixed absolute--fill flex items-center justify-center"


modelBackdropEl =
    withClass "absolute--fill"


modelContentEl =
    withClass "pa3 bg-white "


iconBtnEl =
    iconBtnWithStyles []


iconBtnWithStyles styles =
    styled button
        ([ Css.disabled [ Css.opacity <| num 0.5 ]
         , Css.padding Css.zero
         , Css.displayFlex
         , Css.flexDirection Css.row
         , Css.backgroundColor Css.transparent
         , Css.borderWidth Css.zero
         , Css.cursor Css.pointer
         ]
            ++ styles
        )
