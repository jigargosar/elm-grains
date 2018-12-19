module CssLayout exposing (flexCol, flexRow)

import Css
import CssShorthand as CS
import Html.Styled exposing (div, styled)


flexCol styles =
    styled div
        (Css.displayFlex
            :: Css.flexDirection Css.column
            :: CS.flexShrink0
            :: CS.flexGrow1
            :: styles
        )


flexRow styles =
    styled div
        (Css.displayFlex
            :: CS.flexShrink0
            :: Css.flexDirection Css.row
            :: styles
        )
