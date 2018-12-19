module CssLayout exposing (flexCol, flexColIC, flexRow, flexRowIC)

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


flexRowIC styles =
    flexRow (CS.itemsCenter :: styles)


flexColIC styles =
    flexCol (CS.itemsCenter :: styles)
