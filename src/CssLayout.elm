module CssLayout exposing (flexCol, flexRow, flexRowIC)

import Css
import Html.Styled exposing (div, styled)


flexCol styles =
    styled div ([ Css.displayFlex, Css.flexDirection Css.column ] ++ styles)


flexRow styles =
    styled div
        (Css.displayFlex
            :: Css.flexDirection Css.row
            :: styles
        )


flexRowIC styles =
    flexRow
        (Css.alignItems Css.center
            :: styles
        )
