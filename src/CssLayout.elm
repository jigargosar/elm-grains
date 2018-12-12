module CssLayout exposing (flexCol, flexColIC, flexRow, flexRowIC)

import Css
import CssShorthand as CS
import Html.Styled exposing (div, styled)


flexCol styles =
    styled div
        (Css.displayFlex
            :: Css.flexDirection Css.column
            :: CS.fs0
            :: CS.flexGrow1
            :: styles
        )


flexRow styles =
    styled div
        (Css.displayFlex
            :: CS.fs0
            :: Css.flexDirection Css.row
            :: styles
        )


flexRowIC styles =
    flexRow (CS.aic :: styles)


flexColIC styles =
    flexCol (CS.aic :: styles)
