module GrainTreeView exposing (view)

import Css exposing (num, pct, px, rem, vh, vw, zero)
import CssAttrX exposing (attrIf)
import CssElements
import CssEventX
import CssHtml
import CssIcons
import CssLayout exposing (flexCol, flexRow)
import CssShorthand as CS
import CssTheme exposing (black80, blackAlpha, space1, space2, space4, white)
import EventX
import Html.Styled exposing (Html, button, div, input, styled, text, textarea)
import Html.Styled.Attributes exposing (autocomplete, class, css, id, rows, tabindex, value)
import Html.Styled.Events exposing (onBlur, onClick, onFocus, onInput)


view tree =
    [ div [] [ text "hello gt" ] ]
