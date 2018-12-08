module CssIcons exposing (add, viewIcon)

import Color
import Html.Styled
import Material.Icons.Content


viewIcon icon =
    icon Color.charcoal 24 |> Html.Styled.fromUnstyled


add =
    Material.Icons.Content.add
