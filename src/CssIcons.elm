module CssIcons exposing (add, delete, viewIcon)

import Color
import Html.Styled
import Material.Icons.Action as MI
import Material.Icons.Content as MI


viewIcon icon =
    icon Color.charcoal 24 |> Html.Styled.fromUnstyled


add =
    MI.add


delete =
    MI.delete
