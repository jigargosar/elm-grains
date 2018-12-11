module CssIcons exposing (add, delete, view, viewColorWhite)

import Color
import Html.Styled
import Material.Icons.Action as MI
import Material.Icons.Content as MI


view icon =
    icon Color.charcoal 24 |> Html.Styled.fromUnstyled


viewColorWhite icon =
    icon Color.white 24 |> Html.Styled.fromUnstyled


add =
    MI.add


delete =
    MI.delete
