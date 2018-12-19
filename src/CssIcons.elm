module CssIcons exposing
    ( add
    , delete
    , modeEdit
    , moreHoriz
    , moreVert
    , restore
    , view
    , viewColorWhite
    )

import Color
import Html.Styled
import Material.Icons.Action as MI
import Material.Icons.Content as MI
import Material.Icons.Editor as MI
import Material.Icons.Navigation as MI


view icon =
    icon Color.charcoal 24 |> Html.Styled.fromUnstyled


viewColorWhite icon =
    icon Color.white 24 |> Html.Styled.fromUnstyled


add =
    MI.add


delete =
    MI.delete


restore =
    MI.restore


moreVert =
    MI.more_vert


moreHoriz =
    MI.more_horiz


modeEdit =
    MI.mode_edit
