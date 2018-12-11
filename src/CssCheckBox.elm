module CssCheckBox exposing (view)

import BasicsX exposing (ter)
import Css exposing (num)
import CssIcons
import Html.Styled exposing (button, styled)
import Html.Styled.Attributes exposing (type_)
import Html.Styled.Events exposing (onClick)
import List exposing ((::))
import Material.Icons.Toggle as MIcons


view styles checked attrs =
    let
        checkBoxIcon =
            ter checked MIcons.check_box MIcons.check_box_outline_blank
    in
    styled button
        ([ Css.disabled [ Css.opacity <| num 0.5 ]
         , Css.padding Css.zero
         , Css.displayFlex
         , Css.flexDirection Css.row
         , Css.backgroundColor Css.transparent
         , Css.borderWidth Css.zero
         ]
            ++ styles
        )
        (type_ "button" :: attrs)
        [ CssIcons.view checkBoxIcon ]
