module Toast exposing
    ( Toast
    , dismiss
    , init
    , show
    , view
    , viewContent
    )

import Css exposing (pct, px)
import CssHtml
import CssLayout exposing (flexRow)
import CssShorthand as CS
import CssTheme exposing (space4)
import Html.Styled as H exposing (text)
import Html.Styled.Events exposing (onClick)
import StyledHtml exposing (Html)


type alias Toast =
    { title : String, visible : Bool }


init =
    { title = "", visible = False }


dismiss model =
    { model | visible = False }


show title model =
    { model | title = title, visible = True }


view : msg -> Toast -> Html msg
view dismissMsg toast =
    CssHtml.viewIfLazy toast.visible
        (\_ -> viewContent dismissMsg toast.title)


viewContent dismissMsg title =
    flexRow
        [ CS.sticky
        , Css.bottom <| space4
        , Css.minWidth <| px 150
        , Css.maxWidth <| pct 80
        , CS.bgBlack80
        , CS.colorWhite
        ]
        []
        [ flexRow
            [ CS.flexGrow1
            , CS.justifyCenter
            , CS.pa2
            ]
            []
            [ text title ]
        , flexRow [ CS.pa2, CS.pointer ]
            [ onClick dismissMsg ]
            [ text "X" ]
        ]
