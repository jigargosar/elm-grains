module Toast exposing
    ( Toast
    , dismiss
    , init
    , show
    , view
    , viewContent
    )

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import BrowserX exposing (WindowSize)
import Color
import Css exposing (em, num, pct, px)
import CssElements exposing (..)
import CssElevation exposing (elevation)
import CssHtml exposing (viewIf, viewIfLazy)
import CssIcons exposing (view)
import CssLayout exposing (flexCol, flexRow, flexRowIC)
import CssShorthand as CS
import CssTheme exposing (black80, space2, space4, space8, white)
import DecodeX exposing (DecodeResult)
import Either exposing (Either(..))
import EventX exposing (onKeyDownPD)
import Grain exposing (Grain)
import GrainListView exposing (GrainListView)
import GrainStore exposing (GrainStore)
import HotKey as K exposing (SoftKey(..))
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as SA exposing (..)
import Html.Styled.Events as SE exposing (onBlur, onClick, onFocus, onInput, onSubmit)
import Html.Styled.Keyed
import Json.Decode as D
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as E exposing (Value)
import List.Extra as List
import ListIndex exposing (ListIndex)
import Material.Icons.Action as MIcons
import Material.Icons.Alert as MIcons
import Material.Icons.Content as MIcons
import Material.Icons.Editor as MIcons
import Material.Icons.Navigation as MIcons
import Material.Icons.Toggle as MIcons
import Maybe.Extra as Maybe
import Msg exposing (Msg(..))
import Port
import Random exposing (Generator, Seed)
import Random.Pipeline as Random
import RandomId
import Result.Extra as Result
import Return exposing (Return)
import Return3 as R3 exposing (Return3F)
import Tagged
import Task
import Tuple exposing (mapFirst)


type alias Toast =
    { title : String, visible : Bool }


init =
    { title = "", visible = False }


dismiss model =
    { model | visible = False }


show title model =
    { model | title = title, visible = True }


view : Toast -> Html Msg
view toast =
    viewIfLazy toast.visible
        (\_ -> viewContent toast.title)


viewContent title =
    flexRow
        [ CS.sticky
        , Css.bottom <| space4
        , Css.minWidth <| px 150
        , Css.maxWidth <| pct 80
        , Css.backgroundColor black80
        , Css.color white
        ]
        []
        [ flexRow
            [ Css.flexGrow <| num 1
            , Css.justifyContent Css.center
            , CS.p space2
            ]
            []
            [ text title ]
        , flexRow [ CS.p space2 ] [ class "pointer", onClick ToastDismiss ] [ text "X" ]
        ]
