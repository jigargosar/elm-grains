module Toast exposing (Toast, ToastView, dismiss, view, viewContent, visible)

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
import CssIcons exposing (viewIcon)
import CssLayout exposing (flexCol, flexRow, flexRowIC)
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


visible title =
    { title = title, visible = True }


dismiss model =
    { model | visible = False }


type alias ToastView msg =
    { dismiss : msg }


view : ToastView msg -> Toast -> Html msg
view vm toast =
    viewIfLazy toast.visible
        (\_ -> viewContent toast.title vm)


black80 =
    Css.rgba 0 0 0 0.8


space2 =
    px 8


space3 =
    px 12


space4 =
    px 16


white =
    Css.hex "#fff"


viewContent title vm =
    flexRow
        [ Css.position Css.fixed
        , Css.bottom <| px 32
        , Css.right space4
        , Css.minWidth <| px 150
        , Css.maxWidth <| pct 80
        , Css.backgroundColor black80
        , Css.color white
        , Css.padding space2
        ]
        []
        [ flexRow
            [ Css.flexGrow <| num 1
            , Css.justifyContent Css.center
            ]
            []
            [ text title ]
        , flexRow [] [ class "pointer", onClick vm.dismiss ] [ text "X" ]
        ]
