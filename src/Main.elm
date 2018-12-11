module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import BrowserX exposing (WindowSize)
import Color
import Css exposing (em, num, pct, px)
import CssElements exposing (..)
import CssElevation exposing (elevation)
import CssIcons exposing (viewIcon)
import CssLayout exposing (flexCol, flexRow, flexRowIC)
import DecodeX exposing (DecodeResult)
import Either exposing (Either(..))
import EventX exposing (onKeyDownPD)
import Grain exposing (Grain)
import GrainId exposing (GrainId)
import GrainListView exposing (GrainListView)
import GrainStore exposing (GrainStore)
import GrainView
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
import Route exposing (Route)
import Skeleton
import Tagged
import Task
import Toast exposing (Toast)
import Tuple exposing (mapFirst)



---- MODEL ----


type alias Flags =
    { now : Millis
    , windowSize : WindowSize
    , grains : Value
    , url : String
    }


initialSeed : Flags -> Seed
initialSeed =
    .now >> Random.initialSeed


type alias Model =
    { grainStore : GrainStore
    , hasFocusIn : Bool
    , toast : Toast
    , route : Route
    , seed : Seed
    }


initialHasFocusIn =
    False


init : Flags -> Return Msg Model
init flags =
    Model
        |> Random.from (initialSeed flags)
        |> Random.with GrainStore.generator
        |> Random.always initialHasFocusIn
        |> Random.always (Toast.visible "App Init")
        |> Random.always (Route.fromString flags.url)
        |> Random.finish
        |> elmUpdate (LoadGrainStore flags.grains)


setGrainStore grainStore model =
    { model | grainStore = grainStore }


setRoute route model =
    { model | route = route }


mapToast fn model =
    { model | toast = fn model.toast }


mapToastR3 =
    R3.map << mapToast


getGrain gid =
    .grainStore >> GrainStore.get gid



---- UPDATE ----


focusBaseLayerCmd =
    Browser.Dom.focus "base-layer"
        |> Task.attempt (\_ -> NoOp)


focusGrain =
    GrainListView.focusGrain >> Task.attempt (\_ -> NoOp)


globalKeyBinding model =
    K.bindEachToMsg []


subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <| D.succeed BrowserAnyKeyDown
        , Port.urlChanged UrlChanged
        ]


grainStoreSubConfig =
    { subUpdate = GrainStore.update
    , get = .grainStore
    , set = setGrainStore
    , toMsg = GrainStoreSubMsg
    , replyToMsg = GrainStoreReply
    , update = update
    }


logErrorString err =
    R3.do (Port.error err)
        >> mapToastR3 (Toast.showWithTitle err)


dispatchToGrainStore =
    R3.dispatch grainStoreSubConfig


pushUrl =
    R3.doWith .route (Route.toString >> Port.pushUrl)


update : Msg -> Return3F Msg Model ()
update message =
    case message of
        NoOp ->
            identity

        FocusResult (Ok ()) ->
            identity

        FocusResult (Err err) ->
            logErrorString "Dom Focus Error"

        BrowserAnyKeyDown ->
            R3.doWhen (.hasFocusIn >> not) focusBaseLayerCmd

        BaseLayerFocusInChanged hasFocusIn ->
            R3.map (\model -> { model | hasFocusIn = hasFocusIn })

        AddNewClicked ->
            dispatchToGrainStore GrainStore.createNewGrain

        LoadGrainStore val ->
            dispatchToGrainStore (GrainStore.load val)

        GrainStoreSubMsg msg ->
            R3.sub msg grainStoreSubConfig

        GrainStoreReply reply ->
            case reply of
                GrainStore.NoReply ->
                    identity

                GrainStore.NewGrainAddedReply grain ->
                    R3.do (focusGrain grain)

        ToastDismiss ->
            mapToastR3 Toast.dismiss

        RouteTo route ->
            R3.map (setRoute route) >> pushUrl

        UrlChanged url ->
            --            R3.map (setRoute <| Route.fromString url)
            update (RouteTo <| Route.fromString url)


keyBinding model =
    K.bindEachToMsg <|
        [ ( K.arrowUp, ( NoOp, True ) )
        , ( K.arrowDown, ( NoOp, True ) )
        ]


view : Model -> Html Msg
view model =
    flexCol
        [ Css.flexShrink <| num 0
        , Css.minWidth <| pct 100
        , Css.minHeight <| pct 100
        ]
        [ id "base-layer"
        , class "sans-serif"
        , SA.fromUnstyled <| EventX.onFocusIn <| BaseLayerFocusInChanged True
        , SA.fromUnstyled <| EventX.onFocusOut <| BaseLayerFocusInChanged False
        , tabindex -1
        , SA.fromUnstyled <|
            EventX.onKeyDownPD <|
                keyBinding model
        ]
        [ viewRoute model
        , viewToast model.toast
        ]


viewRoute model =
    case model.route of
        Route.GrainList ->
            GrainListView.view (mapStateToGrainListView model)

        Route.Grain gid ->
            getGrain gid model |> GrainView.view

        Route.NotFound string ->
            Skeleton.notFoundView


viewToast toast =
    let
        toastView =
            { dismiss = ToastDismiss }
    in
    Toast.view toastView toast


mapStateToGrainListView : Model -> GrainListView
mapStateToGrainListView model =
    { grainList = model.grainStore |> GrainStore.allAsList }



---- PROGRAM ----


elmUpdate =
    R3.toElmUpdate update


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.toUnstyled << view
        , init = init
        , update = elmUpdate

        --        , update = updateDispatcher
        , subscriptions = subscriptions
        }
