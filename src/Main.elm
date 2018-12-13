module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import BrowserX exposing (WindowSize)
import Color
import Css exposing (em, num, pct, px, rgb)
import CssElements exposing (..)
import CssElevation exposing (elevation)
import CssIcons exposing (view)
import CssLayout exposing (flexCol, flexRow, flexRowIC)
import CssShorthand as CS
import CssTheme exposing (black80, blackAlpha, space2, space4, white)
import DecodeX exposing (DecodeResult)
import Either exposing (Either(..))
import EventX exposing (onKeyDownPD)
import Fire2Elm
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
import NotFoundView
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
        |> Random.always Toast.init
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


focusDomId domId =
    Browser.Dom.focus domId
        |> Task.mapError (\_ -> "FocusError: domId=" ++ domId)
        |> Task.attempt FocusResult


focusMaybeDomId =
    unpackMaybe Cmd.none focusDomId


focusBaseLayer =
    focusDomId "base-layer"


maybeAutoFocusRouteDomId route =
    case route of
        Route.Grain _ ->
            Just GrainView.autoFocusId

        _ ->
            Nothing


autoFocusRoute route =
    maybeAutoFocusRouteDomId route
        |> focusMaybeDomId


focusGrain =
    GrainListView.grainDomId >> focusDomId


globalKeyBinding model =
    K.bindEachToMsg []


subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <| D.succeed BrowserAnyKeyDown
        , Port.urlChanged UrlChanged
        , Port.fire2Elm Firebase
        ]


grainStoreSubConfig =
    { subUpdate = GrainStore.update
    , get = .grainStore
    , set = setGrainStore
    , toMsg = GrainStoreSubMsg
    , replyToMsg = GrainStoreReply
    , update = update
    }


logErrorString : String -> Return3F Msg Model ()
logErrorString err =
    R3.do (Port.error err)
        >> mapToastR3 (Toast.show err)


pushUrl =
    R3.doWith .route (Route.toString >> Port.pushUrl)


update : Msg -> Return3F Msg Model ()
update message =
    case message of
        NoOp ->
            identity

        FocusResult (Ok ()) ->
            identity

        FocusResult (Err errorString) ->
            logErrorString errorString

        BrowserAnyKeyDown ->
            R3.doWhen (.hasFocusIn >> not) focusBaseLayer

        BaseLayerFocusInChanged hasFocusIn ->
            R3.map (\model -> { model | hasFocusIn = hasFocusIn })

        LoadGrainStore val ->
            R3.dispatch grainStoreSubConfig (GrainStore.load val)

        GrainStoreSubMsg msg ->
            R3.sub msg grainStoreSubConfig

        GrainStoreReply reply ->
            case reply of
                GrainStore.NoReply ->
                    identity

                GrainStore.NewGrainAddedReply grain ->
                    update (Msg.routeToGrain grain)

        ToastDismiss ->
            mapToastR3 Toast.dismiss

        RouteTo route ->
            R3.map (setRoute route)
                >> pushUrl
                >> R3.doWith .route autoFocusRoute

        UrlChanged url ->
            R3.map (setRoute <| Route.fromString url)

        Firebase val ->
            R3.andThen (handleFire2Elm val)

        AuthUser user ->
            let
                _ =
                    Debug.log "user" user
            in
            logErrorString "UserLoggedIn"

        AuthUserNone ->
            logErrorString "UserNotLoggedIn"

        SignIn ->
            R3.do (Port.signIn ())


handleFire2Elm val model =
    D.decodeValue Fire2Elm.decoder val
        |> Result.mapError (D.errorToString >> Debug.log "es")
        |> Result.unpack logErrorString update


keyBindings model =
    K.bindEachToMsg <|
        [ ( K.arrowUp, ( NoOp, True ) )
        , ( K.arrowDown, ( NoOp, True ) )
        ]


view : Model -> Html Msg
view model =
    Skeleton.view
        { onKeyDownPD = keyBindings model
        , children =
            [ viewAppBar ]
                ++ viewRouteChildren model
                ++ [ viewToast model.toast
                   ]
        }


viewAppBar =
    let
        viewTitle =
            styled div [ CS.p space2 ] [] [ text "Grains" ]
    in
    flexRow [ CS.sticky, Css.top <| px 0 ]
        [ class "white bg-black-80" ]
        [ viewTitle ]


viewRouteChildren model =
    case model.route of
        Route.GrainList ->
            mapStateToGrainListView model |> GrainListView.view

        Route.Grain gid ->
            getGrain gid model |> GrainView.view

        Route.NotFound string ->
            NotFoundView.view


viewToast toast =
    Toast.view toast


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
        , subscriptions = subscriptions
        }
