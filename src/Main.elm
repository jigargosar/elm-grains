module Main exposing (main)

import AuthState exposing (AuthState)
import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import BrowserX exposing (WindowSize)
import Color
import Css exposing (em, num, pct, px, rgb, zero)
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
import GrainChange
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
    , authState : AuthState
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
        |> Random.always AuthState.init
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


setAuthState authState model =
    { model | authState = authState }


getGrain gid =
    .grainStore >> GrainStore.get gid


mapGrainStore fn model =
    { model | grainStore = fn model.grainStore }



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
    , replyToMsg = \_ -> NoOp
    , update = update
    }


logErrorString : String -> Return3F Msg Model ()
logErrorString err =
    R3.do (Port.error err)
        >> mapToastR3 (Toast.show err)


pushUrl =
    R3.doWith .route (Route.toString >> Port.pushUrl)


cacheAndPersistEncodedGrainStore encoded =
    Cmd.batch [ Port.cacheGrains encoded, Port.persistGrains encoded ]


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

        GrainContentChanged grain title ->
            R3.andThen
                (\model ->
                    let
                        newGrainStore =
                            GrainStore.setGrainTitle grain title model.grainStore
                    in
                    R3.map (setGrainStore newGrainStore)
                        >> R3.effect
                            (.grainStore
                                >> GrainStore.encoder
                                >> cacheAndPersistEncodedGrainStore
                            )
                )

        DeleteGrain grain ->
            R3.andThen
                (\model ->
                    let
                        newGrainStore =
                            GrainStore.deleteGrain grain model.grainStore
                    in
                    R3.map (setGrainStore newGrainStore)
                        >> R3.effect
                            (.grainStore
                                >> GrainStore.encoder
                                >> cacheAndPersistEncodedGrainStore
                            )
                )

        FirestoreGrainChanges changes ->
            R3.andThen
                (\model ->
                    let
                        updateOne { doc, type_ } =
                            case type_ of
                                GrainChange.Added ->
                                    GrainStore.upsertGrain doc

                                GrainChange.Modified ->
                                    GrainStore.upsertGrain doc

                                GrainChange.Removed ->
                                    GrainStore.remove (Grain.id doc)
                    in
                    R3.map (setGrainStore (List.foldr updateOne model.grainStore changes))
                        >> R3.effect
                            (.grainStore
                                >> GrainStore.encoder
                                >> Port.cacheGrains
                            )
                )

        NewGrain ->
            R3.andThen
                (\model ->
                    let
                        grainStore =
                            model.grainStore

                        ( newGrain, newSeed ) =
                            Random.step Grain.generator model.grainStore.seed
                    in
                    R3.map
                        (setGrainStore
                            (GrainStore.addGrainWithNewSeed
                                newGrain
                                newSeed
                                model.grainStore
                            )
                        )
                        >> R3.effect
                            (.grainStore
                                >> GrainStore.encoder
                                >> cacheAndPersistEncodedGrainStore
                            )
                        >> update (Msg.routeToGrain newGrain)
                )

        LoadGrainStore val ->
            R3.andThen
                (\model ->
                    let
                        r2 : GrainStore -> ( GrainStore, Cmd msg )
                        r2 gs =
                            DecodeX.decode gs (GrainStore.decoder gs.seed) val

                        ( grainStore, cmd ) =
                            r2 model.grainStore
                    in
                    R3.map (setGrainStore grainStore)
                        >> R3.do cmd
                )

        GrainStoreSubMsg msg ->
            R3.sub msg grainStoreSubConfig

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
            R3.map (setAuthState <| AuthState.Authenticated user)

        AuthUserNone ->
            R3.map (setAuthState <| AuthState.NoUser)

        SignIn ->
            R3.do (Port.signIn ())

        SignOut ->
            R3.do (Port.signOut ())


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
            [ viewAppBar model.authState ]
                ++ viewRouteChildren model
                ++ [ viewToast model.toast
                   ]
        }


viewAppBar authState =
    let
        viewTitle =
            styled div [ CS.p2 space2 zero, CS.flex11Auto ] [] [ text "Grains" ]

        viewAuthState =
            case authState of
                AuthState.Loading ->
                    button [ class "btn loading" ] [ text "SignIn" ]

                AuthState.Authenticated user ->
                    button [ class "btn", onClick SignOut ] [ text "SignOut" ]

                AuthState.NoUser ->
                    button [ class "btn", onClick SignIn ] [ text "SignIn" ]
    in
    flexRowIC
        [ CS.sticky
        , Css.top <| px 0
        , CS.p2 zero space2
        ]
        [ class "bg-dark" ]
        [ viewTitle
        , viewAuthState
        ]


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
