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
import Firebase
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            Model
                |> Random.from (initialSeed flags)
                |> Random.always GrainStore.init
                |> Random.always initialHasFocusIn
                |> Random.always Toast.init
                |> Random.always (Route.fromString flags.url)
                |> Random.always AuthState.init
                |> Random.finish
    in
    update (LoadGrainStore flags.grains) model


setGrainStore grainStore model =
    { model | grainStore = grainStore }


setRoute route model =
    { model | route = route }


mapToast fn model =
    { model | toast = fn model.toast }


setAuthState authState model =
    { model | authState = authState }


getGrain gid =
    .grainStore >> GrainStore.get gid


mapGrainStore fn model =
    { model | grainStore = fn model.grainStore }


setNewSeed newSeed model =
    { model | seed = newSeed }



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


cacheAndPersistEncodedGrainStore encoded =
    Cmd.batch [ Port.cacheGrains encoded, Port.persistGrains encoded ]


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            Return.singleton model

        LogErrorString errString ->
            Return.return (mapToast (Toast.show errString) model)
                (Port.error errString)

        ToastDismiss ->
            Return.singleton (mapToast Toast.dismiss model)

        FocusResult (Ok ()) ->
            Return.singleton model

        FocusResult (Err errorString) ->
            update (LogErrorString errorString) model

        BrowserAnyKeyDown ->
            Return.return model
                (ifElse (.hasFocusIn >> not)
                    (always focusBaseLayer)
                    (\_ -> Cmd.none)
                    model
                )

        BaseLayerFocusInChanged hasFocusIn ->
            Return.singleton { model | hasFocusIn = hasFocusIn }

        GrainContentChanged grain title ->
            let
                newGrainStore =
                    GrainStore.setGrainTitle grain title model.grainStore
            in
            setGrainStore newGrainStore model
                |> Return.singleton
                |> Return.effect_
                    (.grainStore
                        >> GrainStore.encoder
                        >> cacheAndPersistEncodedGrainStore
                    )

        DeleteGrain grain ->
            let
                newGrainStore =
                    GrainStore.deleteGrain grain model.grainStore
            in
            Return.singleton model
                |> Return.map (setGrainStore newGrainStore)
                >> Return.effect_
                    (.grainStore
                        >> GrainStore.encoder
                        >> cacheAndPersistEncodedGrainStore
                    )

        NewGrain ->
            let
                grainStore =
                    model.grainStore

                ( newGrain, newSeed ) =
                    Random.step Grain.generator model.seed
            in
            Return.singleton model
                |> Return.map
                    (mapGrainStore (GrainStore.addGrain newGrain)
                        >> setNewSeed newSeed
                    )
                >> Return.effect_
                    (.grainStore
                        >> GrainStore.encoder
                        >> cacheAndPersistEncodedGrainStore
                    )
                >> Return.andThen (update (Msg.routeToGrain newGrain))

        LoadGrainStore val ->
            let
                r2 : GrainStore -> ( GrainStore, Cmd msg )
                r2 gs =
                    DecodeX.decode gs GrainStore.decoder val

                ( grainStore, cmd ) =
                    r2 model.grainStore
            in
            Return.return (setGrainStore grainStore model) cmd

        RouteTo route ->
            Return.singleton (setRoute route model)
                |> Return.effect_ (.route >> Route.toString >> Port.pushUrl)
                |> Return.effect_ (.route >> autoFocusRoute)

        UrlChanged url ->
            let
                newRoute =
                    Route.fromString url
            in
            Return.singleton (setRoute newRoute model)

        Firebase val ->
            case D.decodeValue Firebase.decoder val of
                Err error ->
                    update (LogErrorString (D.errorToString error)) model

                Ok fireMsg ->
                    handleFireMsg fireMsg model

        SignIn ->
            Return.return model (Port.signIn ())

        SignOut ->
            Return.return model (Port.signOut ())


handleFireMsg fireMsg model =
    case fireMsg of
        Firebase.UnknownMsg unknown ->
            update (LogErrorString ("Invalid Firebase Msg Received: " ++ unknown)) model

        Firebase.AuthUser user ->
            Return.singleton (setAuthState (AuthState.Authenticated user) model)

        Firebase.AuthUserNone ->
            Return.singleton (setAuthState AuthState.NoUser model)

        Firebase.GrainChanges changes ->
            let
                handleChange { doc, type_ } =
                    case type_ of
                        GrainChange.Added ->
                            GrainStore.upsertGrain doc

                        GrainChange.Modified ->
                            GrainStore.upsertGrain doc

                        GrainChange.Removed ->
                            GrainStore.deleteGrain doc
            in
            Return.singleton model
                |> Return.map
                    (setGrainStore (List.foldr handleChange model.grainStore changes))
                |> Return.effect_
                    (.grainStore
                        >> GrainStore.encoder
                        >> Port.cacheGrains
                    )


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


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.toUnstyled << view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
