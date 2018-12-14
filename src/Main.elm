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
import Return as R2 exposing (Return)
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


logErrorString err =
    R2.command (Port.error err)
        >> R2.map (mapToast <| Toast.show err)


pushUrl =
    R2.effect_ (.route >> Route.toString >> Port.pushUrl)


cacheAndPersistEncodedGrainStore encoded =
    Cmd.batch [ Port.cacheGrains encoded, Port.persistGrains encoded ]


update : Msg -> Model -> ( Model, Cmd Msg )
update message =
    (case message of
        NoOp ->
            identity

        FocusResult (Ok ()) ->
            identity

        FocusResult (Err errorString) ->
            logErrorString errorString

        BrowserAnyKeyDown ->
            R2.effect_ (ifElse (.hasFocusIn >> not) (always focusBaseLayer) (\_ -> Cmd.none))

        BaseLayerFocusInChanged hasFocusIn ->
            R2.map (\model -> { model | hasFocusIn = hasFocusIn })

        GrainContentChanged grain title ->
            R2.andThen
                (\model ->
                    let
                        newGrainStore =
                            GrainStore.setGrainTitle grain title model.grainStore
                    in
                    R2.singleton model
                        |> R2.map (setGrainStore newGrainStore)
                        >> R2.effect_
                            (.grainStore
                                >> GrainStore.encoder
                                >> cacheAndPersistEncodedGrainStore
                            )
                )

        DeleteGrain grain ->
            R2.andThen
                (\model ->
                    let
                        newGrainStore =
                            GrainStore.deleteGrain grain model.grainStore
                    in
                    R2.singleton model
                        |> R2.map (setGrainStore newGrainStore)
                        >> R2.effect_
                            (.grainStore
                                >> GrainStore.encoder
                                >> cacheAndPersistEncodedGrainStore
                            )
                )

        FirestoreGrainChanges changes ->
            R2.andThen
                (\model ->
                    let
                        updateOne { doc, type_ } =
                            case type_ of
                                GrainChange.Added ->
                                    GrainStore.upsertGrain doc

                                GrainChange.Modified ->
                                    GrainStore.upsertGrain doc

                                GrainChange.Removed ->
                                    GrainStore.deleteGrain doc
                    in
                    R2.singleton model
                        |> R2.map
                            (setGrainStore (List.foldr updateOne model.grainStore changes))
                        >> R2.effect_
                            (.grainStore
                                >> GrainStore.encoder
                                >> Port.cacheGrains
                            )
                )

        NewGrain ->
            R2.andThen
                (\model ->
                    let
                        grainStore =
                            model.grainStore

                        ( newGrain, newSeed ) =
                            Random.step Grain.generator model.seed
                    in
                    R2.singleton model
                        |> R2.map
                            (mapGrainStore (GrainStore.addGrain newGrain)
                                >> setNewSeed newSeed
                            )
                        >> R2.effect_
                            (.grainStore
                                >> GrainStore.encoder
                                >> cacheAndPersistEncodedGrainStore
                            )
                        >> R2.andThen (update (Msg.routeToGrain newGrain))
                )

        LoadGrainStore val ->
            R2.andThen
                (\model ->
                    let
                        r2 : GrainStore -> ( GrainStore, Cmd msg )
                        r2 gs =
                            DecodeX.decode gs GrainStore.decoder val

                        ( grainStore, cmd ) =
                            r2 model.grainStore
                    in
                    R2.return (setGrainStore grainStore model) cmd
                )

        ToastDismiss ->
            R2.map <| mapToast <| Toast.dismiss

        RouteTo route ->
            R2.map (setRoute route)
                >> pushUrl
                >> R2.effect_ (.route >> autoFocusRoute)

        UrlChanged url ->
            R2.map (setRoute <| Route.fromString url)

        Firebase val ->
            R2.andThen (handleFire2Elm val)

        AuthUser user ->
            R2.map (setAuthState <| AuthState.Authenticated user)

        AuthUserNone ->
            R2.map (setAuthState <| AuthState.NoUser)

        SignIn ->
            R2.command (Port.signIn ())

        SignOut ->
            R2.command (Port.signOut ())
    )
        << R2.singleton


handleFire2Elm : Value -> Model -> ( Model, Cmd Msg )
handleFire2Elm val model =
    let
        result : Result String Msg
        result =
            D.decodeValue Fire2Elm.decoder val
                |> Result.mapError (D.errorToString >> Debug.log "es")
    in
    result
        |> Result.unpack
            (logErrorString >> callWith (R2.singleton model))
            (update >> callWith model)


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
