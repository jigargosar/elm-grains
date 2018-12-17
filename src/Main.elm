module Main exposing (main)

import ActorId exposing (ActorId)
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
import CssHtml
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
    , toast : Toast
    , route : Route
    , authState : AuthState
    , actorId : ActorId
    , seed : Seed
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            Model
                |> Random.from (initialSeed flags)
                |> Random.always GrainStore.empty
                |> Random.always Toast.init
                |> Random.always (Route.fromString flags.url)
                |> Random.always AuthState.init
                |> Random.with ActorId.generator
                |> Random.finish
    in
    update (LoadGrainStore flags.grains) model


setGrainStore grainStore model =
    { model | grainStore = grainStore }


setRoute route model =
    { model | route = route }


setRouteFromString =
    Route.fromString >> setRoute


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


generateNewGrain : Model -> ( Grain, Model )
generateNewGrain model =
    let
        ( newGrain, newSeed ) =
            Random.step Grain.generator model.seed
    in
    ( newGrain, setNewSeed newSeed model )


autoFocusRoute route =
    let
        maybeDomId =
            case route of
                Route.Grain _ ->
                    Just GrainView.autoFocusId

                _ ->
                    Nothing
    in
    maybeDomId |> unwrapMaybeCmd (BrowserX.focus FocusResult)


unwrapMaybeCmd cmdFn =
    Maybe.unwrap Cmd.none cmdFn


globalKeyBinding model =
    K.bindEachToMsg []


subscriptions model =
    Sub.batch
        [ Port.urlChanged UrlChanged
        , Port.fire2Elm Firebase
        ]


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

        GrainStoreUserMsg msg grain ->
            let
                ( ( newGrainStore, cmd ), outMsg ) =
                    GrainStore.userUpdate msg grain model.actorId model.grainStore
            in
            Return.return (setGrainStore newGrainStore model) cmd
                |> Return.andThen (handleOutMsg outMsg)

        GrainContentChanged grain content ->
            update (GrainStoreUserMsg (GrainStore.setGrainContent content) grain) model

        DeleteGrain grain ->
            update (GrainStoreUserMsg (GrainStore.setGrainDeleted True) grain) model

        PermanentlyDeleteGrain grain ->
            update (GrainStoreUserMsg GrainStore.permanentlyDeleteGrain grain) model

        NewGrain ->
            let
                ( grain, newModel ) =
                    generateNewGrain model
            in
            update (GrainStoreUserMsg GrainStore.addNewGrain grain) newModel

        SetGrainStore gs ->
            Return.singleton (setGrainStore gs model)

        LoadGrainStore val ->
            Return.return model
                (GrainStore.loadFromCache val model.grainStore
                    |> Result.unwrap Cmd.none (Random.generate SetGrainStore)
                )

        RouteTo route ->
            Return.singleton (setRoute route model)
                |> Return.effect_ (.route >> Route.toString >> Port.pushUrl)
                |> Return.effect_ (.route >> autoFocusRoute)

        UrlChanged url ->
            Return.singleton (setRouteFromString url model)

        Firebase val ->
            let
                handleFireMsg fireMsg =
                    case fireMsg of
                        Firebase.Error errString ->
                            update (LogErrorString errString) model

                        Firebase.AuthUser user ->
                            Return.singleton (setAuthState (AuthState.Authenticated user) model)

                        Firebase.AuthUserNone ->
                            Return.singleton (setAuthState AuthState.NoUser model)

                        Firebase.GrainChanges changes ->
                            ( model, Random.generate FirebaseGrainChanges changes )
            in
            Firebase.decode val
                |> handleFireMsg

        SignIn ->
            Return.return model (Firebase.signIn ())

        SignOut ->
            Return.return model (Firebase.signOut ())

        BackPressed ->
            Return.return model (Port.navigateBack ())

        FirebaseGrainChanges changes ->
            let
                ( newGrainStore, cmd ) =
                    GrainStore.onFirebaseChanges changes model.grainStore
            in
            Return.return (setGrainStore newGrainStore model) cmd


handleOutMsg message model =
    case message of
        GrainStore.Added newGrain ->
            update (Msg.routeToGrain newGrain) model

        _ ->
            Return.singleton model


view : Model -> Html Msg
view model =
    let
        routeVM =
            routeViewModel model.route
    in
    Skeleton.view
        { children =
            [ viewAppBar routeVM model.authState ]
                ++ viewRouteChildren model
                ++ [ viewToast model.toast
                   ]
        }


type alias RouteViewModel =
    { title : String, showBackBtn : Bool, children : List (Html Msg) }


routeViewModel : Route -> RouteViewModel
routeViewModel route =
    case route of
        Route.GrainList ->
            { title = "Grain List", showBackBtn = False, children = [] }

        Route.Grain _ ->
            { title = "Grain", showBackBtn = True, children = [] }

        Route.NotFound _ ->
            { title = "Oops!", showBackBtn = True, children = [] }


viewAppBar { title, showBackBtn } authState =
    let
        viewTitle =
            styled div
                [ CS.p2 space2 zero
                , CS.flex11Auto
                , Css.textAlign Css.center
                ]
                []
                [ text title ]

        viewBackBtn =
            button [ class "btn", onClick BackPressed ] [ text "Back" ]

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
        [ CssHtml.viewIf showBackBtn viewBackBtn
        , viewTitle
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
