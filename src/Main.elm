module Main exposing (main)

import ActorId exposing (ActorId)
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
import CssLayout exposing (flexCol, flexRow)
import CssProto
import CssShorthand as CS
import CssTheme exposing (black80, blackAlpha, space2, space4, white)
import DecodeX exposing (DecodeResult)
import Either exposing (Either(..))
import EventX exposing (onKeyDownPD)
import FireUser exposing (FireUser)
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
import InlineEditGrain exposing (InlineEditGrain)
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
import Time
import TimeX
import Toast exposing (Toast)
import Tuple exposing (mapFirst)



---- MODEL ----


type Popup
    = GrainMorePopup GrainId
    | GrainMovePopup GrainId
    | NoPopup


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
    , authState : Firebase.AuthState
    , actorId : ActorId
    , popup : Popup
    , inlineEditGrain : InlineEditGrain
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
                |> Random.always Firebase.initialAuthState
                |> Random.with ActorId.generator
                |> Random.always NoPopup
                |> Random.always InlineEditGrain.initialValue
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


grainById gid =
    .grainStore >> GrainStore.getById gid


mapGrainStore fn model =
    { model | grainStore = fn model.grainStore }


setNewSeed newSeed model =
    { model | seed = newSeed }


dismissPopup model =
    { model | popup = NoPopup }



---- UPDATE ----


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


handleErrorString errString model =
    Return.return (mapToast (Toast.show errString) model)
        (Port.error errString)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            Return.singleton model

        ErrorString errString ->
            handleErrorString errString model

        ToastDismiss ->
            Return.singleton (mapToast Toast.dismiss model)

        FocusResult (Ok ()) ->
            Return.singleton model

        FocusResult (Err errorString) ->
            handleErrorString errorString model

        GrainContentChanged grain content ->
            ( model
            , performWithNow
                (SetGrainContentWithNow (Grain.id grain) content)
            )

        InlineEditGrainContentChanged grain content ->
            case InlineEditGrain.onContentChange content model.inlineEditGrain of
                Err errString ->
                    handleErrorString errString model

                Ok inlineEditGrain ->
                    Return.singleton
                        { model
                            | inlineEditGrain = inlineEditGrain
                        }

        InlineEditGrain grain ->
            Return.singleton
                { model
                    | inlineEditGrain = InlineEditGrain.startEditing grain
                }
                |> Return.command
                    (BrowserX.focus FocusResult <|
                        GrainListView.inlineGrainEditInputDomId grain
                    )

        InlineEditGrainSubmit ->
            case InlineEditGrain.endEditing model.inlineEditGrain of
                Err errString ->
                    handleErrorString errString model

                Ok ( gid, content, inlineEditGrain ) ->
                    Return.return
                        { model
                            | inlineEditGrain = inlineEditGrain
                        }
                        (performWithNow
                            (SetGrainContentWithNow gid content)
                        )

        SetGrainContentWithNow gid content now ->
            case GrainStore.setContent now content gid model.grainStore of
                Err errString ->
                    handleErrorString errString model

                Ok ( newGrainStore, cmd ) ->
                    Return.return (setGrainStore newGrainStore model) cmd

        DeleteGrain grain ->
            ( model
            , Task.perform (SetGrainDeletedWithNow (Grain.id grain) True) Time.now
            )

        RestoreGrain grain ->
            ( model
            , Task.perform (SetGrainDeletedWithNow (Grain.id grain) False) Time.now
            )

        GrainMoreAction msg ->
            update msg model
                |> Return.map dismissPopup

        ShowMoveToPopup grain ->
            Return.singleton { model | popup = GrainMovePopup (Grain.id grain) }

        DismissPopup ->
            dismissPopup model
                |> Return.singleton

        GrainMoreClicked grain ->
            Return.singleton { model | popup = GrainMorePopup (Grain.id grain) }

        SetGrainDeletedWithNow gid bool now ->
            case GrainStore.setDeleted now bool gid model.grainStore of
                Err errString ->
                    handleErrorString errString model

                Ok ( newGrainStore, cmd ) ->
                    Return.return (setGrainStore newGrainStore model) cmd

        CreateAndAddNewGrain ->
            ( model
            , performWithNow CreateAndAddNewGrainWithNow
            )

        CreateAndAddNewGrainWithNow now ->
            Return.return model
                (Random.generate AddNewGrain (Grain.generator now))

        AddNewGrain grain ->
            case GrainStore.addNewGrain grain model.grainStore of
                Err errString ->
                    handleErrorString errString model

                Ok ( newGrainStore, cmd ) ->
                    Return.return (setGrainStore newGrainStore model) cmd
                        |> Return.andThen (update (Msg.routeToGrain grain))

        LoadGrainStore val ->
            let
                ( newGrainStore, cmd ) =
                    GrainStore.loadCache val model.grainStore
            in
            Return.return (setGrainStore newGrainStore model) cmd

        RouteTo route ->
            Return.singleton (setRoute route model)
                |> Return.effect_ (.route >> Route.toString >> Port.pushUrl)
                |> Return.effect_ (.route >> autoFocusRoute)

        UrlChanged url ->
            Return.singleton (setRouteFromString url model)

        Firebase encodedMsg ->
            let
                handleFireMsg fireMsg =
                    case fireMsg of
                        Firebase.Error errString ->
                            handleErrorString errString model

                        Firebase.AuthStateChanged authState ->
                            Return.singleton (setAuthState authState model)

                        Firebase.GrainChanges changes ->
                            let
                                ( newGrainStore, cmd ) =
                                    GrainStore.onFirebaseChanges changes
                                        model.grainStore
                            in
                            Return.return (setGrainStore newGrainStore model) cmd
            in
            handleFireMsg (Firebase.decodeInbound encodedMsg)

        SignIn ->
            Return.return model (Firebase.signIn ())

        SignOut ->
            Return.return model (Firebase.signOut ())

        BackPressed ->
            Return.return model (Port.navigateBack ())


performWithNow nowToMsg =
    Task.perform nowToMsg Time.now


performSetGrainContentWithNow gid content =
    performWithNow <| SetGrainContentWithNow gid content


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
                   , viewPopup model
                   ]
        }


viewPopup model =
    case model.popup of
        GrainMorePopup gid ->
            grainById gid model
                |> Maybe.unwrap CssHtml.noView viewGrainMorePopup

        GrainMovePopup gid ->
            grainById gid model
                |> Maybe.unwrap CssHtml.noView viewGrainMovePopup

        NoPopup ->
            CssHtml.noView


viewGrainMovePopup grain =
    CssProto.modal
        { content =
            [ flexCol []
                []
                [ flexRow [ CS.justifyCenter ] [] [ text "Move Grain" ]
                ]
            ]
        , onDismiss = Msg.DismissPopup
        }


viewGrainMorePopup grain =
    let
        viewEdit : Grain -> Html Msg
        viewEdit g =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick (Msg.GrainMoreAction <| Msg.routeToGrain g) ]
                [ flexCol [] [] [ text "Edit" ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.modeEdit
                    ]
                ]

        viewMoveTo g =
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick (Msg.ShowMoveToPopup g) ]
                [ flexCol [] [] [ text "Move To..." ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view CssIcons.modeEdit
                    ]
                ]

        viewDelete g =
            let
                deleted =
                    Grain.deleted g

                action =
                    (ter deleted Msg.RestoreGrain Msg.DeleteGrain <|
                        g
                    )
                        |> Msg.GrainMoreAction

                actionTitle =
                    ter deleted "Restore" "Trash"

                icon =
                    ter deleted CssIcons.restore CssIcons.delete
            in
            flexRow [ CS.pointer, CS.p2 space2 zero ]
                [ onClick action ]
                [ flexCol [] [] [ text actionTitle ]
                , CssElements.iconBtnWithStyles [ CS.selfCenter ]
                    []
                    [ CssIcons.view icon
                    ]
                ]
    in
    CssProto.modal
        { content =
            [ flexCol []
                []
                [ flexRow [ CS.justifyCenter ] [] [ text "Grain Menu" ]
                , viewEdit grain
                , viewMoveTo grain
                , viewDelete grain
                ]
            ]
        , onDismiss = Msg.DismissPopup
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
                Firebase.AuthStateLoading ->
                    button [ class "btn loading" ] [ text "SignIn" ]

                Firebase.AuthStateUser user ->
                    button [ class "btn", onClick SignOut ] [ text "SignOut" ]

                Firebase.AuthStateNoUser ->
                    button [ class "btn", onClick SignIn ] [ text "SignIn" ]
    in
    CssLayout.flexRow
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
            grainById gid model |> GrainView.view

        Route.NotFound string ->
            NotFoundView.view


viewToast toast =
    Toast.view toast


mapStateToGrainListView : Model -> GrainListView
mapStateToGrainListView model =
    let
        allGrains =
            model.grainStore |> GrainStore.allAsList

        ( deletedGrainList, grainList ) =
            allGrains |> List.partition Grain.deleted

        modifiedAtDesc =
            Grain.modifiedAt >> Time.posixToMillis >> negate

        createdAtAtDesc =
            Grain.createdAt >> Time.posixToMillis >> negate
    in
    { grains = grainList
    , deleted = deletedGrainList
    , inlineEditGrain = model.inlineEditGrain
    }



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.toUnstyled << view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
