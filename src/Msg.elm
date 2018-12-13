module Msg exposing
    ( Msg(..)
    , deleteGrain
    , grainContentChanged
    , grainFirestoreChanges
    , routeToGrain
    )

import Browser.Dom
import FireUser exposing (FireUser)
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import GrainId exposing (GrainId)
import GrainStore
import Json.Encode exposing (Value)
import Random exposing (Generator)
import Route exposing (Route)


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | FocusResult (Result String ())
    | BrowserAnyKeyDown
    | BaseLayerFocusInChanged Bool
    | LoadGrainStore Value
    | NewGrain
    | GrainStoreSubMsg GrainStore.Msg
    | GrainStoreReply GrainStore.Reply
    | ToastDismiss
    | RouteTo Route
    | UrlChanged String
    | Firebase Value
    | AuthUser FireUser
    | AuthUserNone
    | SignIn
    | SignOut


routeTo route =
    RouteTo route


routeToGrain grain =
    routeToGrainId (Grain.id grain)


routeToGrainId gid =
    routeTo <| Route.Grain gid


deleteGrain grain =
    GrainStoreSubMsg (GrainStore.deleteGrain grain)


grainContentChanged grain content =
    GrainStoreSubMsg (GrainStore.setContent grain content)


grainFirestoreChanges =
    GrainStoreSubMsg << GrainStore.firestoreChanges
