module Msg exposing
    ( Msg(..)
    , routeToGrain
    )

import Browser.Dom
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import GrainId exposing (GrainId)
import GrainStore exposing (GrainStore)
import Json.Encode exposing (Value)
import Random exposing (Generator)
import Route exposing (Route)


type Msg
    = ---- INJECT MSG BELOW ----
      NoOp
    | FocusResult (Result String ())
    | LoadGrainStore Value
    | PermanentlyDeleteGrain Grain
    | CreateAndAddNewGrain
    | AddCreatedGrain Grain
    | DeleteGrain Grain
    | BackPressed
    | GrainContentChanged Grain String
    | GrainStoreMsg GrainStore.UpdateGrain Grain
    | ToastDismiss
    | RouteTo Route
    | UrlChanged String
    | Firebase Value
    | LogErrorString String
    | SignIn
    | SignOut


routeTo route =
    RouteTo route


routeToGrain grain =
    routeToGrainId (Grain.id grain)


routeToGrainId gid =
    routeTo <| Route.Grain gid
