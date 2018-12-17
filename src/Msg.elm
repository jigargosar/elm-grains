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
    | GenerateAndAddNewGrain
    | AddNewGrain Grain
    | BackPressed
    | GrainContentChanged Grain String
    | ToastDismiss
    | RouteTo Route
    | UrlChanged String
    | Firebase Value
    | ErrorString String
    | SignIn
    | SignOut


routeTo route =
    RouteTo route


routeToGrain grain =
    routeToGrainId (Grain.id grain)


routeToGrainId gid =
    routeTo <| Route.Grain gid
