module UrlChange exposing
    ( Action(..)
    , UrlChange
    , action
    , decoder
    , state
    , url
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)


type Action
    = Push
    | Pop
    | Replace


actionDecoder : Decoder Action
actionDecoder =
    D.string
        |> D.andThen
            (\string ->
                case string of
                    "PUSH" ->
                        D.succeed Push

                    "POP" ->
                        D.succeed Pop

                    "REPLACE" ->
                        D.succeed Replace

                    _ ->
                        D.fail <| "Invalid Action: " ++ string
            )


type alias Model =
    { url : String, action : Action, state : Value }


type UrlChange
    = UrlChange Model


unwrap (UrlChange model) =
    model


map fn =
    unwrap >> fn >> UrlChange


decoder : Decoder UrlChange
decoder =
    D.map3 Model
        (D.field "url" D.string)
        (D.field "action" actionDecoder)
        (D.field "state" D.value)
        |> D.map UrlChange


url =
    unwrap >> .url


action =
    unwrap >> .action


state =
    unwrap >> .state
