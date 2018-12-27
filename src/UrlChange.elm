module UrlChange exposing (Action(..), UrlChange, action, decoder, url)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type Action
    = Push
    | Pop


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

                    _ ->
                        D.fail <| "Invalid Action: " ++ string
            )


type alias Model =
    { url : String, action : Action }


type UrlChange
    = UrlChange Model


unwrap (UrlChange model) =
    model


map fn =
    unwrap >> fn >> UrlChange


decoder : Decoder UrlChange
decoder =
    D.map2 Model
        (D.field "url" D.string)
        (D.field "action" actionDecoder)
        |> D.map UrlChange


url =
    unwrap >> .url


action =
    unwrap >> .action
