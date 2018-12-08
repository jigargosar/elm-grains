module StoreGenerator exposing (main, storeModule)

import Browser
import Port
import StructuredWriter exposing (..)


moduleLine storeName =
    let
        exposedDoc =
            [ storeName ] |> List.map string |> parensComma True
    in
    [ "module", storeName, "exposing" ]
        |> List.map string
        |> (::) exposedDoc
        |> StructuredWriter.spaced


storeModule : String -> Writer
storeModule modelName =
    let
        storeName =
            modelName ++ "Store"
    in
    moduleLine storeName


main : Platform.Program () () ()
main =
    Platform.worker
        { init = \_ -> ( (), Port.error (write <| storeModule "Grain") )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
