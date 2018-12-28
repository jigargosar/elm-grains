port module Worker exposing (main)

import BasicsX exposing (callWith)
import Elm.Parser
import Elm.Processing
import Elm.RawFile
import Elm.Syntax.File
import Json.Encode exposing (Value)
import Port
import Result.Extra as Result
import Return
import Time exposing (Posix)


port send : Value -> Cmd msg


port receive : (Value -> msg) -> Sub msg


type alias Flags =
    { now : Int, fileContent : String }


type alias Model =
    {}


init flags =
    let
        _ =
            Debug.log "posix" (Time.millisToPosix flags.now)
    in
    ( {}
    , Cmd.batch
        [ {- Port.error <|
                 """fileContent:

                 """
                     ++ flags.fileContent
             ,
          -}
          --          Port.error <|
          --            Debug.toString <|
          --                parse flags.fileContent
          parseAndEncode flags.fileContent
            |> Result.map Port.logJson
            |> Result.mapError Port.error
            |> Result.merge
        ]
    )


parse : String -> String
parse input =
    case Elm.Parser.parse input of
        Err e ->
            "Failed: " ++ Debug.toString e

        Ok v ->
            "Success: " ++ Debug.toString v


parseAndEncode : String -> Result String Value
parseAndEncode input =
    let
        processing : Elm.Processing.ProcessContext
        processing =
            Elm.Processing.init
    in
    Elm.Parser.parse input
        --        |> Result.map Elm.RawFile.encode
        |> Result.map
            --            Elm.Processing.addFile
            --                >> callWith processing
            --                >> Elm.Processing.process
            (Elm.Processing.process processing
                >> Elm.Syntax.File.encode
            )
        |> Result.mapError Debug.toString


subscriptions model =
    Sub.batch []


type Msg
    = NoOp


update msg model =
    Return.singleton model


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
