module UserLabel exposing
    ( UserLabel
    , decoder
    , displayName
    , encoder
    , id
    , newGeneratorCmd
    )

import BasicsX exposing (..)
import DecodeX exposing (Encoder)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Random exposing (Generator)
import RandomId
import Task exposing (Task)
import Time exposing (Posix)
import TimeX
import UserLabelId exposing (UserLabelId)


type alias Model =
    { id : UserLabelId
    , name : String
    , createdAt : Posix
    , modifiedAt : Posix
    }


type UserLabel
    = UserLabel Model


unwrap : UserLabel -> Model
unwrap (UserLabel model) =
    model


id =
    unwrap >> .id


init name_ now id_ =
    UserLabel
        { id = id_
        , name = name_
        , createdAt = now
        , modifiedAt = now
        }


newGenerator : String -> Task Never (Generator UserLabel)
newGenerator name_ =
    Task.map
        (\now -> Random.map (init name_ now) UserLabelId.generator)
        Time.now


newGeneratorCmd msg =
    Task.perform msg << newGenerator


encoder : Encoder UserLabel
encoder (UserLabel model) =
    E.object
        [ ( "id", UserLabelId.encoder model.id )
        , ( "name", E.string model.name )
        , ( "createdAt", TimeX.posixEncoder model.createdAt )
        , ( "modifiedAt", TimeX.posixEncoder model.modifiedAt )
        ]


decoder : Decoder UserLabel
decoder =
    DecodeX.start Model
        |> required "id" UserLabelId.decoder
        |> required "title" D.string
        |> required "createdAt" TimeX.posixDecoder
        |> required "modifiedAt" TimeX.posixDecoder
        |> D.map UserLabel


displayName =
    unwrap >> .name >> defaultEmptyStringTo "<no name>"
