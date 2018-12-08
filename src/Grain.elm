module Grain exposing
    ( Grain
    , bucket
    , decoder
    , encoder
    , hasId
    , id
    , idAsString
    , isComplete
    , isInBucket
    , isInTrash
    , isPending
    , labelIds
    , moveToBucket
    , newGenerator
    , newGeneratorWithTitleCmd
    , setTitle
    , title
    , toggleComplete
    , toggleLabel
    , toggleTrash
    )

import BasicsX exposing (..)
import Bucket exposing (Bucket)
import DecodeX exposing (Encoder)
import GrainId exposing (GrainId)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (optional, required)
import Json.Encode as E exposing (Value)
import Label
import LabelId exposing (LabelId)
import List.Extra as List
import Random exposing (Generator)
import Task exposing (Task)
import Time exposing (Posix)
import TimeX exposing (Millis)


type alias Model =
    { id : GrainId
    , title : String
    , body : String
    , labelIds : List LabelId
    , bucket : Bucket
    , createdAt : Posix
    , modifiedAt : Posix
    }


type Grain
    = Grain Model


defaultBucket =
    Bucket.InBasket


defaultLabelIds =
    [ LabelId.incomplete ]


init : String -> Posix -> GrainId -> Grain
init title_ now id_ =
    Grain
        { id = id_
        , title = title_
        , body = ""
        , labelIds = defaultLabelIds
        , bucket = defaultBucket
        , createdAt = now
        , modifiedAt = now
        }


newGenerator : String -> Task Never (Generator Grain)
newGenerator title_ =
    Task.map
        (\now -> Random.map (init title_ now) GrainId.generator)
        Time.now


newGeneratorWithTitleCmd : (Generator Grain -> msg) -> String -> Cmd msg
newGeneratorWithTitleCmd msg =
    Task.perform msg << newGenerator


encoder : Encoder Grain
encoder (Grain model) =
    E.object
        [ ( "id", GrainId.encoder model.id )
        , ( "title", E.string model.title )
        , ( "body", E.string model.body )
        , ( "labelIds", LabelId.labelIdsEncoder model.labelIds )
        , ( "bucket", Bucket.encoder model.bucket )
        , ( "createdAt", TimeX.posixEncoder model.createdAt )
        , ( "modifiedAt", TimeX.posixEncoder model.modifiedAt )
        ]


decoder : Decoder Grain
decoder =
    DecodeX.start Model
        |> required "id" GrainId.decoder
        |> required "title" D.string
        |> required "body" D.string
        |> optional "labelIds" LabelId.labelIdsDecoder defaultLabelIds
        |> optional "bucket" Bucket.decoder defaultBucket
        |> required "createdAt" TimeX.posixDecoder
        |> required "modifiedAt" TimeX.posixDecoder
        |> D.map Grain


unwrap (Grain model) =
    model


idAsString =
    id >> GrainId.asString


labelIds =
    unwrap >> .labelIds


title =
    unwrap >> .title


id =
    unwrap >> .id


hasId : GrainId -> Grain -> Bool
hasId grainId grain =
    id grain == grainId


hasLabel : LabelId -> Grain -> Bool
hasLabel labelId =
    unwrap >> .labelIds >> List.member labelId


bucket : Grain -> Bucket
bucket =
    unwrap >> .bucket


isInBucket b =
    bucket >> eqs b


isComplete =
    isPending >> not


isPending =
    hasLabel LabelId.incomplete


isInTrash =
    hasLabel LabelId.trash


over : (Model -> Model) -> Grain -> Grain
over fn =
    unwrap >> fn >> Grain


overLabelIds : (List LabelId -> List LabelId) -> Grain -> Grain
overLabelIds fn =
    over (\model -> { model | labelIds = fn model.labelIds })


toggleComplete : Grain -> Grain
toggleComplete =
    toggleLabelId LabelId.incomplete


toggleLabelId labelId =
    overLabelIds (LabelId.toggle labelId)


toggleLabel =
    Label.id >> toggleLabelId


toggleTrash : Grain -> Grain
toggleTrash =
    toggleLabelId LabelId.trash


moveToBucket newBucket =
    over (\model -> { model | bucket = newBucket })


setTitle newTitle =
    over (\model -> { model | title = newTitle })
