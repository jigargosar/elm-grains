module Todo exposing
    ( Msg(..)
    , Todo
    , TodoStore
    , all
    , create
    , emptyStore
    , insert
    , loadStore
    , remove
    , setTitle
    , update
    )

import BasicsX exposing (unpackResult)
import DecodeX exposing (Encoder)
import Dict exposing (Dict)
import Either exposing (Either)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E exposing (Value)
import Port
import Random
import RandomId
import Ret exposing (..)
import TimeX exposing (Millis)


type alias Todo =
    { id : String
    , title : String
    , body : String
    , done : Bool
    , createdAt : Millis
    , modifiedAt : Millis
    , contextId : String
    }


type Msg
    = SetDone Bool
    | SetTitle String


setTitle title =
    update (SetTitle title)


type alias TodoStore =
    { lookup : Dict String Todo
    }


emptyStore : TodoStore
emptyStore =
    { lookup = Dict.empty }


loadStore value =
    let
        todoDecoder : Decoder Todo
        todoDecoder =
            DecodeX.start Todo
                |> required "id" D.string
                |> required "title" D.string
                |> required "body" D.string
                |> required "done" D.bool
                |> required "createdAt" D.int
                |> required "modifiedAt" D.int
                |> required "contextId" D.string

        decoder : Decoder TodoStore
        decoder =
            D.map TodoStore
                (D.field "lookup" <| D.dict todoDecoder)
    in
    D.decodeValue decoder value
        |> unpackResult (\err -> ( emptyStore, Port.error <| "TodoStore: " ++ D.errorToString err )) pure


type alias HasMaybeIdNow x =
    { x
        | id : Maybe String
        , now : Maybe Millis
    }


create { id, now } contextId title =
    { id = id
    , createdAt = now
    , modifiedAt = now
    , title = title
    , body = ""
    , done = False
    , contextId = contextId
    }


remove todo model =
    { model | lookup = Dict.remove todo.id model.lookup }
        |> withEffect cache


insert : Todo -> TodoStore -> ( TodoStore, Cmd msg )
insert =
    upsertAndCache


update : Msg -> Todo -> TodoStore -> ( TodoStore, Cmd msg )
update msg todo model =
    upsertAndCache (updateTodo msg (getOr todo model)) model


updateTodo : Msg -> Todo -> Todo
updateTodo message model =
    case message of
        SetDone done ->
            { model | done = done }

        SetTitle title ->
            { model | title = title }


upsertAndCache todo model =
    pure { model | lookup = Dict.insert todo.id todo model.lookup }
        |> andThenDo cache


cache =
    let
        todoEncoder : Encoder Todo
        todoEncoder model =
            E.object
                [ ( "id", E.string model.id )
                , ( "title", E.string model.title )
                , ( "body", E.string model.body )
                , ( "done", E.bool model.done )
                , ( "createdAt", E.int model.createdAt )
                , ( "modifiedAt", E.int model.modifiedAt )
                , ( "contextId", E.string model.contextId )
                ]

        encoder model =
            E.object
                [ ( "lookup", E.dict identity todoEncoder model.lookup )
                ]
    in
    Port.cacheGrains << encoder


all =
    .lookup >> Dict.values


getOr todo model =
    model.lookup |> Dict.get todo.id |> Maybe.withDefault todo
