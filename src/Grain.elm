module Grain exposing
    ( Grain
    , ParentId
    , Update(..)
    , content
    , createdAt
    , decoder
    , deleted
    , encoder
    , eqById
    , generator
    , id
    , idAsParentId
    , idAsString
    , idEq
    , modifiedAt
    , titleOrEmpty
    , toDomIdWithPrefix
    , update
    )

import BasicsX exposing (eqs)
import DecodeX exposing (Encoder)
import GrainId exposing (GrainId(..))
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required, resolve)
import Json.Encode as E exposing (Value)
import Maybe.Extra as Maybe
import Random exposing (Generator)
import Time exposing (Posix)
import TimeX


type alias ParentId =
    Maybe GrainId


defaultParentId : ParentId
defaultParentId =
    Nothing


parentIdEncoder : ParentId -> Value
parentIdEncoder =
    Maybe.unwrap E.null GrainId.encoder


parentIdDecoder : Decoder ParentId
parentIdDecoder =
    GrainId.decoder |> D.map Just


type alias Model =
    { id : GrainId
    , parentId : ParentId
    , content : String
    , deleted : Bool
    , createdAt : Posix
    , modifiedAt : Posix
    }


type Grain
    = Grain Model


new : Posix -> GrainId -> Grain
new now newId =
    Grain
        { id = newId
        , parentId = defaultParentId
        , content = ""
        , deleted = False
        , createdAt = now
        , modifiedAt = now
        }


encoder : Encoder Grain
encoder (Grain model) =
    E.object
        [ ( "id", GrainId.encoder model.id )
        , ( "parentId", parentIdEncoder model.parentId )
        , ( "content", E.string model.content )
        , ( "deleted", E.bool model.deleted )
        , ( "createdAt", TimeX.posixEncoder model.createdAt )
        , ( "modifiedAt", TimeX.posixEncoder model.modifiedAt )
        ]


decoder : Decoder Grain
decoder =
    DecodeX.start Model
        |> required "id" GrainId.decoder
        |> optional "parentId" parentIdDecoder defaultParentId
        |> required "content" D.string
        |> optional "deleted" D.bool False
        |> required "createdAt" TimeX.posixDecoder
        |> required "modifiedAt" TimeX.posixDecoder
        |> D.map Grain


unwrap (Grain model) =
    model


map : (Model -> Model) -> Grain -> Grain
map fn =
    unwrap >> fn >> Grain


titleFromContent =
    String.trim
        >> String.split "\n"
        >> List.head
        >> Maybe.map String.trim


titleOrEmpty =
    content >> titleFromContent >> Maybe.withDefault ""


content =
    unwrap >> .content


id =
    unwrap >> .id


idEq gid =
    id >> eqs gid


eqById g2 =
    idEq (id g2)


toDomIdWithPrefix prefix =
    id >> GrainId.toDomIdWithPrefix prefix


idAsString =
    id >> GrainId.toString


idAsParentId =
    id >> Just


deleted =
    unwrap >> .deleted


modifiedAt =
    unwrap >> .modifiedAt


createdAt =
    unwrap >> .createdAt


generator : Posix -> Generator Grain
generator now =
    Random.map (new now) GrainId.generator


setContent : String -> Grain -> Grain
setContent newContent =
    map (\model -> { model | content = newContent })


setDeleted : Bool -> Grain -> Grain
setDeleted newDeleted =
    map (\model -> { model | deleted = newDeleted })


setModifiedAt : Posix -> Grain -> Grain
setModifiedAt newModifiedAt =
    map (\model -> { model | modifiedAt = newModifiedAt })


setParentId : ParentId -> Grain -> Grain
setParentId newParentId =
    map (\model -> { model | parentId = newParentId })


type Update
    = SetContent String
    | SetDeleted Bool
    | SetParentId ParentId


update : Posix -> Update -> Grain -> Grain
update now msg =
    let
        innerUpdate =
            case msg of
                SetContent content_ ->
                    setContent content_

                SetDeleted deleted_ ->
                    setDeleted deleted_

                SetParentId parentId_ ->
                    setParentId parentId_
    in
    innerUpdate >> setModifiedAt now
