module Grain exposing
    ( Grain
    , content
    , decoder
    , encoder
    , generator
    , id
    , idEq
    , setContent
    , setDeleted
    , titleOrEmpty
    , toDomIdWithPrefix
    )

import BasicsX exposing (eqs)
import DecodeX exposing (Encoder)
import GrainId exposing (GrainId(..))
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as E
import Random exposing (Generator)


type alias Model =
    { id : GrainId
    , content : String
    , deleted : Bool
    , revision : Int
    }


type Grain
    = Grain Model


new : GrainId -> Grain
new newId =
    Grain
        { id = newId
        , content = ""
        , deleted = False
        , revision = 0
        }


encoder : Encoder Grain
encoder (Grain model) =
    E.object
        [ ( "id", GrainId.encoder model.id )
        , ( "deleted", E.bool model.deleted )
        , ( "content", E.string model.content )
        , ( "revision", E.int model.revision )
        ]


decoder : Decoder Grain
decoder =
    DecodeX.start Model
        |> required "id" GrainId.decoder
        |> required "content" D.string
        |> optional "deleted" D.bool False
        |> optional "revision" D.int 0
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


toDomIdWithPrefix prefix =
    id >> GrainId.toDomIdWithPrefix prefix


generator : Generator Grain
generator =
    GrainId.generator |> Random.map new


setContent : String -> Grain -> Grain
setContent newContent =
    map (\model -> { model | content = newContent })


setDeleted newDeleted =
    map (\model -> { model | deleted = newDeleted })
