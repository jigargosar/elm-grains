module Grain exposing
    ( Grain
    , content
    , decoderGenerator
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
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Json.Encode as E
import Random exposing (Generator)
import RevisionId exposing (RevisionId)


type alias Model =
    { id : GrainId
    , content : String
    , deleted : Bool
    , revisionId : RevisionId
    }


type Grain
    = Grain Model


new : GrainId -> RevisionId -> Grain
new newId newRevisionId =
    Grain
        { id = newId
        , content = ""
        , deleted = False
        , revisionId = newRevisionId
        }


encoder : Encoder Grain
encoder (Grain model) =
    E.object
        [ ( "id", GrainId.encoder model.id )
        , ( "deleted", E.bool model.deleted )
        , ( "content", E.string model.content )
        , ( "revisionId", RevisionId.encoder model.revisionId )
        ]


decoderGenerator : Decoder (Generator Grain)
decoderGenerator =
    DecodeX.start
        (\id_ content_ deleted revisionIdValue ->
            RevisionId.generator
                |> Random.map
                    (\newRevisionId ->
                        let
                            revisionId =
                                D.decodeValue RevisionId.decoder revisionIdValue
                                    |> Result.withDefault newRevisionId
                        in
                        Model id_ content_ deleted revisionId
                            |> Grain
                    )
        )
        |> required "id" GrainId.decoder
        |> required "content" D.string
        |> optional "deleted" D.bool False
        |> optional "revisionId" D.value E.null


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
    Random.map2 new GrainId.generator RevisionId.generator


setContent : String -> Grain -> Grain
setContent newContent =
    map (\model -> { model | content = newContent })


setDeleted newDeleted =
    map (\model -> { model | deleted = newDeleted })
