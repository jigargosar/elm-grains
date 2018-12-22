module Grain exposing
    ( Grain
    , ParentId
    , SortIdx
    , Update(..)
    , content
    , createdAt
    , decoder
    , defaultComparator
    , deleted
    , encoder
    , eqById
    , eqByParentId
    , generator
    , id
    , idAsParentId
    , idAsString
    , idEq
    , isChildOf
    , isParentOf
    , listToEffectiveSortIndices
    , modifiedAt
    , parentIdAsGrainId
    , parentIdEq
    , rootParentId
    , titleOrEmpty
    , toDomIdWithPrefix
    , update
    , updateSortIndices
    )

import BasicsX exposing (eqs, flip)
import Compare
import DecodeX exposing (Encoder)
import GrainId exposing (GrainId(..))
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required, resolve)
import Json.Encode as E exposing (Value)
import Maybe.Extra as Maybe
import Random exposing (Generator)
import Time exposing (Posix)
import TimeX


type alias SortIdx =
    Int


defaultSortIdx =
    -1


sortIdxEncoder =
    E.int


sortIdxDecoder =
    D.int


type ParentId
    = ParentId GrainId
    | RootId


rootParentId : ParentId
rootParentId =
    RootId


defaultParentId : ParentId
defaultParentId =
    RootId


parentIdEncoder : ParentId -> Value
parentIdEncoder model =
    case model of
        ParentId gid ->
            GrainId.encoder gid

        RootId ->
            E.null


parentIdDecoder : Decoder ParentId
parentIdDecoder =
    D.oneOf [ GrainId.decoder |> D.map ParentId, D.succeed RootId ]


type alias Model =
    { id : GrainId
    , parentId : ParentId
    , sortIdx : SortIdx
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
        , sortIdx = defaultSortIdx
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
        , ( "sortIdx", sortIdxEncoder model.sortIdx )
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
        |> optional "sortIdx" sortIdxDecoder defaultSortIdx
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


parentIdEq pid =
    parentId >> eqs pid


eqByParentId : Grain -> Grain -> Bool
eqByParentId g2 =
    parentIdEq (parentId g2)


eqById g2 =
    idEq (id g2)


toDomIdWithPrefix prefix =
    id >> GrainId.toDomIdWithPrefix prefix


idAsString =
    id >> GrainId.toString


idAsParentId =
    id >> ParentId


isParentOf child =
    idAsParentId >> eqs (parentId child)


isChildOf =
    flip isParentOf


parentIdAsGrainId grain =
    case parentId grain of
        ParentId gid ->
            Just gid

        RootId ->
            Nothing


parentId =
    unwrap >> .parentId


sortIdx =
    unwrap >> .sortIdx


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


setSortIdx : SortIdx -> Grain -> Grain
setSortIdx newSortIdx =
    map (\model -> { model | sortIdx = newSortIdx })


sortIdxComparator =
    Compare.by identity


defaultComparator : Compare.Comparator Grain
defaultComparator =
    Compare.concat
        [ Compare.compose sortIdx sortIdxComparator
        , Compare.compose createdAt (Compare.reverse TimeX.comparator)
        ]


type Update
    = SetContent String
    | SetDeleted Bool
    | SetParentId ParentId
    | SetSortIdx SortIdx


updateSortIndices : Posix -> List Grain -> List Grain
updateSortIndices now =
    List.indexedMap (SetSortIdx >> update now)


listToEffectiveSortIndices : List Grain -> List ( SortIdx, Grain )
listToEffectiveSortIndices =
    List.indexedMap Tuple.pair


update : Posix -> Update -> Grain -> Grain
update now msg grain =
    let
        innerUpdate =
            case msg of
                SetContent content_ ->
                    setContent content_

                SetDeleted deleted_ ->
                    setDeleted deleted_

                SetParentId parentId_ ->
                    setParentId parentId_

                SetSortIdx sortIdx_ ->
                    setSortIdx sortIdx_

        newGrain =
            innerUpdate grain
    in
    if grain == newGrain then
        newGrain

    else
        setModifiedAt now newGrain
