module GrainCache exposing
    ( GrainCache
    , addNew
    , addNewAfter
    , addNewGrainBefore
    , batchUpdate
    , decoder
    , encoder
    , get
    , init
    , lastLeafGid
    , load
    , move
    , moveBy
    , moveOneLevelDown
    , moveOneLevelUp
    , rejectSubTreeAndFlatten
    , rootGid
    , rootTree
    , toRawList
    , treeFromGid
    , updateFromFirebaseChangeList
    , updateWithGrainUpdate
    )

import ActorId exposing (ActorId)
import BasicsX exposing (callWith, callWith2, eqs, ifElse, unwrapMaybe)
import Compare
import DecodeX exposing (Encoder)
import Direction exposing (Direction)
import Firebase
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import GrainDict exposing (GrainDict)
import GrainId exposing (GrainId)
import GrainIdLookup exposing (GrainIdLookup)
import GrainZipper as Z exposing (GrainForest, GrainTree, GrainZipper)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as E exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import Pivot exposing (Pivot)
import Port
import Random exposing (Generator, Seed)
import Random.Pipeline as Random
import RandomX
import Return exposing (Return)
import Return3 as R3 exposing (Return3F)
import SavedGrain exposing (SavedGrain)
import Time exposing (Posix)
import Tree
import Tree.Zipper as TZ
import Tuple2


type alias GrainCache =
    GrainIdLookup SavedGrain


decoder : Decoder GrainCache
decoder =
    D.list SavedGrain.decoder
        |> D.map
            (GrainIdLookup.fromList SavedGrain.id
                >> addRootIfAbsent
            )


encoder : GrainCache -> Value
encoder =
    GrainIdLookup.toList >> E.list SavedGrain.encoder


init : GrainCache
init =
    GrainIdLookup.empty
        |> addRootIfAbsent


addRootIfAbsent model =
    if idExists GrainId.root model then
        model

    else
        blindInsertGrain Grain.root model


allGrains : GrainCache -> List Grain
allGrains =
    toRawList
        >> List.map SavedGrain.value
        >> List.sortWith Grain.defaultComparator


childGrains : Grain -> GrainCache -> List Grain
childGrains grain =
    allGrains >> List.filter (Grain.isChildOf grain)


treeFromCache : GrainCache -> Grain -> GrainTree
treeFromCache grainCache grain =
    let
        newForest : GrainForest
        newForest =
            childGrains grain grainCache
                |> List.map (treeFromCache grainCache)
    in
    Tree.tree grain newForest


rootGrain : GrainCache -> Grain
rootGrain =
    get GrainId.root >> Maybe.withDefault Grain.root


treeFromGid : GrainId -> GrainCache -> Maybe GrainTree
treeFromGid gid =
    rootTreeZipper
        >> Z.findTreeById gid


rootTree : GrainCache -> GrainTree
rootTree model =
    treeFromCache model (rootGrain model)


rootTreeZipper : GrainCache -> GrainZipper
rootTreeZipper =
    rootTree >> Z.fromTree


rejectSubTreeAndFlatten : Grain -> GrainCache -> List Grain
rejectSubTreeAndFlatten grain =
    rootTreeZipper >> Z.removeEqByIdThenFlatten grain


get : GrainId -> GrainCache -> Maybe Grain
get gid =
    GrainIdLookup.get gid >> Maybe.map SavedGrain.value


rootGid : GrainCache -> GrainId
rootGid =
    rootGrain >> Grain.id


lastLeafGid : GrainCache -> GrainId
lastLeafGid =
    rootTreeZipper
        >> Z.lastDescendentGrain
        >> Grain.id


addNew : Grain -> GrainCache -> UpdateResult
addNew =
    ifCanAddGrainThen <|
        \grain model ->
            Result.Ok <|
                blindInsertGrain grain model


addNewAfter : GrainId -> Grain -> GrainCache -> UpdateResult
addNewAfter siblingGid =
    addNewAndThenBatchUpdate <|
        addNewAfterBatchUpdaters siblingGid


addNewGrainBefore : GrainId -> Grain -> GrainCache -> UpdateResult
addNewGrainBefore siblingGid =
    addNewAndThenBatchUpdate <|
        addNewBeforeBatchUpdaters siblingGid


ifCanAddGrainThen :
    (Grain -> GrainCache -> UpdateResult)
    -> Grain
    -> GrainCache
    -> UpdateResult
ifCanAddGrainThen fn grain model =
    if idExists (Grain.id grain) model then
        Result.Err "Error: Add Grain. GrainId exists"

    else
        fn grain model


addNewAndThenBatchUpdate fn grain model =
    if idExists (Grain.id grain) model then
        Result.Err "Error: Add Grain. GrainId exists"

    else
        fn (Grain.createdAt grain) grain model
            |> Maybe.unwrap
                (Result.Err "Err: addNewGrainAfter")
                (insertGrainThenBatchUpdate >> callWith2 grain model)


insertGrainThenBatchUpdate updaters grain model =
    blindInsertGrain grain model
        |> batchUpdate updaters


listToSortIdxUpdaters : Posix -> List Grain -> List GrainUpdater
listToSortIdxUpdaters now =
    List.indexedMap
        (\idx g ->
            ( Grain.update now <| Grain.SetSortIdx idx
            , Grain.id g
            )
        )


parentIdUpdater pid now gid =
    ( Grain.update now (Grain.SetParentId pid)
    , gid
    )


addNewAfterBatchUpdaters siblingGid now grain =
    let
        gid =
            Grain.id grain
    in
    rootTreeZipper
        >> Z.appendWhenIdEqAndGetParentAndChildGrains siblingGid grain
        >> Maybe.map
            (afterAddGrainUpdaters now gid)


afterAddGrainUpdaters now gid pc =
    pc
        |> Tuple.mapBoth
            (Grain.idAsParentId
                >> parentIdUpdater
                >> callWith2 now gid
            )
            (listToSortIdxUpdaters now)
        |> Tuple2.uncurry (::)


addNewBeforeBatchUpdaters siblingGid now grain =
    let
        gid =
            Grain.id grain
    in
    rootTreeZipper
        >> Z.prependWhenIdEqAndGetParentAndChildGrains siblingGid grain
        >> Maybe.map
            (afterAddGrainUpdaters now gid)



-- EXPOSED UPDATERS


updateFromFirebaseChangeList changeList model =
    let
        handleChange change =
            let
                grain =
                    GrainChange.grain change
            in
            case GrainChange.type_ change of
                GrainChange.Added ->
                    setSaved grain

                GrainChange.Modified ->
                    setSaved grain

                GrainChange.Removed ->
                    remove grain
    in
    List.foldr handleChange model changeList
        |> Result.Ok


load =
    D.decodeValue decoder
        >> Result.mapError D.errorToString


type alias UpdateResult =
    Result String GrainCache


update :
    (Grain -> Grain)
    -> GrainId
    -> GrainCache
    -> UpdateResult
update changeFn gid model =
    if GrainIdLookup.member gid model then
        let
            updateFn =
                SavedGrain.change changeFn
        in
        Result.Ok <| GrainIdLookup.updateIfExists gid updateFn model

    else
        Result.Err "GrainNotFound"



-- UPDATE HELPERS


blindInsertGrain grain model =
    GrainIdLookup.insert (Grain.id grain) (SavedGrain.new grain) model


setSaved : Grain -> GrainCache -> GrainCache
setSaved grain =
    GrainIdLookup.update (Grain.id grain)
        (Maybe.map (SavedGrain.setPersisted grain)
            >> Maybe.orElseLazy (\_ -> Just <| SavedGrain.new grain)
        )


remove grain =
    GrainIdLookup.remove (Grain.id grain)


toRawList =
    GrainIdLookup.toList


idExists gid =
    GrainIdLookup.member gid


type alias GrainUpdater =
    ( Grain -> Grain, GrainId )


batchUpdate : List GrainUpdater -> GrainCache -> UpdateResult
batchUpdate list model =
    let
        reducer ( changeFn, gid ) =
            Result.andThen (update changeFn gid)
    in
    List.foldl reducer (Result.Ok model) list


updateWithGrainUpdate :
    Grain.Update
    -> GrainId
    -> Posix
    -> GrainCache
    -> UpdateResult
updateWithGrainUpdate grainUpdate gid now model =
    update (Grain.update now grainUpdate) gid model


move :
    Direction
    -> GrainId
    -> Posix
    -> GrainCache
    -> UpdateResult
move direction gid now model =
    let
        fn =
            case direction of
                Direction.Up ->
                    moveBy -1

                Direction.Down ->
                    moveBy 1

                Direction.Left ->
                    moveOneLevelUp

                Direction.Right ->
                    moveOneLevelDown
    in
    fn gid now model


moveBy :
    Int
    -> GrainId
    -> Posix
    -> GrainCache
    -> UpdateResult
moveBy offset gid now model =
    let
        zipper =
            rootTreeZipper model

        siblings : List Grain
        siblings =
            Z.siblingsOf gid zipper

        gIdx : Int
        gIdx =
            List.findIndex (Grain.idEq gid) siblings
                |> Maybe.withDefault -1

        updaters : List ( Grain -> Grain, GrainId )
        updaters =
            List.swapAt gIdx (gIdx + offset) siblings
                |> Grain.listToEffectiveSortIndices
                |> List.map
                    (Tuple.mapBoth
                        (Grain.SetSortIdx >> Grain.update now)
                        Grain.id
                    )
    in
    batchUpdate updaters model


moveOneLevelUp gid now model =
    let
        zipper =
            rootTreeZipper model

        setParentId newParentId =
            updateWithGrainUpdate
                (Grain.SetParentId newParentId)
                gid
                now
                model
    in
    Z.parentWhenIdEq gid zipper
        |> Maybe.map (Grain.parentId >> setParentId)
        |> Maybe.withDefault (Result.Err "Grain Not Found")


moveOneLevelDown gid now model =
    let
        zipper =
            rootTreeZipper model

        siblings =
            Z.siblingsOf gid zipper

        newParentIdx : Int
        newParentIdx =
            List.findIndex (Grain.idEq gid) siblings
                |> Maybe.unwrap -1 ((+) -1)
                |> Debug.log "newParentIdx"
    in
    List.getAt newParentIdx siblings
        |> Maybe.map
            (Grain.idAsParentId
                >> (\pid ->
                        updateWithGrainUpdate
                            (Grain.SetParentId pid)
                            gid
                            now
                            model
                   )
            )
        |> Maybe.withDefault (Result.Err "Grain Not Found")
