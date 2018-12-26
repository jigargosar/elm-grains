module GrainCache exposing
    ( GrainCache
    , addNew
    , addNewAfter
    , addNewGrainBefore
    , batchUpdate
    , decoder
    , encoder
    , get__
    , init
    , lastLeafGid
    , load
    , moveBy
    , moveOneLevelDown
    , moveOneLevelUp
    , nextByGid
    , parentByGid
    , prevByGid
    , rejectSubTreeAndFlatten
    , rootGid
    , rootGrains__
    , toRawList
    , updateFromFirebaseChangeList
    , updateWithGrainUpdate
    )

import ActorId exposing (ActorId)
import BasicsX exposing (callWith, callWith2, eqs, ifElse, unwrapMaybe)
import Compare
import DecodeX exposing (Encoder)
import Firebase
import Grain exposing (Grain)
import GrainChange exposing (GrainChange)
import GrainDict exposing (GrainDict)
import GrainId exposing (GrainId)
import GrainIdLookup exposing (GrainIdLookup)
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


type alias Forest =
    List GrainTree


type alias GrainTree =
    Tree.Tree Grain


type alias GrainZipper =
    TZ.Zipper Grain


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
        newForest : Forest
        newForest =
            childGrains grain grainCache
                |> List.map (treeFromCache grainCache)
    in
    Tree.tree grain newForest


rootGrain : GrainCache -> Grain
rootGrain =
    getGrainById GrainId.root >> Maybe.withDefault Grain.root


rootTree : GrainCache -> GrainTree
rootTree model =
    treeFromCache model (rootGrain model)


rootTreeZipper : GrainCache -> GrainZipper
rootTreeZipper =
    rootTree >> TZ.fromTree


rejectSubTreeOf : Grain -> GrainCache -> Maybe GrainZipper
rejectSubTreeOf grain =
    rootTreeZipper
        >> TZ.findFromRoot (Grain.eqById grain)
        >> Maybe.andThen TZ.removeTree


rejectSubTreeAndFlatten : Grain -> GrainCache -> List Grain
rejectSubTreeAndFlatten grain =
    rejectSubTreeOf grain >> Maybe.unwrap [] (TZ.toTree >> Tree.flatten)


getGrainById : GrainId -> GrainCache -> Maybe Grain
getGrainById gid =
    get__ gid >> Maybe.map SavedGrain.value


rootGid : GrainCache -> GrainId
rootGid =
    rootGrain >> Grain.id


lastLeafGid : GrainCache -> GrainId
lastLeafGid =
    rootTreeZipper
        >> TZ.lastDescendant
        >> TZ.label
        >> Grain.id


nextByGid : GrainId -> GrainCache -> GrainId
nextByGid gid =
    rootTreeZipper
        >> TZ.findFromRoot (Grain.idEq gid)
        >> Maybe.andThen TZ.forward
        >> Maybe.unwrap gid (TZ.label >> Grain.id)


prevByGid : GrainId -> GrainCache -> GrainId
prevByGid gid =
    rootTreeZipper
        >> TZ.findFromRoot (Grain.idEq gid)
        >> Maybe.andThen TZ.backward
        >> Maybe.unwrap gid (TZ.label >> Grain.id)


parentByGid : GrainId -> GrainCache -> GrainId
parentByGid gid =
    rootTreeZipper
        >> TZ.findFromRoot (Grain.idEq gid)
        >> Maybe.andThen TZ.parent
        >> Maybe.unwrap gid (TZ.label >> Grain.id)


rootGrains__ =
    allGrains >> List.filter Grain.isRoot


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
    rootTreeZipper
        >> TZ.findFromRoot (Grain.idEq siblingGid)
        >> Maybe.andThen
            (TZ.append (Tree.tree grain [])
                >> updatePidAndSortIndices now grain
            )


updatePidAndSortIndices now grain =
    TZ.parent
        >> Maybe.map
            (\pz ->
                let
                    childUpdaters =
                        pz
                            |> TZ.children
                            >> List.map Tree.label
                            >> listToSortIdxUpdaters now

                    pidUpdater =
                        pz
                            |> TZ.label
                            >> Grain.idAsParentId
                            >> parentIdUpdater
                            >> callWith2 now (Grain.id grain)
                in
                pidUpdater :: childUpdaters
            )


addNewBeforeBatchUpdaters siblingGid now grain =
    rootTreeZipper
        >> TZ.findFromRoot (Grain.idEq siblingGid)
        >> Maybe.andThen
            (TZ.prepend (Tree.tree grain [])
                >> updatePidAndSortIndices now grain
            )



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
        (Maybe.map (SavedGrain.setSaved grain)
            >> Maybe.orElseLazy (\_ -> Just <| SavedGrain.new grain)
        )


remove grain =
    GrainIdLookup.remove (Grain.id grain)


getSiblingsById : GrainId -> GrainCache -> List SavedGrain
getSiblingsById gid model =
    get__ gid model |> Maybe.unwrap [] (getSiblingsOf__ >> callWith model)


get__ : GrainId -> GrainCache -> Maybe SavedGrain
get__ gid =
    GrainIdLookup.get gid


getParentOfGrain : Grain -> GrainCache -> Maybe SavedGrain
getParentOfGrain grain model =
    Grain.parentIdAsGrainId grain
        |> Maybe.andThen (GrainIdLookup.get >> callWith model)


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


moveBy :
    Int
    -> GrainId
    -> Posix
    -> GrainCache
    -> UpdateResult
moveBy offset gid now model =
    get__ gid model
        |> Result.fromMaybe "Error: setSortIdx: Grain Not Found in Cache"
        |> Result.andThen (moveHelp now offset >> callWith model)


moveHelp : Posix -> Int -> SavedGrain -> GrainCache -> UpdateResult
moveHelp now offset savedGrain model =
    let
        siblings : List SavedGrain
        siblings =
            getSiblingsOf__ savedGrain model

        gIdx : Int
        gIdx =
            List.findIndex (eqById savedGrain)
                siblings
                |> Maybe.withDefault -1

        updaters : List ( Grain -> Grain, GrainId )
        updaters =
            List.swapAt gIdx (gIdx + offset) siblings
                |> List.map SavedGrain.value
                |> Grain.listToEffectiveSortIndices
                |> List.map
                    (Tuple.mapBoth
                        (Grain.SetSortIdx >> Grain.update now)
                        Grain.id
                    )
    in
    batchUpdate updaters model


moveOneLevelUp gid now model =
    getGrainById gid model
        |> Maybe.andThen (moveOneLevelUpHelp now model)
        |> Maybe.withDefault (Result.Err "Grain Not Found")


moveOneLevelDown gid now model =
    let
        siblings =
            getSiblingsById gid model

        newParentIdx : Int
        newParentIdx =
            List.findIndex (idEq gid) siblings
                |> Maybe.unwrap -1 ((+) -1)
                |> Debug.log "newParentIdx"
    in
    List.getAt newParentIdx siblings
        |> Maybe.map
            (SavedGrain.value
                >> Grain.idAsParentId
                >> Debug.log "pid"
                >> (\pid ->
                        updateWithGrainUpdate
                            (Grain.SetParentId pid)
                            gid
                            now
                            model
                   )
            )
        |> Maybe.withDefault (Result.Err "Grain Not Found")


moveOneLevelUpHelp now model grain =
    getParentOfGrain grain model
        |> Maybe.map (SavedGrain.value >> moveGrainAfter now model grain)


moveGrainAfter now model grain sibling =
    let
        gid =
            Grain.id grain

        newParentId =
            Grain.parentId sibling
    in
    updateWithGrainUpdate (Grain.SetParentId newParentId) gid now model


getSiblingsOf__ : SavedGrain -> GrainCache -> List SavedGrain
getSiblingsOf__ savedGrain model =
    toRawList model
        |> List.filter (eqByParentId savedGrain)
        |> List.sortWith defaultComparator


getChildrenWithParentId pid model =
    toRawList model
        |> List.filter (eqByParentId pid)
        |> List.sortWith defaultComparator


defaultComparator =
    Compare.compose SavedGrain.value Grain.defaultComparator


eqByParentId savedGrain =
    SavedGrain.value
        >> Grain.isSibling (SavedGrain.value savedGrain)


idEq gid =
    SavedGrain.value >> Grain.idEq gid


eqById savedGrain =
    SavedGrain.value
        >> Grain.eqById (SavedGrain.value savedGrain)


id =
    SavedGrain.id


isRoot =
    SavedGrain.value >> Grain.isRoot


parentId =
    SavedGrain.value >> Grain.parentId


isChildOfGrainId gid =
    SavedGrain.value >> Grain.isChildOfGrainId gid


parentIdAsGrainId =
    SavedGrain.value >> Grain.parentIdAsGrainId
