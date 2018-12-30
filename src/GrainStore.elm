module GrainStore exposing
    ( Add(..)
    , GrainStore
    , Msg(..)
    , Update(..)
    , decoder
    , encoder
    , get
    , init
    , rejectSubTreeAndFlatten
    , toRawList
    , treeFromGid
    , update
    )

import ActorId exposing (ActorId)
import BasicsX exposing (callWith, callWith2, eqs, ifElse, unwrapMaybe)
import Compare
import DecodeX exposing (Encoder)
import Direction exposing (Direction)
import Firebase
import Grain exposing (Grain, ParentId, SortIdx)
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


type alias GrainStore =
    GrainIdLookup SavedGrain


decoder : Decoder GrainStore
decoder =
    D.list SavedGrain.decoder
        |> D.map
            (GrainIdLookup.fromList SavedGrain.id
                >> addRootIfAbsent
            )


encoder : GrainStore -> Value
encoder =
    GrainIdLookup.toList >> E.list SavedGrain.encoder


init : GrainStore
init =
    GrainIdLookup.empty
        |> addRootIfAbsent


addRootIfAbsent model =
    if idExists GrainId.root model then
        model

    else
        blindInsertGrain Grain.root model


allGrains : GrainStore -> List Grain
allGrains =
    toRawList
        >> List.map SavedGrain.value
        >> List.sortWith Grain.defaultComparator


childGrains : Grain -> GrainStore -> List Grain
childGrains grain =
    allGrains >> List.filter (Grain.isChildOf grain)


treeFromCache : GrainStore -> Grain -> GrainTree
treeFromCache grainStore grain =
    let
        newForest : GrainForest
        newForest =
            childGrains grain grainStore
                |> List.map (treeFromCache grainStore)
    in
    Tree.tree grain newForest


rootGrain : GrainStore -> Grain
rootGrain =
    get GrainId.root >> Maybe.withDefault Grain.root


treeFromGid : GrainId -> GrainStore -> Maybe GrainTree
treeFromGid gid =
    rootTreeZipper
        >> Z.findTreeById gid


rootTree : GrainStore -> GrainTree
rootTree model =
    treeFromCache model (rootGrain model)


rootTreeZipper : GrainStore -> GrainZipper
rootTreeZipper =
    rootTree >> Z.fromTree


rejectSubTreeAndFlatten : Grain -> GrainStore -> List Grain
rejectSubTreeAndFlatten grain =
    rootTreeZipper >> Z.removeEqByIdThenFlatten grain


get : GrainId -> GrainStore -> Maybe Grain
get gid =
    GrainIdLookup.get gid >> Maybe.map SavedGrain.value


addNewDefault : Grain -> GrainStore -> UpdateResult
addNewDefault =
    ifCanAddGrainThen <|
        \grain model ->
            Result.Ok <|
                blindInsertGrain grain model


addNewAfter : GrainId -> Grain -> GrainStore -> UpdateResult
addNewAfter siblingGid =
    addNewAndThenBatchUpdate <|
        addNewAfterBatchUpdaters siblingGid


addNewBefore : GrainId -> Grain -> GrainStore -> UpdateResult
addNewBefore siblingGid =
    addNewAndThenBatchUpdate <|
        addNewBeforeBatchUpdaters siblingGid


type Add
    = AddAfter GrainId
    | AddBefore GrainId
    | AddDefault


addNew msg grain =
    case msg of
        AddAfter gid ->
            addNewAfter gid grain

        AddBefore gid ->
            addNewBefore gid grain

        AddDefault ->
            addNewDefault grain


type Update
    = Move Direction
      --    | SetContent String
    | SetDeleted Bool
    | SetParentId ParentId



--    | SetSortIdx SortIdx
--updateGrainWithNow : Posix -> Grain.Set -> Grain -> Grain
--updateGrainWithNow now msg grain =
--    let
--        innerUpdate =
--            case msg of
--                Grain.SetContent content_ ->
--                    Grain.setContent content_
--
--                Grain.SetDeleted deleted_ ->
--                    Grain.setDeleted deleted_
--
--                Grain.SetParentId parentId_ ->
--                    Grain.setParentId parentId_
--
--                Grain.SetSortIdx sortIdx_ ->
--                    Grain.setSortIdx sortIdx_
--
--        newGrain =
--            innerUpdate grain
--    in
--    if grain == newGrain then
--        newGrain
--
--    else
--        Grain.setModifiedAt now newGrain


type Msg
    = AddGrain Add Grain
    | UpdateGrain Update GrainId Posix
    | Load Value
    | FirebaseChanges (List GrainChange)


update message model =
    case message of
        AddGrain msg grain ->
            addNew msg
                grain
                model.grainStore

        UpdateGrain msg gid now ->
            updateGrain msg
                gid
                now
                model.grainStore

        FirebaseChanges changeList ->
            updateFromFirebaseChangeList changeList
                model.grainStore

        Load encoded ->
            load encoded


updateGrain msg gid now =
    case msg of
        Move direction ->
            move direction gid now

        --        SetContent val ->
        --            updateWithSetMsg (Grain.SetContent val) gid now
        SetDeleted val ->
            updateWithSetMsg (Grain.SetDeleted val) gid now

        SetParentId val ->
            updateWithSetMsg (Grain.SetParentId val) gid now



--        SetSortIdx val ->
--            updateWithSetMsg (Grain.SetSortIdx val) gid now


ifCanAddGrainThen :
    (Grain -> GrainStore -> UpdateResult)
    -> Grain
    -> GrainStore
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
                    setPersisted grain

                GrainChange.Modified ->
                    setPersisted grain

                GrainChange.Removed ->
                    remove grain
    in
    List.foldr handleChange model changeList
        |> Result.Ok


load =
    D.decodeValue decoder
        >> Result.mapError D.errorToString


type alias UpdateResult =
    Result String GrainStore


updateWithChangeFn :
    (Grain -> Grain)
    -> GrainId
    -> GrainStore
    -> UpdateResult
updateWithChangeFn changeFn gid model =
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


setPersisted : Grain -> GrainStore -> GrainStore
setPersisted grain =
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


batchUpdate : List GrainUpdater -> GrainStore -> UpdateResult
batchUpdate list model =
    let
        reducer ( changeFn, gid ) =
            Result.andThen (updateWithChangeFn changeFn gid)
    in
    List.foldl reducer (Result.Ok model) list


updateWithSetMsg :
    Grain.Set
    -> GrainId
    -> Posix
    -> GrainStore
    -> UpdateResult
updateWithSetMsg grainUpdate gid now model =
    updateWithChangeFn (Grain.update now grainUpdate) gid model


move :
    Direction
    -> GrainId
    -> Posix
    -> GrainStore
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
    -> GrainStore
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
            updateWithSetMsg
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
                        updateWithSetMsg
                            (Grain.SetParentId pid)
                            gid
                            now
                            model
                   )
            )
        |> Maybe.withDefault (Result.Err "Grain Not Found")
