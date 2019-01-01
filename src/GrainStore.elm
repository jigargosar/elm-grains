module GrainStore exposing
    ( Add(..)
    , GrainForest
    , GrainStore
    , GrainTree
    , GrainZipper
    , Msg(..)
    , Update(..)
    , decoder
    , encoder
    , get
    , getSavedGrain
    , init
    , rejectSubTreeAndFlatten
    , removeSavedGrain
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
import GrainZipper__ exposing (GrainForest, GrainTree)
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
import Tree.Zipper as Z exposing (Zipper)
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


rootGrain : GrainStore -> Grain
rootGrain =
    get GrainId.root >> Maybe.withDefault Grain.root


toTree : GrainStore -> GrainTree
toTree model =
    toTreeHelp model (rootGrain model)


toTreeHelp : GrainStore -> Grain -> GrainTree
toTreeHelp grainStore grain =
    let
        newForest : GrainForest
        newForest =
            childGrains grain grainStore
                |> List.map (toTreeHelp grainStore)
    in
    Tree.tree grain newForest


type alias GrainForest =
    List GrainTree


type alias GrainTree =
    Tree.Tree Grain


type alias GrainZipper =
    Zipper Grain


toZipper : GrainStore -> GrainZipper
toZipper =
    toTree >> Z.fromTree


findFromRoot gid =
    toZipper >> Z.findFromRoot (Grain.idEq gid)


treeFromGid : GrainId -> GrainStore -> Maybe GrainTree
treeFromGid gid =
    findFromRoot gid >> Maybe.map Z.tree


rejectSubTreeAndFlatten : Grain -> GrainStore -> List Grain
rejectSubTreeAndFlatten grain model =
    []


get : GrainId -> GrainStore -> Maybe Grain
get gid =
    GrainIdLookup.get gid >> Maybe.map SavedGrain.value


getSavedGrain : GrainId -> GrainStore -> Maybe SavedGrain
getSavedGrain gid =
    GrainIdLookup.get gid


removeSavedGrain : SavedGrain -> GrainStore -> GrainStore
removeSavedGrain savedGrain =
    remove (SavedGrain.value savedGrain)


type Add
    = AddAfter GrainId
    | AddBefore GrainId
    | AddChild GrainId
    | AddDefault


z_prependChild childTree =
    Z.mapTree (Tree.prependChild childTree)
        >> Z.firstChild


addNew : Add -> Grain -> GrainStore -> UpdateResult
addNew msg newGrain model =
    let
        newGrainTree =
            Tree.singleton newGrain

        addWithZipper =
            case msg of
                AddAfter gid ->
                    findFromRoot gid
                        >> Maybe.map (Z.append newGrainTree)

                AddBefore gid ->
                    findFromRoot gid
                        >> Maybe.map (Z.append newGrainTree)

                AddChild gid ->
                    findFromRoot gid
                        >> Maybe.andThen (z_prependChild newGrainTree)

                AddDefault ->
                    toZipper
                        >> z_prependChild newGrainTree

        newGid =
            Grain.id newGrain
    in
    if idExists newGid model then
        Result.Err "Error: Add Grain. GrainId exists"

    else
        let
            now =
                Grain.createdAt newGrain
        in
        model
            |> addWithZipper
            |> Maybe.unwrap
                (Result.Err "Err: addNewGrainAfter")
                (Z.tree >> addGrainWithParentTree now newGrain >> callWith model)


addGrainWithParentTree now newGrain tree =
    let
        newGid =
            Grain.id newGrain

        parentIdUpdater =
            Tree.label tree
                |> Grain.idAsParentId
                |> (\pid ->
                        ( Grain.update now (Grain.SetParentId pid)
                        , newGid
                        )
                   )

        childUpdaters =
            Tree.children tree
                |> List.map Tree.label
                |> List.indexedMap
                    (\idx g ->
                        ( Grain.update now <| Grain.SetSortIdx idx
                        , Grain.id g
                        )
                    )

        updaters =
            parentIdUpdater :: childUpdaters
    in
    blindInsertGrain newGrain
        >> batchUpdate updaters


type Update
    = Move Direction
    | SetContent String
    | SetDeleted Bool
    | SetParentId ParentId


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
                model

        UpdateGrain msg gid now ->
            updateGrain msg
                gid
                now
                model

        FirebaseChanges changeList ->
            updateFromFirebaseChangeList changeList
                model

        Load encoded ->
            load encoded


updateGrain msg gid now =
    case msg of
        Move direction ->
            move direction gid now

        SetContent val ->
            updateWithSetMsg (Grain.SetContent val) gid now

        SetDeleted val ->
            updateWithSetMsg (Grain.SetDeleted val) gid now

        SetParentId val ->
            updateWithSetMsg (Grain.SetParentId val) gid now



--        SetSortIdx val ->
--            updateWithSetMsg (Grain.SetSortIdx val) gid now
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
        siblings : List Grain
        siblings =
            findFromRoot gid model
                |> Maybe.unwrap []
                    (Z.tree >> Tree.children >> List.map Tree.label)

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
        setParentId newParentId =
            updateWithSetMsg
                (Grain.SetParentId newParentId)
                gid
                now
                model
    in
    findFromRoot gid model
        |> Maybe.andThen Z.parent
        |> Maybe.map (Z.label >> Grain.parentId >> setParentId)
        |> Maybe.withDefault (Result.Err "Grain Not Found")


moveOneLevelDown gid now model =
    let
        siblings : List Grain
        siblings =
            findFromRoot gid model
                |> Maybe.unwrap []
                    (Z.tree >> Tree.children >> List.map Tree.label)

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
