module GrainStore exposing
    ( Add(..)
    , GrainStore
    , Msg(..)
    , Update(..)
    , decoder
    , encoder
    , get
    , getGrain
    , init
    , toRawList
    , toTree
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
import GrainTree exposing (GrainZipper)
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
import Time exposing (Posix)
import Tree
import Tree.Zipper as Z exposing (Zipper)
import Tuple2


type alias GrainStore =
    GrainIdLookup Grain


decoder : Decoder GrainStore
decoder =
    D.list Grain.decoder
        |> D.map
            (GrainIdLookup.fromList Grain.id
                >> addRootIfAbsent
            )


encoder : GrainStore -> Value
encoder =
    GrainIdLookup.toList >> E.list Grain.encoder


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


findFromRoot gid =
    toTree >> Z.fromTree >> GrainTree.findFromRoot gid


get : GrainId -> GrainStore -> Maybe Grain
get gid =
    GrainIdLookup.get gid


getGrain : GrainId -> GrainStore -> Maybe Grain
getGrain gid =
    GrainIdLookup.get gid


type Add
    = AddAfter GrainId
    | AddBefore GrainId
    | AddChild GrainId


getLCRSiblingsOfGid :
    GrainId
    -> GrainStore
    -> Maybe ( List Grain, Grain, List Grain )
getLCRSiblingsOfGid gid model =
    get gid model
        |> Maybe.andThen
            (\grain ->
                let
                    siblingGrains =
                        GrainIdLookup.toList model
                            |> List.filter (Grain.isSibling grain)
                            |> List.sortWith Grain.defaultComparator
                in
                List.elemIndex grain siblingGrains
                    |> Maybe.map
                        (\siblingIdx ->
                            ( List.take siblingIdx siblingGrains
                            , grain
                            , List.drop (siblingIdx + 1) siblingGrains
                            )
                        )
            )


getSortedChildGrainsOfGid : GrainId -> GrainStore -> Maybe ( Grain, List Grain )
getSortedChildGrainsOfGid gid model =
    get gid model
        |> Maybe.map
            (\parentGrain ->
                let
                    children =
                        GrainIdLookup.toList model
                            |> List.filter (Grain.isChildOf parentGrain)
                            |> List.sortWith Grain.defaultComparator
                in
                ( parentGrain, children )
            )


addNew : Add -> Grain -> GrainStore -> UpdateResult
addNew msg newGrain model =
    let
        newGid =
            Grain.id newGrain
    in
    if idExists newGid model then
        Result.Err "Error: Add Grain. GrainId exists"

    else
        model
            |> addAndGetPidAndChildren msg newGrain
            |> Maybe.unwrap
                (Result.Err "Err: addNew")
                (updateWithPidAndChildren newGrain >> callWith model)


addAndGetPidAndChildren msg newGrain =
    case msg of
        AddAfter gid ->
            getLCRSiblingsOfGid gid
                >> Maybe.map
                    (\( l, c, r ) ->
                        ( Grain.parentId c, l ++ [ c, newGrain ] ++ r )
                    )

        AddBefore gid ->
            getLCRSiblingsOfGid gid
                >> Maybe.map
                    (\( l, c, r ) ->
                        ( Grain.parentId c, l ++ [ newGrain, c ] ++ r )
                    )

        AddChild gid ->
            getSortedChildGrainsOfGid gid
                >> Maybe.map
                    (\( p, c ) ->
                        ( Grain.idAsParentId p, newGrain :: c )
                    )


updateWithPidAndChildren newGrain ( pid, children ) =
    let
        now =
            Grain.createdAt newGrain

        newGid =
            Grain.id newGrain

        parentIdUpdater =
            ( Grain.update now (Grain.SetParentId pid)
            , newGid
            )

        childUpdaters =
            children
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
                    blindInsertGrain grain

                GrainChange.Modified ->
                    blindInsertGrain grain

                GrainChange.Removed ->
                    blindRemoveGrain grain
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
        Result.Ok <| GrainIdLookup.updateIfExists gid changeFn model

    else
        Result.Err "GrainNotFound"



-- UPDATE HELPERS


blindInsertGrain grain model =
    GrainIdLookup.insert (Grain.id grain) grain model


blindRemoveGrain grain model =
    GrainIdLookup.insert (Grain.id grain) grain model


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
