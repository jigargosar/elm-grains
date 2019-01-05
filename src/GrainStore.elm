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


getSiblingsAndSortIdxOfGid : GrainId -> GrainStore -> Maybe ( Int, List Grain )
getSiblingsAndSortIdxOfGid gid model =
    if GrainId.root == gid then
        Nothing

    else
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
                        |> Maybe.map (\idx -> ( idx, siblingGrains ))
                )



--getSiblingsAndSortIdxOfParentOfGid : GrainId -> GrainStore -> Maybe ( Int, List Grain )
--getSiblingsAndSortIdxOfParentOfGid gid model =
--    get gid model
--        |> Maybe.andThen Grain.parentIdAsGrainId
--        |> Maybe.andThen (getSiblingsAndSortIdxOfGid >> callWith model)
--
--
--getLCRSiblingsOfParentOfGid :
--    GrainId
--    -> GrainStore
--    -> Maybe ( List Grain, Grain, List Grain )
--getLCRSiblingsOfParentOfGid gid model =
--    get gid model
--        |> Maybe.andThen Grain.parentIdAsGrainId
--        |> Maybe.andThen (getLCRSiblingsOfGid >> callWith model)


getLCRSiblingsOfGid :
    GrainId
    -> GrainStore
    -> Maybe ( List Grain, Grain, List Grain )
getLCRSiblingsOfGid gid model =
    if GrainId.root == gid then
        Nothing

    else
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
            (getSortedChildGrainsOfGrain >> callWith model)


getSortedChildGrainsOfGrain : Grain -> GrainStore -> ( Grain, List Grain )
getSortedChildGrainsOfGrain parentGrain model =
    let
        children =
            GrainIdLookup.toList model
                |> List.filter (Grain.isChildOf parentGrain)
                |> List.sortWith Grain.defaultComparator
    in
    ( parentGrain, children )


addNew : Add -> Grain -> GrainStore -> UpdateResult
addNew msg newGrain model =
    let
        newGid =
            Grain.id newGrain
    in
    if idExists newGid model then
        Result.Err "Error: Add Grain. GrainId exists"

    else
        let
            addAndGetPidAndChildren =
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

            toSetMsgUpdaters ( pid, children ) =
                ( Grain.SetParentId pid
                , newGrain
                )
                    :: grainListToSetSortIdxSetter children

            blindInsertAndUpdate setMsgUpdaters =
                let
                    now =
                        Grain.createdAt newGrain
                in
                blindInsertGrain newGrain
                    >> batchUpdateWithSetMessages setMsgUpdaters now
        in
        model
            |> addAndGetPidAndChildren
            |> Maybe.map
                (toSetMsgUpdaters
                    >> blindInsertAndUpdate
                    >> callWith model
                )
            |> Result.fromMaybe "Err: addNew"


grainListToSetSortIdxSetter : List Grain -> List GrainSetter
grainListToSetSortIdxSetter =
    List.indexedMap
        (\idx grain ->
            ( Grain.SetSortIdx idx
            , grain
            )
        )


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
            updateGrainWithId msg
                gid
                now
                model

        FirebaseChanges changeList ->
            updateFromFirebaseChangeList changeList
                model

        Load encoded ->
            load encoded


updateGrainWithId msg gid now model =
    let
        toGrainSetterSingletonResult setMsg =
            get gid
                >> Maybe.map
                    (\grain ->
                        [ ( setMsg, grain ) ]
                    )
                >> Result.fromMaybe "Err: updateGrainWithId: toGrainSetterSingleton"

        grainSettersResult =
            case msg of
                Move direction ->
                    move direction gid

                SetContent val ->
                    toGrainSetterSingletonResult (Grain.SetContent val)

                SetDeleted val ->
                    toGrainSetterSingletonResult (Grain.SetDeleted val)

                SetParentId val ->
                    toGrainSetterSingletonResult (Grain.SetParentId val)
    in
    grainSettersResult model
        |> Result.map
            (batchUpdateWithSetMessages
                >> callWith2 now model
            )


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


type alias UpdateResult =
    Result String GrainStore


type alias GrainSetterResult =
    Result String (List ( Grain.Set, Grain ))


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


batchUpdate_ : List GrainUpdater -> GrainStore -> UpdateResult
batchUpdate_ list model =
    let
        reducer ( changeFn, gid ) =
            Result.andThen (updateWithChangeFn changeFn gid)
    in
    List.foldl reducer (Result.Ok model) list


type alias GrainSetter =
    ( Grain.Set, Grain )


batchUpdateWithSetMessages :
    List GrainSetter
    -> Posix
    -> GrainStore
    -> GrainStore
batchUpdateWithSetMessages grainSetters now model =
    let
        reducer =
            blindInsertGrain << Tuple2.uncurry (Grain.update now)
    in
    List.foldl reducer model grainSetters


move :
    Direction
    -> GrainId
    -> GrainStore
    -> GrainSetterResult
move direction gid model =
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
    fn gid model


moveBy :
    Int
    -> GrainId
    -> GrainStore
    -> GrainSetterResult
moveBy offset gid model =
    getSiblingsAndSortIdxOfGid gid model
        |> Maybe.map
            (\( idx, siblings ) ->
                List.swapAt idx (idx + offset) siblings
                    |> grainListToSetSortIdxSetter
            )
        |> Result.fromMaybe "Error: moveBy"


moveOneLevelUp gid model =
    get gid model
        |> Maybe.andThen
            (\grain ->
                Grain.parentIdAsGrainId grain
                    |> Maybe.andThen (getLCRSiblingsOfGid >> callWith model)
                    |> Maybe.map
                        (\( l, parentGrain, r ) ->
                            ( Grain.SetParentId
                                (Grain.parentId parentGrain)
                            , grain
                            )
                                :: (l
                                        ++ [ parentGrain, grain ]
                                        ++ r
                                        |> grainListToSetSortIdxSetter
                                   )
                        )
            )
        |> Result.fromMaybe "Error: moveOneLevelUp"


moveOneLevelDown gid model =
    getLCRSiblingsOfGid gid model
        |> Maybe.andThen
            (\( prevSiblings, grain, _ ) ->
                List.last prevSiblings
                    |> Maybe.map
                        (getSortedChildGrainsOfGrain >> callWith model)
                    |> Maybe.map
                        (\( newParent, newSiblings ) ->
                            ( Grain.SetParentId
                                (Grain.parentId newParent)
                            , grain
                            )
                                :: (grain
                                        :: newSiblings
                                        |> grainListToSetSortIdxSetter
                                   )
                        )
            )
        |> Result.fromMaybe "Error: moveOneLevelDown"
