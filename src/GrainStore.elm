module GrainStore exposing
    ( Add(..)
    , GrainStore
    , Msg(..)
    , Update(..)
    , decoder
    , encoder
    , get
    , init
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



--- TO TREE ---


toTree : GrainStore -> GrainTree
toTree model =
    toTreeHelp model (rootGrain model)


rootGrain : GrainStore -> Grain
rootGrain =
    get GrainId.root >> Maybe.withDefault Grain.root


allSortedGrains : GrainStore -> List Grain
allSortedGrains =
    GrainIdLookup.toList
        >> List.sortWith Grain.defaultComparator


childGrains : Grain -> GrainStore -> List Grain
childGrains grain =
    allSortedGrains >> List.filter (Grain.isChildOf grain)


toTreeHelp : GrainStore -> Grain -> GrainTree
toTreeHelp grainStore grain =
    let
        newForest : GrainForest
        newForest =
            childGrains grain grainStore
                |> List.map (toTreeHelp grainStore)
    in
    Tree.tree grain newForest



--- END TO TREE


get : GrainId -> GrainStore -> Maybe Grain
get gid =
    GrainIdLookup.get gid



-- UPDATE


type Add
    = AddAfter GrainId
    | AddBefore GrainId
    | AddChild GrainId


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
                        getSortedLCRSiblingsOfGid gid
                            >> Maybe.map
                                (\( l, c, r ) ->
                                    ( Grain.parentId c, l ++ [ c, newGrain ] ++ r )
                                )

                    AddBefore gid ->
                        getSortedLCRSiblingsOfGid gid
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
            encoded
                |> D.decodeValue decoder
                >> Result.mapError D.errorToString


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
                    moveLeft

                Direction.Right ->
                    moveRight
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


moveLeft gid model =
    get gid model
        |> Maybe.andThen
            (\grain ->
                Grain.parentIdAsGrainId grain
                    |> Maybe.andThen (getSortedLCRSiblingsOfGid >> callWith model)
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


moveRight gid model =
    getSortedLCRSiblingsOfGid gid model
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



-- INTERNAL UPDATE


type alias UpdateResult =
    Result String GrainStore


type alias GrainSetterResult =
    Result String (List ( Grain.Set, Grain ))


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


blindInsertGrain grain model =
    GrainIdLookup.insert (Grain.id grain) grain model


blindRemoveGrain grain model =
    GrainIdLookup.remove (Grain.id grain) model



--- INTERNAL GETTERS


idExists gid =
    GrainIdLookup.member gid


grainListToSetSortIdxSetter : List Grain -> List GrainSetter
grainListToSetSortIdxSetter =
    List.indexedMap
        (\idx grain ->
            ( Grain.SetSortIdx idx
            , grain
            )
        )


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


getSortedLCRSiblingsOfGid :
    GrainId
    -> GrainStore
    -> Maybe ( List Grain, Grain, List Grain )
getSortedLCRSiblingsOfGid gid model =
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
