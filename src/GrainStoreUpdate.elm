module GrainStoreUpdate exposing (Msg(..), update)

import Grain exposing (Grain)
import GrainId exposing (GrainId)
import GrainStore


type Msg
    = AddAfter GrainId Grain
    | AddBefore GrainId Grain


update msg grainStore =
    case msg of
        AddBefore gid grain ->
            GrainStore.addNewBefore gid
                grain
                grainStore

        AddAfter gid grain ->
            GrainStore.addNewAfter gid
                grain
                grainStore
