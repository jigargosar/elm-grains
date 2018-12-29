module GrainBuilder exposing (GrainBuilder, continue, init)

import Either exposing (Either)
import Grain exposing (Grain)
import Random
import Task
import Time exposing (Posix)


type Step
    = Step2_GenerateGrain Posix
    | Step3_Complete Grain


type GrainBuilder ctx
    = GrainBuilder ctx Step


init :
    ctx
    -> (GrainBuilder ctx -> msg)
    -> Cmd msg
init ctx toMessage =
    Time.now
        |> Task.map
            (Step2_GenerateGrain
                >> GrainBuilder ctx
            )
        |> Task.perform toMessage


continue :
    (GrainBuilder ctx -> msg)
    -> GrainBuilder ctx
    -> Either (Cmd msg) ( ctx, Grain )
continue toMessage (GrainBuilder ctx step) =
    case step of
        Step2_GenerateGrain now ->
            Grain.generator now
                |> Random.generate
                    (Step3_Complete >> GrainBuilder ctx >> toMessage)
                |> Either.Left

        Step3_Complete grain ->
            Either.Right ( ctx, grain )
