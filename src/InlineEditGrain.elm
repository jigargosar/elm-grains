module InlineEditGrain exposing
    ( InlineEditGrain
    , edit
    , maybeContentFor
    , maybeGid
    , none
    )

import BasicsX exposing (callWith)
import Grain
import GrainId exposing (GrainId)
import Maybe.Extra as Maybe


type alias EditingModel =
    { gid : GrainId
    , content : String
    }


type InlineEditGrain
    = Editing EditingModel
    | NotEditing


none =
    NotEditing


edit grain =
    Editing
        { gid = Grain.id grain
        , content = Grain.content grain
        }


maybeGid model =
    case model of
        NotEditing ->
            Nothing

        Editing { gid } ->
            Just gid



--isEditing grain =
--    maybeGid >> Maybe.unwrap False (Grain.idEq >> callWith grain)


maybeContentFor grain model =
    case model of
        NotEditing ->
            Nothing

        Editing { gid, content } ->
            if Grain.idEq gid grain then
                Just content

            else
                Nothing
