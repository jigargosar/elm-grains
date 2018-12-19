module InlineEditGrain exposing
    ( InlineEditGrain
    , initFor
    , initialValue
    , maybeContentFor
    , maybeGid
    , onContentChange
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


init gid content =
    Editing
        { gid = gid
        , content = content
        }


initialValue =
    NotEditing


initFor grain =
    init (Grain.id grain) (Grain.content grain)


onContentChange content model =
    case model of
        NotEditing ->
            Result.Err "Error: onContentChange. Not Editing"

        Editing editModel ->
            Result.Ok <| Editing { editModel | content = content }


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
