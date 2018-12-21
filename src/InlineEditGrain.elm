module InlineEditGrain exposing
    ( InlineEditGrain
    , endEditing
    , initialValue
    , maybeContentFor
    , onContentChange
    , startEditing
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


startEditing grain =
    init (Grain.id grain) (Grain.content grain)


onContentChange content model =
    case model of
        NotEditing ->
            Result.Err "Error: onContentChange. Not Editing"

        Editing editModel ->
            Result.Ok <| Editing { editModel | content = content }


endEditing model =
    case model of
        NotEditing ->
            Result.Err "Error: endEditing. Not Editing"

        Editing { gid, content } ->
            Result.Ok <| ( gid, content, initialValue )


maybeContentFor grain model =
    case model of
        NotEditing ->
            Nothing

        Editing { gid, content } ->
            if Grain.idEq gid grain then
                Just content

            else
                Nothing
