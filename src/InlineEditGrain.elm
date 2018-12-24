module InlineEditGrain exposing
    ( InlineEditGrain
    , discard
    , endEditing
    , initialValue
    , maybeContentFor
    , onContentChange
    , onKeyboardFocusChange
    , startEditing
    )

import BasicsX exposing (callWith)
import Grain
import GrainId exposing (GrainId)
import Maybe.Extra as Maybe


type alias EditingModel =
    { gid : GrainId
    , content : String
    , hasKeyboardFocus : Bool
    }


type InlineEditGrain
    = Editing EditingModel
    | NotEditing


init gid content =
    Editing
        { gid = gid
        , content = content
        , hasKeyboardFocus = False
        }


initialValue =
    NotEditing


startEditing grain =
    init (Grain.id grain) (Grain.content grain)


onKeyboardFocusChange hasFocus model =
    case model of
        NotEditing ->
            if hasFocus then
                Result.Err "Error: onFocusChange. Not Editing"

            else
                Result.Ok model

        Editing editModel ->
            Result.Ok <| Editing { editModel | hasKeyboardFocus = hasFocus }


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


maybeContentFor forGrainId model =
    case model of
        NotEditing ->
            Nothing

        Editing { gid, content } ->
            if gid == forGrainId then
                Just content

            else
                Nothing


discard : InlineEditGrain -> Result String InlineEditGrain
discard model =
    case model of
        Editing { gid, content } ->
            Result.Ok initialValue

        NotEditing ->
            Result.Err "Error: endEditing. Not Editing"
