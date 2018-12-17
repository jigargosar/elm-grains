module FireGrain exposing (FireGrain, decoder, encoder, idString, latest, new, update)

import DecodeX
import Grain exposing (Grain)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E exposing (Value)


type FireGrain
    = Clean { base : Grain }
    | Dirty { base : Grain, dirty : Grain }
    | SaveRequestSent { base : Grain, dirty : Grain }
    | DirtyAfterSaveRequestSent { base : Grain, saveRequestSentFor : Grain, dirty : Grain }


new grain =
    Clean { base = grain }


idString =
    latest >> Grain.idString


latest model =
    case model of
        Clean { base } ->
            base

        Dirty { dirty } ->
            dirty

        SaveRequestSent { dirty } ->
            dirty

        DirtyAfterSaveRequestSent { dirty } ->
            dirty


update fn model =
    case model of
        Clean { base } ->
            Dirty { base = base, dirty = fn base }

        Dirty rec ->
            Dirty <| setDirty (fn rec.dirty) rec

        SaveRequestSent { base, dirty } ->
            DirtyAfterSaveRequestSent
                { base = base
                , saveRequestSentFor = dirty
                , dirty = fn dirty
                }

        DirtyAfterSaveRequestSent rec ->
            DirtyAfterSaveRequestSent <| setDirty (fn rec.dirty) rec


setDirty newDirty partialRecord =
    { partialRecord | dirty = newDirty }


encoder : FireGrain -> Value
encoder model =
    case model of
        Clean { base } ->
            E.object
                [ ( "status", E.string "Clean" )
                , ( "base", Grain.encoder base )
                ]

        Dirty record ->
            dirtyEncoder record

        SaveRequestSent record ->
            dirtyEncoder record

        DirtyAfterSaveRequestSent record ->
            dirtyEncoder record


dirtyEncoder { base, dirty } =
    E.object
        [ ( "status", E.string "Dirty" )
        , ( "base", Grain.encoder base )
        , ( "dirty", Grain.encoder dirty )
        ]


decoder =
    D.field "status" D.string
        |> D.andThen decoderWithStatus


decoderWithStatus : String -> Decoder FireGrain
decoderWithStatus status =
    case status of
        "Clean" ->
            D.field "base" Grain.decoder
                |> D.map
                    (\base ->
                        Clean { base = base }
                    )

        "Dirty" ->
            DecodeX.start (\base dirty -> Dirty { base = base, dirty = dirty })
                |> required "base" Grain.decoder
                |> required "dirty" Grain.decoder

        _ ->
            D.fail ("Invalid status: " ++ status)
