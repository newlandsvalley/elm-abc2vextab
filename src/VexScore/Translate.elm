module VexScore.Translate exposing (translate)

{-|

@docs translate
-}

import Abc.ParseTree exposing (..)
import Music.Notation exposing (getHeaderMap)
import VexScore.Score exposing (..)
import Dict exposing (Dict, get)
import Ratio exposing (Rational, over)


type alias Context =
    { modifiedKeySig : ModifiedKeySignature
    , meter : MeterSignature
    , unitNoteLength : NoteDuration
    }


{-| translate an ABC tune to a VexTab Score representation
-}
translate : AbcTune -> Score
translate t =
    let
        ctx =
            initialContext t
    in
        tuneBody ctx (snd t)
            |> fst



{- translate the tune body -}


tuneBody : Context -> TuneBody -> ( Score, Context )
tuneBody ctx tb =
    let
        -- append via the pair (we really need a monad here.....)
        apnd : ( VexLine, Context ) -> ( List VexLine, Context ) -> ( List VexLine, Context )
        apnd vlc vlcs =
            let
                newvls =
                    fst vlc :: fst vlcs
            in
                ( newvls, snd vlc )

        f bp acc =
            apnd (bodyPart ctx bp) acc
    in
        List.foldl f ( [], ctx ) tb


bodyPart : Context -> BodyPart -> ( VexLine, Context )
bodyPart ctx bp =
    let
        mKey =
            Just (fst ctx.modifiedKeySig)

        vexStave =
            { clef = Treble, mKey = mKey, mMeter = Just ctx.meter }

        vexLine =
            { stave = vexStave, items = [] }
    in
        ( vexLine, ctx )



{- get the key signature defaulted to C Major -}


getKeySig : Maybe Header -> ModifiedKeySignature
getKeySig mkh =
    let
        cMajor : ModifiedKeySignature
        cMajor =
            ( { pitchClass = C, accidental = Nothing, mode = Major }, [] )
    in
        case mkh of
            Just kh ->
                case kh of
                    Key mks ->
                        mks

                    _ ->
                        cMajor

            _ ->
                cMajor



{- get the meter defaulted to 4 4 -}


getMeter : Maybe Header -> MeterSignature
getMeter mmh =
    case mmh of
        Just mh ->
            case mh of
                Meter (Just ms) ->
                    ms

                _ ->
                    ( 4, 4 )

        _ ->
            ( 4, 4 )



{- get the unit note length defaulted to 1/8 -}


unitNoteLen : Maybe Header -> NoteDuration
unitNoteLen muh =
    case muh of
        Just uh ->
            case uh of
                UnitNoteLength l ->
                    l

                _ ->
                    over 1 8

        _ ->
            over 1 8



{- get the initial translation context from the tune headers -}


initialContext : AbcTune -> Context
initialContext t =
    let
        headerMap =
            getHeaderMap t

        keySig =
            Dict.get 'K' headerMap
                |> getKeySig

        meter =
            Dict.get 'M' headerMap
                |> getMeter

        unl =
            Dict.get 'L' headerMap
                |> unitNoteLen
    in
        { modifiedKeySig = keySig
        , meter = meter
        , unitNoteLength = unl
        }
