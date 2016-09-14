module VexScore.Translate exposing (translate)

{-|

@docs translate
-}

import Abc.ParseTree exposing (..)
import Abc.Canonical as AbcText
import Music.Notation exposing (getHeaderMap)
import VexScore.Score exposing (..)
import Dict exposing (Dict, get)
import Result exposing (Result)
import Ratio exposing (Rational, over, numerator, denominator)
import Debug exposing (log)


type alias Context =
    { modifiedKeySig : ModifiedKeySignature
    , meter : MeterSignature
    , unitNoteLength : NoteDuration
    , notesContext :
        Bool
        -- are we within the context of translating note sequences
    }


{-| translate an ABC tune to a VexTab Score representation
-}
translate : AbcTune -> Result String Score
translate t =
    let
        ctx =
            initialContext t

        ksmod =
            snd ctx.modifiedKeySig
    in
        let
            result =
                tuneBody ctx (snd t)
        in
            if (List.isEmpty ksmod) then
                case result of
                    Ok sc ->
                        Ok (fst sc)

                    Err e ->
                        Err e
            else
                Err "modified key signatures not supported"



{- translate the tune body -}


tuneBody : Context -> TuneBody -> Result String ( Score, Context )
tuneBody ctx tb =
    foldOverResult ctx tb bodyPart



{-
   tuneBody ctx tb =
       let
           -- append via the pair (we really need a monad here.....)
           apnd : Result String ( VexLine, Context ) -> Result String ( List VexLine, Context ) -> Result String ( List VexLine, Context )
           apnd rvlc rvlcs =
               case ( rvlc, rvlcs ) of
                   ( Ok vlc, Ok vlcs ) ->
                       let
                           newvls =
                               fst vlc :: fst vlcs
                       in
                           Ok ( newvls, snd vlc )

                   ( _, Err acc ) ->
                       Err acc

                   ( Err next, _ ) ->
                       Err next

           f bp acc =
               apnd (bodyPart ctx bp) acc
       in
           List.foldl f (Ok ( [], ctx )) tb
-}


bodyPart : Context -> BodyPart -> Result String ( VexLine, Context )
bodyPart ctx bp =
    let
        mKey =
            Just (fst ctx.modifiedKeySig)

        vexStave =
            { clef = Treble, mKey = mKey, mMeter = Just ctx.meter }

        vexLine =
            { stave = vexStave, items = [] }

        -- new stave so not within a notes context
        staveCtx =
            setNotesContext False ctx
    in
        case bp of
            Score line ->
                let
                    itemsRes =
                        musicLine staveCtx line
                in
                    case itemsRes of
                        Ok ( items, newCtx ) ->
                            Ok ( { stave = vexStave, items = items }, newCtx )

                        Err e ->
                            Err e

            BodyInfo header ->
                -- not yet implemented
                Ok ( vexLine, ctx )


musicLine : Context -> MusicLine -> Result String ( List VexItem, Context )
musicLine ctx ml =
    foldOverResult ctx ml music



{-
   musicLine ctx ml =
       let
           -- append via the pair (we really need a monad here.....)
           apnd : Result String ( VexItem, Context ) -> Result String ( List VexItem, Context ) -> Result String ( List VexItem, Context )
           apnd rvic rvics =
               case ( rvic, rvics ) of
                   ( Ok vic, Ok vics ) ->
                       let
                           newvis =
                               fst vic :: fst vics

                           _ =
                               log "acc ctx, new ctx" ( (snd vics).notesContext, (snd vic).notesContext )
                       in
                           Ok ( newvis, snd vic )

                   ( _, Err acc ) ->
                       Err acc

                   ( Err next, _ ) ->
                       Err next

           f mus acc =
               let
                   applicableCtx =
                       case acc of
                           Ok ( _, accCtx ) ->
                               accCtx

                           _ ->
                               ctx
               in
                   apnd (music applicableCtx mus) acc
       in
           let
               result =
                   List.foldl f (Ok ( [], ctx )) ml
           in
               case result of
                   Ok ( vis, ctx ) ->
                       Ok ( List.reverse vis, ctx )

                   _ ->
                       result
-}


music : Context -> Music -> Result String ( VexItem, Context )
music ctx m =
    case m of
        Barline bar ->
            Ok ( VBar, ctx )

        Note abcNote ->
            let
                -- look after the generation of a 'notes' keyword for a new group
                newNoteGroup =
                    not (ctx.notesContext)

                newCtx =
                    if newNoteGroup then
                        setNotesContext newNoteGroup ctx
                    else
                        ctx

                noteDurResult =
                    noteDur ctx abcNote.duration

                -- _ = log "pc existing new" ( abcNote.pitchClass, ctx.notesContext, newCtx.notesContext )
            in
                case noteDurResult of
                    Ok d ->
                        let
                            vexNote =
                                { pitchClass = abcNote.pitchClass
                                , accidental = abcNote.accidental
                                , octave = abcNote.octave - 1
                                , duration =
                                    d
                                    -- not implemented yet
                                , tied = abcNote.tied
                                }
                        in
                            Ok ( VNote vexNote newNoteGroup, newCtx )

                    Err e ->
                        Err ("Note " ++ e ++ ": " ++ (AbcText.abcNote abcNote))

        Rest duration ->
            let
                noteDurResult =
                    noteDur ctx duration
            in
                case noteDurResult of
                    Ok d ->
                        Ok ( VRest d, ctx )

                    Err e ->
                        Err ("Rest " ++ e ++ ": " ++ ("rest"))

        _ ->
            Ok ( VUnimplemented, ctx )



{- translate a note or rest duration, wrapping in a Result which will be
   in error if we can't quantise the duration
-}


noteDur : Context -> NoteDuration -> Result String VexNoteDuration
noteDur ctx d =
    let
        numer =
            numerator ctx.unitNoteLength
                * (numerator d)
                * 64

        denom =
            denominator ctx.unitNoteLength
                * (denominator d)

        -- replace this with precise arithmetic?
        durn =
            numer // denom
    in
        case durn of
            96 ->
                Ok WholeDotted

            64 ->
                Ok Whole

            48 ->
                Ok HalfDotted

            32 ->
                Ok Half

            24 ->
                Ok QuarterDotted

            16 ->
                Ok Quarter

            12 ->
                Ok EighthDotted

            8 ->
                Ok Eighth

            6 ->
                Ok SixteenthDotted

            4 ->
                Ok Sixteenth

            3 ->
                Ok ThirtySecondDotted

            2 ->
                Ok ThirtySecond

            _ ->
                Err "too long or too dotted"



{- keep track of whether we're needing to translate notes or not within the state -
   we need to generate the 'notes' keyword to introduce them
-}


setNotesContext : Bool -> Context -> Context
setNotesContext b ctx =
    { ctx | notesContext = b }



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
        , notesContext = False
        }



-- Helper Functions
{- This is a generic function that operates where we start with a list in ABC and need to end up with the
   equivalent list in VexTab Score.  It performs a left fold over the list using the next function in the tree
   that we need to use in the fold.  It threads the context through the fold.  Because it's a left fold
   then we need to reverse the list in the result when we finish

-}


foldOverResult : Context -> List a -> (Context -> a -> Result String ( b, Context )) -> Result String ( List b, Context )
foldOverResult ctx aline fmus =
    let
        -- append via the pair through the result (we really need a monad here.....)
        apnd : Result String ( b, Context ) -> Result String ( List b, Context ) -> Result String ( List b, Context )
        apnd rvic rvics =
            case ( rvic, rvics ) of
                ( Ok vic, Ok vics ) ->
                    let
                        newvis =
                            fst vic :: fst vics

                        --  _ = log "acc ctx, new ctx" ( (snd vics).notesContext, (snd vic).notesContext )
                    in
                        Ok ( newvis, snd vic )

                ( _, Err acc ) ->
                    Err acc

                ( Err next, _ ) ->
                    Err next

        -- thread the context through the fold
        f mus acc =
            let
                applicableCtx =
                    case acc of
                        Ok ( _, accCtx ) ->
                            accCtx

                        _ ->
                            ctx
            in
                -- fmus is the next function in the tree to apply in the fold
                apnd (fmus applicableCtx mus) acc
    in
        let
            result =
                List.foldl f (Ok ( [], ctx )) aline
        in
            -- we have done a left fold so we need to reverse the result
            case result of
                Ok ( vis, ctx ) ->
                    Ok ( List.reverse vis, ctx )

                _ ->
                    result
