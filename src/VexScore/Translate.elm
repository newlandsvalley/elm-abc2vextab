module VexScore.Translate exposing (translate, translateText)

{-|

@docs translate, translateText
-}

import Abc.ParseTree exposing (..)
import Abc.Canonical as AbcText
import Abc exposing (parse, parseError)
import Music.Notation exposing (getHeaderMap, dotFactor)
import VexScore.Score exposing (..)
import Dict exposing (Dict, get)
import Result exposing (Result)
import Maybe exposing (withDefault)
import Ratio exposing (Rational, over, numerator, denominator, multiply)
import Debug exposing (log)


type alias Context =
    { modifiedKeySig : ModifiedKeySignature
    , meter : MeterSignature
    , unitNoteLength : NoteDuration
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


{-| translate ABC text to a VexTab Score representation
-}
translateText : String -> Result String Score
translateText s =
    let
        parseResult =
            parse s
    in
        case parseResult of
            Ok tune ->
                translate tune

            Err e ->
                Err ("parse error: " ++ (parseError e))



{- translate the tune body -}


tuneBody : Context -> TuneBody -> Result String ( Score, Context )
tuneBody ctx tb =
    foldOverResult ctx tb bodyPart


bodyPart : Context -> BodyPart -> Result String ( VexLine, Context )
bodyPart ctx bp =
    let
        mKey =
            Just (fst ctx.modifiedKeySig)

        vexStave =
            { clef = Treble, mKey = mKey, mMeter = Just ctx.meter }

        vexLine =
            { stave = vexStave, items = [] }

        -- not needed now I think - we are now generating a single line of notes per stave with no newline
        staveCtx =
            ctx
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


music : Context -> Music -> Result String ( VexItem, Context )
music ctx m =
    case m of
        Barline bar ->
            Ok ( VBar, ctx )

        Note abcNote ->
            note ctx abcNote
                |> Result.map (\( vn, c ) -> ( VNote vn, c ))

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

        Tuplet tupletSignature notes ->
            let
                ( size, _, noteCount ) =
                    tupletSignature

                notesResult =
                    noteList ctx notes
            in
                if (size /= noteCount) then
                    Err ("Tuplets with uneven note lengths not supported")
                else
                    case notesResult of
                        Ok ( vnotes, _ ) ->
                            Ok ( VTuplet size vnotes, ctx )

                        Err e ->
                            Err e

        Chord abcChord ->
            -- not finished - we need to take account of the overall chord duration
            let
                notesResult =
                    noteList ctx abcChord.notes

                nDur =
                    firstNoteDuration abcChord.notes

                overallDur =
                    multiply (abcChord.duration) nDur

                chordDurResult =
                    noteDur ctx overallDur
            in
                case ( notesResult, chordDurResult ) of
                    ( Ok ( vnotes, _ ), Ok vexd ) ->
                        Ok ( VChord vexd vnotes, ctx )

                    ( Err e, _ ) ->
                        Err e

                    ( _, Err e ) ->
                        Err ("Chord " ++ e ++ ": " ++ (AbcText.abcChord abcChord))

        BrokenRhythmPair abcNote1 broken abcNote2 ->
            let
                ( bNote1, bNote2 ) =
                    makeBroken broken abcNote1 abcNote2

                note1Result =
                    note ctx bNote1

                note2Result =
                    note ctx bNote2
            in
                case ( note1Result, note2Result ) of
                    ( Ok ( vnote1, _ ), Ok ( vnote2, _ ) ) ->
                        Ok ( VNotePair vnote1 vnote2, ctx )

                    ( Err e, _ ) ->
                        Err ("Note " ++ e ++ ": " ++ (AbcText.abcNote abcNote1))

                    ( _, Err e ) ->
                        Err ("Note " ++ e ++ ": " ++ (AbcText.abcNote abcNote2))

        _ ->
            Ok ( VUnimplemented, ctx )


note : Context -> AbcNote -> Result String ( VexNote, Context )
note ctx abcNote =
    let
        noteDurResult =
            noteDur ctx abcNote.duration
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
                    -- Ok ( VNote vexNote, ctx )
                    Ok ( vexNote, ctx )

            Err e ->
                Err ("Note " ++ e ++ ": " ++ (AbcText.abcNote abcNote))



{- translate a note or rest duration, wrapping in a Result which indicates an
   unsupported duration.  This rounds values of 'short enough' note durations
   to the nearest supported value
-}


noteDur : Context -> NoteDuration -> Result String VexDuration
noteDur ctx d =
    let
        numer =
            numerator ctx.unitNoteLength
                * (numerator d)
                * 128

        denom =
            denominator ctx.unitNoteLength
                * (denominator d)

        -- replace this with precise arithmetic?
        durn =
            numer // denom
    in
        case durn of
            128 ->
                Ok Whole

            96 ->
                Ok HalfDotted

            64 ->
                Ok Half

            48 ->
                Ok QuarterDotted

            32 ->
                Ok Quarter

            24 ->
                Ok EighthDotted

            16 ->
                Ok Eighth

            12 ->
                Ok SixteenthDotted

            8 ->
                Ok Sixteenth

            6 ->
                Ok ThirtySecondDotted

            4 ->
                Ok ThirtySecond

            3 ->
                Ok SixtyFourthDotted

            2 ->
                Ok SixtyFourth

            _ ->
                Err "too long or too dotted"



{- apply the specified broken rhythm to each note in the note pair (presented individually)
   and return the broken note pair
-}


makeBroken : Broken -> AbcNote -> AbcNote -> ( AbcNote, AbcNote )
makeBroken broken n1 n2 =
    let
        down i =
            Ratio.add (1 `over` 1) (Ratio.negate (dotFactor i))

        up i =
            Ratio.add (1 `over` 1) (dotFactor i)
    in
        case broken of
            LeftArrow i ->
                let
                    left =
                        { n1 | duration = multiply n1.duration (down i) }

                    right =
                        { n2 | duration = multiply n2.duration (up i) }
                in
                    ( left, right )

            RightArrow i ->
                let
                    left =
                        { n1 | duration = multiply n1.duration (up i) }

                    right =
                        { n2 | duration = multiply n2.duration (down i) }
                in
                    ( left, right )


noteList : Context -> List AbcNote -> Result String ( List VexNote, Context )
noteList ctx notes =
    foldOverResult ctx notes note



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



{- get the duration of the first note in a sequence -}


firstNoteDuration : List AbcNote -> NoteDuration
firstNoteDuration ns =
    List.map (\a -> a.duration) ns
        |> List.head
        |> withDefault (over 1 1)



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
