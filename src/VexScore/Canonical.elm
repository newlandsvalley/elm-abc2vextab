module VexScore.Canonical exposing (toScoreText)

{-|

@docs toString

-}

import VexScore.Score exposing (..)
import String exposing (concat)
import Maybe exposing (withDefault)
import Abc.ParseTree
    exposing
        ( Accidental(..)
        , Mode(..)
        , AbcNote
        , Bar
        , Thickness(..)
        , Repeat(..)
        )


type NoteContext
    = Staved
    | Tupleted
    | Chordal


eol : String
eol =
    "\x0D\n"



{- concatenate strings and space them simply -}


nicelySpace : List String -> String
nicelySpace xs =
    List.intersperse " " xs
        |> String.concat


toScoreText : Score -> String
toScoreText score =
    let
        f vl acc =
            acc ++ vexBodyPart vl
    in
        List.foldl f "" score


vexBodyPart : VexBodyPart -> String
vexBodyPart bp =
    case bp of
        VLine line ->
            vexLine line

        VContextChange ->
            ""


vexLine : VexLine -> String
vexLine vl =
    vexStave vl.stave ++ vexItems vl.items


vexStave : VexStave -> String
vexStave vs =
    let
        clef =
            "clef=" ++ ((String.toLower << toString) (vs.clef))

        time =
            case vs.mMeter of
                Just m ->
                    "time=" ++ toString (fst m) ++ "/" ++ toString (snd m)

                _ ->
                    ""

        key =
            case vs.mKey of
                Just k ->
                    let
                        accidental =
                            headerAccidental k.accidental

                        md =
                            mode k.mode
                    in
                        "key=" ++ toString k.pitchClass ++ accidental ++ md

                _ ->
                    ""
    in
        "\x0D\n"
            ++ (nicelySpace [ "stave notation=true", clef, key, time, eol, "notes" ])


vexItems : List VexItem -> String
vexItems vis =
    List.map vexItem vis
        |> String.concat


vexItem : VexItem -> String
vexItem vi =
    case vi of
        VBar bar ->
            vexBar bar

        VNote vnote ->
            vexNote Staved vnote

        VRest duration ->
            let
                dur =
                    noteDur duration

                rest =
                    "##"
            in
                nicelySpace [ "", dur, rest ]

        VTuplet size vnotes ->
            " "
                ++ (List.map (vexNote Tupleted) vnotes
                        |> List.intersperse "-"
                        |> String.concat
                   )
                ++ " ^"
                ++ toString size
                ++ "^"

        VChord dur vnotes ->
            " ( "
                ++ (List.map (vexNote Chordal) vnotes
                        |> List.intersperse "."
                        |> String.concat
                   )
                ++ " )"

        VNotePair vnote1 vnote2 ->
            vexNote Staved vnote1
                ++ vexNote Staved vnote2

        {- the following are not yet implemented in VexTab:
           grace notes
           first and second repeats
        -}
        _ ->
            ""


vexNote : NoteContext -> VexNote -> String
vexNote ctx vnote =
    let
        accident =
            Maybe.map accidental vnote.accidental
                |> withDefault ""

        pitch =
            toString vnote.pitchClass
                ++ accident
                ++ "/"
                ++ toString vnote.octave

        dur =
            noteDur vnote.duration
    in
        case ctx of
            Chordal ->
                pitch

            Tupleted ->
                -- this format not supported in VexTab nicelySpace [ dur, pitch ]
                pitch

            _ ->
                nicelySpace [ "", dur, pitch ]


noteDur : VexDuration -> String
noteDur nd =
    case nd of
        Whole ->
            ":w"

        Half ->
            ":h"

        Quarter ->
            ":q"

        Eighth ->
            ":8"

        Sixteenth ->
            ":16"

        ThirtySecond ->
            ":32"

        SixtyFourth ->
            ":64"

        HalfDotted ->
            ":hd"

        QuarterDotted ->
            ":qd"

        EighthDotted ->
            ":8d"

        SixteenthDotted ->
            ":16d"

        ThirtySecondDotted ->
            ":32d"

        SixtyFourthDotted ->
            ":64d"


accidental : Accidental -> String
accidental a =
    case a of
        Sharp ->
            "#"

        Flat ->
            "@"

        DoubleSharp ->
            "##"

        DoubleFlat ->
            "@@"

        Natural ->
            "n"


vexBar : Bar -> String
vexBar b =
    case b.repeat of
        Just Begin ->
            " =|:"

        Just End ->
            " =:|"

        Just BeginAndEnd ->
            " =::"

        Nothing ->
            case b.thickness of
                Thin ->
                    " |"

                _ ->
                    " =||"


headerAccidental : Maybe Accidental -> String
headerAccidental ma =
    case ma of
        Just Sharp ->
            "#"

        Just Flat ->
            "b"

        _ ->
            ""


mode : Mode -> String
mode m =
    case m of
        Major ->
            ""

        Minor ->
            "m"

        Ionian ->
            ""

        Aeolian ->
            "m"

        -- we need to trap this in translate - probably by converting modes to canonical forms
        _ ->
            "error not supported"
