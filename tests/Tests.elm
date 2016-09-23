module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Abc exposing (parse, parseError)
import Abc.ParseTree exposing (..)
import VexScore.Translate exposing (translate)
import VexScore.Canonical exposing (toScoreText)
import Debug exposing (..)


tuneToScoreText : AbcTune -> Result String String
tuneToScoreText t =
    translate t
        |> Result.map toScoreText


expectParses : String -> Expectation
expectParses s =
    let
        parseResult =
            parse s
    in
        case parseResult of
            Ok res ->
                let
                    _ =
                        tuneToScoreText res
                            |> log "VexTab"
                in
                    Expect.pass

            Err errs ->
                Expect.fail ("parse error" ++ toString errs)


expectScoreMatches : Result String String -> String -> Expectation
expectScoreMatches target s =
    let
        parseResult =
            parse s
    in
        case parseResult of
            Ok res ->
                let
                    vexResult =
                        tuneToScoreText res
                in
                    Expect.equal target vexResult

            Err errs ->
                Expect.fail ("parse error" ++ toString errs)


all : Test
all =
    describe "A Test Suite"
        [ test "one line" <|
            \() ->
                expectScoreMatches oneLineScore oneLine
        , test "two lines" <|
            \() ->
                expectParses twoLines
        , test "repeats" <|
            \() ->
                expectScoreMatches repeatsScore repeats
        , test "sharp key" <|
            \() ->
                expectScoreMatches sharpKeyScore sharpKey
        , test "minor key" <|
            \() ->
                expectScoreMatches minorKeyScore minorKey
        , test "modified key unsupported" <|
            \() ->
                expectScoreMatches modifiedKeyFailure modifiedKey
        , test "simple note" <|
            \() ->
                expectScoreMatches simpleNoteScore simpleNote
        , test "accidental note" <|
            \() ->
                expectScoreMatches accidentalNoteScore accidentalNote
        , test "tie" <|
            \() ->
                expectScoreMatches tieScore tie
        , test "simple rest" <|
            \() ->
                expectScoreMatches simpleRestScore simpleRest
        , test "triplet" <|
            \() ->
                expectScoreMatches tripletScore triplet
        , test "basic chord" <|
            \() ->
                expectScoreMatches basicChordScore basicChord
        , test "basic broken right rhythm" <|
            \() ->
                expectScoreMatches basicBrokenRightRhythmScore basicBrokenRightRhythm
        , test "basic broken left rhythm" <|
            \() ->
                expectScoreMatches basicBrokenLeftRhythmScore basicBrokenLeftRhythm
        , test "key change" <|
            \() ->
                expectScoreMatches keyChangeScore keyChange
        , test "meter change" <|
            \() ->
                expectScoreMatches meterChangeScore meterChange
        , test "unsupported tuplet" <|
            \() ->
                expectScoreMatches unsupportedTupletFailure unsupportedTuplet
        , test "bad note length" <|
            \() ->
                expectScoreMatches badNoteLengthFailure badNoteLength
        , test "bad chord length" <|
            \() ->
                expectScoreMatches badChordLengthFailure badChordLength
        ]


oneLine : String
oneLine =
    "M: 3/4\x0D\nK: D\x0D\n|A\x0D\n"


oneLineScore : Result String String
oneLineScore =
    Ok "\x0D\nstave notation=true clef=treble key=D time=3/4 \x0D\n notes | :8 A/4"


modifiedKey : String
modifiedKey =
    "M: 3/4\x0D\nK: D Phr ^f\x0D\n| ABC |\x0D\n"


modifiedKeyFailure : Result String String
modifiedKeyFailure =
    Err "modified key signatures not supported"


sharpKey : String
sharpKey =
    "M: 3/4\x0D\nK: F#\x0D\n|A\x0D\n"


sharpKeyScore : Result String String
sharpKeyScore =
    Ok "\x0D\nstave notation=true clef=treble key=F# time=3/4 \x0D\n notes | :8 A/4"


minorKey : String
minorKey =
    "M: 3/4\x0D\nK: Gm\x0D\n|A\x0D\n"


minorKeyScore : Result String String
minorKeyScore =
    Ok "\x0D\nstave notation=true clef=treble key=Gm time=3/4 \x0D\n notes | :8 A/4"


twoLines : String
twoLines =
    "| ABC \x0D\n| def |\x0D\n"


repeats : String
repeats =
    "|: AB :|\x0D\n"


repeatsScore : Result String String
repeatsScore =
    Ok (defaultStave ++ " notes =|: :8 A/4 :8 B/4 =:|")


simpleNote : String
simpleNote =
    "AB |\x0D\n"


simpleNoteScore : Result String String
simpleNoteScore =
    Ok (defaultStave ++ " notes :8 A/4 :8 B/4 |")


accidentalNote : String
accidentalNote =
    "^A^^B _c__d =e |\x0D\n"


accidentalNoteScore : Result String String
accidentalNoteScore =
    Ok (defaultStave ++ " notes :8 A#/4 :8 B##/4 :8 C@/5 :8 D@@/5 :8 En/5 |")


tie : String
tie =
    "ABc- | c |\x0D\n"


tieScore : Result String String
tieScore =
    Ok (defaultStave ++ " notes :8 A/4 :8 B/4 :8 C/5 | :8 T C/5 |")


simpleRest : String
simpleRest =
    "Az |\x0D\n"


simpleRestScore : Result String String
simpleRestScore =
    Ok (defaultStave ++ " notes :8 A/4 :8 ## |")


triplet : String
triplet =
    "(3ABc |\x0D\n"



-- note - VexTab support not complete yet for tuplets


tripletScore : Result String String
tripletScore =
    Ok (defaultStave ++ " notes A/4-B/4-C/5 ^3^ |")


unsupportedTuplet : String
unsupportedTuplet =
    "(3:2:4 A2B2cd |\x0D\n"


unsupportedTupletFailure : Result String String
unsupportedTupletFailure =
    Err "Tuplets with uneven note lengths not supported"


basicChord : String
basicChord =
    "[ABc] |\x0D\n"


basicChordScore : Result String String
basicChordScore =
    Ok (defaultStave ++ " notes ( A/4.B/4.C/5 ) |")


basicBrokenRightRhythm : String
basicBrokenRightRhythm =
    "A>B |\x0D\n"


basicBrokenRightRhythmScore : Result String String
basicBrokenRightRhythmScore =
    Ok (defaultStave ++ " notes :8d A/4 :16 B/4 |")


basicBrokenLeftRhythm : String
basicBrokenLeftRhythm =
    "A2<B2 |\x0D\n"


basicBrokenLeftRhythmScore : Result String String
basicBrokenLeftRhythmScore =
    Ok (defaultStave ++ " notes :8 A/4 :qd B/4 |")


keyChange : String
keyChange =
    "| ABc |\x0D\nK: D\x0D\n| cde |\x0D\n"


keyChangeScore : Result String String
keyChangeScore =
    Ok
        (defaultStave
            ++ " notes | :8 A/4 :8 B/4 :8 C/5 |"
            ++ "\x0D\nstave notation=true clef=treble key=D  "
            ++ "\x0D\n notes | :8 C/5 :8 D/5 :8 E/5 |"
        )


meterChange : String
meterChange =
    "| ABc |\x0D\nM: 3/4\x0D\n| cde |\x0D\n"


meterChangeScore : Result String String
meterChangeScore =
    Ok
        (defaultStave
            ++ " notes | :8 A/4 :8 B/4 :8 C/5 |"
            ++ "\x0D\nstave notation=true clef=treble key=C time=3/4 "
            ++ "\x0D\n notes | :8 C/5 :8 D/5 :8 E/5 |"
        )


badNoteLength : String
badNoteLength =
    "A B30 |\x0D\n"


badNoteLengthFailure : Result String String
badNoteLengthFailure =
    Err "Note too long or too dotted: B30"


badChordLength : String
badChordLength =
    "[A1/2B1/2c1/2]30 |\x0D\n"


badChordLengthFailure : Result String String
badChordLengthFailure =
    Err "Chord too long or too dotted: [A/B/c/]30"


defaultStave : String
defaultStave =
    "\x0D\nstave notation=true clef=treble key=C time=4/4 \x0D\n"
