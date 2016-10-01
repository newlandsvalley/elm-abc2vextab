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
        , test "modal key" <|
            \() ->
                expectScoreMatches modalKeyScore modalKey
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
        , test "staccato" <|
            \() ->
                expectScoreMatches staccatoScore staccato
        , test "bowing" <|
            \() ->
                expectScoreMatches bowingScore bowing
        , test "fermata" <|
            \() ->
                expectScoreMatches fermataScore fermata
        , test "key change" <|
            \() ->
                expectScoreMatches keyChangeScore keyChange
        , test "meter change" <|
            \() ->
                expectScoreMatches meterChangeScore meterChange
        , test "mixed tuplet" <|
            \() ->
                expectScoreMatches mixedTupletScore mixedTuplet
        , test "bad note length" <|
            \() ->
                expectScoreMatches badNoteLengthFailure badNoteLength
        , test "bad chord length" <|
            \() ->
                expectScoreMatches badChordLengthFailure badChordLength
        , test "empty line" <|
            \() ->
                expectScoreMatches emptyLineScore emptyLine
        , test "continuation" <|
            \() ->
                expectScoreMatches continuationScore continuation
        , test "inline key change unsupported" <|
            \() ->
                expectScoreMatches inlineKeyChangeFailure inlineKeyChange
        , test "inline meter change unsupported" <|
            \() ->
                expectScoreMatches inlineMeterChangeFailure inlineMeterChange
        , test "inline note length change is supported" <|
            \() ->
                expectScoreMatches inlineNoteLenChangeScore inlineNoteLenChange
        ]


oneLine : String
oneLine =
    "M: 3/4\x0D\nK: D\x0D\n|A\x0D\n"


oneLineScore : Result String String
oneLineScore =
    Ok (options ++ "stave notation=true clef=treble key=D time=3/4 \x0D\n notes | :8 A/4\x0D\n")


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
    Ok (options ++ "stave notation=true clef=treble key=F# time=3/4 \x0D\n notes | :8 A/4\x0D\n")


minorKey : String
minorKey =
    "M: 3/4\x0D\nK: Gm\x0D\n|A\x0D\n"


minorKeyScore : Result String String
minorKeyScore =
    Ok (options ++ "stave notation=true clef=treble key=Gm time=3/4 \x0D\n notes | :8 A/4\x0D\n")


modalKey : String
modalKey =
    "M: 3/4\x0D\nK: FMix\x0D\n|A\x0D\n"


modalKeyScore : Result String String
modalKeyScore =
    Ok (options ++ "stave notation=true clef=treble key=Bb time=3/4 \x0D\n notes | :8 A/4\x0D\n")


twoLines : String
twoLines =
    "| ABC \x0D\n| def |\x0D\n"


repeats : String
repeats =
    "|: AB :|\x0D\n"


repeatsScore : Result String String
repeatsScore =
    Ok (defaultStave ++ " notes =|: :8 A/4 :8 B/4 =:|\x0D\n")


simpleNote : String
simpleNote =
    "AB |\x0D\n"


simpleNoteScore : Result String String
simpleNoteScore =
    Ok (defaultStave ++ " notes :8 A/4 :8 B/4 |\x0D\n")


accidentalNote : String
accidentalNote =
    "^A^^B _c__d =e |\x0D\n"


accidentalNoteScore : Result String String
accidentalNoteScore =
    Ok (defaultStave ++ " notes :8 A#/4 :8 B##/4 :8 C@/5 :8 D@@/5 :8 En/5 |\x0D\n")


tie : String
tie =
    "ABc- | cd |\x0D\n"


tieScore : Result String String
tieScore =
    Ok (defaultStave ++ " notes :8 A/4 :8 B/4 :8 C/5 | :8 T C/5 :8 D/5 |\x0D\n")


simpleRest : String
simpleRest =
    "Az |\x0D\n"


simpleRestScore : Result String String
simpleRestScore =
    Ok (defaultStave ++ " notes :8 A/4 :8 ## |\x0D\n")


triplet : String
triplet =
    "(3ABc |\x0D\n"


tripletScore : Result String String
tripletScore =
    Ok (defaultStave ++ " notes :8 A/4 :8 B/4 :8 C/5 ^3,3^ |\x0D\n")


mixedTuplet : String
mixedTuplet =
    "(3:2:4 A2B2cd |\x0D\n"


mixedTupletScore : Result String String
mixedTupletScore =
    Ok (defaultStave ++ " notes :q A/4 :q B/4 :8 C/5 :8 D/5 ^3,4^ |\x0D\n")


basicChord : String
basicChord =
    "[ABc] |\x0D\n"


basicChordScore : Result String String
basicChordScore =
    Ok (defaultStave ++ " notes ( A/4.B/4.C/5 ) |\x0D\n")


basicBrokenRightRhythm : String
basicBrokenRightRhythm =
    "A>B |\x0D\n"


basicBrokenRightRhythmScore : Result String String
basicBrokenRightRhythmScore =
    Ok (defaultStave ++ " notes :8d A/4 :16 B/4 |\x0D\n")


basicBrokenLeftRhythm : String
basicBrokenLeftRhythm =
    "A2<B2 |\x0D\n"


basicBrokenLeftRhythmScore : Result String String
basicBrokenLeftRhythmScore =
    Ok (defaultStave ++ " notes :8 A/4 :qd B/4 |\x0D\n")


staccato : String
staccato =
    ".A .d |\x0D\n"


staccatoScore : Result String String
staccatoScore =
    Ok (defaultStave ++ " notes :8 A/4 $.a./bottom.$ :8 D/5 $.a./top.$ |\x0D\n")


bowing : String
bowing =
    "uA vd |\x0D\n"


bowingScore : Result String String
bowingScore =
    Ok (defaultStave ++ " notes :8 A/4 $.a|/top.$ :8 D/5 $.am/top.$ |\x0D\n")


fermata : String
fermata =
    "HA Hd |\x0D\n"


fermataScore : Result String String
fermataScore =
    Ok (defaultStave ++ " notes :8 A/4 $.a@a/top.$ :8 D/5 $.a@a/top.$ |\x0D\n")


keyChange : String
keyChange =
    "| ABc |\x0D\nK: D\x0D\n| cde |\x0D\n"


keyChangeScore : Result String String
keyChangeScore =
    Ok
        (defaultStave
            ++ " notes | :8 A/4 :8 B/4 :8 C/5 |\x0D\n"
            ++ "stave notation=true clef=treble key=D  \x0D\n"
            ++ " notes | :8 C/5 :8 D/5 :8 E/5 |\x0D\n"
        )


meterChange : String
meterChange =
    "| ABc |\x0D\nM: 3/4\x0D\n| cde |\x0D\n"


meterChangeScore : Result String String
meterChangeScore =
    Ok
        (defaultStave
            ++ " notes | :8 A/4 :8 B/4 :8 C/5 |\x0D\n"
            ++ "stave notation=true clef=treble key=C time=3/4 \x0D\n"
            ++ " notes | :8 C/5 :8 D/5 :8 E/5 |\x0D\n"
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


emptyLine : String
emptyLine =
    "AB |  \x0D\n\x0D\n"


emptyLineScore : Result String String
emptyLineScore =
    Ok (defaultStave ++ " notes :8 A/4 :8 B/4 |\x0D\n")


continuation : String
continuation =
    "|ABc |\\ comment \x0D\n  def |\x0D\n"


continuationScore : Result String String
continuationScore =
    Ok (defaultStave ++ " notes | :8 A/4 :8 B/4 :8 C/5 |\x0D\n notes :8 D/5 :8 E/5 :8 F/5 |\x0D\n")


inlineKeyChange : String
inlineKeyChange =
    "K: D\x0D\n| ABC |[K: A] def|\x0D\n"


inlineKeyChangeFailure : Result String String
inlineKeyChangeFailure =
    Err "inline key signature changes not supported"


inlineMeterChange : String
inlineMeterChange =
    "M: 4/4\x0D\n| ABC |[M: 3/4] def|\x0D\n"


inlineMeterChangeFailure : Result String String
inlineMeterChangeFailure =
    Err "inline time signature changes not supported"


inlineNoteLenChange : String
inlineNoteLenChange =
    "L: 1/8\x0D\n| ABC |[L: 1/16] def|\x0D\n"


inlineNoteLenChangeScore : Result String String
inlineNoteLenChangeScore =
    Ok (defaultStave ++ " notes | :8 A/4 :8 B/4 :8 C/4 | :16 D/5 :16 E/5 :16 F/5 |\x0D\n")


defaultStave : String
defaultStave =
    options
        ++ "stave notation=true clef=treble key=C time=4/4 \x0D\n"


options : String
options =
    "options beam-rests=false\x0D\n"
