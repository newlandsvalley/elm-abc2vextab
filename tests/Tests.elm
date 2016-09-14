module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Abc exposing (parse, parseKeySignature, parseError)
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
                Expect.fail "parse error"


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
                Expect.fail "parse error"


all : Test
all =
    describe "A Test Suite"
        [ test "one line" <|
            \() ->
                expectScoreMatches oneLineScore oneLine
        , test "two lines" <|
            \() ->
                expectParses twoLines
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
        , test "bad note length" <|
            \() ->
                expectScoreMatches badNoteLengthFailure badNoteLength
        ]


oneLine : String
oneLine =
    "M: 3/4\x0D\nK: D\x0D\n|\x0D\n"


oneLineScore : Result String String
oneLineScore =
    Ok "stave notation=true clef=treble key=D time=3/4 \x0D\n|\x0D\n"


modifiedKey : String
modifiedKey =
    "M: 3/4\x0D\nK: D Phr ^f\x0D\n| ABC |\x0D\n"


modifiedKeyFailure : Result String String
modifiedKeyFailure =
    Err "modified key signatures not supported"


sharpKey : String
sharpKey =
    "M: 3/4\x0D\nK: F#\x0D\n|\x0D\n"


sharpKeyScore : Result String String
sharpKeyScore =
    Ok "stave notation=true clef=treble key=F# time=3/4 \x0D\n|\x0D\n"


minorKey : String
minorKey =
    "M: 3/4\x0D\nK: Gm\x0D\n|\x0D\n"


minorKeyScore : Result String String
minorKeyScore =
    Ok "stave notation=true clef=treble key=Gm time=3/4 \x0D\n|\x0D\n"


twoLines : String
twoLines =
    "| ABC \x0D\n| def |\x0D\n"


simpleNote : String
simpleNote =
    "AB |\x0D\n"


simpleNoteScore : Result String String
simpleNoteScore =
    Ok (defaultStave ++ "notes :8 A/4 :8 B/4|\x0D\n")


badNoteLength : String
badNoteLength =
    "A B30 |\x0D\n"


badNoteLengthFailure : Result String String
badNoteLengthFailure =
    Err "Note too long or too dotted: B30"


defaultStave : String
defaultStave =
    "stave notation=true clef=treble key=C time=4/4 \x0D\n"
