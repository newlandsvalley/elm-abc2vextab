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
        , test "modified key unsupported" <|
            \() ->
                expectScoreMatches modifiedKeyFailure modifiedKey
        ]


oneLine : String
oneLine =
    "M: 3/4\x0D\nK: D\x0D\n| ABC |\x0D\n"


oneLineScore : Result String String
oneLineScore =
    Ok "stave notation=true clef=treble time=3/4 key=D \x0D\n"


modifiedKey : String
modifiedKey =
    "M: 3/4\x0D\nK: D Phr ^f\x0D\n| ABC |\x0D\n"


modifiedKeyFailure : Result String String
modifiedKeyFailure =
    Err "modified key signatures not supported"


twoLines : String
twoLines =
    "| ABC \x0D\n| def |\x0D\n"
