module VexScore.Canonical exposing (toScoreText)

{-|

@docs toString

-}

import VexScore.Score exposing (..)
import String exposing (concat)
import Abc.ParseTree exposing (Accidental(..), Mode(..))


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
            acc ++ vexLine vl
    in
        List.foldl f "" score


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
        nicelySpace [ "stave notation=true", clef, key, time, eol ]


vexItems : List VexItem -> String
vexItems vis =
    List.map vexItem vis
        |> String.concat


vexItem : VexItem -> String
vexItem vi =
    case vi of
        VexBar ->
            "|\x0D\n"

        _ ->
            ""


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

        -- we need to trap this in translate - probably by converting modes to canonical forms
        _ ->
            "error not supported"
