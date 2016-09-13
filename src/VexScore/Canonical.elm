module VexScore.Canonical exposing (toScoreText)

{-|

@docs toString

-}

import VexScore.Score exposing (..)
import String exposing (concat)


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
    vexStave vl.stave


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
                    "key=" ++ toString k.pitchClass

                _ ->
                    ""
    in
        nicelySpace [ "stave notation=true", clef, time, key, eol ]
