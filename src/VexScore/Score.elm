module VexScore.Score exposing (..)

import Abc.ParseTree exposing (KeySignature, MeterSignature, PitchClass, Accidental, AbcNote)


type alias Score =
    List VexLine


type alias VexLine =
    { stave : VexStave
    , items : List VexItem
    }


type VexItem
    = VNote VexNote
    | VRest VexDuration
    | VBar
    | VTuplet Int (List VexNote)
    | VChord VexDuration (List VexNote)
    | VNotePair VexNote VexNote
    | VUnimplemented


type alias VexStave =
    { clef : Clef
    , mKey : Maybe KeySignature
    , mMeter : Maybe MeterSignature
    }


type Clef
    = Treble
    | Bass


type VexDuration
    = Whole
    | Half
    | Quarter
    | Eighth
    | Sixteenth
    | ThirtySecond
    | SixtyFourth
    | HalfDotted
    | QuarterDotted
    | EighthDotted
    | SixteenthDotted
    | ThirtySecondDotted
    | SixtyFourthDotted


type alias VexNote =
    { pitchClass : PitchClass
    , accidental : Maybe Accidental
    , octave : Int
    , duration : VexDuration
    , tied :
        Bool
        -- to the next note
    }
