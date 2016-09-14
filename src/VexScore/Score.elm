module VexScore.Score exposing (..)

import Abc.ParseTree exposing (KeySignature, MeterSignature, PitchClass, Accidental, AbcNote)


type alias Score =
    List VexLine


type alias VexLine =
    { stave : VexStave
    , items : List VexItem
    }


type VexItem
    = VNote VexNote Bool
      -- Bool is the NotesContext - introducing a group of notes
    | VRest VexNoteDuration
    | VBar
    | VUnimplemented


type alias VexStave =
    { clef : Clef
    , mKey : Maybe KeySignature
    , mMeter : Maybe MeterSignature
    }


type Clef
    = Treble
    | Bass


type VexNoteDuration
    = Whole
    | Half
    | Quarter
    | Eighth
    | Sixteenth
    | ThirtySecond
    | WholeDotted
    | HalfDotted
    | QuarterDotted
    | EighthDotted
    | SixteenthDotted
    | ThirtySecondDotted


type alias VexNote =
    { pitchClass : PitchClass
    , accidental : Maybe Accidental
    , octave : Int
    , duration : VexNoteDuration
    , tied :
        Bool
        -- to the next note
    }
