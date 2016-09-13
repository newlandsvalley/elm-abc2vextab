module VexScore.Score exposing (..)

import Abc.ParseTree exposing (KeySignature, MeterSignature, PitchClass, Accidental)


type alias Score =
    List VexLine


type alias VexLine =
    { stave : VexStave
    , items : List VexItem
    }


type VexItem
    = VexNote NoteProperties
    | VexBar
    | VexUnimplemented


type alias VexStave =
    { clef : Clef
    , mKey : Maybe KeySignature
    , mMeter : Maybe MeterSignature
    }


type alias NoteProperties =
    { pitchClass : PitchClass
    , mAccidental : Maybe Accidental
    }


type Clef
    = Treble
    | Bass
