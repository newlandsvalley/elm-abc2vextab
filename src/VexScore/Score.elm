module VexScore.Score exposing (..)

import Abc.ParseTree exposing (KeySignature, MeterSignature, PitchClass, Accidental, AbcNote)


type alias Score =
    List VexLine


type alias VexLine =
    { stave : VexStave
    , items : List VexItem
    }


type VexItem
    = VexNote AbcNote Bool
      -- Bool is the NotesContext - introducing a group of notes
    | VexBar
    | VexUnimplemented


type alias VexStave =
    { clef : Clef
    , mKey : Maybe KeySignature
    , mMeter : Maybe MeterSignature
    }


type Clef
    = Treble
    | Bass
