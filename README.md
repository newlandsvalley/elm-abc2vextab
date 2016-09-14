elm-abc2vextab
==============

WORK IN PROGRESS

This is intended to be a utility which translates an __AbcTune__ (defined in [Abc.ParseTree](https://github.com/newlandsvalley/elm-abc-parser/blob/master/src/Abc/ParseTree.elm)) into [VexTab](http://www.vexflow.com/vextab/tutorial.html) and hence will eventually allow scores to be produced from ABC tunes.

done
----
* Staves (mostly) - key signature, meter, (treble) clef
* Basic bar lines
* Basic notes (pitch, octave, ordering, duration)
* Basic rests

next things to do
-----------------

* Normalise modal key signatures - needs a refactoring from Abc Parser
* Inline headers - are they supported by VexTab?
* Tuples
* Chords
* Stave differences between line one and the rest
* Key, TimeSig, UnitNoteLength changes for in-body headers
* What happens if the tune has very low notes?  How to detect a bass clef?
* We probably need to define newline in Score rather than Canonical
* We need to trigger the generation of the 'notes' keyword differently
