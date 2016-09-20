elm-abc2vextab
==============

WORK IN PROGRESS

This is intended to be a utility which translates an __AbcTune__ (defined in [Abc.ParseTree](https://github.com/newlandsvalley/elm-abc-parser/blob/master/src/Abc/ParseTree.elm)) into [VexTab](http://www.vexflow.com/vextab/tutorial.html) and hence will eventually allow scores to be produced from ABC tunes.

done
----
* Staves - key signature, meter, (treble) clef
* Basic bar lines
* Basic notes (pitch, octave, ordering, duration)
* Basic tuplets
* Basic rests
* Basic chords

next things to do
-----------------

* Complete stave, bar, tuplet, chord fime detail
* Normalise modal key signatures - needs a refactoring from Abc Parser
* Inline headers - are they supported by VexTab?
* Stave differences between line one and the rest
* Key, TimeSig, UnitNoteLength changes for in-body headers
* What happens if the tune has very low notes?  How to detect a bass clef?
* Annotations
* Slurs


features currently unsupported by VexTab
* First and second repeats
* Tuplets with notes of different lengths 
