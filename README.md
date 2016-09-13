elm-abc2vextab
==============

WORK IN PROGRESS

This is intended to be a utility which translates an __AbcTune__ which is defined in [Abc.ParseTree](https://github.com/newlandsvalley/elm-abc-parser/blob/master/src/Abc/ParseTree.elm) into [VexTab](http://www.vexflow.com/vextab/tutorial.html) and hence will eventually allow scores to be produced from ABC tunes.

done
----
* Staves (msstly) - key signature, meter, (treble) clef
* Basic bar lines

next things to do
-----------------

* Normalise modal key signatures - needs a refactoring from Abc Parser
* inline headers - are they supported by VexTab
* Basic notes
* Note durations
* Stave differences between line one and the rest
* Key, TimeSig, UnitNoteLength changes 
* What happens if the tune has very low notes?  How to detect a bass clef