elm-abc2vextab
==============

WORK IN PROGRESS

This is intended to be a utility which translates an __AbcTune__ (defined in [Abc.ParseTree](https://github.com/newlandsvalley/elm-abc-parser/blob/master/src/Abc/ParseTree.elm)) into [VexTab](http://www.vexflow.com/vextab/tutorial.html) and hence will eventually allow scores to be produced from ABC tunes.

VexTab is a pre-alpha release.  There are various features of a score which almost any self-respecting tune will require but which currently remain unsupported.  These include:

* First and second repeats
* Tuplets with notes of different lengths
* Slurs (it seems as if these are supported in VexFlow but not yet available in VexTab syntax. The closest approach sappears to be a bend which only associates two adjacent notes)
* Grace notes

## Example


There is a single example __abc2score.html__ which is built from __examples/Abc2Score.elm__ by changing to the examples directory and invoking __compile.sh__.  This allows you to enter ABC text and then attempt to render it as a score.  The intermediate VexTab text is also displayed, as are any error messages.

## Progress

### done

* staves - key signature, meter, (treble) clef
* bar lines
* notes
* basic tuplets
* basic rests
* basic chords
* broken rhythm pairs
* ties
* headers between stave lines

### next things to do

* Normalise modal key signatures - needs a refactoring from Abc Parser
* Inline headers - are they supported by VexTab?
* What happens if the tune has very low notes?  How to detect a bass clef?
* Annotations

