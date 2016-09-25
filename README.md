elm-abc2vextab
==============

WORK IN PROGRESS

This is a utility which translates an __AbcTune__ (defined in [Abc.ParseTree](https://github.com/newlandsvalley/elm-abc-parser/blob/master/src/Abc/ParseTree.elm)) into [VexTab](http://www.vexflow.com/vextab/tutorial.html) and hence allows scores to be produced from ABC tunes.

VexTab is a pre-alpha release.  There are various features of a score which almost any self-respecting tune will require but which currently remain unsupported.  These include:

* First and second repeats
* Slurs (it seems as if these are supported in VexFlow but not yet available in VexTab syntax. The closest approach appears to be a bend which only associates two adjacent notes)
* Grace notes

## examples

__scoreEditor.html__ is an interactive editor for ABC scores.  As each edit is made to the ABC, then, (assuming it is valid), the amended score is displayed. It is built from ScoreEditor.elm.  To build, cd to the examples directory and invoke compilee.sh.


__abc2ScoreDebugger.html__  allows you to enter ABC text and then attempt to render it as a score by clicking on the render button.  The intermediate VexTab text is also displayed, as are any error messages. It is built from Abc2ScoreDebugger.elm.  To build, cd to the examples directory and invoke compiled.sh.

## Progress

### done

* staves - key signature, meter, (treble) clef
* modal keys
* bar lines
* notes
* tuplets
* rests
* basic chords
* broken rhythm pairs
* ties
* headers between stave lines
* Decorations (up and down bow, staccato, accent, fermata)

### next things to do

* Inline headers - are they supported by VexTab?
* What happens if the tune has very low notes?  How to detect a bass clef?
* Further decorations
