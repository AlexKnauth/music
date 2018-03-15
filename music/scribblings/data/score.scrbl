#lang scribble/manual

@(require "../util/dd.rkt"
          scribble/eval
          (for-label racket/base
                     music/data/time/main
                     music/data/note/main
                     music/data/score/main))

@title{Scores}

@defmodule[music/data/score/main]

@(declare-dd Score)
@(declare-dd Part)
@(declare-dd Note)
@(declare-dd Timed)
@(declare-dd TimePeriod)

The main top-level data type for a music piece or song is a
@dd[#:def Score]. A @dd[Score] has a list of @dd[Part]s.

A @dd[#:def Part] is a structure that contains a name for the
part along with a list of all the notes and other musical
elements played by that part. These notes are represented
with @dd[[Timed Note]]. The @racket[part] constructor
automatically sorts the elements by time period.

@examples[
(require music/data/score/main
         music/data/time/main
         music/data/note/main)
(code:comment "this is the long way of writing a \"score\"")
(define example-score/longhand
  (score
   #f
   (list
    (part "Piano"
          (list
           (timed (time-period (position 0 beat-one) duration-quarter)
                  (C 4)))))))
]

