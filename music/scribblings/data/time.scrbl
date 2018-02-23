#lang scribble/manual

@(require scribble/example
          "../util/dd.rkt")

@title{Time}

@(declare-dd Timed)
@(declare-dd TimePeriod)
@(declare-dd Duration)
@(declare-dd Position)

@(declare-dd Note)

@(define (make-ev)
   (make-base-eval
    '(require music/data/time/main
              music/data/note/main)))

A @dd[#:def [Timed X]] is a structure that
combines a @dd[TimePeriod] with @racket[X]. The time period
specifies the start time and duration for the "@racket[X]".
One example of this is @dd[[Timed Note]], which represents a
note played for a time period.

A @dd[#:def TimePeriod] is a structure that combines a start
time with a duration.

A @dd[#:def Duration] is a length of time in the music. Some
examples of durations are @racket[duration-quarter] for a
quarter note, @racket[duration-eighth] for an eighth note,
@racket[duration-half] for a half note, and so on. There is
also @racket[duration-zero], for musical elements that occur
instantaneously. A duration does not contain tempo
information, a quarter note is just a quarter note.

A @dd[#:def Position] is a position in time in the music,
represented by structure combining a measure number with the
duration between the start of the measure and the position.
Both of these start at zero, so the start of a song is
represented by measure number zero, and the start of a
measure is represented by the zero duration.

@defstruct[timed ([period @#,dd[TimePeriod]] [value X])]{
Produces a @dd[[Timed X]].

@examples[
  #:eval (make-ev)
  (timed (time-period (position 0 beat-one) duration-quarter) (G 4))
]
}

@defstruct[time-period ([start @#,dd[Position]] [duration @#,dd[Duration]])]{
Produces a @dd[TimePeriod].

@examples[
  #:eval (make-ev)
  (time-period (position 4 beat-three) duration-quarter)
]
}

