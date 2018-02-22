#lang scribble/manual

@(require "../util/dd.rkt")

@title{Time}

@(define Timed @tech{Timed})
@(define TimePeriod @tech{TimePeriod})
@(define Duration @tech{Duration})
@(define Position @tech{Position})

@(define Note @tech{Note})

A @racket[[@#,deftech[Timed] X]] is a structure that
combines a @dd[TimePeriod] with @racket[X]. The time period
specifies the start time and duration for the "@racket[X]".
One example of this is @dd[[Timed Note]], which represents a
note played for a time period.

A @deftech[TimePeriod] is a structure that combines a start
time with a duration.

A @deftech[Duration] is a length of time in the music. Some
examples of durations are @racket[duration-quarter] for a
quarter note, @racket[duration-eighth] for an eighth note,
@racket[duration-half] for a half note, and so on. There is
also @racket[duration-zero], for musical elements that occur
instantaneously. A duration does not contain tempo
information, a quarter note is just a quarter note.

A @deftech[Position] is a position in time in the music,
represented by structure combining a measure number with the
duration between the start of the measure and the position.
Both of these start at zero, so the start of a song is
represented by measure number zero, and the start of a
measure is represented by the zero duration.

