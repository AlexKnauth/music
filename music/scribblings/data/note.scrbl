#lang scribble/manual

@(require "../util/dd.rkt")

@title{Notes}

@(define Note @tech{Note})
@(define Timed @tech{Timed})
@(define TimePeriod @tech{TimePeriod})

A @deftech[Note] contains the pitch information for a
note, but no time information.

A @dd[[Timed Note]] contains both pitch and time
information, including start time and duration.

A @racket[[@#,deftech[Timed] X]] is a structure that
combines a @dd[TimePeriod] with @racket[X]. The time period
specifies the start time and duration for the "@racket[X]".

A @deftech[TimePeriod] is a structure that combines a start
time with a duration.

