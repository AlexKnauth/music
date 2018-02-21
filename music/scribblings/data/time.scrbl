#lang scribble/manual

@(require "../util/dd.rkt")

@title{Time}

@(define Timed @tech{Timed})
@(define TimePeriod @tech{TimePeriod})

@(define Note @tech{Note})

A @racket[[@#,deftech[Timed] X]] is a structure that
combines a @dd[TimePeriod] with @racket[X]. The time period
specifies the start time and duration for the "@racket[X]".
One example of this is @dd[[Timed Note]], which represents a
note played for a time period.

A @deftech[TimePeriod] is a structure that combines a start
time with a duration.

