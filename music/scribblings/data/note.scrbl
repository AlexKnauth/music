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

