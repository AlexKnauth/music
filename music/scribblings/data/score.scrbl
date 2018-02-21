#lang scribble/manual

@(require "../util/dd.rkt")

@title{Scores}

@(define Score @tech{Score})
@(define Part @tech{Part})
@(define Note @tech{Note})
@(define Timed @tech{Timed})
@(define TimePeriod @tech{TimePeriod})

The main top-level data type for a music piece or song is a
@deftech[Score]. A @dd[Score] has a list of @dd[Part]s.

A @deftech[Part] is a structure that contains a name for the
part along with a list of all the notes and other musical
elements played by that part. These notes are represented
with @dd[[Timed Note]].

