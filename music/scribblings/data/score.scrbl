#lang scribble/manual

@(require "../util/dd.rkt"
          (for-label music/data/time/main
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
with @dd[[Timed Note]].

