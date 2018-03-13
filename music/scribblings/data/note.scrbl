#lang scribble/manual

@(require "../util/dd.rkt"
          (for-label music/data/time/main
                     music/data/note/main
                     music/data/score/main))

@title{Notes}

@defmodule[music/data/note/main]

@(declare-dd Note)
@(declare-dd Timed)
@(declare-dd TimePeriod)

A @dd[#:def Note] contains the pitch information for a
note, but no time information.

A @dd[[Timed Note]] contains both pitch and time
information, including start time and duration. 

