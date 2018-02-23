#lang scribble/manual

@(require "../util/dd.rkt")

@title{Notes}

@(declare-dd Note)
@(declare-dd Timed)
@(declare-dd TimePeriod)

A @dd[#:def Note] contains the pitch information for a
note, but no time information.

A @dd[[Timed Note]] contains both pitch and time
information, including start time and duration. 

