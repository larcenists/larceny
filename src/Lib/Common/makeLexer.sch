; Copyright 2007 William D Clinger
;
; $Id$
;
; Script for generating state machine and parser to use in reader.sch
;
; Requires:
;     Larceny v0.94 or later.
;        (To modify for other systems, see FIXME comments.)
;     LexGen and ParseGen
;        ( http://www.ccs.neu.edu/home/will/Research/SW2006/*.tar.gz )
;     larcenyTokens.sch (regular syntax of Larceny)
;     larceny.pg (context-free syntax of Larceny)
;
; Creates:
;     dfaLarceny.sch
;     parserLarceny.sch
;     tablesLarceny
;
; The definitions of state0 through stateN must be extracted
; by hand from dfaLarceny.sch and copied into reader.sch.
;
; The entire contents of parserLarceny.sch must be copied into
; reader.sch.


; Change these path names for your system.

(define input:lexgen "/proj/will/LarcenyDev/lib/LexGen/loadlexgen.sch")
(define input:parsegen "/proj/will/LarcenyDev/lib/ParseGen/loadparsegen.sch")

(define input:regexps "larcenyTokens.sch")
(define input:grammar "larceny.pg")

(define output:dfa "dfaLarceny.sch")
(define output:parser "parserLarceny.sch")
(define output:tables "tablesLarceny")

; FIXME: the host system must be case-sensitive because terminals.sch is.

(case-sensitive? #t)

; ParseGen must be loaded before LexGen, I think.

(load input:parsegen)

(load input:lexgen)

(load input:regexps)

(display "Generating minimal DFA, which may take several minutes.")
(newline)

(let ((x (time (generate-scheme-lexer scheme_terminals))))
  (call-with-output-file
   output:dfa
   (lambda (p)
     (pretty-print x p))))

(display "Generating parser.")
(newline)

(generate-scheme input:grammar output:parser output:tables)
