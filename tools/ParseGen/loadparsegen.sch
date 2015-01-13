; Loads the entire parser generator.

; The following definition assumes the Larceny top-level directory
; is the current working directory.
; Edit this definition to accomodate your circumstances.

(define *parsegen-directory* "./tools/ParseGen/")

; The parser generator is written in ANSI/IEEE Scheme,
; I think, but parts of it are conditionalized to take advantage
; of extensions found in implementations I use.
; The recognized values for *host-implementation* are:
;
;     IEEEScheme
;     ChezScheme
;     Gambit
;     MacScheme (works for Larceny also)

(define *host-implementation* 'MacScheme)
;(define *host-implementation* 'ChezScheme)
;(define *host-implementation* 'IEEEScheme)

(load "./tools/ParseGen/sets.sch")
(load "./tools/ParseGen/follow.sch")
(load "./tools/ParseGen/parsegen.sch")
(load "./tools/ParseGen/parsegen0.sch")

; There is no need to load "parsegen.c.sch" unless a C parser
; is to be generated.  Similarly for "parsegen.pascal.sch"
; and "parsegen.scheme.sch".

;(load "./tools/ParseGen/parsegen.c.sch")
;(load "./tools/ParseGen/parsegen.java.sch")
;(load "./tools/ParseGen/parsegen.modula3.sch")
;(load "./tools/ParseGen/parsegen.pascal.sch")
(load "./tools/ParseGen/parsegen.scheme.sch")

