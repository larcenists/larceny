; Larceny -- Switches for the Sparc assembler
;
; lth@cs.uoregon.edu / August 25, 1995
; $Id$
;
; FIXME: these switches need to be cleaned up. They should all be procedures
; which take no args to return the setting and take one boolean arg to
; change the setting. See Compiler/switches.sch.

(define assume-short-distance-to-call #f) ; Faster SAVE/SETRTN

(define unsafe-code
  (make-twobit-flag 'unsafe-code))

(define catch-undefined-globals
  (make-twobit-flag 'catch-undefined-globals))

(define inline-cons
  (make-twobit-flag 'inline-cons))
  
(define inline-assignment
  (make-twobit-flag 'inline-assignment))

(define write-barrier
  (make-twobit-flag 'write-barrier))  


; Initialization

(inline-cons #f)              ; Allocation of pairs in-line.
(inline-assignment #f)        ; Assignment generation check in-line.
(write-barrier #t)            ; For generational garbage collection.
(catch-undefined-globals #t)  ; #!undefined causes error
(unsafe-code #f)              ; Generate unsafe code

; eof
