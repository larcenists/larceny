; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Heap dumper / tree shaker code.
;
; This program dumps a noninteractive heap containing a program and only
; the parts of the run-time system not referenced from that program.
; In particular, if the program does not call EVAL, LOAD, or MACRO-EXPAND,
; then the macro expander and interpreter will have been garbage collected.
;
; Usage: compile and load this file, and compile and load the code for your
; application.  Your application should have a startup procedure of 0 
; arguments, call it "app-main".  Say you want to create heap image "foo.heap".
; Evaluate:
;   (dump-garbage-collected-heap "foo.heap" app-main)
; and you're done.  Exit, and start larceny with foo.heap.

(define (dump-garbage-collected-heap heap-name main-fn)
  (dump-heap "prefix0.heap" (make-prefix0 heap-name main-fn))
  (system "./larceny.bin -stopcopy prefix0.heap")
  (delete-file "prefix0.heap"))

(define (make-prefix0 heap-name main-fn)
  (lambda (argv)
    (reset-handler (lambda () (display "RESET!") (newline) (exit 0)))
    (collect 'full)
    (dump-heap heap-name (make-prefix1 main-fn))
    (system (string-append "./larceny.bin -reorganize-and-dump " heap-name))
    (rename-file (string-append heap-name ".split") heap-name)
    (exit 0)))

(define (make-prefix1 main-fn)
  (lambda (argv)
    (command-line-arguments argv)
    (call-with-current-continuation
     (lambda (k)
       (reset-handler (lambda () 
                        (display "RESET!")
                        (newline)
                        (k #f)))
       (enable-interrupts (standard-timeslice))
       (main-fn)))
    (exit 0)))

; eof
