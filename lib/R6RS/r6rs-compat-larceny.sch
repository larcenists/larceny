;;;===============================================================================
;;;
;;; Larceny compatibility file
;;;
;;; Uncomment appropriate LOAD comand in macros-core.scm
;;;
;;;===============================================================================

; $Id$

;; A numeric string that uniquely identifies this run in the universe:
;; the time in microseconds when it is first asked for.  It is made
;; again when a dumped heap starts, or every run of a heap dumped after
;; expanding something would generate the same names, and libraries
;; compiled in different runs would share keys of environments.

(define ex:unique-token
  (let ((token #f))
    (add-init-procedure! (lambda () (set! token #f)))
    (lambda ()
      (or token
          (begin
           (set! token
                 (larceny:number->compactstring
                  (exact (round (* 1e6 (current-second))))))
           token)))))

;; The letrec black hole and corresponding setter.

(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.
;;
;; FIXME: If these are changed, then src/Compiler/syntaxenv.sch
;; should also be changed.

(define ex:guid-prefix "\x0;")
(define ex:free-prefix "\x1;")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Short identifier names take up less space on disk and during
; execution, and load faster too.  To conserve disk space and
; speed loading, the names should require as few escapes as
; possible.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a non-negative integer, encodes it as a string.

(define (larceny:number->compactstring n)
  (define digits
    "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ%&")
  (assert (and (number? n) (integer? n) (exact? n) (not (negative? n))))
  (do ((n n (div n 64))
       (chars '()
              (cons (string-ref digits (bitwise-and n 63))
                    chars)))
      ((= n 0)
       (if (null? chars)
           "0"
           (list->string chars)))))
