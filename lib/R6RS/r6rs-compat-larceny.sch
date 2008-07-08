;;;===============================================================================
;;;
;;; Larceny compatibility file
;;;
;;; Uncomment appropriate LOAD comand in macros-core.scm
;;;
;;;===============================================================================

; $Id$

;; A numeric string that uniquely identifies this run in the universe.

(define (ex:unique-token)
  (larceny:number->compactstring (current-seconds)))

;; The letrec black hole and corresponding setter.

(define ex:undefined 'undefined)
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.

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
