; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny -- second part of compatibility code
; This file ought to be compiled, but doesn't have to be.
;
; 20 August 1999

(define host-system 'larceny)           ; Don't remove this!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; These definitions were moved here from (oft interpreted) compat.sch
; as they may improve when loaded compiled form from compat2.fasl.

(define (write-byte bytenum . rest)
  (let ((port (if (null? rest) 
                  (current-output-port)
                  (car rest))))
    (write-char (integer->char bytenum) port)))

; Temporary?

(define (.check! flag exn . args)
  (if (not flag)
      (apply error "Runtime check exception: " exn args)))

(define (with-optimization level thunk) 
  (thunk))

; Calls thunk1, and if thunk1 causes an error to be signalled, calls thunk2.

(define (call-with-error-control thunk1 thunk2) 
  (let ((eh (error-handler)))
    (error-handler (lambda args
                     (error-handler eh)
                     (parameterize ((print-length 7)
                                    (print-level 7))
                       (decode-error args))
                     (thunk2)
                     (apply eh args)))
    (thunk1)
    (error-handler eh)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FIXME:  These definitions should go away after v0.95 is released.

;(define open-raw-latin-1-output-file open-output-file)
;(define open-raw-latin-1-input-file open-input-file)
;(define call-with-raw-latin-1-output-file call-with-output-file)
;(define call-with-raw-latin-1-input-file call-with-input-file)

; FIXME:  These names should go away as soon as possible.

;(define open-binary-input-file open-raw-latin-1-input-file)
;(define open-binary-output-file open-raw-latin-1-output-file)
;(define call-with-binary-input-file call-with-raw-latin-1-input-file)
;(define call-with-binary-output-file call-with-raw-latin-1-output-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; A well-defined sorting procedure.

(define compat:sort (lambda (list less?) (sort list less?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Well-defined character codes.
; Returns the UCS-2 code for a character.

(define compat:char->integer char->integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Input and output

(define (write-lop item port)
  (lowlevel-write item port)
  (newline port)
  (newline port))

(define write-fasl-datum lowlevel-write)

; Used only when dumping a heap with the flat1 representation
; for strings.

(define (flat1->bytevector s)
  (let* ((n (string-length s))
         (bv (make-bytevector n)))
    (do ((i 0 (+ i 1)))
        ((= i n) bv)
      (bytevector-set! bv i (char->integer (string-ref s i))))))

(define bignum->bytevector)
(define flonum->bytevector)
(define compnum->bytevector)

(cond (#f (eq? (nbuild-parameter 'host-endianness)
               (nbuild-parameter 'target-endianness))
       (let ((misc->bytevector
              (lambda (x)
                (let ((bv (bytevector-like-copy x)))
                  (typetag-set! bv $tag.bytevector-typetag)
                  bv)))
             (clear-first-word 
              (lambda (bv)
                (bytevector-like-set! bv 0 0)
                (bytevector-like-set! bv 1 0)
                (bytevector-like-set! bv 2 0)
                (bytevector-like-set! bv 3 0)
                bv)))
         (set! bignum->bytevector
               (lambda (x)
                 (let ((bv (misc->bytevector x)))
                   ;FIXME: remove all this after new bignums are stable
                   (case (nbuild-parameter 'target-endianness)
                    ((little)
                     (bytevector-like-set! bv 2 0)
                     (bytevector-like-set! bv 3 (if (< x 0) 1 0)))
                    ((big)
                     (bytevector-like-set! bv 1 0)
                     (bytevector-like-set! bv 0 (if (< x 0) 1 0)))))))
         (set! flonum->bytevector
               (lambda (x)
                 (clear-first-word (misc->bytevector x))))
         (set! compnum->bytevector
               (lambda (x)
                 (clear-first-word (misc->bytevector x))))
         #t))
      ((eq? (nbuild-parameter 'target-endianness) 'big)
       (compat:load (string-append (nbuild-parameter 'compatibility)
                                   "tobytevector-be.sch")))
      ((eq? (nbuild-parameter 'target-endianness) 'little)
       (compat:load (string-append (nbuild-parameter 'compatibility)
                                   "tobytevector-el.sch")))
      (else
       ???))

(define (list->bytevector l)
  (let ((b (make-bytevector (length l))))
    (do ((i 0 (+ i 1))
         (l l (cdr l)))
        ((null? l) b)
      (bytevector-set! b i (car l)))))

(define bytevector-word-ref 
  (let ((two^8  (expt 2 8))
        (two^16 (expt 2 16))
        (two^24 (expt 2 24)))
    (lambda (bv i)
      (+ (* (bytevector-ref bv i) two^24)
         (* (bytevector-ref bv (+ i 1)) two^16)
         (* (bytevector-ref bv (+ i 2)) two^8)
         (bytevector-ref bv (+ i 3))))))

;; A Predicate[X] is a (Any -> Boolean) that produces #t for instances
;; of X and a #f for all other values.

;; twobit-iterative-try/fallback 
;;    : X (X -> Y) Predicate[Z] (X Z -> X) (X -> Y) -> Y
;; Invokes try on input repeatedly, responding to exceptional condition Z
;; by attempting to revise the input (passing along  and try again.
;; No matter what, the system should behave sanely (if sub-optimally)
;; when last-resort is invoked on (revise^n input) for all n in Nat.

(define twobit-iterative-try/fallback 
  (if compat:has-exception-support
      (lambda (input try fail? revise last-resort)
        (let loop ((x input))
          (guard (con ((fail? con)
                       (loop (revise x con))))
            (try x))))
      (lambda (input try fail? revise last-resort)
        (last-resort input))))

;(define (twobit-format fmt . rest)
;  (let ((out (open-output-string)))
;    (apply format out fmt rest)
;    (get-output-string out)))

(define (twobit-format port fmt . rest)
  (if (not port)
      (let ((s (open-output-string)))
        (apply format s fmt rest)
        (get-output-string s))
      (apply format port fmt rest)))

; This needs to be a random number in both a weaker and stronger sense
; than `random': it doesn't need to be a truly random number, so a sequence
; of calls can return a non-random sequence, but if two processes generate
; two sequences, then those sequences should not be the same.
;
; Gross, huh?

'(define (an-arbitrary-number)
  (system "echo \\\"`date`\\\" > a-random-number")
  (let ((x (string-hash (call-with-input-file "a-random-number" read))))
    (delete-file "a-random-number")
    x))

; More portable but still gross.  No worse race condition than the 
; previous, at least.

(define (an-arbitrary-number)
  (call-with-output-file "a-random-number"
    (lambda (out)
      (display "Hello, sailor!" out)
      (newline out)))
  (let ((s (open-output-string)))
    (display (file-modification-time "a-random-number") s)
    (let ((x (string-hash (get-output-string s))))
      (delete-file "a-random-number")
      x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Miscellaneous

(define cerror error)

; eof
