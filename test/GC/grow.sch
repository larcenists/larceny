; 03 Mar 1998 / lth
;
; A simple GC benchmark that builds up ever-longer vectors and then 
; releases them again.  This program was inspired by a post by John 
; Kominek to comp.lang.modula3 (Message-ID: <34DFBE06.59E2@cs.cmu.edu>) 
; on 09 Feb 1998, where a similar program was demonstrated to cause fairly
; major problems for the DEC SRC Modula-3 incremental garbage collector.
;
; The benchmark builds 25 sequences of length 100,000, all of which become
; garbage after having been built up.  In Larceny, a sequence of 100,000
; fixnums occupies 800,008 bytes of memory.
;
; This program is is particularly nice to test the large-object space
; for space leaks.  For the int-sequence-benchmark, heap usage should
; not exceed 6MB in the stop-and-copy collector with a load of 3.0.

(define (int-sequence-benchmark . rest)
  (run-benchmark "sequence"
		 (lambda ()
		   (sequence-run build-sequence-of-ints))
		 (if (null? rest) 1 (car rest))))

(define (list-sequence-benchmark . rest)
  (run-benchmark "sequence" 
		 (lambda ()
		   (sequence-run build-sequence-of-lists))
		 (if (null? rest) 1 (car rest))))

(define (sequence-run p)
  (do ((i 0 (+ i 1)))
      ((= i 25) #t)
    (p)))

; Builds a sequence of 100,000 ints and then drops it on the floor.

(define (build-sequence-of-ints)
  (let ((s (make-sequence)))
    (do ((i 0 (+ i 1)))
	((= i 100000) #t)
      (sequence-addhi! s i))))

; Builds a sequence of 100,000 lists of int of length 1, and then drops it
; on the floor.

(define (build-sequence-of-lists)
  (let ((s (make-sequence)))
    (do ((i 0 (+ i 1)))
	((= i 100000) #t)
      (sequence-addhi! s (cons i '())))))


; A sequence is a growable array.
;
; It's represented as a pair: the car is the next index to use, the cdr 
; is the current vector.  Every time an element is added at the end and 
; there is no room, the vector's length is doubled (this is what the 
; Modula-3 implementation does).

(define *seq-default-size* 100)

(define (make-sequence)
  (cons 0 (make-vector *seq-default-size* 0)))

(define (sequence-addhi! s x)
  (let ((next (car s))
	(v    (cdr s)))
    (if (= next (vector-length v))
	(let ((w (make-vector (* (vector-length v) 2) 0)))
	  (do ((i (- (vector-length v) 1) (- i 1)))
	      ((< i 0))
	    (vector-set! w i (vector-ref v i)))
	  (set-cdr! s w)
	  (sequence-addhi! s x))
	(begin 
	  (vector-set! v next x)
	  (set-car! s (+ next 1))))))

; eof
