; Util/process-stats.sch
; Accessor procedures for the statistics dump output format
;
; $Id: process-stats.sch,v 1.1 1997/02/11 14:41:42 lth Exp lth $
;
; In addition to the accessor procedures, a sample procedure called 
; process-dump is defined.  This procedure reads the dump and produces
; human-readable, annotated output.

; Accessors

(define (words-allocated x) (bignum (list-ref x 0) (list-ref x 1)))
(define (words-reclaimed x) (bignum (list-ref x 2) (list-ref x 3)))
(define (words-copied x)    (bignum (list-ref x 4) (list-ref x 5)))
(define (overall-gc-time x) (list-ref x 6))
(define (overall-words-live x) (list-ref x 7))
(define (last-gc x)         (cons (list-ref x 8) (list-ref x 9)))

(define (gen-list x)        (list-ref x 10))
(define (generations x)     (length (gen-list x)))

(define (collections x gen) (list-ref (list-ref (gen-list x) gen) 0))
(define (promotions x gen)  (list-ref (list-ref (gen-list x) gen) 1))
(define (gc-time x gen)     (list-ref (list-ref (gen-list x) gen) 2))
(define (words-live x gen)  (list-ref (list-ref (gen-list x) gen) 3))

(define (rem-list x)        (list-ref x 11))
(define (remsets x)         (length (rem-list x)))

(define (pool-words-allocated x rem) (list-ref (list-ref (rem-list x) rem) 0))
(define (pool-words-used x rem) (list-ref (list-ref (rem-list x) rem) 1))
(define (hash-entries-allocated x rem)
  (list-ref (list-ref (rem-list x) rem) 2))

(define (hash-entries-used x rem)
  (list-ref (list-ref (rem-list x) rem) 3))

(define (hash-entries-recorded x rem)
  (let ((g (list-ref (rem-list x) rem)))
    (bignum (list-ref g 4) (list-ref g 5))))

(define (hash-entries-removed x rem)
  (let ((g (list-ref (rem-list x) rem)))
    (bignum (list-ref g 6) (list-ref g 7))))

(define (hash-entries-scanned x rem)
  (let ((g (list-ref (rem-list x) rem)))
    (bignum (list-ref g 8) (list-ref g 9))))
  
(define (old-words-scanned x rem)
  (let ((g (list-ref (rem-list x) rem)))
    (bignum (list-ref g 10) (list-ref g 11))))

(define (ssb-transactions-recorded x rem)
  (let ((g (list-ref (rem-list x) rem)))
    (bignum (list-ref g 12) (list-ref g 13))))

(define (frames-flushed x) (bignum (list-ref x 12) (list-ref x 13)))
(define (words-flushed x) (bignum (list-ref x 14) (list-ref x 15)))
(define (stacks-created x) (list-ref x 16))
(define (frames-restored x) (bignum (list-ref x 17) (list-ref x 18)))
(define (words-in-heaps x) (list-ref x 19))
(define (words-in-remsets x) (list-ref x 20))
(define (words-in-rts x) (list-ref x 21))

; simulated write barrier ("swb") numbers.

(define (has-swb-data? x) (not (null? (list-tail x 22))))

(define (swb-array-assignments x) (list-ref x 22))
(define (swb-lhs-young-or-remembered x) (list-ref x 23))
(define (swb-rhs-constant x) (list-ref x 24))
(define (swb-cross-gen-check x) (list-ref x 25))
(define (swb-vector-transactions x) (list-ref x 26))


; Produce human-readable output.  Optionally take the procedure to
; apply to each record.

(define (process-stats filename . rest)
  (let ((process-record (if (null? rest)
			    readify-statsdump-record
			    (car rest))))
    (call-with-input-file filename
      (lambda (p)
	(let loop ((x (read p)))
	  (if (not (eof-object? x))
	      (begin (process-record x)
		     (loop (read p)))))))))

(define (readify-statsdump-record x)

  (define (print . rest)
    (for-each display rest)
    (newline))

  (print "Words allocated: " (words-allocated x))
  (print "Words reclaimed: " (words-reclaimed x))
  (print "Words copied: " (words-copied x))
  (print "GC time: " (overall-gc-time x))
  (print "Words live: " (overall-words-live x))
  (print "Last gc: " (car (last-gc x)) " " (cdr (last-gc x)))
  (print "Generations:")
  (do ((i 0 (+ i 1)))
      ((= i (generations x)) #t)
    (print "  Generation " i)
    (print "   Collections: " (collections x i))
    (print "   Promotions: " (promotions x i))
    (print "   GC time: " (gc-time x i))
    (print "   Words live: " (words-live x i)))
  (print "Remembered sets: ")
  (do ((i 0 (+ i 1)))
      ((= i (remsets x)) #t)
    (print "  Remset " i)
    (print "   Words allocated to pool: "
	   (pool-words-allocated x i))
    (print "   Words used in pool: " (pool-words-used x i))
    (print "   Hash entries allocated: " (hash-entries-allocated x i))
    (print "   Hash entries used: " (hash-entries-used x i))
    (print "   Hash entries recorded: " (hash-entries-recorded x i))
    (print "   Hash entries removed: " (hash-entries-removed x i))
    (print "   Hash entries scanned: " (hash-entries-scanned x i))
    (print "   Words of oldspace scanned: " (old-words-scanned x i))
    (print "   SSB transactions recorded: " (ssb-transactions-recorded x i)))
  (print "Frames flushed: " (frames-flushed x))
  (print "Words flushed: " (words-flushed x))
  (print "Frames restored: " (frames-restored x))
  (print "Stacks created: " (stacks-created x))
  (print "Words allocated to heaps: " (words-in-heaps x))
  (print "Words allocated to remsets: " (words-in-remsets x))
  (print "Words allocated to RTS (other): " (words-in-rts x))
  (if (has-swb-data? x)
      (begin
	(print "Simulated write barrier: ")
	(print "  Array assignments: " (swb-array-assignments x))
	(print "  LHS was young or already remembered: "
	       (swb-lhs-young-or-remembered x))
	(print "  RHS was constant: " (swb-rhs-constant x))
	(print "  Old->young pointer was not created: " 
	       (swb-cross-gen-check x))
	(print "  Vector assignment transactions created: " 
	       (swb-vector-transactions x))))
  (print "------------------------------------------------------------")
  (print))


; Prints a profile of number of transactions recorded and the number of 
; hash table entries that resulted.

(define (ssb-profile filename)
  (display "trans  hash")
  (newline)
  (process-stats filename
    (let ((previous-t 0)
	  (previous-h 0))
      (lambda (x)
	(let ((current-t 0)
	      (current-h 0))
	  (do ((i 0 (+ i 1)))
	      ((= i (remsets x)) #t)
	    (set! current-t
		  (+ current-t (ssb-transactions-recorded x i)))
	    (set! current-h
		  (+ current-h (hash-entries-recorded x i))))
	  (display (- current-t previous-t))
	  (display " ")
	  (display (- current-h previous-h))
	  (newline)
	  (set! previous-t current-t)
	  (set! previous-h current-h))))))

; Internal

(define bignum
  (let ((two^29 (expt 2 29)))
    (lambda (x y)
      (+ (* x two^29) y))))

; eof
