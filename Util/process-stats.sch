; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Accessor procedures for the statistics dump output format
;
; This file has two sections.
; 
; The first section defines accessors and predicates for all the entries
; in a stats-dump vector, and a generic processing procedure called
; process-stats that is parameterized by a procedure that accepts
; a single record.
;
; The second section is a bunch of stats-dump processors built on the
; procedures in the first section.


; Section 1: Accessors

(define (words-allocated x) (bignum (list-ref x 0) (list-ref x 1)))
(define (words-reclaimed x) (bignum (list-ref x 2) (list-ref x 3)))
(define (words-copied x)    (bignum (list-ref x 4) (list-ref x 5)))
(define (words-moved x)     (bignum (list-ref x 22) (list-ref x 23)))
(define (overall-gc-time x) (list-ref x 6))
(define (overall-gc-time! x v) (list-set! x 6 v))
(define (overall-words-live x) (list-ref x 7))
(define (last-gc x)         (cons (list-ref x 8) (list-ref x 9)))

(define (gen-list x)        (list-ref x 10))
(define (generations x)     (length (gen-list x)))

(define (collections x gen) (list-ref (list-ref (gen-list x) gen) 0))
(define (promotions x gen)  (list-ref (list-ref (gen-list x) gen) 1))
(define (gc-time x gen)     (list-ref (list-ref (gen-list x) gen) 2))
(define (gc-time! x gen v)  (list-set! (list-ref (gen-list x) gen) 2 v))
(define (words-live x gen)  (list-ref (list-ref (gen-list x) gen) 3))

(define (rem-list x)        (list-ref x 11))
(define (remsets x)         (length (rem-list x)))

; If rem is #f, return np-remset, otherwise return conventional remset #rem.

(define (remset x rem)
  (if (not rem)
      (np-remset x)
      (list-ref (rem-list x) rem)))

(define (pool-words-allocated x rem)
  (list-ref (remset x rem) 0))

(define (pool-words-used x rem)
  (list-ref (remset x rem) 1))

(define (hash-entries-allocated x rem)
  (list-ref (remset x rem) 2))

(define (hash-entries-used x rem)
  (list-ref (remset x rem) 3))

(define (hash-entries-recorded x rem)
  (let ((g (remset x rem)))
    (bignum (list-ref g 4) (list-ref g 5))))

(define (hash-entries-removed x rem)
  (let ((g (remset x rem)))
    (bignum (list-ref g 6) (list-ref g 7))))

(define (hash-entries-scanned x rem)
  (let ((g (remset x rem)))
    (bignum (list-ref g 8) (list-ref g 9))))
  
(define (old-words-scanned x rem)
  (let ((g (remset x rem)))
    (bignum (list-ref g 10) (list-ref g 11))))

(define (ssb-transactions-recorded x rem)
  (let ((g (remset x rem)))
    (bignum (list-ref g 12) (list-ref g 13))))

(define (frames-flushed x) (bignum (list-ref x 12) (list-ref x 13)))
(define (words-flushed x) (bignum (list-ref x 14) (list-ref x 15)))
(define (stacks-created x) (list-ref x 16))
(define (frames-restored x) (bignum (list-ref x 17) (list-ref x 18)))
(define (words-in-heaps x) (list-ref x 19))
(define (words-in-remsets x) (list-ref x 20))
(define (words-in-rts x) (list-ref x 21))

(define (extra-assoc-list x) (list-tail x 24))


; Simulated write barrier ("swb") entries.

(define (has-swb-data? x) 
  (let ((probe (assq 'swb (extra-assoc-list x))))
    (if probe
	#t
	#f)))

(define (swb-data x)
  (cdr (assq 'swb (extra-assoc-list x))))

(define (swb-array-assignments x) (list-ref (swb-data x) 0))
(define (swb-lhs-young-or-remembered x) (list-ref (swb-data x) 1))
(define (swb-rhs-constant x) (list-ref (swb-data x) 2))
(define (swb-cross-gen-check x) (list-ref (swb-data x) 3))
(define (swb-vector-transactions x) (list-ref (swb-data x) 4))


; Non-predictive remembered set ("np-remset") entries.
; Use the same accessors as above with a remset index of '#f'.

(define (has-np-remset-data? x)
  (let ((probe (assq 'np-remset (extra-assoc-list x))))
    (if probe
	#t
	#f)))

(define (np-remset x)
  (cdr (assq 'np-remset (extra-assoc-list x))))


; Generic processing function.
;
; Optionally, it takes a procedure to be applied to each record.
; The default behavior is to print each record in human-readable form.

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
  (print "Words allocated: " (words-allocated x))
  (print "Words reclaimed: " (words-reclaimed x))
  (print "Words copied: " (words-copied x))
  (print "Words moved: " (words-moved x))
  (print "GC time: " (overall-gc-time x))
  (print "Words live: " (overall-words-live x))
  (print "Last gc: " (car (last-gc x)) " " (cdr (last-gc x)))

  (print "Generations:")
  (do ((i 0 (+ i 1)))
      ((= i (generations x)) #t)
    (print "  Generation " i)
    (print-gen-data x i))

  (print "Remembered sets: ")
  (do ((i 0 (+ i 1)))
      ((= i (remsets x)) #t)
    (print "  Remset " i)
    (print-remset-data x i))

  (print-stack-data x)

  (print "Words allocated to heaps: " (words-in-heaps x))
  (print "Words allocated to remsets: " (words-in-remsets x))
  (print "Words allocated to RTS (other): " (words-in-rts x))

  (print-swb-data x)

  (if (has-np-remset-data? x)
      (begin 
	(print "  Non-predictive remembered set")
	(print-remset-data x #f)))

  (print "------------------------------------------------------------")
  (print))

; Fixme: should print indication of non-predictive generations.

(define (print-gen-data x i)
  (print "   Collections: " (collections x i))
  (print "   Promotions: " (promotions x i))
  (print "   GC time: " (gc-time x i))
  (print "   Words live: " (words-live x i)))

(define (print-remset-data x i)
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

(define (print-stack-data x)
  (print "Frames flushed: " (frames-flushed x))
  (print "Words flushed: " (words-flushed x))
  (print "Frames restored: " (frames-restored x))
  (print "Stacks created: " (stacks-created x)))

(define (print-swb-data x)
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
	       (swb-vector-transactions x)))))


; Section 2: Processing functions

; Prints a profile of number of transactions recorded and the number of 
; hash table entries that resulted (excluding the simulated barrier).

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

; Remembered-set profile #1
;
; NOTE! This is not general; it works with a system that has an ephemeral
; heap and a 2-generational non-predictive heap.

(define (remset-profile filename)
  (display 
"There is one data line per garbage collection, and it shows the number
of objects and words traced as part of doing remembered-set tracing
during that garbage collection.  If a collection affects the non-predictive
heap, then this is duly noted with a message preceding the data line
for that collection.
")
  (process-stats filename
    (let ((previous-o 0)
	  (previous-w 0))
      (lambda (x)
	(cond ((equal? (last-gc x) '(1 . promote))
	       (display "Promoting into np heap")
	       (newline))
	      ((equal? (last-gc x) '(1 . collect))
	       (display "Promoting into np heap and collecting")
	       (newline)))
	(let ((current-o 0)
	      (current-w 0))
	  (do ((i 0 (+ i 1)))
	      ((= i (remsets x)) #t)
	    (set! current-o
		  (+ current-o (hash-entries-scanned x i)))
	    (set! current-w
		  (+ current-w (old-words-scanned x i))))
	  (display (- current-o previous-o))
	  (display "; ")
	  (display (- current-w previous-w))
	  (newline)
	  (set! previous-w current-w)
	  (set! previous-o current-o))))))


; Remembered-set profile #2
;
; This is pretty general.

(define (remset-profile2 filename)
  (process-stats 
   filename
   (let ((prev #f)
	 (enow  #f)
	 (wnow  #f))
     (lambda (r)
       (if (not prev)
	   (begin (set! prev r)
		  (set! wnow (make-vector (remsets r)))
		  (set! enow (make-vector (remsets r)))))
       (do ((i 0 (+ i 1)))
	   ((= i (remsets r)))
	 (vector-set! enow i (- (hash-entries-scanned r i)
				(hash-entries-scanned prev i)))
	 (vector-set! wnow i (- (old-words-scanned r i)
				(old-words-scanned prev i))))
       (set! prev r)
       (nprint "GC: " (last-gc r))
       (do ((i 0 (+ i 1)))
	   ((= i (remsets r)))
	 (nprint " " ;" ("
		 (field (vector-ref enow i) 5)
		 ;";"
		 ;(vector-ref wnow i)
		 ;")"
		 ))
       (newline)))))


; Gc profile:  for each record, print gc type and time spent in each
; collector for that gc.  Try it -- it's a pretty picture.

(define (gc-profile filename)
  (let ((prev #f))
    (process-stats
     filename
     (lambda (r)
       (if (not prev)
	   (set! prev r))
       (nprint "GC: " (last-gc r))
       (do ((i 0 (+ i 1)))
	   ((= i (generations r)))
	 (nprint " " (field (- (gc-time r i) (gc-time prev i)) 7)))
       (set! prev r)
       (newline)))
    (nprint "Total time       ")
    (do ((i 0 (+ i 1)))
	((= i (generations prev)))
      (nprint " " (field (gc-time prev i) 7)))
    (newline)))

; Same as above, but also prints two columns with copied and moved data.

(define (gc-profile-with-cp/mv filename)
  (let ((prev #f))
    (process-stats
     filename
     (lambda (r)
       (if (not prev)
	   (set! prev r))
       (nprint "GC: " (last-gc r))
       (do ((i 0 (+ i 1)))
	   ((= i (generations r)))
	 (nprint " " (field (- (gc-time r i) (gc-time prev i)) 7)))
       (nprint " "
	       (field (- (words-copied r) (words-copied prev)) 7)
	       (field (- (words-moved r) (words-moved prev)) 7))
       (set! prev r)
       (newline)))
    (nprint "Total time       ")
    (do ((i 0 (+ i 1)))
	((= i (generations prev)))
      (nprint " " (field (gc-time prev i) 7)))
    (newline)))

; Plots the size of each generation in words following each collection.
; Try nboyer3 with five generations and watch the pig move!

(define (generation-size-profile filename)
  (process-stats 
   filename
   (lambda (r)
     (nprint "GC: " (last-gc r))
     (do ((i 0 (+ i 1)))
         ((= i (generations r)))
       (nprint " " (field (words-live r i) 12)))
     (newline))))

(define (ssb-profile filename)
  (let ((prev #f))
    (process-stats
     filename
     (lambda (r)
       (if (not prev)
	   (set! prev r)
	   (begin
	     (nprint "GC: " (last-gc r))
	     (do ((i 0 (+ i 1)))
		 ((= i (remsets r)))
	       (nprint " " (field (- (ssb-transactions-recorded r i)
				     (ssb-transactions-recorded prev i)) 5)
		       "," (field (- (hash-entries-scanned r i)
				     (hash-entries-scanned prev i))
				  5
				  'left)))
	     (newline)
	     (set! prev r)))))))

(define (swb-profile filename)
  (process-stats filename (lambda (r) 
			    (display (swb-data r))
			    (newline))))

(define (filter-times infile outfile)
  (call-with-input-file infile
    (lambda (in)
      (call-with-output-file outfile
	(lambda (out)
	  (let loop ((item (read in)))
	    (if (eof-object? item)
		#t
		(begin (overall-gc-time! item 0)
		       (do ((i 0 (+ i 1)))
			   ((= i (generations item)))
			 (gc-time! item i 0))
		       (write item out)
		       (newline out)
		       (loop (read in))))))))))


; ---------------------------------------------------------------------------

; Utility functions

(define bignum
  (let ((two^29 (expt 2 29)))
    (lambda (x y)
      (+ (* x two^29) y))))

(define (list-set! l n v)
  (set-car! (list-tail l n) v))

; Print a bunch of data and a newline.

(define (print . rest)
  (for-each display rest)
  (newline))

; Print a bunch of data but no newline.

(define (nprint . rest)
  (for-each display rest))

; Format a number n in a field of width k, right-justified unless the
; optional symbol argument 'left is also given, in which case it 
; is left-justified.  If the number fills the field, then a space is
; added on the right.

(define (field n k . attr)
  (let ((s (number->string n)))
    (if (= (string-length s) k)
	(string-append s " ")
	(let loop ((s (number->string n)))
	  (if (< (string-length s) k)
	      (if (memq 'left attr)
		  (loop (string-append s " "))
		  (loop (string-append " " s)))
	      s)))))
	

; eof
