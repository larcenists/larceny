; Lib/memstats.sch
; Larceny runtime library -- system statistics
;
; $Id: memstats.sch,v 1.4 1997/02/11 19:50:57 lth Exp $
;
; System statistics is maintained by the parts of the system written in C
; and are obtained by a call on the primitive sys$get-resource-usage (which
; will vary from OS to OS).  That returns a vector of resource information.
;
; The fields of the returned vector and their offsets are defined by 
; constants that are defined in Rts/globals.cfg.
;
; All times are in milliseconds; all memory sizes are in words.
;
; Some entries have a high and a low part so that the low levels of the system
; do not have to use bignums to keep the counts. Arguably, the time fields 
; should also be split, as they will overflow after 149 hours or so. For
; most of these fields, though, more than 29 bits is really overkill.


; (memstats) => vector
;
; This procedure is the only reasonable way to access the memstats vector. 
; It coalesces the split values in the vector and returns a vector with
; counts of collections, counts of words, and time spent collecting.
;
; The offsets in the returned vector are documented in the User's Manual,
; so tread carefully.

(define memstats
  (let* ((two^29 536870912)  ; (expt 2 29)
	 (bignum (lambda (v n)
		   (+ (* (vector-ref v n) two^29)
		      (vector-ref v (+ n 1))))))
    (lambda ()
      (let ((v (sys$get-resource-usage)))
	(vector (bignum v $mstat.wallocated-hi)
		(bignum v $mstat.wcollected-hi)
		(bignum v $mstat.wcopied-hi)
		(vector-ref v $mstat.gctime)
		(vector-ref v $mstat.wlive)
		(vector-ref v $mstat.gc-last-gen)
		(vector-ref v $mstat.gc-last-type)
		(vector-ref v $mstat.generations)
		(let* ((l (vector-length (vector-ref v $mstat.remsets)))
		       (w (make-vector l #f)))
		  (do ((i 0 (+ i 1)))
		      ((= i l) w)
		    (let ((r (vector-ref (vector-ref v $mstat.remsets) i)))
		      (vector-set! w i (vector
					(vector-ref r $mstat.r-apool)
					(vector-ref r $mstat.r-upool)
					(vector-ref r $mstat.r-ahash)
					(vector-ref r $mstat.r-uhash)
					(bignum r $mstat.r-hrec-hi)
					(bignum r $mstat.r-hrem-hi)
					(bignum r $mstat.r-hscan-hi)
					(bignum r $mstat.r-wscan-hi)
					(bignum r $mstat.r-ssbrec-hi))))))
		(bignum v $mstat.fflushed-hi)
		(bignum v $mstat.wflushed-hi)
		(vector-ref v $mstat.stk-created)
		(bignum v $mstat.frestored-hi)
		(vector-ref v $mstat.words-heap)
		(vector-ref v $mstat.words-remset)
		(vector-ref v $mstat.words-rts)
		(vector-ref v $mstat.swb-assign)
		(vector-ref v $mstat.swb-lhs-ok)
		(vector-ref v $mstat.swb-rhs-const)
		(vector-ref v $mstat.swb-not-xgen)
		(vector-ref v $mstat.swb-trans)
		(vector-ref v $mstat.rtime)
		(vector-ref v $mstat.stime)
		(vector-ref v $mstat.utime)
		(vector-ref v $mstat.minfaults)
		(vector-ref v $mstat.majfaults))))))


; I moved this out of display-memstats because I encountered some strange
; apparent compiler bug when having it inside.  --lars

(define (mprint . rest)
  (for-each display rest) (newline))

; Takes a vector as returned from memstats and displays it with useful
; labels. 

(define (display-memstats v . rest)
  (let ((full? (and (not (null? rest)) (eq? (car rest) 'full)))
	(minimal? (and (not (null? rest)) (eq? (car rest) 'minimal))))

    (define (remset i)
      (vector-ref (vector-ref v 8) i))

    (define (gen i)
      (vector-ref (vector-ref v 7)i))

    (define (print-all)
      (mprint "Overall memory statistics")
      (mprint "  Generations....: " (vector-length (vector-ref v 7)))
      (mprint "  Words allocated: " (vector-ref v 0))
      (mprint "  Words reclaimed: " (vector-ref v 1))
      (mprint "  Words copied...: " (vector-ref v 2))
      (mprint "  Total GC time..: " (vector-ref v 3) " ms")
      (mprint "  Heap allocated.: " (vector-ref v 13))
      (mprint "  Words live.....: " (vector-ref v 4))
      (if full?
	  (mprint "  Last GC........: Generation " (vector-ref v 5) 
		  "; type " (if (zero? (vector-ref v 6))
				"collection"
				"promotion")))
      (mprint "Per-generation information")
      (do ((i 0 (+ i 1)))
	  ((= i (vector-length (vector-ref v 7))))
	(mprint "  Generation # " i)
	(mprint "    Collections....: " (vector-ref (gen i) 0))
	(mprint "    Promotions.....: " (vector-ref (gen i) 1))
	(mprint "    GC time........: " (vector-ref (gen i) 2))
	(mprint "    Words live.....: " (vector-ref (gen i) 3)))
      (if full?
	  (begin
	    (mprint "Per-remembered-set information")
	    (do ((i 0 (+ i 1)))
		((= i (vector-length (vector-ref v 8))))
	      (mprint "  Remembered set # " i)
	      (mprint "    Pool allocated.: " (vector-ref (remset i) 0))
	      (mprint "    Pool used......: " (vector-ref (remset i) 1))
	      (mprint "    Hash allocated.: " (vector-ref (remset i) 2))
	      (mprint "    Hash used......: " (vector-ref (remset i) 3))
	      (mprint "    Hash recorded..: " (vector-ref (remset i) 4))
	      (mprint "    Hash removed...: " (vector-ref (remset i) 5))
	      (mprint "    Objects scanned: " (vector-ref (remset i) 6))
	      (mprint "    Words scanned..: " (vector-ref (remset i) 7))
	      (mprint "    Transactions...: " (vector-ref (remset i) 8)))))
      (mprint "Stack information")
      (mprint "  Frames flushed.: " (vector-ref v 9))
      (mprint "  Words flushed..: " (vector-ref v 10))
      (mprint "  Frames restored: " (vector-ref v 12))
      (mprint "  Stacks created.: " (vector-ref v 11))
      (if full?
	  (begin
	    (mprint "Simulated write barrier")
	    (mprint "  Assignments....: " (vector-ref v 16))
	    (mprint "  LHS young/known: " (vector-ref v 17))
	    (mprint "  RHS immediate..: " (vector-ref v 18))
	    (mprint "  Not old->young.: " (vector-ref v 19))
	    (mprint "  Transactions...: " (vector-ref v 20))))
      (mprint "Miscellaneous")
      (mprint "  Elapsed time...: " (vector-ref v 21))
      (if full?
	  (begin
	    (mprint "  User time......: " (vector-ref v 23))
	    (mprint "  System time....: " (vector-ref v 22)))
	  (begin
	    (mprint "  CPU time.......: " (+ (vector-ref v 23)
					     (vector-ref v 22)))))
      (if full?
	  (begin 
	    (mprint "  Minor faults...: " (vector-ref v 24))
	    (mprint "  Major faults...: " (vector-ref v 25)))))

    (define (print-minimal)
      (mprint "Words allocated: " (vector-ref v 0))
      (mprint "Words reclaimed: " (vector-ref v 1))
      (mprint "Elapsed time...: " (vector-ref v 21) 
	     "(User: " (vector-ref v 23) "; System: " (vector-ref v 22) ")")
      (let ((gcs 0))
	(do ((i 0 (+ i 1)))
	    ((= i (vector-length (vector-ref v 7))))
	  (let ((x (vector-ref (vector-ref v 7) i)))
	    (set! gcs (+ gcs (vector-ref x 0)))
	    (set! gcs (+ gcs (vector-ref x 1)))))
	(mprint "Elapsed GC time: " (vector-ref v 3) " ms (in " 
	       gcs " collections.)")))

    (if minimal?
	(print-minimal)
	(print-all))

    (unspecified)))


; Run a thunk and print out stats afterwards about how long it took and
; how much memory was used. This is accurate only if a collection is performed
; immediately prior to calling this procedure, and in fact a collection should
; be performed after the thunk returns as well to get a completely accurate
; picture. See comments in the code.
;
; Returns the result of the thunk.

(define (run-with-stats thunk)

  (define (pr allocated reclaimed elapsed user system gcs gctime)
    (mprint "Words allocated: " allocated)
    (mprint "Words reclaimed: " reclaimed)
    (mprint "Elapsed time...: " elapsed
	   " ms (User: " user " ms; System: " system " ms)")
    (mprint "Elapsed GC time: " gctime " ms (in " gcs " collections.)"))

  (define (print-stats s1 s2)
    (pr (- (vector-ref s2 0) (vector-ref s1 0))   ; allocated
	(- (vector-ref s2 1) (vector-ref s1 1))   ; reclaimed
	(- (vector-ref s2 21) (vector-ref s1 21)) ; elapsed
	(- (vector-ref s2 23) (vector-ref s1 23)) ; user
	(- (vector-ref s2 22) (vector-ref s1 22)) ; system
	(let ((gcs0 0)
	      (gcs1 0))
	  (do ((i 0 (+ i 1)))
	      ((= i (vector-length (vector-ref s1 7))) 
	       (- gcs1 gcs0))
	    (let ((x0 (vector-ref (vector-ref s1 7) i))
		  (x1 (vector-ref (vector-ref s2 7) i)))
	      (set! gcs0 (+ gcs0 (vector-ref x0 0) (vector-ref x0 1)))
	      (set! gcs1 (+ gcs1 (vector-ref x1 0) (vector-ref x1 1))))))
	(- (vector-ref s2 3) (vector-ref s1 3))   ; gc time
	))
  
  (let* ((s1 (memstats))
	 (r  (thunk))
	 (s2 (memstats)))
    (print-stats s1 s2)
    r))


; Nice procedure to have around.

(define (run-benchmark name thunk . rest)
  (let ((n (if (null? rest) 1 (car rest))))
    
    (define (loop n)
      (if (zero? n)
	  #t
	  (begin (thunk)
		 (loop (- n 1)))))

    (newline)
    (display "--------------------------------------------------------")
    (newline)
    (display name)
    (newline)
    (run-with-stats (lambda () (loop n)))))

; eof
