; Lib/memstats.sch
; Larceny runtime library -- system statistics
;
; $Id: memstats.sch,v 1.6 1997/05/31 01:50:28 lth Exp lth $
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

      (define (make-remset-stats r)
	(vector (vector-ref r $mstat.r-apool)
		(vector-ref r $mstat.r-upool)
		(vector-ref r $mstat.r-ahash)
		(vector-ref r $mstat.r-uhash)
		(bignum r $mstat.r-hrec-hi)
		(bignum r $mstat.r-hrem-hi)
		(bignum r $mstat.r-hscan-hi)
		(bignum r $mstat.r-wscan-hi)
		(bignum r $mstat.r-ssbrec-hi)))

      (define (make-basic-vector v)
	(vector (bignum v $mstat.wallocated-hi)
		(bignum v $mstat.wcollected-hi)
		(bignum v $mstat.wcopied-hi)
		(vector-ref v $mstat.gctime)
		(vector-ref v $mstat.wlive)
		(vector-ref v $mstat.gc-last-gen)
		(vector-ref v $mstat.gc-last-type)
		(vector-ref v $mstat.generations)
		#f			; remembered-set information
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
		(vector-ref v $mstat.majfaults)
		#f			; non-predictive remembered sets
		(vector-ref v $mstat.max-heap)
		(vector-ref v $mstat.promtime)
		))

      (define (count-remsets rv)
	(let loop ((n 0) (i 0))
	  (cond ((= i (vector-length rv))
		 n)
		((not (vector-ref (vector-ref rv i) $mstat.r-np-p))
		 (loop (+ n 1) (+ i 1)))
		(else
		 (loop n (+ i 1))))))

      (define (make-remset-vector remset-stats limit test)
	(let ((r (make-vector limit #f)))
	  (let loop ((i 0) (j 0))
	    (if (= i (vector-length remset-stats))
		r
		(let ((rvec (vector-ref remset-stats i)))
		  (if (eq? (vector-ref rvec $mstat.r-np-p) test)
		      (begin (vector-set! r j (make-remset-stats rvec))
			     (loop (+ i 1) (+ j 1)))
		      (loop (+ i 1) j)))))))

      (define (transform-gen-stats! gen-stats)
	(do ((i 0 (+ i 1)))
	    ((= i (vector-length gen-stats)))
	  (let ((g (vector-ref gen-stats i)))
	    (vector-set! g $mstat.g-np-youngp
			 (= 1 (vector-ref g $mstat.g-np-youngp)))
	    (vector-set! g $mstat.g-np-oldp
			 (= 1 (vector-ref g $mstat.g-np-oldp))))))

      (let* ((raw-stats      (sys$get-resource-usage))
	     (remset-stats   (vector-ref raw-stats $mstat.remsets))
	     (remset-len     (vector-length remset-stats))
	     (std-remsets    (count-remsets remset-stats))
	     (stats-vec      (make-basic-vector raw-stats))
	     (has-np-remset? (vector-ref raw-stats $mstat.np-remsetp)))
					  
	(transform-gen-stats! (vector-ref raw-stats $mstat.generations))
	(vector-set! stats-vec 8
		     (make-remset-vector remset-stats std-remsets #f))
	(if has-np-remset?
	    (vector-set! stats-vec 26
			 (make-remset-vector remset-stats
					     (- remset-len std-remsets)
					     #t)))
	stats-vec))))


; I moved this out of display-memstats because I encountered an
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

    (define (print-generation g i)
      (mprint "  Generation # " i)
      (if (or (vector-ref g 4) (vector-ref g 5))
	  (begin (if (vector-ref g 4)
		     (mprint "    (Non-predictive young generation; "
			     "j=" (vector-ref g 6)
			     "; k=" (vector-ref g 7) ")")
		     (mprint "    (Non-predictive old generation)"))))
      (mprint "    Collections....: " (vector-ref g 0))
      (mprint "    Promotions.....: " (vector-ref g 1))
      (mprint "    GC time (ms)...: " (vector-ref g 2))
      (mprint "      promotion....: " (vector-ref g 10))
      (mprint "      collection...: " (- (vector-ref g 2) (vector-ref g 10)))
      (mprint "    Generation size: " (vector-ref g 9))
      (mprint "      allocated....: " (vector-ref g 8))
      (mprint "      in use.......: " (vector-ref g 3)))

    (define (print-per-generation)
      (mprint "Per-generation information")
      (do ((i 0 (+ i 1)))
	  ((= i (vector-length (vector-ref v 7))))
	(print-generation (gen i) i)))

    (define (print-remset r i)
      (mprint "  Remembered set # " i)
      (mprint "    Pool allocated.: " (vector-ref r 0))
      (mprint "    Pool used......: " (vector-ref r 1))
      (mprint "    Hash allocated.: " (vector-ref r 2))
      (mprint "    Hash used......: " (vector-ref r 3))
      (mprint "    Hash recorded..: " (vector-ref r 4))
      (mprint "    Hash removed...: " (vector-ref r 5))
      (mprint "    Objects scanned: " (vector-ref r 6))
      (mprint "    Words scanned..: " (vector-ref r 7))
      (mprint "    Transactions...: " (vector-ref r 8)))

    (define (print-per-remset remsets banner)
      (if full?
	  (begin
	    (mprint banner)
	    (do ((i 0 (+ i 1)))
		((= i (vector-length remsets)))
	      (print-remset (vector-ref remsets i) i)))))

    (define (print-overall)
      (mprint "Overall memory statistics")
      (mprint "  Generations....: " (vector-length (vector-ref v 7)))
      (mprint "  Words allocated: " (vector-ref v 0))
      (mprint "  Words reclaimed: " (vector-ref v 1))
      (mprint "  Words copied...: " (vector-ref v 2))
      (mprint "  GC time (ms)...: " (vector-ref v 3))
      (mprint "    promotion....: " (vector-ref v 28))
      (mprint "    collection...: " (- (vector-ref v 3) (vector-ref v 28)))
      (mprint "  Heap allocation")
      (mprint "    maximum......: " (vector-ref v 27))
      (mprint "    current......: " (vector-ref v 13))
      (mprint "    current used.: " (vector-ref v 4))
      (if full?
	  (mprint "  Last GC........: Generation " (vector-ref v 5) 
		  "; type " (if (zero? (vector-ref v 6))
				"collection"
				"promotion"))))

    (define (print-stack)
      (mprint "Stack information")
      (mprint "  Frames flushed.: " (vector-ref v 9))
      (mprint "  Words flushed..: " (vector-ref v 10))
      (mprint "  Frames restored: " (vector-ref v 12))
      (mprint "  Stacks created.: " (vector-ref v 11)))

    (define (print-simulated-barrier)
      (if full?
	  (begin
	    (mprint "Simulated write barrier")
	    (mprint "  Assignments....: " (vector-ref v 16))
	    (mprint "  LHS young/known: " (vector-ref v 17))
	    (mprint "  RHS immediate..: " (vector-ref v 18))
	    (mprint "  Not old->young.: " (vector-ref v 19))
	    (mprint "  Transactions...: " (vector-ref v 20)))))

    (define (print-misc)
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

    (define (print-all)
      (print-overall)
      (print-per-generation)
      (print-per-remset (vector-ref v 8) "Per-remembered-set information")
      (if (vector-ref v 26)
	  (print-per-remset (vector-ref v 26)
			    "Non-predictive remembered sets"))
      (print-stack)
      (print-simulated-barrier)
      (print-misc))

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
