; -*- scheme -*-
; This is the file Lib/memstats.sch.
; Larceny runtime library -- Memory management statistics.
;
; $Id: memstats.sch,v 1.1 1995/08/03 00:18:21 lth Exp lth $
;
; History
;   July 1, 1995 / lth
;     Updated to use unix:get-resource-usage rather than sys$resource-usage!.
;
;   July 13, 1994 / lth
;     Massively rewritten for v0.20, which maintains memory statistics in
;     a manner much different from earlier versions.
;
; Memory statistics is maintained by the parts of the system written in C
; and are obtained by a call on the primitive get-resource-usage (which
; will vary from OS to OS). This
; primitive takes as an argument a vector (containing arbitrary values); the
; primitive changes the vector to reflect the resources consumed by
; Larceny.
;
; The fields of the vector and their offsets are defined by constants which
; are defined in globals.cfg. Currently, they are:
;
;  $mstat.rtime           real time of execution (from RUSAGE syscall)
;  $mstat.stime           system time of execution (ditto)
;  $mstat.utime           user time of execution (ditto)
;  $mstat.minfaults       minor page faults (ditto)
;  $mstat.majfaults       major page faults (ditto)
;  $mstat.wcollected-hi   words of data garbage collected (high 29 bits)
;  $mstat.wcollected-lo   words of data garbage collected (low 29 bits)
;  $mstat.wallocated-hi   words of data allocated (high 29 bits)
;  $mstat.wallocated-lo   words of data allocated (low 29 bits)
;  $mstat.tscanned-hi     transactions scanned during gc (high 29 bits)
;  $mstat.tscanned-lo     transactions scanned during gc (low 29 bits)
;  $mstat.ecollections    number of ephemeral collections
;  $mstat.tcollections    number of tenuring collections
;  $mstat.fcollections    number of full collections
;  $mstat.framesflushed   number of stack frames flushed
;  $mstat.gctime          time spent in garbage collection
;  $mstat.tallocated-hi   transactions stored in SSB (high 29 bits)
;  $mstat.tallocated-lo   transactions stored in SSB (low 29 bits)
;
; The size of the vector (in # of elements) is given by $mstat.vsize.
;
; All times are in milliseconds.
;
; Some entries have a high and a low part so that the low levels of the system
; do not have to use bignums to keep the counts. Arguably, the time fields 
; should also be split, as they will overflow after 149 hours or so. For
; most of these fields, though, more than 29 bits is really overkill...


; This procedure is the only reasonable way to access the memstats vector. 
; It coalesces the split values in the vector and returns a vector with
; counts of collections, counts of words, and time spent collecting.
;
; This vector is backward compatible with pre-v0.20 code in a rudimentary
; manner: the fields which are still supported are in the same locations in
; the returned vector, and the fields which are no longer supported are 0.
; However, the returned vector is longer than the vector in older code,
; since there are new fields.

(define memstats
  (let ((two^29 536870912))  ; (expt 2 29)
    (lambda ()
      (let ((v (make-vector $mstat.vsize 0)))
	(unix:get-resource-usage v)
	(vector (vector-ref v $mstat.ecollections)
		(vector-ref v $mstat.tcollections)
		(vector-ref v $mstat.fcollections)
		(+ (* (vector-ref v $mstat.wcollected-hi) two^29)
		   (vector-ref v $mstat.wcollected-lo))
		(+ (* (vector-ref v $mstat.wallocated-hi) two^29)
		   (vector-ref v $mstat.wallocated-lo))
		0
		(vector-ref v $mstat.gctime)
		0
		(+ (* (vector-ref v $mstat.tscanned-hi) two^29)
		   (vector-ref v $mstat.tscanned-lo))
		(+ (* (vector-ref v $mstat.tallocated-hi) two^29)
		   (vector-ref v $mstat.tallocated-lo))
		(vector-ref v $mstat.framesflushed)
		0
		(vector-ref v $mstat.rtime)
		(vector-ref v $mstat.stime)
		(vector-ref v $mstat.utime)
		(vector-ref v $mstat.minfaults)
		(vector-ref v $mstat.majfaults))))))


; "label-memstats" takes a vector as returned from "memstats" and returns
; a list of lists. Each sublist has a label and the correct value from the
; memstats vector.

(define (label-memstats v)

  (define (oldlabels)
    (list (list 'ephemeral-collections (vector-ref v 0))
	  (list 'tenuring-collections (vector-ref v 1))
	  (list 'full-collections (vector-ref v 2))
	  (list 'words-collected (vector-ref v 3))
	  (list 'words-allocated (vector-ref v 4))
	  ; unsupported in v0.20: words copied, loc 5
	  (list 'time-collecting (vector-ref v 6))
	  ; unsupported in v0.20: time last collection, loc 7
	  (list 'transactions-scanned (vector-ref v 8))
	  (list 'transactions-registered (vector-ref v 9))
	  (list 'frames-flushed (vector-ref v 10))	
	  ; unsupported in v0.20: words allocated to flushed frames, loc 11
	  ))

  (define (newlabels)
    (list (list 'real-time (vector-ref v 12))
	  (list 'system-time (vector-ref v 13))
	  (list 'user-time (Vector-ref v 14))
	  (list 'minor-faults (vector-ref v 15))
	  (list 'major-faults (vector-ref v 16))))

  (let ((old (oldlabels)))
    (if (> (vector-length v) 12)
	(append old (newlabels))
	old)))


; Takes a vector as returned from memstats and displays it with useful
; labels. 

(define (display-memstats v)
  (display "Ephemeral collections: ") (display (vector-ref v 0)) (newline)
  (display "Tenuring collections: ") (display (vector-ref v 1)) (newline)
  (display "Full collections: ") (display (vector-ref v 2)) (newline)
  (display "Words collected: ") (display (vector-ref v 3)) (newline)
  (display "Words allocated: ") (display (vector-ref v 4)) (newline)
; Unsupported
;  (display "Words copied: ") (display (vector-ref v 5)) (newline)
  (display "Time spent in collector: ") (display (vector-ref v 6)) 
  (display " ms") (newline)
; Unsupported
;  (display "Time spent in last collection: ") (display (vector-ref v 7))
;  (display " ms") (newline)
  (display "Transactions scanned: ") (display (Vector-ref v 8)) (newline)
  (display "Transactions registered: ") (display (vector-ref v 9)) (newline)
  (display "Stack frames flushed: ") (display (vector-ref v 10)) (newline)
; Unsupported
;  (display "Heap words allocated to flushed frames: ") 
;  (display (vector-ref v 11)) (newline)
  (if (> (vector-length v) 12)
      (begin
	(display "Real time: ") (display (vector-ref v 12))
	(display " ms") (newline)
	(display "Sys time: ") (display (vector-ref v 13))
	(display " ms") (newline)
	(display "User time: ") (display (vector-ref v 14))
	(display " ms") (newline)
	(display "Minor faults: ") (display (vector-ref v 15)) (newline)
	(display "Major faults: ") (display (vector-ref v 16)) (newline)))
  #t)


; Run a thunk and print out stats afterwards about how long it took and
; how much memory was used. This is accurate only if a collection is performed
; immediately prior to calling this procedure, and in fact a collection should
; be performed after the thunk returns as well to get a completely accurate
; picture. See comments in the code.
;
; Returns the result of the thunk.

(define (run-with-stats thunk)

  (define (munge-stats s1 s2)
    (vector (- (vector-ref s2 0) (vector-ref s1 0))
	    (- (vector-ref s2 1) (vector-ref s1 1))
	    (- (vector-ref s2 2) (vector-ref s1 2))
	    (- (vector-ref s2 3) (vector-ref s1 3))
	    (- (vector-ref s2 4) (vector-ref s1 4))
	    (- (vector-ref s2 5) (vector-ref s1 5))
	    (- (vector-ref s2 6) (vector-ref s1 6))
	    (vector-ref s2 7)
	    (- (vector-ref s2 8) (vector-ref s1 8))
	    (- (vector-ref s2 9) (vector-ref s1 9))
	    (- (vector-ref s2 10) (vector-ref s1 10))
	    (- (vector-ref s2 11) (vector-ref s1 11))
	    (- (vector-ref s2 12) (vector-ref s1 12))
	    (- (vector-ref s2 13) (vector-ref s1 13))
	    (- (vector-ref s2 14) (vector-ref s1 14))
	    (- (vector-ref s2 15) (vector-ref s1 15))
	    (- (vector-ref s2 16) (vector-ref s1 16))))
  
  (let* ((s1 (memstats))
	 (r  (thunk))
	 (s2 (memstats)))
    (display-memstats (munge-stats s1 s2))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Old stuff (amazing how this changes)

; Return resource parameters in a vector. Obsolete.

(define (resource-usage)
  (error "The procedure memstats:resource-usage is no longer supported."))

; Return elapsed time. Very obsolete.

(define (getrusage)
  (error "The procedure memstats:getrusage is no longer supported."))

; Put resource data in resouce vector. Supplanted by OS-specific stuff.

(define sys$resource-usage!
  (lambda (x)
    (error "The primitive sys$resource-usage! is no longer supported.")))

; eof
