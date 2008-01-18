; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; System statistics are maintained by the parts of the system written in
; C and are obtained by a call on the primitive sys$get-resource-usage,
; which returns a vector of resource information.
;
; The fields of the returned vector and their offsets are defined by 
; constants that are defined in Rts/globals.cfg.
;
; All times are in milliseconds; all memory sizes are in words.
;
; Some entries have a high and a low part so that the low levels of the
; system do not have to use bignums to keep the counts.  Arguably, the
; time fields should also be split, as they will overflow after 149
; hours or so.

($$trace "memstats")

; Take raw data and return a vector with slightly processed data.  The
; offsets in the returned vector are documented in the User's Manual.
; That will change (the accessors will become the documented way of getting
; to the data) but for now, tread carefully when changing anything.

(define (memstats)

  (define two^29 536870912)

  (define (bignum v n)
    (+ (* (vector-ref v n) two^29)
       (vector-ref v (+ n 1))))

  (define (make-remset-stats r)
    (vector (vector-ref r $mstat.r-alloc)     ; new meaning (but close)
            (vector-ref r $mstat.r-used)      ; new meaning (but close)
            (vector-ref r $mstat.r-live)      ; new meaning (but close)
            0                                 ; removed: hash-alloc
            (bignum r $mstat.r-hrec-hi)
            (bignum r $mstat.r-hrem-hi)       ;           #5
            (bignum r $mstat.r-hscan-hi)
            (bignum r $mstat.r-wscan-hi)
            (bignum r $mstat.r-ssbrec-hi)
            (vector-ref r $mstat.r-cleared)   ; new field
            (vector-ref r $mstat.r-scanned)   ; new field #10
            (vector-ref r $mstat.r-compacted) ; new field
            (vector-ref r $mstat.r-max-size)  ; new field
            (vector-ref r $mstat.r-major-id)  ; new field
            (vector-ref r $mstat.r-minor-id)  ; new field
            (vector-ref r $mstat.r-max-hscan) ;           #15
            (vector-ref r $mstat.r-max-wscan)
            ))

  (define (make-generation-stats g)
    (vector (vector-ref g $mstat.g-gc-count)
            (vector-ref g $mstat.g-prom-count)
            (+ (vector-ref g $mstat.g-gctime)
               (vector-ref g $mstat.g-promtime))
            (vector-ref g $mstat.g-wlive) ; new meaning (but close)
            #f                            ; removed: np young flag
            #f                            ; removed: np old flag
            0                             ; removed: np 'j'
            0                             ; removed: np 'k'
            (vector-ref g $mstat.g-alloc) ; new meaning (but close)
            (vector-ref g $mstat.g-target)
            (vector-ref g $mstat.g-promtime)
            (vector-ref g $mstat.g-major-id) ; new field
            (vector-ref g $mstat.g-minor-id) ; new field
            (+ (vector-ref g $mstat.g-gctime-cpu)
               (vector-ref g $mstat.g-promtime-cpu))
            (vector-ref g $mstat.g-promtime-cpu)
            ))

  (define (make-basic-vector v)
    (vector (bignum v $mstat.wallocated-hi)
            (bignum v $mstat.wcollected-hi)
            (bignum v $mstat.wcopied-hi)
            0                           ; total gc elapsed time
            0                           ; words live
            0                           ; removed: generation of last gc
            0                           ; removed: type of last gc
            #f                          ; generation information
            #f                          ; remembered-set information
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
            #f                          ; removed: np remembered set
            (vector-ref v $mstat.heap-max)
            0                           ; total promotion elapsed time
            (vector-ref v $mstat.swb-total)
            (vector-ref v $mstat.wastage) ; new fields    #30
            (vector-ref v $mstat.remset-max)
            (vector-ref v $mstat.rts-max)
            (vector-ref v $mstat.wastage-max)
            (vector-ref v $mstat.full-gcs)
            (vector-ref v $mstat.full-gctime)
            (bignum v $mstat.full-copied-hi)
            (bignum v $mstat.full-moved-hi)
            (bignum v $mstat.full-marked-hi)
            (bignum v $mstat.full-words-marked-hi)
            (bignum v $mstat.full-pointers-traced-hi) ;   # 40
            (vector-ref v $mstat.words-mem)
            (vector-ref v $mstat.words-mem-max)
            (vector-ref v $mstat.full-gctime-cpu)
            0                           ; total gc cpu time
            0                           ; total promotion cpu time
            (vector-ref v $mstat.dof-resets)
            (vector-ref v $mstat.dof-repeats)
            0                           ; GC event counters
            (vector-ref v $mstat.max-gctime)
            (vector-ref v $mstat.max-gctime-cpu)      ;   # 50
            (vector-ref v $mstat.max-remset-scan)
            (vector-ref v $mstat.max-remset-scan-cpu)
            (vector-ref v $mstat.total-remset-scan)
            (vector-ref v $mstat.total-remset-scan-cpu)
            (vector-ref v $mstat.remset-scan-count)
            (vector-ref v $mstat.max-entries-remset-scan)
            (vector-ref v $mstat.total-entries-remset-scan)
            ))

  (define (make-gc-event-vector v)
    (vector (bignum v $mstat.gce-gctime-hi)
            (bignum v $mstat.gce-promtime-hi)
            (bignum v $mstat.gce-free-unused-hi)
            (bignum v $mstat.gce-root-scan-gc-hi)
            (bignum v $mstat.gce-root-scan-prom-hi)
            (bignum v $mstat.gce-los-sweep-gc-hi)
            (bignum v $mstat.gce-los-sweep-prom-hi)
            (bignum v $mstat.gce-remset-scan-gc-hi)
            (bignum v $mstat.gce-remset-scan-prom-hi)
            (bignum v $mstat.gce-tospace-scan-gc-hi)
            (bignum v $mstat.gce-tospace-scan-prom-hi) ;    #10
            (bignum v $mstat.gce-reset-after-gc-hi)
            (bignum v $mstat.gce-decrement-after-gc-hi)
            (bignum v $mstat.gce-dof-remset-scan-hi)
            (bignum v $mstat.gce-sweep-shadow-hi)
            (bignum v $mstat.gce-msgc-mark-hi)
            (bignum v $mstat.gce-sweep-dof-sets-hi)
            (bignum v $mstat.gce-sweep-remset-hi)
            (bignum v $mstat.gce-sweep-los-hi)
            (bignum v $mstat.gce-assimilate-prom-hi)
            (bignum v $mstat.gce-assimilate-gc-hi) ;    #20
            (vector-ref v $mstat.gce-copied-by-gc)
            (vector-ref v $mstat.gce-copied-by-prom)
            (vector-ref v $mstat.gce-words-forwarded)
            (vector-ref v $mstat.gce-ptrs-forwarded)
            (vector-ref v $mstat.gce-gc-barrier-hit)
            (vector-ref v $mstat.gce-remset-lo-scanned)
            (vector-ref v $mstat.gce-remset-low-scanned)))

  (define (make-remset-vector rems)
    (let* ((len (vector-length rems))
           (r   (make-vector len #f)))
      (do ((i 0 (+ i 1)))
          ((= i len)
           r)
        (vector-set! r i (make-remset-stats (vector-ref rems i))))))

  (define (make-generation-vector gens)
    (let* ((len (vector-length gens))
           (g   (make-vector len #f)))
      (do ((i 0 (+ i 1)))
          ((= i len) 
           g)
        (vector-set! g i (make-generation-stats (vector-ref gens i))))))

  ; Fill in some of the removed fields:
  ;   - total elapsed gc+promotion time for slot 3
  ;   - total cpu gc+promotion time for slot 44
  ;   - total words live for slot 4
  ;   - total promotion time for slot 28
  ;   - total promotion time for slot 45

  (define (compute-useful-data v)
    (define generations (vector-ref v 7))
    (define (g i) 
      (vector-ref generations i))

    (do ((i        0 (+ i 1))
         (gcreal   0 (+ gcreal (memstats-gen-total-elapsed-time (g i))))
         (promreal 0 (+ promreal (memstats-gen-promotion-elapsed-time (g i))))
         (gccpu    0 (+ gccpu (memstats-gen-total-cpu-time (g i))))
         (promcpu  0 (+ promcpu (memstats-gen-promotion-cpu-time (g i))))
         (live     0 (+ live (memstats-gen-live-now (g i)))))
        ((= i (vector-length generations))
         (vector-set! v 3 (+ gcreal (memstats-fullgc-elapsed-time v)))
         (vector-set! v 44 (+ gccpu (memstats-fullgc-cpu-time v)))
         (vector-set! v 4 live)
         (vector-set! v 28 promreal)
         (vector-set! v 45 promcpu))))

  (let* ((raw-stats      (sys$get-resource-usage))
         (stats-vec      (make-basic-vector raw-stats)))
    (vector-set! stats-vec 7
                 (make-generation-vector 
                  (vector-ref raw-stats $mstat.generations)))
    (vector-set! stats-vec 8
                 (make-remset-vector 
                  (vector-ref raw-stats $mstat.remsets)))
    (vector-set! stats-vec 48
                 (make-gc-event-vector raw-stats))
    (compute-useful-data stats-vec)
    stats-vec))

; Accessors for memstats structure

(define (memstats-allocated v) (vector-ref v 0))
(define (memstats-gc-reclaimed v) (vector-ref v 1))
(define (memstats-gc-copied v) (vector-ref v 2))
(define (memstats-gc-total-elapsed-time v) (vector-ref v 3))
(define (memstats-gc-total-cpu-time v) (vector-ref v 44))
(define (memstats-gc-max-elapsed-time v) (vector-ref v 49))
(define (memstats-gc-max-cpu-time v) (vector-ref v 50))
(define (memstats-gc-max-remset-scan-elapsed-time v) (vector-ref v 51))
(define (memstats-gc-max-remset-scan-cpu-time v) (vector-ref v 52))
(define (memstats-gc-total-remset-scan-elapsed-time v) (vector-ref v 53))
(define (memstats-gc-total-remset-scan-cpu-time v) (vector-ref v 54))
(define (memstats-gc-remset-scan-count v) (vector-ref v 55))
(define (memstats-gc-max-entries-remset-scan v) (vector-ref v 56))
(define (memstats-gc-total-entries-remset-scan v) (vector-ref v 57))
(define (memstats-gc-promotion-elapsed-time v) (vector-ref v 28))
(define (memstats-gc-promotion-cpu-time v) (vector-ref v 45))
(define (memstats-heap-allocated-now v) (vector-ref v 13))
(define (memstats-heap-allocated-max v) (vector-ref v 27))
(define (memstats-heap-live-now v) (vector-ref v 4))
(define (memstats-remsets-allocated-now v) (vector-ref v 14))
(define (memstats-remsets-allocated-max v) (vector-ref v 31))
(define (memstats-rts-allocated-now v) (vector-ref v 15))
(define (memstats-rts-allocated-max v) (vector-ref v 32))
(define (memstats-heap-fragmentation-now v) (vector-ref v 30))
(define (memstats-heap-fragmentation-max v) (vector-ref v 33))
(define (memstats-mem-allocated-now v) (vector-ref v 41))
(define (memstats-mem-allocated-max v) (vector-ref v 42))
(define (memstats-generations v) (vector-ref v 7))
(define (memstats-remsets v) (vector-ref v 8))
(define (memstats-frames-flushed v) (vector-ref v 9))
(define (memstats-words-flushed v) (vector-ref v 10))
(define (memstats-stacks-created v) (vector-ref v 11))
(define (memstats-frames-restored v) (vector-ref v 12))
(define (memstats-swb-total-assignments v) (vector-ref v 29))
(define (memstats-swb-vector-assignments v) (vector-ref v 16))
(define (memstats-swb-lhs-young-or-remembered v) (vector-ref v 17))
(define (memstats-swb-rhs-immediate v) (vector-ref v 18))
(define (memstats-swb-not-intergenerational v) (vector-ref v 19))
(define (memstats-swb-transactions v) (vector-ref v 20))
(define (memstats-elapsed-time v) (vector-ref v 21))
(define (memstats-system-time v) (vector-ref v 22))
(define (memstats-user-time v) (vector-ref v 23))
(define (memstats-minor-faults v) (vector-ref v 24))
(define (memstats-major-faults v) (vector-ref v 25))
(define (memstats-fullgc-collections v) (vector-ref v 34))
(define (memstats-fullgc-elapsed-time v) (vector-ref v 35))
(define (memstats-fullgc-cpu-time v) (vector-ref v 43))
(define (memstats-fullgc-copied v) (vector-ref v 36))
(define (memstats-fullgc-moved v) (vector-ref v 37))
(define (memstats-fullgc-marked v) (vector-ref v 38))
(define (memstats-fullgc-words-marked v) (vector-ref v 39))
(define (memstats-fullgc-traced v) (vector-ref v 40))
(define (memstats-dofgc-resets v) (vector-ref v 46))
(define (memstats-dofgc-repeats v) (vector-ref v 47))
(define (memstats-gc-accounting v) (vector-ref v 48))

; Accessors for GC accounting substructure

(define (memstats-acc-gc v) (vector-ref v 0))
(define (memstats-acc-promotion v) (vector-ref v 1))
(define (memstats-acc-free-unused v) (vector-ref v 2))
(define (memstats-acc-root-scan-gc v) (vector-ref v 3))
(define (memstats-acc-root-scan-promotion v) (vector-ref v 4))
(define (memstats-acc-los-sweep-gc v) (vector-ref v 5))
(define (memstats-acc-los-sweep-promotion v) (vector-ref v 6))
(define (memstats-acc-remset-scan-gc v) (vector-ref v 7))
(define (memstats-acc-remset-scan-promotion v) (vector-ref v 8))
(define (memstats-acc-tospace-scan-gc v) (vector-ref v 9))
(define (memstats-acc-tospace-scan-promotion v) (vector-ref v 10))
(define (memstats-acc-reset-after-gc v) (vector-ref v 11))
(define (memstats-acc-decrement-after-gc v) (vector-ref v 12))
(define (memstats-acc-dof-remset-scan v) (vector-ref v 13))
(define (memstats-acc-sweep-shadow v) (vector-ref v 14))
(define (memstats-acc-msgc-mark v) (vector-ref v 15))
(define (memstats-acc-sweep-dof-sets v) (vector-ref v 16))
(define (memstats-acc-sweep-remset v) (vector-ref v 17))
(define (memstats-acc-sweep-los v) (vector-ref v 18))
(define (memstats-acc-assimilate-promotion v) (vector-ref v 19))
(define (memstats-acc-assimilate-gc v) (vector-ref v 20))
(define (memstats-acc-words-copied-by-gc v) (vector-ref v 21))
(define (memstats-acc-words-copied-by-promotion v) (vector-ref v 22))
(define (memstats-acc-words-forwarded v) (vector-ref v 23))
(define (memstats-acc-pointers-forwarded v) (vector-ref v 24))
(define (memstats-acc-gc-barrier-hits v) (vector-ref v 25))
(define (memstats-acc-remset-large-objects-scanned v) (vector-ref v 26))
(define (memstats-acc-remset-large-object-words-scanned v) (vector-ref v 27))

; Accessors for generation substructures

(define (memstats-gen-major-id v) (vector-ref v 11))
(define (memstats-gen-minor-id v) (vector-ref v 12))
(define (memstats-gen-collections v) (vector-ref v 0))
(define (memstats-gen-promotions v) (vector-ref v 1))
(define (memstats-gen-total-elapsed-time v) (vector-ref v 2))
(define (memstats-gen-total-cpu-time v) (vector-ref v 13))
(define (memstats-gen-promotion-elapsed-time v) (vector-ref v 10))
(define (memstats-gen-promotion-cpu-time v) (vector-ref v 14))
(define (memstats-gen-target-size-now v) (vector-ref v 9))
(define (memstats-gen-allocated-now v) (vector-ref v 8))
(define (memstats-gen-live-now v) (vector-ref v 3))

; Accessors for remset substructures

(define (memstats-remset-major-id v) (vector-ref v 13))
(define (memstats-remset-minor-id v) (vector-ref v 14))
(define (memstats-remset-allocated-max v) (vector-ref v 12))
(define (memstats-remset-allocated-now v) (vector-ref v 0))
(define (memstats-remset-used-now v) (vector-ref v 1))
(define (memstats-remset-live-now v) (vector-ref v 2))
(define (memstats-remset-recorded v) (vector-ref v 4))
(define (memstats-remset-removed v) (vector-ref v 5))
(define (memstats-remset-scanned v) (vector-ref v 6))
(define (memstats-remset-object-words-scanned v) (vector-ref v 7))
(define (memstats-remset-transactions v) (vector-ref v 8))
(define (memstats-remset-times-cleared v) (vector-ref v 9))
(define (memstats-remset-times-scanned v) (vector-ref v 10))
(define (memstats-remset-times-compacted v) (vector-ref v 11))
(define (memstats-remset-max-scanned v) (vector-ref v 15))
(define (memstats-remset-max-object-words-scanned v) (vector-ref v 16))

; Takes a vector as returned from memstats and displays it with useful
; labels.

(define (display-memstats v . rest)

  (define full? (memq 'full rest))

  (define minimal? (memq 'minimal rest))
    
  (define (mprint . rest)
    (for-each display rest) (newline))

  (define (sorter major minor)
    (lambda (a b)
      (or (< (major a) (major b))
          (and (= (major a) (major b))
               (< (minor a) (minor b))))))

  (define (print-generation g)
    (mprint "  Generation #" 
            (memstats-gen-major-id g) "," (memstats-gen-minor-id g))
    (mprint "    Collections....: " (memstats-gen-collections g))
    (mprint "    Promotions.....: " (memstats-gen-promotions g))
    (mprint "    GC time (ms)...: " 
            (memstats-gen-total-elapsed-time g) " real, " 
            (memstats-gen-total-cpu-time g) " cpu")
    (mprint "      promotion....: " 
            (memstats-gen-promotion-elapsed-time g) " real, "
            (memstats-gen-promotion-cpu-time g) " cpu")
    (mprint "      collection...: " 
            (- (memstats-gen-total-elapsed-time g)
               (memstats-gen-promotion-elapsed-time g)) " real, "
            (- (memstats-gen-total-cpu-time g)
               (memstats-gen-promotion-cpu-time g)) " cpu")
    (mprint "    Generation size: " (memstats-gen-target-size-now g))
    (mprint "      allocated....: " (memstats-gen-allocated-now g))
    (mprint "      in use.......: " (memstats-gen-live-now g)))

  (define (print-per-generation)
    (mprint "Per-generation information")
    (for-each print-generation 
              (sort (vector->list (memstats-generations v))
                    (sorter memstats-gen-major-id memstats-gen-minor-id))))

  (define (print-remset r)
    (mprint "  Remembered set #" 
            (memstats-remset-major-id r) "," (memstats-remset-minor-id r))
    (mprint "    Max size............: " (memstats-remset-allocated-max r))
    (mprint "    Allocated...........: " (memstats-remset-allocated-now r))
    (mprint "    Used................: " (memstats-remset-used-now r))
    (mprint "    Live................: " (memstats-remset-live-now r))
    (mprint "    SSB transactions....: " (memstats-remset-transactions r))
    (mprint "    Entries recorded....: " (memstats-remset-recorded r))
    (mprint "    Entries removed.....: " (memstats-remset-removed r))
    (mprint "    Entries scanned.....: " (memstats-remset-scanned r))
    (mprint "    Object words scanned: " 
            (memstats-remset-object-words-scanned r))
    (mprint "    Times cleared.......: " (memstats-remset-times-cleared r))
    (mprint "    Times scanned.......: " (memstats-remset-times-scanned r))
    (mprint "    Times compacted.....: " (memstats-remset-times-compacted r)))

  (define (print-per-remset)
    (if full?
        (begin 
          (mprint "Per-remembered-set information")
          (for-each print-remset
                    (sort (vector->list (memstats-remsets v))
                          (sorter memstats-remset-major-id 
                                  memstats-remset-minor-id))))))

  (define (print-overall)
    (mprint "Overall memory statistics")
    (mprint "  Generations....: " (vector-length (memstats-generations v)))
    (mprint "  Words allocated: " (memstats-allocated v))
    (mprint "  Words reclaimed: " (memstats-gc-reclaimed v))
    (mprint "  Words copied...: " (memstats-gc-copied v))
    (mprint "  GC time (ms)...: " 
            (memstats-gc-total-elapsed-time v) " real, "
            (memstats-gc-total-cpu-time v) " cpu")
    (mprint "    promotion....: " 
            (memstats-gc-promotion-elapsed-time v) " real, "
            (memstats-gc-promotion-cpu-time v) " cpu")
    (mprint "    collection...: " 
            (- (memstats-gc-total-elapsed-time v)
               (memstats-fullgc-elapsed-time v)
               (memstats-gc-promotion-elapsed-time v)) " real, "
            (- (memstats-gc-total-cpu-time v)
               (memstats-fullgc-cpu-time v)
               (memstats-gc-promotion-cpu-time v)) " cpu")
    (mprint "    full gc......: " 
            (memstats-fullgc-elapsed-time v) " real, "
            (memstats-fullgc-cpu-time v) " cpu")
    (mprint "  Heap allocation")
    (mprint "    maximum......: " (memstats-heap-allocated-max v))
    (mprint "    current......: " (memstats-heap-allocated-now v))
    (mprint "    current used.: " (memstats-heap-live-now v))
    (mprint "  Heap fragmentation")
    (mprint "    maximum......: " (memstats-heap-fragmentation-max v))
    (mprint "    current......: " (memstats-heap-fragmentation-now v))
    (mprint "  Remset allocation")
    (mprint "    maximum......: " (memstats-remsets-allocated-max v))
    (mprint "    current......: " (memstats-remsets-allocated-now v))
    (mprint "  Other RTS allocation")
    (mprint "    maximum......: " (memstats-rts-allocated-max v))
    (mprint "    current......: " (memstats-rts-allocated-now v))
    (mprint "  Total allocation")
    (mprint "    maximum......: " (memstats-mem-allocated-max v))
    (mprint "    current......: " (memstats-mem-allocated-now v))
    (mprint "  Full collection")
    (mprint "    collections..: " (memstats-fullgc-collections v))
    (mprint "    objs marked..: " (memstats-fullgc-marked v))
    (mprint "    ptrs traced..: " (memstats-fullgc-traced v))
    )

  (define (print-stack)
    (mprint "Stack information")
    (mprint "  Frames flushed.: " (memstats-frames-flushed v))
    (mprint "  Words flushed..: " (memstats-words-flushed v))
    (mprint "  Frames restored: " (memstats-frames-restored v))
    (mprint "  Stacks created.: " (memstats-stacks-created v)))

  (define (print-simulated-barrier)
    (if full?
        (begin
          (mprint "Simulated write barrier")
          (mprint "  Total assigns..: " (memstats-swb-total-assignments v))
          (mprint "  Vector assigns.: " (memstats-swb-vector-assignments v))
          (mprint "  LHS young/known: " 
                  (memstats-swb-lhs-young-or-remembered v))
          (mprint "  RHS immediate..: " (memstats-swb-rhs-immediate v))
          (mprint "  Not old->young.: " (memstats-swb-not-intergenerational v))
          (mprint "  Transactions...: " (memstats-swb-transactions v)))))

  (define (print-misc)
    (mprint "Miscellaneous")
    (mprint "  Elapsed time...: " (memstats-elapsed-time v))
    (if full?
        (begin
          (mprint "  User time......: " (memstats-user-time v))
          (mprint "  System time....: " (memstats-system-time v)))
        (begin
          (mprint "  CPU time.......: " (+ (memstats-user-time v)
                                           (memstats-system-time v)))))
    (if full?
        (begin 
          (mprint "  Minor faults...: " (memstats-minor-faults v))
          (mprint "  Major faults...: " (memstats-major-faults v)))))

  (define (print-all)
    (print-overall)
    (print-per-generation)
    (print-per-remset)
    (print-stack)
    (print-simulated-barrier)
    (print-misc))

  (define (print-minimal)
    (mprint "Words allocated: " (memstats-allocated v))
    (mprint "Words reclaimed: " (memstats-gc-reclaimed v))
    (mprint "Elapsed time...: " (memstats-elapsed-time v)
            " (User: " (memstats-user-time v)
            "; System: " (memstats-system-time v) ")")
    (let ((gcs 0))
      (do ((i 0 (+ i 1)))
          ((= i (vector-length (memstats-generations v))))
        (let ((x (vector-ref (memstats-generations v) i)))
          (set! gcs (+ gcs (memstats-gen-collections x)))
          (set! gcs (+ gcs (memstats-gen-promotions x)))))
      (mprint "Elapsed GC time: " (memstats-gc-total-elapsed-time v) 
              " ms (CPU: " (memstats-gc-total-cpu-time v)
              " ms, in " gcs " collections.)"
              " Max pause elapsed: " (memstats-gc-max-elapsed-time v)
              " Max pause CPU: " (memstats-gc-max-cpu-time v)
              )))

  (if minimal?
      (print-minimal)
      (print-all))
  
  (unspecified))


; Run a thunk and print out stats afterwards about how long it took and
; how much memory was used. This is accurate only if a collection is performed
; immediately prior to calling this procedure, and in fact a collection should
; be performed after the thunk returns as well to get a completely accurate
; picture. See comments in the code.
;
; Returns the result of the thunk.

(define (run-with-stats thunk)

  (define (mprint . rest)
    (for-each display rest) (newline))

  (define (pr allocated reclaimed elapsed user system gcs gctime gccpu maxgctime maxgccpu maxscantime maxscancpu maxscanentries avgscantime avgscancpu avgscanentries)
    (mprint "Words allocated: " allocated)
    (mprint "Words reclaimed: " reclaimed)
    (mprint "Elapsed time...: " elapsed
	   " ms (User: " user " ms; System: " system " ms)")
    (mprint "Elapsed GC time: " gctime " ms (CPU: " gccpu 
            " in " gcs " collections.)")
    (mprint "{Max pause elapsed: " maxgctime " ms"
            ", CPU: " maxgccpu " ms} ")
    (mprint "{Max remset scan elapsed: " maxscantime " ms"
            ", CPU: " maxscancpu " ms, entries: " maxscanentries "} ")
    (mprint "{Avg remset scan elapsed: " avgscantime " ms"
            ", CPU: " avgscancpu " ms, entries: " avgscanentries "} ")
    )

  (define (print-stats s1 s2)
    (pr (- (memstats-allocated s2) (memstats-allocated s1))
	(- (memstats-gc-reclaimed s2) (memstats-gc-reclaimed s1))
	(- (memstats-elapsed-time s2) (memstats-elapsed-time s1))
	(- (memstats-user-time s2) (memstats-user-time s1))
	(- (memstats-system-time s2) (memstats-system-time s1))
	(let ((gcs0 0)
	      (gcs1 0))
	  (do ((i 0 (+ i 1)))
	      ((= i (vector-length (memstats-generations s1)))
               (- gcs1 gcs0))
	    (let ((x0 (vector-ref (memstats-generations s1) i))
		  (x1 (vector-ref (memstats-generations s2) i)))
	      (set! gcs0 (+ gcs0 
                            (memstats-gen-collections x0) 
                            (memstats-gen-promotions x0)))
	      (set! gcs1 (+ gcs1 
                            (memstats-gen-collections x1)
                            (memstats-gen-promotions x1))))))
	(- (memstats-gc-total-elapsed-time s2) 
           (memstats-gc-total-elapsed-time s1))
        (- (memstats-gc-total-cpu-time s2)
           (memstats-gc-total-cpu-time s1))
        (memstats-gc-max-elapsed-time s2)
        (memstats-gc-max-cpu-time s2)
        (memstats-gc-max-remset-scan-elapsed-time s2)
        (memstats-gc-max-remset-scan-cpu-time s2)
        (memstats-gc-max-entries-remset-scan s2)
        (if (zero? (memstats-gc-remset-scan-count s2))
            0
            (/ (memstats-gc-total-remset-scan-elapsed-time s2)
               (memstats-gc-remset-scan-count s2)))
        (if (zero? (memstats-gc-remset-scan-count s2))
            0
            (/ (memstats-gc-total-remset-scan-cpu-time s2)
               (memstats-gc-remset-scan-count s2)))
        (if (zero? (memstats-gc-remset-scan-count s2))
            0
            (/ (memstats-gc-total-entries-remset-scan s2)
               (memstats-gc-remset-scan-count s2)))
	))
  
  (let* ((s1 (memstats))
	 (r  (thunk))
	 (s2 (memstats)))
    (print-stats s1 s2)
    r))


; Run-benchmark
;
; (run-benchmark name thunk)
; (run-benchmark name thunk iterations)
; (run-benchmark name iterations thunk ok?)
;
; The 2/3 arg version is compatible with old Larceny releases, and is
; deprecated: a warning is printed if it is used.
;
; The 4 arg version is compatible with benchmarking code that comes 
; with Gambit-C (and superior, because it tests the final value).

(define (run-benchmark . args)
  (case (length args)
    ((2 3) (newline)
	   (display "WARNING: Using old run-benchmark.")
	   (newline)
	   (apply old-run-benchmark args))
    ((4)   (apply new-run-benchmark args))
    (else  (error "Wrong number of arguments to run-benchmark."))))

(define (new-run-benchmark name iterations thunk ok?)

  (define (loop n last-result)
    (if (zero? n)
	last-result
	(loop (- n 1) (thunk))))

  (newline)
  (display "--------------------------------------------------------")
  (newline)
  (display name)
  (newline)
  (let ((result #f))
    (run-with-stats (lambda ()
		      (set! result (loop iterations (unspecified)))))
    (if (not (ok? result))
	(error "Benchmark program returned wrong result: " result))
    (unspecified)))

(define (old-run-benchmark name thunk . rest)
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
