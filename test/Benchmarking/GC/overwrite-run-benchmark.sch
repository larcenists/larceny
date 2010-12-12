;; This file redefined run-benchmark to a version that produces 

;; Here are some memstats observer functions of note:
;; memstats-allocated 
;; memstats-gc-reclaimed
;; memstats-elapsed-time
;; memstats-user-time
;; memstats-system-time 
;; memstats-gc-total-elapsed-time
;; memstats-gc-total-cpu-time
;; memstats-gc-max-truegc-elapsed-time
;; memstats-gc-max-truegc-cpu-time
;; memstats-gc-max-cheney-elapsed-time
;; memstats-gc-max-cheney-cpu-time
;; memstats-gc-max-remset-scan-elapsed-time
;; memstats-gc-max-remset-scan-cpu-time
;; memstats-gc-max-entries-remset-scan
;;   memstats-gc-total-remset-scan-elapsed-time 
;;   memstats-gc-total-remset-scan-cpu-time 
;;   memstats-gc-total-entries-remset-scan
;;   memstats-gc-remset-scan-count
;; 
;; memstats-gc-all-major-faults-during-gcs
;; memstats-gc-all-minor-faults-during-gcs
;; memstats-gc-major-faults-during-max-truegc-pause
;; memstats-gc-minor-faults-during-max-truegc-pause

(define *last-stashed-stats* #f)

(error-handler 
 (lambda l 
   (decode-error l)
   (display "bench DIED!") (newline) (exit 118)))

;; tree-diff : [F number] [F Number] -> [F Number]
(define (tree-diff t2-orig t1-orig)
  (let rec ((t2 t2-orig) (t1 t1-orig))
    (cond ((and (number? t1) (number? t2)) (- t2 t1))
          ((and (pair? t1) (pair? t2))     (cons (rec (car t2) (car t1))
                                                 (rec (cdr t2) (cdr t1))))
          ((and (null? t1) (null? t2))     '())
          ((and (vector? t1) (vector? t2)) (list->vector
                                            (rec (vector->list t2) 
                                                 (vector->list t1))))
          ((and (symbol? t1) (symbol? t2) (eq? t1 t2)) t1)
          (else (error 'tree-diff "mismatch of ~a vs ~a in ~a ~a" 
                       t2 t1 t2-orig t1-orig)))))

(define (run-with-stats/stashing* thunk)

  (define (totalled-stats s)
    `(allocated:      ,(memstats-allocated s)
      gc-reclaimed:   ,(memstats-gc-reclaimed s)
      elapsed-time:   ,(memstats-elapsed-time s)
      user-time:      ,(memstats-user-time s)
      system-time:    ,(memstats-system-time s)
      gc-total-time:  (elapsed ,(memstats-gc-total-elapsed-time s)
                       cpu     ,(memstats-gc-total-cpu-time s)
                       faults-major ,(memstats-gc-all-major-faults-during-gcs s)
                       faults-minor ,(memstats-gc-all-minor-faults-during-gcs s)
                       count
                       ,(let ((gcs 0))
                          (do ((i 0 (+ i 1)))
                              ((= i (vector-length (memstats-generations s))))
                            (let ((x (vector-ref (memstats-generations s) i)))
                              (set! gcs (+ gcs (memstats-gen-collections x)))
                              (set! gcs (+ gcs (memstats-gen-promotions x)))))
                          gcs))
      mark-time:      (elapsed ,(memstats-mark-elapsed s)
                       cpu     ,(memstats-mark-cpu s)
                       count   ,(memstats-mark-count s))
      summarize-time: (elapsed ,(memstats-summarize-elapsed s)
                       cpu     ,(memstats-summarize-cpu s)
                       count   ,(memstats-summarize-count s))))

  (define (trackmax-stats s)
    `(gc-max-pause:
      (elapsed ,(memstats-gc-max-truegc-elapsed-time s)
       cpu     ,(memstats-gc-max-truegc-cpu-time s)
       faults-major ,(memstats-gc-major-faults-during-max-truegc-pause s)
       faults-minor ,(memstats-gc-minor-faults-during-max-truegc-pause s))
      gc-max-cheney:
      (elapsed ,(memstats-gc-max-cheney-elapsed-time s)
       cpu     ,(memstats-gc-max-cheney-cpu-time s))
      gc-max-summar:
      (elapsed ,(memstats-gc-max-summar-elapsed-time s)
       cpu     ,(memstats-gc-max-summar-cpu-time s))
      gc-max-smircy-mark:
      (elapsed ,(memstats-gc-max-smircy-mark-elapsed-time s)
       cpu     ,(memstats-gc-max-smircy-mark-cpu-time s))
      gc-max-smircy-refine:
      (elapsed ,(memstats-gc-max-smircy-refine-elapsed-time s)
       cpu     ,(memstats-gc-max-smircy-refine-cpu-time s))
      gc-max-remset-scan:
      (elapsed ,(memstats-gc-max-remset-scan-elapsed-time s)
       cpu     ,(memstats-gc-max-remset-scan-cpu-time s)
       entries ,(memstats-gc-max-entries-remset-scan s))
      ))

  (define (stashed-stats s1 s2)
    (list (tree-diff (totalled-stats s2)
                     (totalled-stats s1))
          (trackmax-stats s2)))

  (let* ((s1 (memstats))
         (r  (thunk))
         (s2 (memstats)))
    (values r (stashed-stats s1 s2))))

;; run-benchmark : String Nat (X -> Boolean) (A ... -> (-> X)) A ... -> unspec
;; run-benchmark : String Nat (-> X)         (X ->Boolean)           -> unspec
;; This is meant to handle both the Larceny run-benchmark interface
;; and the Feeley (CrossPlatform) run-benchmark interface.  Its a
;; little tricky in the case when args is null, because in that case I
;; need to detect if f2 is the predicate and f1 the thunk, or f1 is
;; the predicate and f2 is the thunk-creating thunk.

(define (run-benchmark name iterations f1 f2 . args)
  (define (exact=? n1 n2) (and (exact? n1) (exact? n2) (= n1 n2)))
  (cond
   ((not (null? args))
    (run-benchmark/core name iterations (apply f2 args) f1)) ;; Feeley
   ((or (exact=? 0 (procedure-arity f2))
        (exact=? 1 (procedure-arity f1)))
    (run-benchmark/core name iterations (f2) f1)) ;; Feeley
   ((or (exact=? 0 (procedure-arity f1))
        (exact=? 1 (procedure-arity f2)))
    (run-benchmark/core name iterations f1 f2)) ;; Larceny
   (else
    (display "cannot infer which run-benchmark; assuming Larceny") (newline)
    (run-benchmark/core name iterations f1 f2))))

(define (run-benchmark/core name iterations thunk ok?)

  (define (loop n last-result)
    (if (zero? n)
        last-result
        (loop (- n 1) (thunk))))

  ;; Leaving in benchmark header output since it helps separate the
  ;; extraneous output produced by many of the benchmarks

  (newline)
  (display "--------------------------------------------------------")
  (newline)
  (display name)
  (newline)

  (let ((result #f))
    (call-with-values (lambda ()
                        (run-with-stats/stashing 
                         (lambda ()
                           (set! result (loop iterations (unspecified))))))
      (lambda (ignore-return-val stashed-stats)
        (if (not (ok? result))
            (error "Benchmark program returned wrong result: " result))
        (set! *last-stashed-stats* (cons (list 'name: name) stashed-stats))
        stashed-stats))))

(define run-with-stats/stashing
  (let ((run-with-stats run-with-stats))
    (lambda (thunk)
      (run-with-stats/stashing* (lambda () (run-with-stats thunk))))))

(define (dump-stashed-and-current-stats output-file)
  (call-with-output-file output-file
    (lambda (p)
      (load "parse-stats-output.sch")
      (let ((f (lambda (s x)
                 (display   "    " p)
                 (write (list s x) p)
                 (newline p))))
        (f 'last-stashed-stats
           *last-stashed-stats*)
        (f 'stats-dump
           (stats-read))))))
