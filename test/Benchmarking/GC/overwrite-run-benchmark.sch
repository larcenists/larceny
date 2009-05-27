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

(define *last-stashed-stats* #f)

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

(define (run-with-stats/stashing thunk)

  (define (totalled-stats s)
    `(allocated:      ,(memstats-allocated s)
      gc-reclaimed:   ,(memstats-gc-reclaimed s)
      elapsed-time:   ,(memstats-elapsed-time s)
      user-time:      ,(memstats-user-time s)
      system-time:    ,(memstats-system-time s)
      gc-total-time:  (elapsed ,(memstats-gc-total-elapsed-time s)
                       cpu     ,(memstats-gc-total-cpu-time s))
      mark-time:      (elapsed ,(memstats-mark-elapsed s)
                       cpu     ,(memstats-mark-cpu s)
                       count   ,(memstats-mark-count s))
      summarize-time: (elapsed ,(memstats-summarize-elapsed s)
                       cpu     ,(memstats-summarize-cpu s)
                       count   ,(memstats-summarize-count s))))

  (define (trackmax-stats s)
    `(gc-max-pause:
      (elapsed ,(memstats-gc-max-truegc-elapsed-time s)
       cpu     ,(memstats-gc-max-truegc-cpu-time s))
      gc-max-cheney:
      (elapsed ,(memstats-gc-max-cheney-elapsed-time s)
       cpu     ,(memstats-gc-max-cheney-cpu-time s))
      gc-max-remset-scan:
      (elapsed ,(memstats-gc-max-remset-scan-elapsed-time s)
       cpu     ,(memstats-gc-max-remset-scan-cpu-time s)
       entries ,(memstats-gc-max-entries-remset-scan s))))

  (define (stashed-stats s1 s2)
    (list (tree-diff (totalled-stats s2)
                     (totalled-stats s1))
          (trackmax-stats s2)))

  (let* ((s1 (memstats))
         (r  (thunk))
         (s2 (memstats)))
    (values r (stashed-stats s1 s2))))

(define (run-benchmark name iterations thunk ok?)

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
        (set! *last-stashed-stats* stashed-stats)
        stashed-stats))))



