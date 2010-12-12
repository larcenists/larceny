(define (count-promotions stats-vec)
  (apply + (map memstats-gen-promotions (vector->list (memstats-generations stats-vec)))))
(define (count-collections stats-vec)
  (apply + (map memstats-gen-collections (vector->list (memstats-generations stats-vec)))))

(define (simpler-run-with-stats thunk)
  (define (mprint . rest)
    (for-each display rest) (newline))
  (define (adjust-number-to num width)
    (let* ((n (number->string num))
           (pad (make-string (max 0 (- width (string-length n))) #\space)))
      (string-append pad n)))
  (define (pr elapsed gc-elapsed words-mem-max collections promotions pause)
    (mprint "Elapsed time: "    (adjust-number-to elapsed       9)  " ms " 
            "Elapsed GC time: " (adjust-number-to gc-elapsed    9)  " ms "
            "Max words: "       (adjust-number-to words-mem-max 12) " words "
            "Collections: "     (adjust-number-to collections   9)  " "
            "Promotions: "      (adjust-number-to promotions    9)  " "
            "Max pause: "       (adjust-number-to pause         6)  " ms "
            ))
  (define (print-stats s1 s2)
    (pr (- (memstats-elapsed-time s2) (memstats-elapsed-time s1))
        (- (memstats-gc-total-elapsed-time s2) (memstats-gc-total-elapsed-time s1))
        (memstats-mem-allocated-max s2)
        (- (count-collections s2) (count-collections s1))
        (- (count-promotions s2) (count-promotions s1))
        (memstats-gc-max-truegc-elapsed-time s2)
        ))
  (let* ((s1 (memstats))
         (r  (thunk))
         (s2 (memstats)))
    (print-stats s1 s2)
    r))

(define (simpler-run-benchmark name iterations thunk ok?)
  (define (loop n result) (if (zero? n) result (loop (- n 1) (thunk))))
  (display name)
  (display " ")
  (display (make-string (- 78 (string-length name)) #\-))
  (newline)
  (let ((result #f))
    (simpler-run-with-stats (lambda () 
                              (set! result (loop iterations (unspecified)))))
    (if (not (ok? result))
        (error "Benchmark program returned wrong result: " result))
    (unspecified)))

(define run-benchmark
  (let ((run-bench simpler-run-benchmark))
    (lambda args
      (let ((name  (list-ref args 0))
            (iters (list-ref args 1))
            (check (list-ref args 2))
            (proc  (list-ref args 3))
            (feed  (cddddr args)))
        (let ((thunk (apply proc feed)))
          (run-bench name iters thunk check))))))
