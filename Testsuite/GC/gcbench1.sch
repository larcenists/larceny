; Hacked to work around Larceny bug: all objects must be
; small enough to fit in the smallest ephemeral generation.

; Macro-expanded version of gcbench.sch, for systems that
; don't support the hygienic macro system.

(define (print x)
  (display "; ")
  (display x))

(define printdiagnostics
  (lambda ()
    (begin (print " Total memory available= ???????? bytes")
           (print "  Free memory= ???????? bytes")
           (newline))))

'let-class

(define gcbench
  (lambda (kstretchtreedepth_1)
    (letrec
     ((treesize_3 (lambda (i_40) (- (expt 2 (+ i_40 1)) 1)))
      (numiters_3
       (lambda (i_39)
         (quotient (* 2 (treesize_3 kstretchtreedepth_1))
                   (treesize_3 i_39)))))
     (let*
      ((klonglivedtreedepth_4 (- kstretchtreedepth_1 2))
       (karraysize_5 (* 4 (treesize_3 klonglivedtreedepth_4)))
       (kmintreedepth_6 4)
       (kmaxtreedepth_7 klonglivedtreedepth_4))
      (letrec
       ((populate_10
         (lambda (idepth_31 thisnode_31)
           (if (<= idepth_31 0)
               #f
               (let ((idepth_32 (- idepth_31 1)))
                    (begin
                     (vector-set! thisnode_31
                                  0
                                  (begin (make-vector 4 0)))
                     (begin
                      (vector-set! thisnode_31
                                   1
                                   (begin (make-vector 4 0))))
                     (populate_10 idepth_32
                                  (begin (vector-ref thisnode_31 0)))
                     (populate_10 idepth_32
                                  (begin (vector-ref thisnode_31
                                                     1))))))))
        (maketree_10
         (lambda (idepth_26)
           (if (<= idepth_26 0)
               (begin (make-vector 4 0))
               (begin
                (let ((v_27_28 (make-vector 4 0)))
                     (begin
                      (vector-set! v_27_28
                                   0
                                   (maketree_10 (- idepth_26 1)))
                      (vector-set! v_27_28
                                   1
                                   (maketree_10 (- idepth_26 1)))
                      v_27_28))))))
        (timeconstruction_10
         (lambda (depth_19)
           (let ((inumiters_20 (numiters_3 depth_19)))
                (begin
                 (print
                  (string-append "Creating "
                                 (number->string inumiters_20)
                                 " trees of depth "
                                 (number->string depth_19)))
                 (newline)
                 (run-benchmark "GCBench: Top down construction"
                                (lambda ()
                                  (do ((i_24 0 (+ i_24 1)))
                                      ((>= i_24 inumiters_20))
                                      (populate_10 depth_19
                                                   (begin
                                                    (make-vector 4
                                                                 0))))))
                 (run-benchmark "GCBench: Bottom up construction"
                                (lambda ()
                                  (do ((i_22 0 (+ i_22 1)))
                                      ((>= i_22 inumiters_20))
                                      (maketree_10 depth_19))))))))
        (main_10
         (lambda ()
           (begin (print "Garbage Collector Test")
                  (newline)
                  (print
                   (string-append
                    " Stretching memory with a binary tree of depth "
                    (number->string kstretchtreedepth_1)))
                  (newline)
                  (printdiagnostics)
                  (run-benchmark "GCBench: Main"
                                 (lambda ()
                                   (begin
                                    (maketree_10 kstretchtreedepth_1)
                                    (print
                                     (string-append
                                      " Creating a long-lived binary tree of depth "
                                      (number->string
                                       klonglivedtreedepth_4)))
                                    (newline)
                                    (let
                                     ((longlivedtree_13
                                       (make-vector 4 0)))
                                     (begin
                                      (populate_10
                                       klonglivedtreedepth_4
                                       longlivedtree_13)
                                      (print
                                       (string-append
                                        " Creating a long-lived array of "
                                        (number->string karraysize_5)
                                        " inexact reals"))
                                      (newline)
                                      (let*                                ;hack
                                       ((k (quotient karraysize_5 16))     ;hack
                                        (array_14
                                         (vector                           ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          (make-vector k 0.0)              ;hack
                                          )))
                                       (begin
                                        (do ((i_17 0 (+ i_17 1)))
                                            ((>= i_17
                                                 (quotient
                                                  karraysize_5
                                                  2)))
                                            (vector-set!
                                             (vector-ref array_14            ;hack
                                                         (quotient i_17 k))  ;hack
                                             (remainder i_17 k)              ;hack
                                             (/ 1.0
                                                (exact->inexact
                                                 i_17))))
                                        (printdiagnostics)
                                        (do
                                         ((d_16 kmintreedepth_6
                                                (+ d_16 2)))
                                         ((> d_16 kmaxtreedepth_7))
                                         (timeconstruction_10 d_16))
                                        (if
                                         (or
                                          (eq? longlivedtree_13 '())
                                          (let
                                           ((n_15
                                             (min 1000
                                                  (-
                                                   (quotient
                                                    (vector-length
                                                     array_14)
                                                    2)
                                                   1))))
                                           (not
                                            (=
                                             (vector-ref
                                              (vector-ref array_14           ;hack
                                                          (quotient n_15 k)) ;hack
                                              (remainder n_15 k))            ;hack
                                             (/ 1.0
                                                (exact->inexact n_15))))))
                                         (begin (print "Failed")
                                                (newline))))))))))
                  (printdiagnostics)))))
       (main_10))))))

(define gc-benchmark
  (lambda rest_1
    (let ((k_2 (if (null? rest_1) 18 (car rest_1))))
         (begin (print "The garbage collector should touch about ")
                (print (expt 2 (- k_2 13)))
                (print " megabytes of heap storage.")
                (newline)
                (print
                 "The use of more or less memory will skew the results.")
                (newline)
                (run-benchmark
                 (string-append "GCBench" (number->string k_2))
                 (lambda () (gcbench k_2)))))))
