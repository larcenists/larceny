; The quick-1 benchmark.  (Figure 35, page 132.)

(define (quick-1 v less?)
  
  (define (helper left right)
    (if (< left right)
        (let ((median (partition v left right less?)))
          (if (< (- median left) (- right median))
              (begin (helper left (- median 1))
                     (helper (+ median 1) right))
              (begin (helper (+ median 1) right)
                     (helper left (- median 1)))))
        v))
  
  (helper 0 (- (vector-length v) 1)))


(define (partition v left right less?)
  (let ((mid (vector-ref v right)))
    
    (define (uploop i)
      (let ((i (+ i 1)))
        (if (and (< i right) (less? (vector-ref v i) mid))
            (uploop i)
            i)))
    
    (define (downloop j)
      (let ((j (- j 1)))
        (if (and (> j left) (less? mid (vector-ref v j)))
            (downloop j)
            j)))
    
    (define (ploop i j)
      (let* ((i (uploop i))
             (j (downloop j)))
        (let ((tmp (vector-ref v i)))
          (vector-set! v i (vector-ref v j))
          (vector-set! v j tmp)
          (if (< i j)
              (ploop i j)
              (begin (vector-set! v j (vector-ref v i))
                     (vector-set! v i (vector-ref v right))
                     (vector-set! v right tmp)
                     i)))))
    
    (ploop (- left 1) right)))

; minimal standard random number generator
; 32 bit integer version
; cacm 31 10, oct 88
;

(define *seed* (list 1))

(define (srand seed)
  (set-car! *seed* seed))

(define (rand)
  (let* ((hi (quotient (car *seed*) 127773))
         (lo (modulo (car *seed*) 127773))
         (test (- (* 16807 lo) (* 2836 hi))))
    (if (> test 0)
        (set-car! *seed* test)
        (set-car! *seed* (+ test 2147483647)))
    (car *seed*)))

;; return a random number in the interval [0,n)
(define random
  (lambda (n)
    (modulo (abs (rand)) n)))


(define (quicksort-benchmark)
  (let* ((n 30000)
         (v (make-vector n)))
    (do ((i 0 (+ i 1)))
        ((= i n))
        (vector-set! v i (random 4000)))
    (quick-1 v (lambda (x y) (< x y)))))

(define (main . args)
  (run-benchmark
    "quicksort30"
    quicksort-iters
    (lambda (v)
      (call-with-current-continuation
        (lambda (return)
          (do ((i 1 (+ i 1)))
              ((= i (vector-length v))
               #t)
              (if (not (<= (vector-ref v (- i 1))
                           (vector-ref v i)))
                  (return #f))))))
    (lambda () quicksort-benchmark)))
