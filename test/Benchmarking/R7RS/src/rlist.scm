;;; Random-access list benchmark for (scheme rlist).

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time)
        (scheme rlist))

(define (riota n)
  (let loop ((n n)
             (r (rlist)))
    (if (= n 0)
        r
        (let ((n-1 (- n 1)))
          (loop n-1
                (rcons n-1 r))))))

(define (rconcatenate rs)
  (if (rnull? rs)
      (rlist)
      (rappend (rcar rs)
               (rconcatenate (rcdr rs)))))

(define (rtake r i)
  (let loop ((i i)
             (r2 (rlist)))
    (if (= i 0)
        r2
        (let ((i-1 (- i 1)))
          (loop i-1
                (rcons (rlist-ref r i-1) r2))))))

(define (rdrop r i)
  (rlist-tail r i))

(define (rfilter pred r)
  (let ((r2 (rlist)))
    (rfor-each (lambda (x)
                 (if (pred x)
                     (set! r2 (rcons x r2))))
               (rreverse r))
    r2))

(define (rmember x r . rest)
  (let ((same? (if (null? rest) equal? (car rest))))
    (let loop ((r r))
      (cond ((rnull? r)
             #f)
            ((same? x (rcar r))
             r)
            (else
             (loop (rcdr r)))))))

(define (requal? x y)
  (cond ((or (not (rlist? x))
             (not (rlist? y)))
         (equal? x y))
        ((rnull? x)
         (rnull? y))
        ((rnull? y)
         (rnull? x))
        ((requal? (rcar x) (rcar y))
         (requal? (rcdr x) (rcdr y)))
        (else
         #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (symbols n)
  (rmap string->symbol
        (rmap (lambda (s) (string-append "x" s))
              (rmap number->string
                    (riota n)))))

(define (powerset universe)
  (if (rnull? universe)
      (rlist (rlist))
      (let* ((x (rcar universe))
             (u2 (rcdr universe))
             (pu2 (powerset u2)))
        (rappend pu2
                 (rmap (lambda (y)
                         (rcons x y))
                       pu2)))))

(define (permutations universe)
  (if (rnull? universe)
      (rlist (rlist))
      (let* ((x (rcar universe))
             (u2 (rcdr universe))
             (perms2 (permutations u2)))
        (rconcatenate
         (rmap (lambda (perm)
                 (rmap (lambda (i)
                         (rappend (rtake perm i)
                                  (rcons x (rdrop perm i))))
                       (riota (+ 1 (rlength perm)))))
               perms2)))))

(define (go n)
  (let* ((universe (symbols n))
         (subsets (powerset universe))
         (perms (permutations universe)))
    (map rlist->list
         (rlist->list
          (rfilter (lambda (perm)
                     (rmember perm subsets requal?))
                   perms)))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "rlist"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (go (hide count input1)))
     (lambda (result) (equal? result output)))))
