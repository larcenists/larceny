;;; Immutable list benchmark for (scheme ilist).

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time)
        (scheme ilist))

(define (iequal? x y)
  (cond ((or (not (ilist? x))
             (not (ilist? y)))
         (equal? x y))
        ((null-ilist? x)
         (null-ilist? y))
        ((null-ilist? y)
         (null-ilist? x))
        ((iequal? (icar x) (icar y))
         (iequal? (icdr x) (icdr y)))
        (else
         #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (symbols n)
  (imap string->symbol
        (imap (lambda (s) (string-append "x" s))
              (imap number->string
                    (iiota n)))))

(define (powerset universe)
  (if (null-ilist? universe)
      (ilist (ilist))
      (let* ((x (icar universe))
             (u2 (icdr universe))
             (pu2 (powerset u2)))
        (iappend pu2
                 (imap (lambda (y)
                         (ipair x y))
                       pu2)))))

(define (permutations universe)
  (if (null-ilist? universe)
      (ilist (ilist))
      (let* ((x (icar universe))
             (u2 (icdr universe))
             (perms2 (permutations u2)))
        (iconcatenate
         (imap (lambda (perm)
                 (imap (lambda (i)
                         (iappend (itake perm i)
                                  (ipair x (idrop perm i))))
                       (iiota (+ 1 (ilength perm)))))
               perms2)))))

(define (go n)
  (let* ((universe (symbols n))
         (subsets (powerset universe))
         (perms (permutations universe)))
    (map ilist->list
         (ilist->list
          (ifilter (lambda (perm)
                     (imember perm subsets iequal?))
                   perms)))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "ilist"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (go (hide count input1)))
     (lambda (result) (equal? result output)))))
