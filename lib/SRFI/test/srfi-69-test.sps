; Test suite for SRFI 69
;
; $Id$

(import (rnrs base)
        (rnrs io simple)
        (rnrs unicode)
        (rnrs sorting)
        (rnrs arithmetic fixnums)
        (srfi :69 basic-hash-tables))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define ht1equal (make-hash-table))
(define ht2equal (make-hash-table equal?))
(define ht3equal (make-hash-table equal? hash))

(define ht2eqv (make-hash-table eqv?))
(define ht3eqv (make-hash-table eqv? hash))

(define ht2eq (make-hash-table eq?))
(define ht3eq (make-hash-table eq? hash))

(define ht3string= (make-hash-table string=? string-hash))

(define ht3string-ci= (make-hash-table string-ci=? string-ci-hash))

(define ht3fx= (make-hash-table fx=? values))


(define ht4equal (alist->hash-table '()))
(define ht5equal (alist->hash-table '() equal?))
(define ht6equal (alist->hash-table '() equal? hash))

(define ht5eqv (alist->hash-table '() eqv?))
(define ht6eqv (alist->hash-table '() eqv? hash))

(define ht5eq (alist->hash-table '() eq?))
(define ht6eq (alist->hash-table '() eq? hash))

(define ht6string= (alist->hash-table '() string=? string-hash))

(define ht6string-ci= (alist->hash-table '() string-ci=? string-ci-hash))

(define ht6fx= (alist->hash-table '() fx=? values))


(define (test-tables)
  (list ht1equal ht2equal ht3equal
                 ht2eqv   ht3eqv
                 ht2eq    ht3eq
        ht3string= ht3string-ci= ht3fx=
        ht4equal ht5equal ht6equal
                 ht5eqv   ht6eqv
                 ht5eq    ht6eq
        ht6string= ht6string-ci= ht6fx=))

(define (test-tables-general&nonempty)
  (list ht4equal ht5equal ht6equal
                 ht5eqv   ht6eqv
                 ht5eq    ht6eq))

(or (equal? (map hash-table? (test-tables))
            (map (lambda (x) #t) (test-tables)))
    (fail 'hash-table?))

(or (equal? (map hash-table-size (test-tables))
            (map (lambda (x) 0) (test-tables)))
    (fail 'alist->hash-table:1))

(set! ht4equal (alist->hash-table '((a 11) ("b" 12) (cee 13)  (47.8 14))))

(set! ht5equal (alist->hash-table '((a 11) ("b" 12) (cee 13)  (47.8 14))
                                  equal?))

(set! ht6equal (alist->hash-table '((a 11) ("b" 12) (cee 13)  (47.8 14))
                                  equal? hash))

(set! ht5eqv (alist->hash-table '((a 11) ("b" 12) (cee 13)  (47.8 14))
                                eqv?))

(set! ht6eqv (alist->hash-table '((a 11) ("b" 12) (cee 13)  (47.8 14))
                                eqv? hash))

(set! ht5eq (alist->hash-table '((a 11) ("b" 12) (cee 13)  (47.8 14))
                               eq?))

(set! ht6eq (alist->hash-table '((a 11) ("b" 12) (cee 13)  (47.8 14))
                               eq? hash))

(set! ht6string=
      (alist->hash-table '(("a" 11) ("b" 12) ("cee" 13)  ("d" 14))
                         string=? string-hash))

(set! ht6string-ci=
      (alist->hash-table '(("a" 11) ("b" 12) ("CeE" 13)  ("d" 14))
                         string-ci=? string-ci-hash))

(set! ht6fx= (alist->hash-table '((101 201) (102 202) (103 203) (104 204))
                                 fx=? values))

(or (equal? (map hash-table-size (test-tables))
            '(0 0 0 0 0 0 0 0 0 0 4 4 4 4 4 4 4 4 4 4))
    (fail 'alist->hash-table:2))

(or (equal? (map hash-table-equivalence-function (test-tables))
            (list equal? equal? equal? eqv? eqv? eq? eq?
                  string=? string-ci=? fx=?
                  equal? equal? equal? eqv? eqv? eq? eq?
                  string=? string-ci=? fx=?))
    (fail 'hash-table-equivalence-function:1))

(or (equal? (map hash-table-hash-function
                 (list ht1equal ht2equal ht3equal
                       ht3eqv ht3eq ht3string= ht3string-ci= ht3fx=))
            (list hash hash hash hash hash
                  string-hash string-ci-hash values))
    (fail 'hash-table-hash-function:1))

(or (equal? (map (lambda (ht)
                   (hash-table-ref ht 'cee))
                 (test-tables-general&nonempty))
            '((13) (13) (13) (13) (13) (13) (13)))
    (fail 'hash-table-ref:1))

(or (equal? (map (lambda (ht)
                   (hash-table-ref ht 47.8))
                 (list ht4equal ht5equal ht6equal ht5eqv ht6eqv))
            '((14) (14) (14) (14) (14)))
    (fail 'hash-table-ref:2))

(or (equal? (map (lambda (ht)
                   (hash-table-ref ht "cee" (lambda () #f)))
                 (append (test-tables-general&nonempty)
                         (list ht6string= ht6string-ci=)))
            '(#f #f #f #f #f #f #f (13) (13)))
    (fail 'hash-table-ref:3))

(or (equal? (map (lambda (ht)
                   (hash-table-ref ht "CeE" (lambda () 99)))
                 (append (test-tables-general&nonempty)
                         (list ht6string= ht6string-ci=)))
            '(99 99 99 99 99 99 99 99 (13)))
    (fail 'hash-table-ref:4))

(or (equal? (map (lambda (ht)
                   (hash-table-ref/default ht "CeE" 97))
                 (append (test-tables-general&nonempty)
                         (list ht6string= ht6string-ci=)))
            '(97 97 97 97 97 97 97 97 (13)))
    (fail 'hash-table-ref:5))

(for-each (lambda (ht) (hash-table-set! ht "cee" 'see))
          (append (test-tables-general&nonempty)
                  (list ht6string= ht6string-ci=)))

(or (equal? (map hash-table-size
                 (append (test-tables-general&nonempty)
                         (list ht6string= ht6string-ci=)))
            '(5 5 5 5 5 5 5 4 4))
    (fail 'hash-table-set!:1))

(for-each (lambda (ht) (hash-table-delete! ht (string #\b)))
          (append (test-tables-general&nonempty)
                  (list ht6string= ht6string-ci=)))

(or (equal? (map hash-table-size
                 (append (test-tables-general&nonempty)
                         (list ht6string= ht6string-ci=)))
            '(4 4 4 5 5 5 5 3 3))
    (fail 'hash-table-delete!:1))

(or (equal? (map (lambda (ht) (hash-table-exists? ht "om"))
                 (append (test-tables-general&nonempty)
                         (list ht6string= ht6string-ci=)))
            '(#f #f #f #f #f #f #f #f #f))
    (fail 'hash-table-exists?:1))

(or (equal? (map (lambda (ht) (hash-table-exists? ht "cee"))
                 (append (test-tables-general&nonempty)
                         (list ht6string= ht6string-ci=)))
            '(#t #t #t #f #f #f #f #t #t))
    (fail 'hash-table-exists?:2))

(for-each (lambda (ht) (hash-table-update! ht 'a car))
          (test-tables-general&nonempty))

(or (equal? (map (lambda (ht) (hash-table-ref/default ht 'a #f))
                 (test-tables-general&nonempty))
            '(11 11 11 11 11 11 11))
    (fail 'hash-table-update!:1))

(or (equal? (map hash-table-size (test-tables))
            '(0 0 0 0 0 0 0 0 0 0 4 4 4 5 5 5 5 3 3 4))
    (fail 'hash-table-size:1))

;;; This is slightly flaky, because hash might hash two keys
;;; to the same value.  In particular, a symbol might be hashed
;;; the same as its print string.

(define (canonical-order? x y)
  (let ((i (hash x))
        (j (hash y)))
    (or (< i j)
        (and (= i j) (symbol? x) (string? y)))))

(define (canonical-order lis)
  (list-sort canonical-order? lis))

(or (equal? (map canonical-order
                 (map hash-table-keys (test-tables)))
            (map canonical-order
                 '(() () () () () () () () () ()
                   (a cee 47.8 "cee")
                   (a cee 47.8 "cee")
                   (a cee 47.8 "cee")
                   (a "b" cee 47.8 "cee")
                   (a "b" cee 47.8 "cee")
                   (a "b" cee 47.8 "cee")
                   (a "b" cee 47.8 "cee")
                   ("a" "cee" "d")
                   ("a" "CeE" "d")
                   (101 102 103 104))))
    (fail 'hash-table-keys:1))

(or (equal? (map canonical-order
                 (map hash-table-values (test-tables)))
            (map canonical-order
                 '(() () () () () () () () () ()
                   (see 11 (13) (14))
                   (see 11 (13) (14))
                   (see 11 (13) (14))
                   (see 11 (12) (13) (14))
                   (see 11 (12) (13) (14))
                   (see 11 (12) (13) (14))
                   (see 11 (12) (13) (14))
                   (see (11) (14))
                   (see (11) (14))
                   ((201) (202) (203) (204)))))
    (fail 'hash-table-values:1))

(let ((keys '())
      (vals '()))
  (hash-table-walk ht4equal
                   (lambda (key val)
                     (set! keys (cons key keys))
                     (set! vals (cons val vals))))
  (or (and (equal? (canonical-order keys)
                   (canonical-order (hash-table-keys ht4equal)))
           (equal? (canonical-order vals)
                   (canonical-order (hash-table-values ht4equal))))
      (fail 'hash-table-walk:1)))

(or (and (equal? (canonical-order
                  (hash-table-fold ht4equal
                                   (lambda (key val x) (cons key x))
                                   '()))
                 (canonical-order (hash-table-keys ht4equal)))
         (equal? (canonical-order
                  (hash-table-fold ht4equal
                                   (lambda (key val x) (cons val x))
                                   '()))
                 (canonical-order (hash-table-values ht4equal))))
    (fail 'hash-table-fold:1))

; Not yet tested:
;
; hash-table->alist
; hash-table-copy
; hash-table-merge!
;
; hash
; string-hash
; string-ci-hash
; hash-by-identity

(writeln "Done (but these tests are incomplete).")
