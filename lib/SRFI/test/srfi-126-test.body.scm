;;; This doesn't test weakness, external representation, and quasiquote.

(test-begin "SRFI-126")

(test-group "constructors & inspection"
  (test-group "eq"
    (let ((tables (list (make-eq-hashtable)
                        (make-eq-hashtable 10)
                        (make-eq-hashtable #f #f)
                        (make-hashtable #f eq?)
                        (alist->eq-hashtable '((a . b) (c . d)))
                        (alist->eq-hashtable 10 '((a . b) (c . d)))
                        (alist->eq-hashtable #f #f '((a . b) (c . d))))))
      (do ((tables tables (cdr tables))
           (i 0 (+ i 1)))
          ((null? tables))
        (let ((table (car tables))
              (label (number->string i)))
          (test-assert label (hashtable? table))
          (test-eq label #f (hashtable-hash-function table))
          (test-eq label eq? (hashtable-equivalence-function table))
          (test-eq label #f (hashtable-weakness table))
          (test-assert label (hashtable-mutable? table))))))
  (test-group "eqv"
    (let ((tables (list (make-eqv-hashtable)
                        (make-eqv-hashtable 10)
                        (make-eqv-hashtable #f #f)
                        (make-hashtable #f eqv?)
                        (alist->eqv-hashtable '((a . b) (c . d)))
                        (alist->eqv-hashtable 10 '((a . b) (c . d)))
                        (alist->eqv-hashtable #f #f '((a . b) (c . d))))))
      (do ((tables tables (cdr tables))
           (i 0 (+ i 1)))
          ((null? tables))
        (let ((table (car tables))
              (label (number->string i)))
          (test-assert label (hashtable? table))
          (test-eq label #f (hashtable-hash-function table))
          (test-eq label eqv? (hashtable-equivalence-function table))
          (test-eq label #f (hashtable-weakness table))
          (test-assert label (hashtable-mutable? table))))))
  (test-group "equal"
    (let ((tables (list (make-hashtable equal-hash equal?)
                        (make-hashtable equal-hash equal? 10)
                        (make-hashtable equal-hash equal? #f #f)
                        (alist->hashtable equal-hash equal?
                                          '((a . b) (c . d)))
                        (alist->hashtable equal-hash equal? 10
                                          '((a . b) (c . d)))
                        (alist->hashtable equal-hash equal? #f #f
                                          '((a . b) (c . d))))))
      (do ((tables tables (cdr tables))
           (i 0 (+ i 1)))
          ((null? tables))
        (let ((table (car tables))
              (label (number->string i)))
          (test-assert label (hashtable? table))
          (test-eq label equal-hash (hashtable-hash-function table))
          (test-eq label equal? (hashtable-equivalence-function table))
          (test-eq label #f (hashtable-weakness table))
          (test-assert label (hashtable-mutable? table))))
      (let ((table (make-hashtable (cons equal-hash equal-hash) equal?)))
        (let ((hash (hashtable-hash-function table)))
          (test-assert (or (eq? equal-hash hash)
                           (and (eq? equal-hash (car hash))
                                (eq? equal-hash (cdr hash))))))))))

(test-group "procedures"
  (test-group "basics"
    (let ((table (make-eq-hashtable)))
      (test-group "ref"
        (test-error (hashtable-ref table 'a))
        (test-eq 'b (hashtable-ref table 'a 'b))
        (test-assert (not (hashtable-contains? table 'a)))
        (test-eqv 0 (hashtable-size table)))
      (test-group "set"
        (hashtable-set! table 'a 'c)
        (test-eq 'c (hashtable-ref table 'a))
        (test-eq 'c (hashtable-ref table 'a 'b))
        (test-assert (hashtable-contains? table 'a))
        (test-eqv 1 (hashtable-size table)))
      (test-group "delete"
        (hashtable-delete! table 'a)
        (test-error (hashtable-ref table 'a))
        (test-eq 'b (hashtable-ref table 'a 'b))
        (test-assert (not (hashtable-contains? table 'a)))
        (test-eqv 0 (hashtable-size table)))))
  (test-group "advanced"
    (let ((table (make-eq-hashtable)))
      (test-group "lookup"
        (let-values (((x found?) (hashtable-lookup table 'a)))
          (test-assert (not found?))))
      (test-group "update"
        (test-error (hashtable-update! table 'a (lambda (x) (+ x 1))))
        (hashtable-update! table 'a (lambda (x) (+ x 1)) 0)
        (let-values (((x found?) (hashtable-lookup table 'a)))
          (test-eqv 1 x)
          (test-assert found?))
        (hashtable-update! table 'a (lambda (x) (+ x 1)))
        (let-values (((x found?) (hashtable-lookup table 'a)))
          (test-eqv x 2)
          (test-assert found?))
        (hashtable-update! table 'a (lambda (x) (+ x 1)) 0)
        (let-values (((x found?) (hashtable-lookup table 'a)))
          (test-eqv x 3)
          (test-assert found?)))
      (test-group "intern"
        (test-eqv 0 (hashtable-intern! table 'b (lambda () 0)))
        (test-eqv 0 (hashtable-intern! table 'b (lambda () 1))))))
  (test-group "copy/clear"
    (let ((table (alist->hashtable equal-hash equal? '((a . b)))))
      (test-group "copy"
        (let ((table2 (hashtable-copy table)))
          (test-eq equal-hash (hashtable-hash-function table2))
          (test-eq equal? (hashtable-equivalence-function table2))
          (test-eq 'b (hashtable-ref table2 'a))
          (test-error (hashtable-set! table2 'a 'c)))
        (let ((table2 (hashtable-copy table #f)))
          (test-eq equal-hash (hashtable-hash-function table2))
          (test-eq equal? (hashtable-equivalence-function table2))
          (test-eq 'b (hashtable-ref table2 'a))
          (test-error (hashtable-set! table2 'a 'c)))
        (let ((table2 (hashtable-copy table #t)))
          (test-eq equal-hash (hashtable-hash-function table2))
          (test-eq equal? (hashtable-equivalence-function table2))
          (test-eq 'b (hashtable-ref table2 'a))
          (hashtable-set! table2 'a 'c)
          (test-eq 'c (hashtable-ref table2 'a)))
        (let ((table2 (hashtable-copy table #f #f)))
          (test-eq equal-hash (hashtable-hash-function table2))
          (test-eq equal? (hashtable-equivalence-function table2))
          (test-eq #f (hashtable-weakness table2))))
      (test-group "clear"
        (let ((table2 (hashtable-copy table #t)))
          (hashtable-clear! table2)
          (test-eqv 0 (hashtable-size table2)))
        (let ((table2 (hashtable-copy table #t)))
          (hashtable-clear! table2 10)
          (test-eqv 0 (hashtable-size table2))))
      (test-group "empty-copy"
        (let ((table2 (hashtable-empty-copy table)))
          (test-eq equal-hash (hashtable-hash-function table2))
          (test-eq equal? (hashtable-equivalence-function table2))
          (test-eqv 0 (hashtable-size table2)))
        (let ((table2 (hashtable-empty-copy table 10)))
          (test-eq equal-hash (hashtable-hash-function table2))
          (test-eq equal? (hashtable-equivalence-function table2))
          (test-eqv 0 (hashtable-size table2))))))
  (test-group "keys/values"
    (let ((table (alist->eq-hashtable '((a . b) (c . d)))))
      (test-assert (lset= eq? '(a c) (vector->list (hashtable-keys table))))
      (test-assert (lset= eq? '(b d) (vector->list (hashtable-values table))))
      (let-values (((keys values) (hashtable-entries table)))
        (test-assert (lset= eq? '(a c) (vector->list keys)))
        (test-assert (lset= eq? '(b d) (vector->list values))))
      (test-assert (lset= eq? '(a c) (hashtable-key-list table)))
      (test-assert (lset= eq? '(b d) (hashtable-value-list table)))
      (let-values (((keys values) (hashtable-entry-lists table)))
        (test-assert (lset= eq? '(a c) keys))
        (test-assert (lset= eq? '(b d) values)))))
  (test-group "iteration"
    (test-group "walk"
      (let ((keys '())
            (values '()))
        (hashtable-walk (alist->eq-hashtable '((a . b) (c . d)))
          (lambda (k v)
            (set! keys (cons k keys))
            (set! values (cons v values))))
        (test-assert (lset= eq? '(a c) keys))
        (test-assert (lset= eq? '(b d) values))))
    (test-group "update-all"
      (let ((table (alist->eq-hashtable '((a . b) (c . d)))))
        (hashtable-update-all! table
          (lambda (k v)
            (string->symbol (string-append (symbol->string v) "x"))))
        (test-assert (lset= eq? '(a c) (hashtable-key-list table)))
        (test-assert (lset= eq? '(bx dx) (hashtable-value-list table)))))
    (test-group "prune"
      (let ((table (alist->eq-hashtable '((a . b) (c . d)))))
        (hashtable-prune! table (lambda (k v) (eq? k 'a)))
        (test-assert (not (hashtable-contains? table 'a)))
        (test-assert (hashtable-contains? table 'c))))
    (test-group "merge"
      (let ((table (alist->eq-hashtable '((a . b) (c . d))))
            (table2 (alist->eq-hashtable '((a . x) (e . f)))))
        (hashtable-merge! table table2)
        (test-assert (lset= eq? '(a c e) (hashtable-key-list table)))
        (test-assert (lset= eq? '(x d f) (hashtable-value-list table)))))
    (test-group "sum"
      (let ((table (alist->eq-hashtable '((a . b) (c . d)))))
        (test-assert (lset= eq? '(a b c d)
                            (hashtable-sum table '()
                              (lambda (k v acc)
                                (lset-adjoin eq? acc k v)))))))
    (test-group "map->lset"
      (let ((table (alist->eq-hashtable '((a . b) (c . d)))))
        (test-assert (lset= equal? '((a . b) (c . d))
                            (hashtable-map->lset table cons)))))
    (test-group "find"
      (let ((table (alist->eq-hashtable '((a . b) (c . d)))))
        (let-values (((k v f?) (hashtable-find table
                                 (lambda (k v)
                                   (eq? k 'a)))))
          (test-assert (and f? (eq? k 'a) (eq? v 'b))))
        (let-values (((k v f?) (hashtable-find table (lambda (k v) #f))))
          (test-assert (not f?)))))
    (test-group "misc"
      (test-group "empty?"
        (test-assert (hashtable-empty? (alist->eq-hashtable '())))
        (test-assert (not (hashtable-empty? (alist->eq-hashtable '((a . b)))))))
      (test-group "pop!"
        (test-error (hashtable-pop! (make-eq-hashtable)))
        (let ((table (alist->eq-hashtable '((a . b)))))
          (let-values (((k v) (hashtable-pop! table)))
            (test-eq 'a k)
            (test-eq 'b v)
            (test-assert (hashtable-empty? table)))))
      (test-group "inc!"
        (let ((table (alist->eq-hashtable '((a . 0)))))
          (hashtable-inc! table 'a)
          (test-eqv 1 (hashtable-ref table 'a))
          (hashtable-inc! table 'a 2)
          (test-eqv 3 (hashtable-ref table 'a))))
      (test-group "dec!"
        (let ((table (alist->eq-hashtable '((a . 0)))))
          (hashtable-dec! table 'a)
          (test-eqv -1 (hashtable-ref table 'a))
          (hashtable-dec! table 'a 2)
          (test-eqv -3 (hashtable-ref table 'a)))))))

(test-group "hashing"
  (test-assert (and (exact-integer? (hash-salt))))
  (test-assert (not (negative? (hash-salt))))
  (test-assert (= (equal-hash (list "foo" 'bar 42))
                  (equal-hash (list "foo" 'bar 42))))
  (test-assert (= (string-hash (string-copy "foo"))
                  (string-hash (string-copy "foo"))))
  (test-assert (= (string-ci-hash (string-copy "foo"))
                  (string-ci-hash (string-copy "FOO"))))
  (test-assert (= (symbol-hash (string->symbol "foo"))
                  (symbol-hash (string->symbol "foo")))))

(test-end "SRFI-126")

;;; The following note is commented out for Larceny.

#;

(display
 (string-append
  "\n"
  "NOTE: On implementations using the (r6rs hashtables) library from Larceny,\n"
  "      14 tests are expected to fail in relation to make-eq-hashtable and\n"
  "      make-eqv-hashtable returning hashtables whose hash functions are\n"
  "      exposed instead of being #f.  We have no obvious way to detect this\n"
  "      within this portable test suite, hence no XFAIL results.\n"))

;;; The following is added for Larceny.

(system "fgrep \"result-kind:\" SRFI-126.log")
(display "Done.\n")

;; Local Variables:
;; eval: (put (quote test-group) (quote scheme-indent-function) 1)
;; End:
