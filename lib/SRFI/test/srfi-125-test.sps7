;;; Copyright (C) William D Clinger 2015. All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; This is a very shallow sanity test for hash tables.
;;;
;;; Tests marked by a "FIXME: glass-box" comment test behavior of the
;;; reference implementation that is not required by the specification.

(import (scheme base)
        (scheme char)
        (scheme write)
        (scheme process-context) ; for exit
        (srfi 114 comparators)
        (r6rs sorting)
        (in-progress hash tables)
        (in-progress hash bimaps))

(define (writeln . xs)
  (for-each write xs)
  (newline))

(define (displayln . xs)
  (for-each display xs)
  (newline))

(define ultimate-exit-status 0)

(define (fail token . more)
  (set! ultimate-exit-status 1)
  (displayln "Error: test failed: ")
  (writeln token)
  (if (not (null? more))
      (for-each writeln more))
  (newline)
  #f)

;;; FIXME: when debugging catastrophic failures, printing every expression
;;; before it's executed may help.

(define-syntax test
  (syntax-rules ()
   ((_ expr expected)
    (let ()
;     (write 'expr) (newline)
      (let ((actual expr))
        (or (equal? actual expected)
            (fail 'expr actual expected)))))))

(define-syntax test-assert
  (syntax-rules ()
   ((_ expr)
    (or expr (fail 'expr)))))

(define-syntax test-deny
  (syntax-rules ()
   ((_ expr)
    (or (not expr) (fail 'expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constructors.

(define ht-default (make-hash-table default-comparator))

(define ht-eq (make-hash-table eq-comparator 'random-argument "another"))

(define ht-eqv (make-hash-table eqv-comparator))

(define ht-eq2 (make-hash-table eq?))

(define ht-eqv2 (make-hash-table eqv?))

(define ht-equal (make-hash-table equal?))

(define ht-string (make-hash-table string=?))

(define ht-string-ci (make-hash-table string-ci=?))

(define ht-symbol (make-hash-table symbol=?))    ; FIXME: glass-box

(define ht-fixnum (make-hash-table = abs))

(define ht-default2
  (hash-table default-comparator 'foo 'bar 101.3 "fever" '(x y z) '#()))

(define ht-fixnum2
  (hash-table-tabulate number-comparator
                       10
                       (lambda (i) (values (* i i) i))))

(define ht-string2
  (hash-table-unfold (lambda (s) (= 0 (string-length s)))
                     (lambda (s) (values s (string-length s)))
                     (lambda (s) (substring s 0 (- (string-length s) 1)))
                     "prefixes"
                     string-comparator
                     'ignored1 'ignored2 "ignored3" '#(ignored 4 5)))

(define ht-string-ci2
  (alist->hash-table '(("" . 0) ("Mary" . 4) ("Paul" . 4) ("Peter" . 5))
                     string-ci-comparator
                     "ignored1" 'ignored2))

(define ht-symbol2
  (alist->hash-table '((mary . travers) (noel . stookey) (peter .yarrow))
                     eq?))

(define ht-equal2
  (alist->hash-table '(((edward) . abbey)
                       ((dashiell) . hammett)
                       ((edward) . teach)
                       ((mark) . twain))
                     equal?
                     (comparator-hash-function default-comparator)))

(define test-tables
  (list ht-default   ht-default2   ; initial keys: foo, 101.3, (x y z)
        ht-eq        ht-eq2        ; initially empty
        ht-eqv       ht-eqv2       ; initially empty
        ht-equal     ht-equal2     ; initial keys: (edward), (dashiell), (mark)
        ht-string    ht-string2    ; initial keys: "p, "pr", ..., "prefixes"
        ht-string-ci ht-string-ci2 ; initial keys: "", "Mary", "Paul", "Peter"
        ht-symbol    ht-symbol2    ; initial keys: mary, noel, peter
        ht-fixnum    ht-fixnum2))  ; initial keys: 0, 1, 4, 9, ..., 81

;;; Predicates

(test (map hash-table?
           (cons '#()
                 (cons default-comparator
                       test-tables)))
      (append '(#f #f) (map (lambda (x) #t) test-tables)))

(test (map hash-table-contains?
           test-tables
           '(foo 101.3
             x "y"
             (14 15) #\newline
             (edward) (mark)
             "p" "pref"
             "mike" "PAUL"
             jane noel
             0 4))
      '(#f #t #f #f #f #f #f #t #f #t #f #t #f #t #f #t))

(test (map hash-table-contains?
           test-tables
           '(#u8() 47.9
             '#() '()
             foo bar
             19 (henry)
             "p" "perp"
             "mike" "Noel"
             jane paul
             0 5))
      (map (lambda (x) #f) test-tables))

(test (map hash-table-empty? test-tables)
      '(#t #f #t #t #t #t #t #f #t #f #t #f #t #f #t #f))

(test (map (lambda (ht1 ht2) (hash-table=? default-comparator ht1 ht2))
           test-tables
           test-tables)
      (map (lambda (x) #t) test-tables))

(test (map (lambda (ht1 ht2) (hash-table=? default-comparator ht1 ht2))
           test-tables
           (do ((tables (reverse test-tables) (cddr tables))
                (rev '() (cons (car tables) (cons (cadr tables) rev))))
               ((null? tables)
                rev)))
      '(#f #f #t #t #t #t #f #f #f #f #f #f #f #f #f #f))

(test (map hash-table-mutable? test-tables)
      '(#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f))

;;; FIXME: glass-box

(test (map hash-table-mutable? (map hash-table-copy test-tables))
      (map (lambda (x) #f) test-tables))

(test (hash-table-mutable? (hash-table-copy ht-fixnum2 #t))
      #t)

;;; Accessors.

;;; FIXME: glass-box (implementations not required to raise an exception here)

(test (map (lambda (ht)
             (guard (exn
                     (else 'err))
              (hash-table-ref ht 'not-a-key)))
           test-tables)
      (map (lambda (ht) 'err) test-tables))

;;; FIXME: glass-box (implementations not required to raise an exception here)

(test (map (lambda (ht)
             (guard (exn
                     (else 'err))
              (hash-table-ref ht 'not-a-key (lambda () 'err))))
           test-tables)
      (map (lambda (ht) 'err) test-tables))

;;; FIXME: glass-box (implementations not required to raise an exception here)

(test (map (lambda (ht)
             (guard (exn
                     (else 'err))
              (hash-table-ref ht 'not-a-key (lambda () 'err) values)))
           test-tables)
      (map (lambda (ht) 'err) test-tables))

(test (map (lambda (ht key)
             (guard (exn
                     (else 'err))
              (hash-table-ref ht key)))
           test-tables
           '(foo 101.3
             x "y"
             (14 15) #\newline
             (edward) (mark)
             "p" "pref"
             "mike" "PAUL"
             jane noel
             0 4))
      '(err "fever" err err err err err twain err 4 err 4 err stookey err 2))

(test (map (lambda (ht key)
             (guard (exn
                     (else 'err))
              (hash-table-ref ht key (lambda () 'eh))))
           test-tables
           '(foo 101.3
             x "y"
             (14 15) #\newline
             (edward) (mark)
             "p" "pref"
             "mike" "PAUL"
             jane noel
             0 4))
      '(eh "fever" eh eh eh eh eh twain eh 4 eh 4 eh stookey eh 2))

(test (map (lambda (ht key)
             (guard (exn
                     (else 'err))
              (hash-table-ref ht key (lambda () 'eh) list)))
           test-tables
           '(foo 101.3
             x "y"
             (14 15) #\newline
             (edward) (mark)
             "p" "pref"
             "mike" "PAUL"
             jane noel
             0 4))
      '(eh ("fever") eh eh eh eh eh (twain) eh (4) eh (4) eh (stookey) eh (2)))

;;; FIXME: glass-box (implementations not required to raise an exception here)

(test (map (lambda (ht)
             (guard (exn
                     (else 'eh))
              (hash-table-ref/default ht 'not-a-key 'eh)))
           test-tables)
      (map (lambda (ht) 'eh) test-tables))

(test (map (lambda (ht key)
             (guard (exn
                     (else 'err))
              (hash-table-ref/default ht key 'eh)))
           test-tables
           '(foo 101.3
             x "y"
             (14 15) #\newline
             (edward) (mark)
             "p" "pref"
             "mike" "PAUL"
             jane noel
             0 4))
      '(eh "fever" eh eh eh eh eh twain eh 4 eh 4 eh stookey eh 2))

(test (begin (hash-table-set! ht-fixnum)
             (list-sort < (hash-table-keys ht-fixnum)))
      '())

(test (begin (hash-table-set! ht-fixnum 121 11 144 12 169 13)
             (list-sort < (hash-table-keys ht-fixnum)))
      '(121 144 169))

(test (begin (hash-table-set-entries! ht-fixnum
                                      '(0 25 1 4 9 16 25 36 49 64 81)
                                      '(0 -5 1 2 3  4  5  6  7  8  9))
             (list-sort < (hash-table-keys ht-fixnum)))
      '(0 1 4 9 16 25 36 49 64 81 121 144 169))

(test (map (lambda (i) (hash-table-ref/default ht-fixnum i 'error))
           '(169 144 121 0 1 4 9 16 25 36 49 64 81))
      '(13 12 11 0 1 2 3 4 5 6 7 8 9))

(test (begin (hash-table-delete! ht-fixnum)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i 'error))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 3 4 5 6 7 8 9))

(test (begin (hash-table-delete! ht-fixnum 1 9 25 49 81 200 121 169 81 1)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(-1 12 -1 0 -1 2 -1 4 -1 6 -1 8 -1))

(test (begin (hash-table-delete-keys! ht-fixnum '(200 100 0 81 36))
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(-1 12 -1 -1 -1 2 -1 4 -1 -1 -1 8 -1))

(test (hash-table-extend! ht-fixnum 144)
      12)

(test (begin (hash-table-extend! ht-fixnum 144)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(-1 12 -1 -1 -1 2 -1 4 -1 -1 -1 8 -1))
                 
(test (begin (hash-table-extend! ht-fixnum 169 (lambda () 13))
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 -1 -1 -1 2 -1 4 -1 -1 -1 8 -1))
                 
(test (hash-table-extend! ht-fixnum 64 (lambda () 13) list)
      '(8))

(test (hash-table-extend! ht-fixnum 121 (lambda () 11) list)
      11)

(test (hash-table-extend!/default ht-fixnum 0 0)
      0)

(test (hash-table-extend!/default ht-fixnum 1 1)
      1)

(test (hash-table-extend!/default ht-fixnum 4 0)
      2)

(test (begin (hash-table-update! ht-fixnum 9 length (lambda () '(a b c)))
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 3 4 -1 -1 -1 8 -1))

(test (begin (hash-table-update! ht-fixnum 16 -)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 3 -4 -1 -1 -1 8 -1))

(test (begin (hash-table-update! ht-fixnum 16 - abs)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 3 4 -1 -1 -1 8 -1))

(test (begin (hash-table-update!/default ht-fixnum 25 - 5)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 3 4 -5 -1 -1 8 -1))

(test (begin (hash-table-update!/default ht-fixnum 25 - 999)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 3 4 5 -1 -1 8 -1))

(test (begin (hash-table-push! ht-fixnum 75 '*** (lambda () 8.66))
             (hash-table-ref/default ht-fixnum 75 -1))
      8.66)

(test (begin (hash-table-push! ht-fixnum 64 '* (lambda () 'okra))
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 75 81)))
      '(13 12 11 0 1 2 3 4 5 -1 -1 (* . 8) 8.66 -1))

(test (hash-table-pop! ht-fixnum 64 (lambda () 'whatever))
      '*)

(test (hash-table-pop! ht-fixnum 65 (lambda () 'whatever))
      'whatever)

;;; FIXME: glass-box (implementations not required to raise an exception here)

(test (guard (exn (else 'error))
       (hash-table-pop! ht-fixnum 75 (lambda () 'whatever)))
      'error)

(test (begin (hash-table-delete! ht-fixnum 75)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 75 81)))
      '(13 12 11 0 1 2 3 4 5 -1 -1 8 -1 -1))

(test (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
           '(169 144 121 0 1 4 9 16 25 36 49 64 81))
      '(13 12 11 0 1 2 3 4 5 -1 -1 8 -1))

(test (hash-table-search! ht-fixnum 99 (lambda (add!) 17) values)
      17)

(test (hash-table-search! ht-fixnum 36 (lambda (add!) (add! 6) 999) values)
      999)

(test (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
           '(169 144 121 0 1 4 9 16 25 36 49 64 81))
      '(13 12 11 0 1 2 3 4 5 6 -1 8 -1))

(test (hash-table-search! ht-fixnum
                          64
                          (lambda (add!) (add! 6) 999)
                          (lambda (val set! delete!)
                            val))
      8)

;;; Fancy implementations of hash-table-search! might get this wrong.

(test (call-with-values
       (lambda ()
         (hash-table-search! ht-fixnum
                             64
                             (lambda (add!) (add! 6) 999)
                             values))
       (lambda (val set! delete!)
         (delete!)
         (let* ((val2 (hash-table-ref/default ht-fixnum 64 97))
                (val3 (hash-table-search! ht-fixnum
                                          81
                                          (lambda (add!)
                                            (add! 9)
                                            987)
                                          list))
                (val4 (hash-table-contains? ht-fixnum 64))
                (val5 (begin (set! 888)
                             (hash-table-ref/default ht-fixnum 64 1234)))
                (val6 (begin (set! 8)
                             (hash-table-ref/default ht-fixnum 64 1234)))
                (val7 (begin (delete!)
                             (hash-table-ref/default ht-fixnum 64 1234)))
                (val8 (begin (set! 8)
                             (hash-table-ref/default ht-fixnum 64 1234))))
           (list val val2 val3 val4 val5 val6 val7 val8))))
      '(8 97 987 #f 888 8 1234 8))

(test (begin (hash-table-clear! ht-eq)
             (hash-table-size ht-eq))
      0)

;;; The whole hash table.

(test (begin (hash-table-set! ht-eq 'foo 13 'bar 14 'baz 18)
             (hash-table-size ht-eq))
      3)

(test (begin (hash-table-clear! ht-eq)
             (hash-table-size ht-eq))
      0)

(test (hash-table-find ht-fixnum
                       (lambda (key val)
                         (if (= 144 key (* val val))
                             (list key val)
                             #f))
                       (lambda () 99))
      '(144 12))

(test (hash-table-find ht-fixnum
                       (lambda (key val)
                         (if (= 144 key val)
                             (list key val)
                             #f))
                       (lambda () 99))
      99)

(test (hash-table-count ht-fixnum <=)
      2)

(test (hash-table-any (lambda (key val)
                        (and (= key val) 'true))
                      ht-fixnum)
      'true)

(test (hash-table-any (lambda (key val)
                        (symbol? key))
                      ht-fixnum)
      #f)

(test (hash-table-every (lambda (key val)
                          (and (= key val) 'true))
                        ht-fixnum)
      #f)

(test (hash-table-every (lambda (key val)
                          (= key (* val val)))
                        ht-fixnum)
      #t)

;;; Mapping and folding.

(test (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
           '(0 1 4 9 16 25 36 49 64 81 100 121 144 169 196))
      '(0 1 2 3 4 5 6 -1 8 9 -1 11 12 13 -1))

(test (let ((ht (hash-table-map (lambda (key val)
                                  (let ((n (+ val 1)))
                                    (values (* n n) n)))
                                eqv-comparator
                                (lambda (oldkey oldval newkey newval)
                                  newkey)
                                ht-fixnum)))
        (map (lambda (i) (hash-table-ref/default ht i -1))
             '(0 1 4 9 16 25 36 49 64 81 100 121 144 169 196)))
      '(-1 1 2 3 4 5 6 7 -1 9 10 -1 12 13 14))

(test (let ((ht (hash-table-map-values square eqv-comparator ht-fixnum)))
        (map (lambda (i) (hash-table-ref/default ht i -1))
             '(0 1 4 9 16 25 36 49 64 81 100 121 144 169 196)))
      '(0 1 4 9 16 25 36 -1 64 81 -1 121 144 169 -1))

(test (let ((keys (make-vector 15 -1))
            (vals (make-vector 15 -1)))
        (hash-table-for-each (lambda (key val)
                               (vector-set! keys val key)
                               (vector-set! vals val val))
                             ht-fixnum)
        (list keys vals))
      '(#(0 1 4 9 16 25 36 -1 64 81 -1 121 144 169 -1)
        #(0 1 2 3  4  5  6 -1  8  9 -1  11  12  13 -1)))

(test (begin (hash-table-map! (lambda (key val)
                                (if (<= 10 key)
                                    (- val)
                                    val))
                              ht-fixnum)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(0 1 4 9 16 25 36 49 64 81 100 121 144 169 196)))
      '(0 1 2 3 -4 -5 -6 -1 -8 -9 -1 -11 -12 -13 -1))

(test (list-sort < (hash-table-collect (lambda (key val) val)
                                       ht-fixnum))
      '(-13 -12 -11 -9 -8 -6 -5 -4 0 1 2 3))

(test (list-sort < (hash-table-collect (lambda (key val) key)
                                       ht-fixnum))
      '(0 1 4 9 16 25 36 64 81 121 144 169))

(test (hash-table-fold (lambda (key val acc)
                         (+ val acc))
                       0
                       ht-string-ci2)
      13)

(test (list-sort < (hash-table-fold (lambda (key val acc)
                                      (cons key acc))
                                    '()
                                    ht-fixnum))
      '(0 1 4 9 16 25 36 64 81 121 144 169))

(test (let ((ht (hash-table-filter! (lambda (key val) (> key 100))
                                    (hash-table-copy ht-fixnum #t))))
        (map (lambda (i) (hash-table-ref/default ht i -1))
             '(0 1 4 9 16 25 36 49 64 81 100 121 144 169 196)))
      '(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -11 -12 -13 -1))

(test (let ((ht (hash-table-remove! (lambda (key val) (> key 100))
                                    (hash-table-copy ht-fixnum #t))))
        (map (lambda (i) (hash-table-ref/default ht i -1))
             '(0 1 4 9 16 25 36 49 64 81 100 121 144 169 196)))
      '(0 1 2 3 -4 -5 -6 -1 -8 -9 -1 -1 -1 -1 -1))

;;; Copying and conversion.

(test (hash-table=? number-comparator ht-fixnum (hash-table-copy ht-fixnum))
      #t)

(test (hash-table=? number-comparator ht-fixnum (hash-table-copy ht-fixnum #f))
      #t)

(test (hash-table=? number-comparator ht-fixnum (hash-table-copy ht-fixnum #t))
      #t)

(test (hash-table-mutable? (hash-table-copy ht-fixnum))
      #f)

(test (hash-table-mutable? (hash-table-copy ht-fixnum #f))
      #f)

(test (hash-table-mutable? (hash-table-copy ht-fixnum #t))
      #t)

(test (hash-table->alist ht-eq)
      '())

(test (list-sort (lambda (x y) (< (car x) (car y)))
                 (hash-table->alist ht-fixnum))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . -4)
        (25 . -5)
        (36 . -6)
        (64 . -8)
        (81 . -9)
        (121 . -11)
        (144 . -12)
        (169 . -13)))

;;; Hash tables as functions.

(test (let ((sqrt1 (hash-table-accessor ht-fixnum))
            (sqrt2 (hash-table-accessor ht-fixnum (lambda () 'error)))
            (sqrt3 (hash-table-accessor ht-fixnum (lambda () 'error) list)))
        (list (sqrt1 25)
              (sqrt2 27)
              (sqrt2 36)
              (sqrt3 40)
              (sqrt3 64)))
      '(-5 error -6 error (-8)))

(test (let ((sqrt (hash-table-accessor/default ht-fixnum 'error)))
        (list (sqrt 25)
              (sqrt 27)
              (sqrt 36)
              (sqrt 40)
              (sqrt 64)))
      '(-5 error -6 error -8))

;;; Hash tables as sets.

(test (begin (hash-table-union! ht-fixnum ht-fixnum2)
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist ht-fixnum)))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . -4)
        (25 . -5)
        (36 . -6)
        (49 . 7)
        (64 . -8)
        (81 . -9)
        (121 . -11)
        (144 . -12)
        (169 . -13)))

(test (let ((ht (hash-table-copy ht-fixnum2 #t)))
        (hash-table-union! ht ht-fixnum)
        (list-sort (lambda (x y) (< (car x) (car y)))
                   (hash-table->alist ht)))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (121 . -11)
        (144 . -12)
        (169 . -13)))

(test (begin (hash-table-union! ht-eqv2 ht-fixnum)
             (hash-table=? default-comparator ht-eqv2 ht-fixnum))
      #t)

(test (begin (hash-table-intersection! ht-eqv2 ht-fixnum)
             (hash-table=? default-comparator ht-eqv2 ht-fixnum))
      #t)

(test (begin (hash-table-intersection! ht-eqv2 ht-eqv)
             (hash-table-empty? ht-eqv2))
      #t)

(test (begin (hash-table-intersection! ht-fixnum ht-fixnum2)
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist ht-fixnum)))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . -4)
        (25 . -5)
        (36 . -6)
        (49 . 7)
        (64 . -8)
        (81 . -9)))

(test (begin (hash-table-intersection!
              ht-fixnum
              (alist->hash-table '((-1 . -1) (4 . 202) (25 . 205) (100 . 10))
                                 number-comparator))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist ht-fixnum)))
      '((4 . 2)
        (25 . -5)))

(test (let ((ht (hash-table-copy ht-fixnum2 #t)))
        (hash-table-difference!
         ht
         (alist->hash-table '((-1 . -1) (4 . 202) (25 . 205) (100 . 10))
                            number-comparator))
        (list-sort (lambda (x y) (< (car x) (car y)))
                   (hash-table->alist ht)))
      '((0 . 0)
        (1 . 1)
        (9 . 3)
        (16 . 4)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)))

(test (let ((ht (hash-table-copy ht-fixnum2 #t)))
        (hash-table-xor!
         ht
         (alist->hash-table '((-1 . -1) (4 . 202) (25 . 205) (100 . 10))
                            number-comparator))
        (list-sort (lambda (x y) (< (car x) (car y)))
                   (hash-table->alist ht)))
      '((-1 . -1)
        (0 . 0)
        (1 . 1)
        (9 . 3)
        (16 . 4)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . 10)))

(test (guard (exn
              (else 'key-not-found))
       (hash-table-ref ht-default "this key won't be present"))
      'key-not-found)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test of bimaps.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bim-squares
  (make-bimap (hash-table-copy ht-fixnum2 #t)
              number-comparator))

(define bim-squares2 (bimap-copy bim-squares))

(test (bimap? ht-fixnum2) #f)
(test (bimap? bim-squares) #t)

(test (bimap? (bimap-forward-hash-table bim-squares)) #f)
(test (bimap? (bimap-reverse-hash-table bim-squares)) #f)
(test (hash-table? (bimap-forward-hash-table bim-squares)) #t)
(test (hash-table? (bimap-reverse-hash-table bim-squares)) #t)

(test (let ((ht (bimap-forward-hash-table bim-squares)))
        (list-sort (lambda (x y) (< (car x) (car y)))
                   (hash-table->alist ht)))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)))

(test (let ((ht (bimap-reverse-hash-table bim-squares)))
        (list-sort (lambda (x y) (< (car x) (car y)))
                   (hash-table->alist ht)))
      '((0 . 0)
        (1 . 1)
        (2 . 4)
        (3 . 9)
        (4 . 16)
        (5 . 25)
        (6 . 36)
        (7 . 49)
        (8 . 64)
        (9 . 81)))

(test (bimap-contains? bim-squares 36) #t)
(test (bimap-contains? bim-squares 6) #f)

(test (bimap-contains-value? bim-squares 36) #f)
(test (bimap-contains-value? bim-squares 6) #t)

(test (bimap=? bim-squares bim-squares2) #t)

(test (guard (exn (else 'error))
       (bimap-ref bim-squares 81))
      9)
(test (guard (exn (else 'error))
       (bimap-ref bim-squares 81 (lambda () 'whatever)))
      9)
(test (guard (exn (else 'error))
       (bimap-ref bim-squares 81 (lambda () 'whatever) list))
      '(9))

;;; FIXME: glass-box (implementations not required to raise an exception here)

(test (guard (exn (else 'error))
       (bimap-ref bim-squares 7))
      'error)
(test (guard (exn (else 'error))
       (bimap-ref bim-squares 7 (lambda () 'whatever)))
      'whatever)
(test (guard (exn (else 'error))
       (bimap-ref bim-squares 7 (lambda () 'whatever) list))
      'whatever)

(test (bimap-ref/default bim-squares 81 'whatever)
      9)
(test (bimap-ref/default bim-squares 7 'whatever)
      'whatever)

;;; FIXME: glass-box (implementations not required to raise an exception here)

(test (guard (exn (else 'error))
       (bimap-value-ref bim-squares 81))
      'error)
(test (guard (exn (else 'error))
       (bimap-value-ref bim-squares 81 (lambda () 'whatever)))
      'whatever)
(test (guard (exn (else 'error))
       (bimap-value-ref bim-squares 81 (lambda () 'whatever) list))
      'whatever)

(test (guard (exn (else 'error))
       (bimap-value-ref bim-squares 7))
      49)
(test (guard (exn (else 'error))
       (bimap-value-ref bim-squares 7 (lambda () 'whatever)))
      49)
(test (guard (exn (else 'error))
       (bimap-value-ref bim-squares 7 (lambda () 'whatever) list))
      '(49))

(test (bimap-value-ref/default bim-squares 81 'whatever)
      'whatever)
(test (bimap-value-ref/default bim-squares 7 'whatever)
      49)

;;; FIXME: glass-box (some implementations may not support immutable tables)

(test (hash-table-mutable? (bimap-forward-hash-table bim-squares2))
      #f)
(test (hash-table-mutable? (bimap-reverse-hash-table bim-squares2))
      #f)
(test (hash-table-mutable?
       (bimap-forward-hash-table (bimap-copy bim-squares2)))
      #f)
(test (hash-table-mutable?
       (bimap-reverse-hash-table (bimap-copy bim-squares2)))
      #f)
(test (hash-table-mutable?
       (bimap-forward-hash-table (bimap-copy bim-squares2 #f)))
      #f)
(test (hash-table-mutable?
       (bimap-reverse-hash-table (bimap-copy bim-squares2 #f)))
      #f)
(test (hash-table-mutable?
       (bimap-forward-hash-table (bimap-copy bim-squares2 #t)))
      #t)
(test (hash-table-mutable?
       (bimap-reverse-hash-table (bimap-copy bim-squares2 #t)))
      #t)

;;; end of glass-box testing for mutability

(test (bimap=? bim-squares2 (bimap-copy bim-squares2))
      #t)
(test (bimap=? bim-squares2 (bimap-copy bim-squares2))
      #t)
(test (bimap=? bim-squares2 (bimap-copy bim-squares2 #f))
      #t)
(test (bimap=? bim-squares2 (bimap-copy bim-squares2 #f))
      #t)
(test (bimap=? bim-squares2 (bimap-copy bim-squares2 #t))
      #t)
(test (bimap=? bim-squares2 (bimap-copy bim-squares2 #t))
      #t)

(test (begin (bimap-set! bim-squares 25 -5)
             (bimap=? bim-squares bim-squares2))
      #f)
(test (bimap-contains? bim-squares 25)
      #t)
(test (bimap-contains-value? bim-squares 5)
      #f)
(test (bimap-contains-value? bim-squares -5)
      #t)

(test (begin (bimap-set! bim-squares 100 -10)
             (bimap-ref/default bim-squares 100 'nonsense))
      -10)
(test (bimap-contains? bim-squares 100)
      #t)
(test (bimap-contains-value? bim-squares -10)
      #t)

(test (begin (bimap-set-entries! bim-squares
                                 '(100 25 81 100 25 121 81)
                                 '(123 17 44 -10  5 -11  9))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . -10)
        (121 . -11)))

(test (let ((ht (bimap-reverse-hash-table bim-squares)))
        (list-sort (lambda (x y) (< (car x) (car y)))
                   (hash-table->alist ht)))
      '((-11 . 121)
        (-10 . 100)
        (0 . 0)
        (1 . 1)
        (2 . 4)
        (3 . 9)
        (4 . 16)
        (5 . 25)
        (6 . 36)
        (7 . 49)
        (8 . 64)
        (9 . 81)))

(test (begin (bimap-set-entries! bim-squares
                                 '(100 121)
                                 '(10  11))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . 10)
        (121 . 11)))

(test (let ((ht (bimap-reverse-hash-table bim-squares)))
        (list-sort (lambda (x y) (< (car x) (car y)))
                   (hash-table->alist ht)))
      '((0 . 0)
        (1 . 1)
        (2 . 4)
        (3 . 9)
        (4 . 16)
        (5 . 25)
        (6 . 36)
        (7 . 49)
        (8 . 64)
        (9 . 81)
        (10 . 100)
        (11 . 121)))

(test (begin (bimap-delete-keys! bim-squares '())
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . 10)
        (121 . 11)))

(test (begin (bimap-delete! bim-squares)
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . 10)
        (121 . 11)))

(test (begin (bimap-delete! bim-squares 99 100 101)
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (121 . 11)))

(test (begin (bimap-delete-keys! bim-squares '(8 7 121 2))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)))

(test (begin (bimap-delete-keys! bim-squares '())
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-reverse-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (2 . 4)
        (3 . 9)
        (4 . 16)
        (5 . 25)
        (6 . 36)
        (7 . 49)
        (8 . 64)
        (9 . 81)))

;;; FIXME: glass-box (implementations not required to raise an exception here)

(test (guard (exn (else 'error))
       (bimap-extend! bim-squares 100))
      'error)

(test (guard (exn (else 'error))
       (bimap-extend! bim-squares 81))
      9)

(test (begin (guard (exn (else 'error))
              (bimap-extend! bim-squares 100 (lambda () -10)))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . -10)))

(test (guard (exn (else 'error))
       (bimap-extend! bim-squares 100 (lambda () -10) abs))
      10)

(test (list-sort (lambda (x y) (< (car x) (car y)))
                 (hash-table->alist
                  (bimap-forward-hash-table bim-squares)))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . -10)))

(test (guard (exn (else 'error))
       (bimap-extend/default! bim-squares 100 67))
      -10)

(test (guard (exn (else 'error))
       (bimap-extend/default! bim-squares 121 11))
      11)

;;; FIXME: glass-box (implementations not required to raise an exception here)

(test (guard (exn (else 'error))
       (bimap-replace! bim-squares 144))
      'error)

(test (guard (exn (else 'error))
       (bimap-replace! bim-squares 81))
      9)

(test (begin (guard (exn (else 'error))
              (bimap-replace! bim-squares 100 (lambda () -77)))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . -10)
        (121 . 11)))

(test (begin (guard (exn (else 'error))
              (bimap-replace! bim-squares 88 (lambda () -77) abs))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . -10)
        (121 . 11)))

(test (guard (exn (else 'error))
       (bimap-replace! bim-squares 100 (lambda () -10) abs))
      10)

(test (list-sort (lambda (x y) (< (car x) (car y)))
                 (hash-table->alist
                  (bimap-forward-hash-table bim-squares)))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . 10)
        (121 . 11)))

(test (guard (exn (else 'error))
       (bimap-replace/default! bim-squares 100 67))
      10)

(test (guard (exn (else 'error))
       (bimap-replace/default! bim-squares 123 45))
      45)

(test (begin (guard (exn (else 'error))
              (bimap-replace! bim-squares 100 (lambda () -77) -))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . -10)
        (121 . 11)))

(test (begin (guard (exn (else 'error))
              (bimap-update! bim-squares 100 abs))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . 10)
        (121 . 11)))

(test (begin (guard (exn (else 'error))
              (bimap-update! bim-squares 100 - (lambda () 99)))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . -10)
        (121 . 11)))

(test (begin (guard (exn (else 'error))
              (bimap-update! bim-squares 100 - (lambda () 99) square))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . -100)
        (121 . 11)))

(test (begin (guard (exn (else 'error))
              (bimap-update! bim-squares 144 - (lambda () 12)))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . -100)
        (121 . 11)
        (144 . -12)))

(test (begin (guard (exn (else 'error))
              (bimap-update! bim-squares 169 - (lambda () 99) square))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . -100)
        (121 . 11)
        (144 . -12)
        (169 . -99)))

(test (begin (guard (exn (else 'error))
              (bimap-update/default! bim-squares 169 - 33))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . -100)
        (121 . 11)
        (144 . -12)
        (169 . 99)))

(test (begin (guard (exn (else 'error))
              (bimap-update/default! bim-squares 196 - 14))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . -100)
        (121 . 11)
        (144 . -12)
        (169 . 99)
        (196 . -14)))

(test (list-sort (lambda (x y) (< (car x) (car y)))
                 (hash-table->alist
                  (bimap-reverse-hash-table bim-squares)))
      '(
        (-100 . 100)
        (-14 . 196)
        (-12 . 144)
        (0 . 0)
        (1 . 1)
        (2 . 4)
        (3 . 9)
        (4 . 16)
        (5 . 25)
        (6 . 36)
        (7 . 49)
        (8 . 64)
        (9 . 81)
        (11 . 121)
        (99 . 169)))

(test (begin (guard (exn (else 'error))
              (bimap-filter! (lambda (key val)
                               (= key (square val)))
                             bim-squares))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (121 . 11)
        (144 . -12)
        (196 . -14)))

(test (begin (guard (exn (else 'error))
              (bimap-remove! (lambda (key val)
                               (< val 0))
                             bim-squares))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist
                         (bimap-forward-hash-table bim-squares))))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (121 . 11)))

(test (begin (bimap-clear! bim-squares)
             (bimap=? bim-squares
                      (make-bimap (make-hash-table string-comparator)
                                  string-comparator)))
      #t)

(displayln "Done.")

(exit ultimate-exit-status)

; eof
