;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme base) procedures and syntaxes:
;;;
;;;     quote                                   ; R7RS 4.1.2
;;;
;;;     lambda                                  ; R7RS 4.1.4
;;;
;;;     if                                      ; R7RS 4.1.5
;;;
;;;     set!                                    ; R7RS 4.1.6
;;;
;;;     include                                 ; R7RS 4.1.7
;;;     include-ci
;;;
;;;     cond                                    ; R7RS 4.2.1
;;;     case
;;;     and
;;;     or
;;;     when
;;;     unless
;;;     cond-expand
;;;
;;;     let                                     ; R7RS 4.2.2
;;;     let*
;;;     letrec
;;;     letrec*
;;;     let-values
;;;     let*-values
;;;
;;;     begin                                   ; R7RS 4.2.3
;;;
;;;     do                                      ; R7RS 4.2.4
;;;     "named let"
;;;
;;;     make-parameter                          ; R7RS 4.2.6
;;;     parameterize
;;;
;;;     guard                                   ; R7RS 4.2.7
;;;
;;;     quasiquote                              ; R7RS 4.2.8
;;;     unquote
;;;     unquote-splicing
;;;
;;;     let-syntax                              ; R7RS 4.3.1
;;;     letrec-syntax
;;;
;;;     syntax-rules                            ; R7RS 4.3.2
;;;
;;;     syntax-error                            ; R7RS 4.3.3
;;;
;;;     define                                  ; R7RS 5.3.1, 5.3.2
;;;
;;;     define-values                           ; R7RS 5.3.3
;;;
;;;     define-syntax                           ; R7RS 5.4
;;;
;;;     define-record-type                      ; R7RS 5.5
;;;
;;;     eqv?                                    ; R7RS 6.1
;;;     eq?
;;;     equal?
;;;
;;;     number?                                 ; R7RS 6.2.6
;;;     complex?
;;;     real?
;;;     rational?
;;;     integer?
;;;     exact?
;;;     inexact?
;;;     exact-integer?
;;;     =
;;;     <
;;;     >
;;;     <=
;;;     >=
;;;     zero?
;;;     positive?
;;;     negative?
;;;     odd?
;;;     even?
;;;     max
;;;     min
;;;     +
;;;     *
;;;     -
;;;     /
;;;     abs
;;;     floor/
;;;     floor-quotient
;;;     floor-remainder
;;;     truncate/
;;;     truncate-quotient
;;;     truncate-remainder
;;;     quotient
;;;     remainder
;;;     modulo
;;;     gcd
;;;     lcm
;;;     numerator
;;;     denominator
;;;     floor
;;;     ceiling
;;;     truncate
;;;     round
;;;     rationalize
;;;     square
;;;     exact-integer-sqrt
;;;     expt
;;;     inexact
;;;     exact
;;;
;;;     number->string                          ; R7RS 6.2.7
;;;     string->number
;;;
;;;     not                                     ; R7RS 6.3
;;;     boolean?
;;;     boolean=?
;;;
;;;     pair?                                   ; R7RS 6.4
;;;     cons
;;;     car
;;;     cdr
;;;     set-car!
;;;     set-cdr!
;;;     caar
;;;     cadr
;;;     cdar
;;;     cddr
;;;     null?
;;;     list?
;;;     make-list
;;;     list
;;;     length
;;;     append
;;;     reverse
;;;     list-tail
;;;     list-ref
;;;     list-set!
;;;     memq
;;;     memv
;;;     member
;;;     assq
;;;     assv
;;;     assoc
;;;     list-copy
;;;
;;;     symbol?                                 ; R7RS 6.5
;;;     symbol=?
;;;     symbol->string
;;;     string->symbol
;;;
;;;     char?                                   ; R7RS 6.6
;;;     char=?
;;;     char<?
;;;     char>?
;;;     char<=?
;;;     char>=?
;;;     char->integer
;;;     integer->char
;;;
;;;     string?                                 ; R7RS 6.7
;;;     make-string
;;;     string
;;;     string-length
;;;     string-ref
;;;     string-set!
;;;     string=?
;;;     string<?
;;;     string>?
;;;     string<=?
;;;     string>=?
;;;     substring
;;;     string-append
;;;     string->list
;;;     list->string
;;;     string-copy
;;;     string-copy!
;;;     string-fill!
;;;
;;;     vector?                                 ; R7RS 6.8
;;;     make-vector
;;;     vector
;;;     vector-length
;;;     vector-ref
;;;     vector-set!
;;;     vector->list
;;;     list->vector
;;;     vector->string
;;;     string->vector
;;;     vector-copy
;;;     vector-copy!
;;;     vector-append
;;;     vector-fill!
;;;
;;;     bytevector?                             ; R7RS 6.9
;;;     make-bytevector
;;;     bytevector
;;;     bytevector-length
;;;     bytevector-u8-ref
;;;     bytevector-u8-set!
;;;     bytevector-copy
;;;     bytevector-copy!
;;;     bytevector-append
;;;     utf8->string
;;;     string->utf8
;;;
;;;     procedure?                              ; R7RS 6.10
;;;     apply
;;;     map
;;;     string-map
;;;     vector-map
;;;     for-each
;;;     string-for-each
;;;     vector-for-each
;;;     call-with-current-continuation
;;;     call/cc
;;;     values
;;;     call-with-values
;;;     dynamic-wind
;;;
;;;     with-exception-handler                  ; R7RS 6.11
;;;     raise
;;;     raise-continuable
;;;     error
;;;     error-object?
;;;     error-object-message
;;;     error-object-irritants
;;;     read-error?
;;;     file-error?
;;;
;;;     call-with-port                          ; R7RS 6.13.1
;;;     input-port?
;;;     output-port?
;;;     textual-port?
;;;     binary-port?
;;;     port?
;;;     input-port-open?
;;;     output-port-open?
;;;     current-input-port
;;;     current-output-port
;;;     current-error-port
;;;     close-port
;;;     close-input-port
;;;     close-output-port
;;;     open-input-string
;;;     open-output-string
;;;     get-output-string
;;;     open-input-bytevector
;;;     open-output-bytevector
;;;     get-output-bytevector
;;;
;;;     read-char                               ; R7RS 6.13.2
;;;     peek-char
;;;     read-line
;;;     eof-object?
;;;     eof-object
;;;     char-ready?
;;;     read-string
;;;     read-u8
;;;     peek-u8
;;;     u8-ready?
;;;     read-bytevector
;;;     read-bytevector!
;;;
;;;     newline                                 ; R7RS 6.13.3
;;;     write-char
;;;     write-string
;;;     write-u8
;;;     write-bytevector
;;;     flush-output-port
;;;
;;;     features                                ; R7RS 6.14
;;;
;;;     ...                                     ; syntactic keywords
;;;     =>                                      ;   recognized by
;;;     _                                       ;   various R7RS
;;;     else                                    ;   syntaxes


(define-library (tests scheme base)
  (export run-base-tests)
  (import (scheme base)
          (tests scheme test))

  ;; For testing R7RS 4.2.1 and 4.2.7

  (cond-expand
   ((library (scheme write))
    (import (scheme write)))
   (else))

  ;; For testing R7RS 4.2.8 and 6.8

  (cond-expand
   ((library (scheme inexact))
    (import (scheme inexact)))
   (else))

  ;; For testing R7RS 4.3.3

  (cond-expand
   ((library (scheme eval))
    (import (scheme eval)))
   (else))

  ;; For testing R7RS 6.4 and 6.10

  (cond-expand
   ((library (scheme char))
    (import (only (scheme char)
                  string-ci=?
                  char-upcase
                  char-downcase
                  char-foldcase)))
   (else))

  ;; For testing R7RS 6.11

  (cond-expand
   ((library (scheme read))
    (import (only (scheme read) read)))
   (else))

  (cond-expand
   ((library (scheme file))
    (import (only (scheme file) open-input-file)))
   (else))

  (begin

   ;; For a test of R7RS 4.1.4

   (define reverse-subtract
     (lambda (x y) (- y x)))

   (define add4
     (let ((x 4))
       (lambda (y) (+ x y))))

   ;; For a test of R7RS 4.1.6 and 4.2.3

   (define xxx 2)

   ;; For a test of R7RS 4.2.6

   (define radix
     (make-parameter
      10
      (lambda (x)
        (if (and (exact-integer? x) (<= 2 x 16))
            x
            (error "invalid radix")))))

   (define (f n) (number->string n (radix)))

   ;; For testing R7RS 4.1.7

   (include "tests/scheme/base-test1.scm")

   (include-ci "tests/scheme/base-test2.scm")

   ;; The R7RS encourages (but does not require) implementations
   ;; "to search for files in the directory which contains the
   ;; including file".  Implementations that fail to do that will
   ;; probably fail the following test.
   ;;
   ;; FIXME: implementation-dependent

   (include "tests/scheme/base-test3.scm")

   ;; For testing R7RS 4.3.2

   (define-syntax be-like-begin
     (syntax-rules ()
      ((be-like-begin name)
       (define-syntax name
         (syntax-rules ()
           ((name expr (... ...))
            (begin expr (... ...))))))))

   (define-syntax be-like-begin-alt
     (syntax-rules &etc ()
      ((be-like-begin-alt name)
       (define-syntax name
         (syntax-rules ()
           ((name expr (&etc ...))
            (begin expr (&etc ...))))))))

   ;; These macros were contributed by Alex Shinn, who reported them
   ;; as bugs in Larceny v0.98.

   (define-syntax underscore-as-literal
    (syntax-rules (_)
     ((underscore-as-literal _)
      'under)
     ((underscore-as-literal x)
      'other)))

  (define-syntax ellipses-as-literal
    (syntax-rules (...)
     ((ellipses-as-literal ...)
      'under)
     ((ellipses-as-literal x)
      'other)))

   ;; For a test of R7RS 5.3.1

   (define add3
     (lambda (x) (+ x 3)))

   (define first car)

   ;; For tests of R7RS 6.1

   (define gen-counter
     (lambda ()
       (let ((n 0))
         (lambda () (set! n (+ n 1)) n))))

   (define gen-loser
     (lambda ()
       (let ((n 0))
         (lambda () (set! n (+ n 1)) 27))))
  
   ;; For tests of R7RS 6.2.6
   ;;
   ;; div and mod aren't R7RS, but these helper syntaxes keep the R6RS names

   (define-syntax divmod-test/?
     (syntax-rules ()
      ((_ x1 x2)
       (begin
        (test/values (floor/ x1 x2)
                     (floor-quotient x1 x2)
                     (floor-remainder x1 x2))
        (test/values (truncate/ x1 x2)
                     (truncate-quotient x1 x2)
                     (truncate-remainder x1 x2))))))

   (define-syntax divmod-test
     (syntax-rules ()
      ((_ x1 x2)
       (begin
        (divmod-test/? x1 x2)
        (test (floor-quotient x1 x2)    (floor    (/ x1 x2)))
        (test (truncate-quotient x1 x2) (truncate (/ x1 x2)))
        (test (+ (* x2 (floor-quotient x1 x2))
                 (floor-remainder x1 x2))
              x1)
        (test (+ (* x2 (truncate-quotient x1 x2))
                 (truncate-remainder x1 x2))
              x1)
        (test (truncate-quotient x1 x2)  (quotient x1 x2))
        (test (truncate-remainder x1 x2) (remainder x1 x2))
        (test (modulo x1 x2) (floor-remainder x1 x2))))))

   ;; For tests of R7RS 6.2.7

   (define-syntax test-string-to-number
     (syntax-rules ()
       ((_ (str num) ...) (begin (test (string->number str) num) ...))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (define (run-base-tests)

     ;;     quote                                   ; R7RS 4.1.2

     ;; R6RS 11.4.1

     (test (quote a) 'a)
     (test (quote #(a b c)) (vector 'a 'b 'c))
     (test (quote (+ 1 2)) '(+ 1 2))
     (test '"abc" "abc")
     (test 'a 'a)
     (test '#(a b c) (vector 'a 'b 'c))
     (test '() (list))
     (test '(+ 1 2) '(+ 1 2))
     (test '(quote a) '(quote a))
     (test ''a '(quote a))
     (test '145932 145932)
     (test '"abc" "abc")
     (test '#\a #\a)                ; R7RS erratum 4
     (test '#(a 10) #(a 10))
     (test '#u8(64 65) #u8(64 65))
     (test '#t #t)

     ;;     (procedure calls)                       ; R7RS 4.1.3

     (test (+ 3 4) 7)
     (test ((if #f + *) 3 4) 12)

     ;;     lambda                                  ; R7RS 4.1.4

     ;; R6RS 11.4.2
     ;; (test (lambda (x) (+ x x)) {a procedure})

     (test ((lambda (x) (+ x x)) 4) 8)
     (test ((lambda (x)
              (define (p y)
                (+ y 1))
              (+ (p x) x))
            5) 
           11)

     (test (reverse-subtract 7 10) 3)

     (test (add4 6) 10)

     (test ((lambda x x) 3 4 5 6) '(3 4 5 6))
     (test ((lambda (x y . z) z) 3 4 5 6)
           '(5 6))
    
     ;;     if                                      ; R7RS 4.1.5

     ;; R6RS 11.4.3

     (test (if (> 3 2) 'yes 'no) 'yes)
     (test (if (> 2 3) 'yes 'no) 'no)
     (test (if (> 3 2)
               (- 3 2)
               (+ 3 2))
           1)
     (test/unspec (if #f #f))

     (test (if (> 3 2) 17) 17)

     ;;     set!                                    ; R7RS 4.1.6

     ;; R6RS 11.4.4

     (test (let ((x 2))
             (+ x 1)
             (set! x 4)
             (+ x 1)) 
           5)

     (test (+ xxx 1) 3)
     (test/unspec (set! xxx 4))
     (test (+ xxx 1) 5)

     ;;     include                                 ; R7RS 4.1.7
     ;;     include-ci

     (test (fact 6) 720)

     (test (fib 10) 55)

     ;; The R7RS encourages (but does not require) implementations
     ;; "to search for files in the directory which contains the
     ;; including file".  Implementations that fail to do that will
     ;; probably fail the following test.
     ;;
     ;; FIXME: implementation-dependent

     (test bd578d6d 319)

     ;;     cond                                    ; R7RS 4.2.1
     ;;     case
     ;;     and
     ;;     or
     ;;     when
     ;;     unless
     ;;     cond-expand

     ;; 11.4.5
     (test (cond ((> 3 2) 'greater)
                 ((< 3 2) 'less))          
           'greater)

     (test (cond ((> 3 3) 'greater)
                 ((< 3 3) 'less)
                 (else 'equal))
           'equal)
     (test (cond ('(1 2 3) => cadr)
                 (else #t))          
           2)
     (test (cond ((assv 'b '((a 1) (b 2))) => cadr)
                 (else #f))
           2)

     (test (case (* 2 3)
             ((2 3 5 7) 'prime)
             ((1 4 6 8 9) 'composite))
           'composite)
     (test/unspec (case (car '(c d))
                    ((a) 'a)
                    ((b) 'b)))
     (test (case (car '(c d))
             ((a e i o u) 'vowel)
             ((w y) 'semivowel)
             (else 'consonant))
           'consonant)
     (test (case (list 1 2) ; newly allocated => not `eqv?'
             (((1 2)) 'two)
             (else 'other))
           'other)

     (test (case (car '(c d))
             ((a e i o u) 'vowel)
             ((w y) 'semivowel)
             (else => (lambda (x) x)))
           'c)

     (test (and (= 2 2) (> 2 1)) #t)
     (test (and (= 2 2) (< 2 1)) #f)
     (test (and 1 2 'c '(f g)) '(f g))
     (test (and) #t)

     (test (or (= 2 2) (> 2 1)) #t)
     (test (or (= 2 2) (< 2 1)) #t)
     (test (or #f #f #f) #f)
     (test (or '(b c) (/ 3 0)) '(b c))

     (test (or (memq 'b '(a b c))
               (/ 3 0))
           '(b c))

     (test (when (> 3 2) 'greater) 'greater)
     (test/unspec (when (< 3 2) 'greater))
     (test/unspec (unless (> 3 2) 'less))
     (test (unless (< 3 2) 'less) 'less)

     (cond-expand
      ((library (scheme write))
       (begin (test (let ((q (open-output-string)))
                      (parameterize ((current-output-port q))
                       (when (= 1 (inexact 1))
                             (display "1")
                             (display "2")))
                      (get-output-string q))
                    "12")))
      (else))

     ;; The previous cond-expand is a top-level expression.
     ;; A cond-expand can appear anywhere within an expression.

     (test (+ 1
              (cond-expand (larceny 2)
                           ((not larceny) 3))
              (cond-expand (larceny 3)
                           (else 2)))
           6)

     ;;     let                                     ; R7RS 4.2.2
     ;;     let*
     ;;     letrec
     ;;     letrec*
     ;;     let-values
     ;;     let*-values

     ;; R6RS 11.4.6

     (test (let ((x 2) (y 3))
             (* x y))
           6)

     (test (let ((x 2) (y 3))
             (let ((x 7)
                   (z (+ x y)))
               (* z x)))
           35)
     (test (let ((x 2) (y 3))
             (let* ((x 7)
                    (z (+ x y)))
               (* z x)))
           70)
     (test (letrec ((even?
                     (lambda (n)
                       (if (zero? n)
                           #t
                           (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (zero? n)
                           #f
                           (even? (- n 1))))))
             (even? 88))   
           #t)
     (test (letrec* ((p
                      (lambda (x)
                        (+ 1 (q (- x 1)))))
                     (q
                      (lambda (y)
                        (if (zero? y)
                            0
                            (+ 1 (p (- y 1))))))
                     (x (p 5))
                     (y x))
                    y)
           5)

     ;; R7RS erratum 9:

     (cond-expand
      ((library (scheme inexact))
       (let ()
         (define (means ton)
           (letrec* ((mean (lambda (f g)
                             (f (/ (sum g ton) n))))
                     (sum  (lambda (g ton)
                             (if (null? ton)
                                 (+)
                                 (if (number? ton)
                                     (g ton)
                                     (+ (sum g (car ton))
                                        (sum g (cdr ton)))))))
                     (n (sum (lambda (x) 1) ton)))
             (values (mean values values)
                     (mean exp log)
                     (mean / /))))
         (let-values (((x y z) (means '(3 (1 4)))))
           (test/approx (inexact x) (inexact (/ 8 3)))
           (test/approx (inexact y) (inexact (/ 22894285 10000000)))
           (test/approx (inexact z) (inexact (/ 36 19))))))
      (else))

     (test (let-values (((root rem) (exact-integer-sqrt 32)))
             (* root rem))
           35)

     (test (let-values (((a b) (values 1 2))
                        ((c d) (values 3 4)))
             (list a b c d))
           '(1 2 3 4))
     (test (let-values (((a b . c) (values 1 2 3 4)))
             (list a b c))
           '(1 2 (3 4)))
     (test (let ((a 'a) (b 'b) (x 'x) (y 'y))
             (let-values (((a b) (values x y))
                          ((x y) (values a b)))
               (list a b x y)))
           '(x y a b))
     (test (let ((a 'a) (b 'b) (x 'x) (y 'y))
             (let*-values (((a b) (values x y))
                           ((x y) (values a b)))
               (list a b x y)))
           '(x y x y))

     ;;     begin                                   ; R7RS 4.2.3

     ;; R6RS 11.4.7

     (test/unspec (set! xxx 0))
     (test (and (= xxx 0)
                (begin (set! xxx 5)
                       (+ xxx 1)))
           6)

     (test (begin (set! xxx 5)
                  (+ xxx 1))
           6)
     (test/output/unspec
      (begin (display "4 plus 1 equals ")
             (display (+ 4 1)))
      "4 plus 1 equals 5")

     ;;     do                                      ; R7RS 4.2.4
     ;;     "named let"

     ;; R6RS libraries chapter 5

     (test (do ((vec (make-vector 5))
                (i 0 (+ i 1)))
               ((= i 5) vec)
             (vector-set! vec i i))
           '#(0 1 2 3 4))

     (test (let ((x '(1 3 5 7 9)))
             (do ((x x (cdr x))
                  (sum 0 (+ sum (car x))))
                 ((null? x) sum)))
           25)

     ;;

     (test (let loop ((numbers '(3 -2 1 6 -5))
                      (nonneg '())
                      (neg '()))
             (cond ((null? numbers) (list nonneg neg))
                   ((>= (car numbers) 0)
                    (loop (cdr numbers)
                          (cons (car numbers) nonneg)
                          neg))
                   ((< (car numbers) 0)
                    (loop (cdr numbers)
                          nonneg
                          (cons (car numbers) neg)))))
           '((6 1 3) (-5 -2)))

     ;;     make-parameter                          ; R7RS 4.2.6
     ;;     parameterize

     (let ()
       (define parry
         (make-parameter "10"
                         (lambda (x)
                           (if (number? x)
                               x
                               (string->number x)))))
       (test (parry) 10)
       (parameterize ((parry 16))
        (test (parry) 16))
       (parameterize ((parry "230"))
        (test (parry) 230))
       (test (parry) 10))

     (test (f 12) "12")
     (test (parameterize ((radix 2))
            (f 12))
           "1100")
     (test (f 12) "12")
     (test/unspec (radix 16))

     (test (error-object? (guard (exn (else exn))
                           (parameterize ((radix 0))
                            (f 12))))
           #t)

     (test (let ((param (make-parameter 1 (lambda (x) (* 10 x)))))
             (parameterize ((param 2)) #f)
             (param))
           10)

     ;;     guard                                   ; R7RS 4.2.7

     (test (guard (condition
                   ((assq 'a condition) => cdr)
                   ((assq 'b condition)))
            (raise (list (cons 'a 42))))
           42)

     (test (guard (condition
                   ((assq 'a condition) => cdr)
                   ((assq 'b condition)))
            (raise (list (cons 'b 23))))
           '(b . 23))

    (let ((v '()))
      (test (guard (exn ((equal? exn 5) 'five))
                   ;; `guard' should jump back in before re-raising
                   (guard (exn ((equal? exn 6) 'six))
                          (dynamic-wind
                              (lambda () (set! v (cons 'in v)))
                              (lambda () (raise 5))
                              (lambda () (set! v (cons 'out v))))))
            'five)
      (test v '(out in out in)))

     (test (let* ((q (open-output-string))
                  (x (guard (con
                             ((error-object? con)
                              (display (error-object-message con) q)
                              'error)
                             (else
                              (display "the program has a bug")
                              'bug))
                      (list (error "John Lennon")))))
             (list x (get-output-string q)))
           '(error "John Lennon"))

     (cond-expand
      ((and (library (scheme read))
            (library (scheme write))
            (library (scheme file)))
       (test (let* ((q (open-output-string))
                    (x (guard (con
                               ((file-error? con)
                                (display "error opening file" q)
                                #f)
                               ((read-error? con)
                                (display "error reading file" q)
                                #f))
                        (open-input-file "foo-must-not-exist.scm"))))
               (list x (get-output-string q)))
             '(#f "error opening file")))
      (else))
    
     (let ((v '()))
       (test (guard (exn ((equal? exn 5) 'five))
                    ;; `guard' should jump back in before re-raising
                    (guard (exn ((equal? exn 6) 'six))
                           (dynamic-wind
                               (lambda () (set! v (cons 'in v)))
                               (lambda () (raise 5))
                               (lambda () (set! v (cons 'out v))))))
             'five)
       (test v '(out in out in)))

     ;;     quasiquote                              ; R7RS 4.2.8
     ;;     unquote
     ;;     unquote-splicing

     ;; R6RS 11.17

     (test `(list ,(+ 1 2) 4)  '(list 3 4))
     (test (let ((name 'a)) `(list ,name ',name)) 
           '(list a (quote a)))
     (test `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
           '(a 3 4 5 6 b))

     (test `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
           '((foo 7) . cons))
     (test `#(10 5 ,(- 4) ,@(map - '(16 9)) 8)
           '#(10 5 -4 -16 -9 8))

     (cond-expand
      ((library (scheme inexact))
       (test (vector-map exact `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))
             '#(10 5 2 4 3 8)))
      (else))

     (test (let ((foo '(foo bar)) (@baz 'baz))
             `(list ,@foo , @baz))
           '(list foo bar baz))

     (test (let ((name 'foo))
             `((unquote name name name)))
           '(foo foo foo))
     (test (let ((name '(foo)))
             `((unquote-splicing name name name)))
           '(foo foo foo))
     (test (let ((q '((append x y) (sqrt 9))))
             ``(foo ,,@q)) 
           '`(foo (unquote (append x y) (sqrt 9))))
     (test (let ((x '(2 3))
                 (y '(4 5)))
             `(foo (unquote (append x y) (- 9))))
           '(foo (2 3 4 5) -9))

     (test `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) 
           '(a `(b ,(+ 1 2) ,(foo 4 d) e) f))
     (test (let ((name1 'x)
                 (name2 'y))
             `(a `(b ,,name1 ,',name2 d) e))
           '(a `(b ,x ,'y d) e))

     (test (let ((a 3)) `((1 2) ,a ,4 ,'five 6))
           '((1 2) 3 4 five 6))
     (test (let ((a 3)) `((1 2) ,a ,4 ,'five 6))
           (let ((a 3)) 
             (cons '(1 2)
                   (cons a (cons 4 (cons 'five '(6)))))))

     (test (quasiquote (list (unquote (+ 1 2)) 4))
           '(list 3 4))

     (test '(quasiquote (list (unquote (+ 1 2)) 4))
           '`(list ,(+ 1 2) 4))
    
     ;;     let-syntax                              ; R7RS 4.3.1
     ;;     letrec-syntax

     ;; R6RS 11.18

     (test (let-syntax ((when (syntax-rules ()
                                ((when test stmt1 stmt2 ...)
                                 (if test
                                     (begin stmt1
                                            stmt2 ...))))))
             (let ((if #t))
               (when if (set! if 'now))
               if))
           'now)

     (test (let ((x 'outer))
             (let-syntax ((m (syntax-rules () ((m) x))))
               (let ((x 'inner))
                 (m))))
           'outer)

     (test (let ()
             (let-syntax ((def (syntax-rules ()
                                 ((def stuff ...) (define stuff ...)))))
               (def foo 42))
             foo)
           42)

     ;; FIXME: The following test is commented out.
     ;; It's legal in R6RS, but probably isn't in R7RS.
     ;;
     ;; The reason I'm uncertain is that the R7RS description of let-syntax
     ;; in section 4.3.1 is almost certainly wrong and looks like a
     ;; copy/paste error:  "<body> is a sequence of one or more definitions
     ;; followed by one or more expressions."
     ;; Surely the first "one or more" should be "zero or more", but was
     ;; the same mistake made with the second "one or more"?

#;   (test (let ()
             (let-syntax ())
             5) 
           5)

     (test (letrec-syntax
               ((my-or (syntax-rules ()
                         ((my-or) #f)
                         ((my-or e) e)
                         ((my-or e1 e2 ...)
                          (let ((temp e1))
                            (if temp
                                temp
                                (my-or e2 ...)))))))
             (let ((x #f)
                   (y 7)
                   (temp 8)
                   (let odd?)
                   (if even?))
               (my-or x
                      (let temp)
                      (if y)
                      y)))
           7)

     (test (let ((f (lambda (x) (+ x 1))))
             (let-syntax ((f (syntax-rules ()
                               ((f x) x)))
                          (g (syntax-rules ()
                               ((g x) (f x)))))
               (list (f 1) (g 1)))) 
           '(1 2))
    
     (test (let ((f (lambda (x) (+ x 1))))
             (letrec-syntax ((f (syntax-rules ()
                                  ((f x) x)))
                             (g (syntax-rules ()
                                  ((g x) (f x)))))
               (list (f 1) (g 1)))) 
           '(1 1))

     ;;     syntax-rules                            ; R7RS 4.3.2

     (test (let ()
             (be-like-begin sequence)
             (sequence 1 2 3 4))
           4)

     (test (let ()
             (be-like-begin-alt sequence)
             (sequence 1 2 3 4))
           4)

     (test (let ((=> #f))
             (cond (#t => 'ok)))
           'ok)

     (test (underscore-as-literal _) 'under)
     (test (underscore-as-literal 5) 'other)
     (test (ellipses-as-literal ...) 'under)
     (test (ellipses-as-literal 6)   'other)
    
     ;;     syntax-error                            ; R7RS 4.3.3

     ;; The R7RS says we "can't count on being able to catch
     ;; syntax errors with exception handlers or guards."
     ;;
     ;; Apparently there is no portable way to test syntax-error
     ;; within a test program like this, so the following example
     ;; is just a comment.

#|
     (define-syntax simple-let
       (syntax-rules ()
        ((_ (head ... ((x . y) val) . tail)
            body1 body2)
         (syntax-error "expected an identifier but got"
                       (x . y)))
         ((_ ((name val) ...) body1 body2 ...)
          ((lambda (name ...) body1 body2 ...)
             val ...))))

     (simple-let ((x 3) (y 4)) (+ x y))

     (simple-let ((x 3) ((y z) 4)) (+ x y))
|#

     ;;     define                                  ; R7RS 5.3.1, 5.3.2

     (test (add3 3) 6)

     (test (first '(1 2)) 1)

     ;;     define-values                           ; R7RS 5.3.3

     (test (let ()
             (define-values (x y)
               (exact-integer-sqrt 17))   ; erratum 12
             (list x y))
           '(4 1))

     (test (let ()
             (define-values (x y) (values 1 2))
             (+ x y))
           3)

     (test (let ()                                  ; bug #738 in Larceny 0.98
             (define-values (a b c) (values 1 2 3))
             (list a b c))
           '(1 2 3))

     ;;     define-syntax                           ; R7RS 5.4

     (test (let ((x 1) (y 2))
             (define-syntax swap!
               (syntax-rules ()
                ((swap! a b)
                 (let ((tmp a))
                   (set! a b)
                   (set! b tmp)))))
             (swap! x y)
             (list x y))
           '(2 1))

     ;;     define-record-type                      ; R7RS 5.5

     (test (let ()
             (define-record-type <pare>
               (kons x y)
               pare?
               (x kar set-kar!)
               (y kdr))
             (list (pare? (kons 1 2))
                   (pare? (cons 1 2))
                   (kar (kons 1 2))
                   (kdr (kons 1 2))
                   (let ((k (kons 1 2)))
                     (set-kar! k 3)
                     (kar k))))
           '(#t #f 1 2 3))

     ;;     eqv?                                    ; R7RS 6.1
     ;;     eq?
     ;;     equal?

     ;; R6RS 11.5

     (test (eqv? 'a 'a) #t)
     (test (eqv? 'a 'b) #f)
     (test (eqv? 2 2) #t)
     (test (eqv? 2 (inexact 2)) #f)
     (test (eqv? '() '()) #t)
     (test (eqv? 100000000 100000000) #t)
     (test (eqv? (cons 1 2) (cons 1 2)) #f)
     (test (eqv? (lambda () 1) (lambda () 2)) #f)
     (test (let ((p (lambda (x) x)))
             (eqv? p p))
           #t)
     (test (eqv? #f 'nil) #f)

     (test/unspec (eqv? "" ""))
     (test/unspec (eqv? '#() '#()))
     (test/unspec (eqv? (lambda (x) x)
                        (lambda (x) x)))
     (test/unspec (eqv? (lambda (x) x)
                        (lambda (y) y)))

     (test/unspec (let ((g (gen-counter)))
                    (eqv? g g)))
     (test (eqv? (gen-counter) (gen-counter)) #f)

     (test (let ((g (gen-loser)))
             (eqv? g g))
           #t)
     (test/unspec (eqv? (gen-loser) (gen-loser)))

     (test/unspec (letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
                           (g (lambda () (if (eqv? f g) 'both 'g))))
                    (eqv? f g)))

     (test (letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
                    (g (lambda () (if (eqv? f g) 'g 'both))))
             (eqv? f g))
           #f)
    
     (test/unspec (eqv? '(a) '(a)))
     (test/unspec (eqv? "a" "a"))
     (test/unspec (eqv? '(b) (cdr '(a b))))
     (test (let ((x '(a)))
             (eqv? x x))
           #t)

     (test (eq? 'a 'a) #t)
     (test/unspec (eq? '(a) '(a)))
     (test (eq? (list 'a) (list 'a)) #f)
     (test/unspec (eq? "a" "a"))
     (test/unspec (eq? "" ""))
     (test (eq? '() '()) #t)
     (test/unspec (eq? 2 2))
     (test/unspec (eq? #\A #\A))
     (test (eq? car car) #t)
     (test/unspec (let ((n (+ 2 3)))
                    (eq? n n)))
     (test (let ((x '(a)))
             (eq? x x)) 
           #t)
     (test (let ((x '#()))
             (eq? x x))
           #t)
     (test (let ((p (lambda (x) x)))
             (eq? p p))
           #t)

     (test (equal? 'a 'a) #t)
     (test (equal? '(a) '(a)) #t)
     (test (equal? '(a (b) c) '(a (b) c)) #t)
     (test (equal? "abc" "abc") #t)
     (test (equal? 2 2) #t)
     (test (equal? (make-vector 5 'a)
                   (make-vector 5 'a)) 
           #t)

     ;; R7RS 2.4 says "It is an error for a <program> or <library>
     ;; to include circular references except in literals.  In
     ;; particular, it is an error for quasiquote (section 4.2.8)
     ;; to contain them."  So this next test is legal.

     (test (equal? '#1=(a b . #1#)
                   '#2=(a b a b . #2#))
           #t)

     (test/unspec (equal? (lambda (x) x)
                          (lambda (y) y)))

     (test (equal? '#u8(1 2 3 4 5)
                   (bytevector 1 2 3 4 5))
           #t)

     (test (let* ((x (list 'a))
                  (y (list 'a))
                  (z (list x y)))
             (list (equal? z (list y x))
                   (equal? z (list x x))))
           '(#t #t))

     ;;     number?                                 ; R7RS 6.2.6
     ;;     complex?
     ;;     real?
     ;;     rational?
     ;;     integer?
     ;;     exact?
     ;;     inexact?
     ;;     exact-integer?
     ;;     =
     ;;     <
     ;;     >
     ;;     <=
     ;;     >=
     ;;     zero?
     ;;     positive?
     ;;     negative?
     ;;     odd?
     ;;     even?
     ;;     max
     ;;     min
     ;;     +
     ;;     *
     ;;     -
     ;;     /
     ;;     abs
     ;;     floor/
     ;;     floor-quotient
     ;;     floor-remainder
     ;;     truncate/
     ;;     truncate-quotient
     ;;     truncate-remainder
     ;;     quotient
     ;;     remainder
     ;;     modulo
     ;;     gcd
     ;;     lcm
     ;;     numerator
     ;;     denominator
     ;;     floor
     ;;     ceiling
     ;;     truncate
     ;;     round
     ;;     rationalize
     ;;     square
     ;;     exact-integer-sqrt
     ;;     expt
     ;;     inexact
     ;;     exact

     ;; R6RS 11.7.4

     (cond-expand
      (exact-complex
       (test (complex? 3+4i)                       #t))
      (else))

     (test (complex? 3)                            #t)
     (test (real? 3)                               #t)

     (test (rational? 2)                           #t)

     (cond-expand
      (exact-complex
       (test (integer? 3+0i)                       #t))
      (else))

     (test (exact? 5)                              #t)

     (test (exact-integer? 32)                     #t)

     (test (finite? 3)                             #t)
     (test (infinite? 3)                           #f)

     (test (nan? 32)                               #f)

     (let* ((w 3)
            (x 4)
            (y (* x x))
            (z (call-with-values
                (lambda () (exact-integer-sqrt y))
                (lambda (z . ignored) z))))

       (test (= x y z)                             #f)
       (test (= x x z)                             #t)
       (test (= w x y)                             #f)
       (test (= y x w)                             #f)

       (test (< x y z)                             #f)
       (test (< x x z)                             #f)
       (test (< w x y)                             #t)
       (test (< y x w)                             #f)

       (test (> x y z)                             #f)
       (test (> x x z)                             #f)
       (test (> w x y)                             #f)
       (test (> y x w)                             #t)

       (test (<= x y z)                            #f)
       (test (<= x x z)                            #t)
       (test (<= w x y)                            #t)
       (test (<= y x w)                            #f)

       (test (>= x y z)                            #f)
       (test (>= x x z)                            #t)
       (test (>= w x y)                            #f)
       (test (>= y x w)                            #t)


       (test (= x x)                               #t)
       (test (= w x)                               #f)
       (test (= y x)                               #f)

       (test (< x x)                               #f)
       (test (< w x)                               #t)
       (test (< y x)                               #f)

       (test (> x x)                               #f)
       (test (> w x)                               #f)
       (test (> y x)                               #t)

       (test (<= x x)                              #t)
       (test (<= w x)                              #t)
       (test (<= y x)                              #f)

       (test (>= x x)                              #t)
       (test (>= w x)                              #f)
       (test (>= y x)                              #t))

     (test (zero? 0) #t)
     (test (zero? (inexact 0)) #t)
     (test (zero? 2) #f)
     (test (zero? (inexact 2)) #f)
     (test (zero? -3) #f)
     (test (zero? (inexact -3)) #f)

     (test (positive? 0) #f)
     (test (positive? (inexact 0)) #f)
     (test (positive? 2) #t)
     (test (positive? (inexact 2)) #t)
     (test (positive? -3) #f)
     (test (positive? (inexact -3)) #f)

     (test (negative? 0) #f)
     (test (negative? (inexact 0)) #f)
     (test (negative? 2) #f)
     (test (negative? (inexact 2)) #f)
     (test (negative? -3) #t)
     (test (negative? (inexact -3)) #t)

     (test (odd? 5) #t)
     (test (odd? 50) #f)
     (test (even? 5) #f)
     (test (even? 50) #t)

     (test (max 3 4)                               4)
     (test (max (inexact 3) 4)                     (inexact 4))

     (test (+ 3 4)                                 7)
     (test (+ 3)                                   3)
     (test (+)                                     0)

     (test (* 4 3)                                 12)
     (test (* 4)                                   4)
     (test (*)                                     1)
    
     (test (- 3 4)                                 -1)
     (test (- 3 4 5)                               -6)
     (test (- 3)                                   -3)
    
     (test (abs 7)                                 7)
     (test (abs -7)                                7)

     (test/values (floor/ 5 2)                     2 1)
     (test/values (floor/ -5 2)                    -3 1)
     (test/values (floor/ 5 -2)                    -3 -1)
     (test/values (floor/ -5 -2)                   2 -1)
     (test/values (truncate/ 5 2)                  2 1)
     (test/values (truncate/ -5 2)                 -2 -1)
     (test/values (truncate/ 5 -2)                 -2 1)
     (test/values (truncate/ -5 -2)                2 -1)
     (test/values (truncate/ (inexact -5) -2)      (inexact 2) (inexact -1))
     (test/values (truncate/ -5 (inexact -2))      (inexact 2) (inexact -1))

    ;; `divmod-test' cases originally from Ikarus:

     (divmod-test +17 +3)
     (divmod-test +17 -3)
     (divmod-test -17 +3)
     (divmod-test -17 -3)
     (divmod-test +16 +3)
     (divmod-test +16 -3)
     (divmod-test -16 +3)
     (divmod-test -16 -3)
     (divmod-test +15 +3)
     (divmod-test +15 -3)
     (divmod-test -15 +3)
     (divmod-test -15 -3)
     (divmod-test +10 +4)
     (divmod-test +10 -4)
     (divmod-test -10 +4)
     (divmod-test -10 -4)

     (test (gcd 32 -36)                            4)
     (test (gcd)                                   0)
     (test (lcm 32 -36)                            288)
     (test (lcm 32.0 -36)                          288.0)
     (test (lcm)                                   1)
    
     (test (numerator 6)                           6)
     (test (denominator 6)                         1)
     (test (denominator (inexact (/ 6 4))) 2.0)

     (test (round 7)                               7)
    
     (test (square -7)                             49)

     (test (exact (sqrt 36))                       6)

     (test/values (exact-integer-sqrt 0) 0 0)
     (test/values (exact-integer-sqrt 4) 2 0)
     (test/values (exact-integer-sqrt 5) 2 1)
    
     (test (expt 5 3)                   125)
     (test (expt 5 0)                   1)
     (test (expt 0 5)                   0)

     (test (expt 0 0)                   1)
    
     (test (inexact? (inexact 3))               #t)
     (test (exact? (inexact 3))                 #f)
     (test (inexact? (exact (inexact 3)))       #f)
     (test (exact? (exact (inexact 3)))         #t)
     (test/approx (inexact 3) 3)
     (test/approx (exact (inexact 3)) 3)

     ;;     number->string                          ; R7RS 6.2.7
     ;;     string->number
     ;;

     (for-each 
      (lambda (n)
        (test (string->number (number->string n)) n)
        (test (string->number (number->string (inexact n) 10)) (inexact n))
        (when (exact? n)
          (test (string->number (number->string n 16) 16) n)
          (test (string->number (string-append "#x" (number->string n 16))) n)
          (test (string->number (number->string n 8) 8) n)
          (test (string->number (string-append "#o" (number->string n 8))) n)
          (test (string->number (number->string n 2) 2) n)
          (test (string->number (string-append "#b" (number->string n 2))) n)
          (test (string->number (number->string n 10) 10) n)
          (test (string->number (string-append "#d" (number->string n 10)))
                n)))
      '(1 15 1023 -5))
     (test (string->number "nope") #f)

     (test (string->number "100")                  100)
     (test (string->number "100" 16)               256)
     (test (string->number "0/0")                  #f)

     ;; Originally from Ikarus:
     (test-string-to-number
      ("10" 10)
      ("1" 1)
      ("-17" -17)
      ("#x24" 36)
      ("#x-24" -36)
      ("#b+00000110110" 54)
      ("#e10" 10)
      ("#e1" 1)
      ("#e-17" -17)
      ("#e#x24" 36)
      ("#e#x-24" -36)
      ("#e#b+00000110110" 54)
      ("#x#e24" 36)
      ("#x#e-24" -36)
      ("#b#e+00000110110" 54))

     ;;     not                                     ; R7RS 6.3
     ;;     boolean?
     ;;     boolean=?

     ;; R6RS 11.8

     (test (not #t)    #f)
     (test (not 3)           #f)
     (test (not (list 3))    #f)
     (test (not #f)   #t)
     (test (not '())         #f)
     (test (not (list))      #f)
     (test (not 'nil)        #f)
    
     (test (boolean? #f)   #t)
     (test (boolean? 0)           #f)
     (test (boolean? '())         #f)

     (test (boolean=? #f #f) #t)
     (test (boolean=? #t #t) #t)
     (test (boolean=? #t #f) #f)
     (test (boolean=? #f #t) #f)
     (test (boolean=? #t #t #f) #f)
     (test (boolean=? #t #t #t #t) #t)

     ;;     pair?                                   ; R7RS 6.4
     ;;     cons
     ;;     car
     ;;     cdr
     ;;     set-car!
     ;;     set-cdr!
     ;;     caar
     ;;     cadr
     ;;     cdar
     ;;     cddr
     ;;     null?
     ;;     list?
     ;;     make-list
     ;;     list
     ;;     length
     ;;     append
     ;;     reverse
     ;;     list-tail
     ;;     list-ref
     ;;     list-set!
     ;;     memq
     ;;     memv
     ;;     member
     ;;     assq
     ;;     assv
     ;;     assoc
     ;;     list-copy

     ;; R6RS 11.9

     (test (pair? '(a . b))         #t)
     (test (pair? '(a b c))         #t)
     (test (pair? '())              #f)
     (test (pair? '#(a b))          #f)

     (test (cons 'a '())            '(a))
     (test (cons '(a) '(b c d))     '((a) b c d))
     (test (cons "a" '(b c))        '("a" b c))
     (test (cons 'a 3)              '(a . 3))
     (test (cons '(a b) 'c)         '((a b) . c))

     (test (car '(a b c))           'a)
     (test (car '((a) b c d))       '(a))
     (test (car '(1 . 2))           1)
    
     (test (cdr '((a) b c d))       '(b c d))
     (test (cdr '(1 . 2))           2)

     (let ()
       (define (f) (list 'not-a-constant-list))
       (let ((x (f)))
         (test/unspec (set-car! (f) 3))
         (test/unspec (set-car! x 33))
         (test (car x) 33)))

     (test (let ((x (list 'a 'b 'c 'a))
                 (y (list 'a 'b 'c 'a 'b 'c 'a)))
             (set-cdr! (list-tail x 2) x)
             (set-cdr! (list-tail y 5) y)
             (list
              (equal? x x)
              (equal? x y)
              (equal? (list x y 'a) (list y x 'b))))
           '(#t #t #f))
      
     (test (cadr '(1 2)) 2)
     (test (cddr '(1 2)) '())
     (test (cdar '((1) 2)) '())
     (test (caar '((1) 2)) 1)

     (test (null? '())           #t)
     (test (null? '(1))          #f)
     (test (null? #f)            #f)

     (test (list? '(a b c))      #t)
     (test (list? '())           #t)
     (test (list? '(a . b))      #f)
     (test (let ((x (list 'a)))
             (set-cdr! x x)
             (list? x))
           #f)

     (test (length (make-list 20)) 20)
     (test (make-list 2 3) '(3 3))
    
     (test (list 'a (+ 3 4) 'c)             '(a 7 c))
     (test (list)                           '())

     (test (length '(a b c))                3)
     (test (length '(a (b) (c d e)))        3)
     (test (length '())                     0)

     (test (append '(x) '(y))               '(x y))
     (test (append '(a) '(b c d))           '(a b c d))
     (test (append '(a (b)) '((c)))         '(a (b) (c)))
     (test (append '(a b) '(c . d))         '(a b c . d))
     (test (append '() 'a)                  'a)

     (test (reverse '(a b c))               '(c b a))
     (test (reverse '(a (b c) d (e (f))))   '((e (f)) d (b c) a))

     (test (list-tail '(a b c d) 2)                  '(c d))
     (test (list-tail '(a b . c) 2)                  'c)

     (test (list-ref '(a b c d) 2)                 'c)
     (test (list-ref '(a b c . d) 2)               'c)
     (test (list-ref '(a b c . d)
                     (exact (round (/ 18 10))))
           'c)

     (let ((abcd (list 'a 'b 'c 'd)))
       (test/unspec (list-set! abcd 0 'AAA))
       (test/unspec (list-set! abcd 3 'dee))
       (test (list-ref abcd 0) 'AAA)
       (test (list-ref abcd 3) 'dee)
       (test (length abcd) 4))


     (test (memq 'a '(a b c))                  '(a b c))
     (test (memq 'b '(a b c))                  '(b c))
     (test (memq 'a '(b c d))                  #f)
     (test (memq (list 'a) '(b (a) c))         #f)
     (test/unspec (memq 101 '(100 101 102)))
     (test (memv 101 '(100 101 102))           '(101 102))
     (test (member (list 'a) '(b (a) c))       '((a) c))

     (cond-expand
      ((library (scheme char))
       (test (member "B"
                     '("a" "b" "c")
                     string-ci=?)
             '("b" "c")))
      (else))

     (test (member 'irrelevant
                   '(3 1 4 1 5 9 2 6 5)
                   (lambda (ignored n) (even? n)))
           '(4 1 5 9 2 6 5))

     (let ((e '((a 1) (b 2) (c 3))))
       (test (assq 'a e)      '(a 1))
       (test (assq 'b e)      '(b 2))
       (test (assq 'd e)      #f))

     (test (assq (list 'a) '(((a)) ((b)) ((c))))
           #f)
     (test (assoc (list 'a) '(((a)) ((b)) ((c))))   
           '((a)))
     (test/unspec (assq 5 '((2 3) (5 7) (11 13))))
     (test (assv 5 '((2 3) (5 7) (11 13))) '(5 7))

     (let ((d '((3 a) (1 b) (4 c))))
       (test (assoc 'irrelevant
                    d
                    (lambda (x y) (even? y)))
             '(4 c))
       (test (assoc 1
                    d
                    (lambda (x y)
                      (boolean=? (odd? x) (odd? y))))
             '(3 a)))

     (test (list-copy '())                  '())
     (test (list-copy '(a b c))             '(a b c))
     (test (list-copy '(a b . c))           '(a b . c))
     (test (list-copy 'c)                   'c)
     (test (eqv? (list-copy '()) '())       #t)
     (test (eqv? (list-copy '(a b)) '(a b)) #f)

     ;;     symbol?                                 ; R7RS 6.5
     ;;     symbol=?
     ;;     symbol->string
     ;;     string->symbol

     ;; R6RS 11.10

     (test (symbol? 'foo)           #t)
     (test (symbol? (car '(a b)))   #t)
     (test (symbol? "bar")          #f)
     (test (symbol? 'nil)           #t)
     (test (symbol? '())            #f)
     (test (symbol? #f)             #f)

     (test (symbol=? 'a 'a)         #t)
     (test (symbol=? 'a 'A)         #f)
     (test (symbol=? 'a 'b)         #f)
     (test (symbol=? 'a 'a 'b)      #f)
     (test (symbol=? 'a 'a 'a 'a)   #t)
    
     (test (symbol->string 'flying-fish)      "flying-fish")
     (test (symbol->string 'Martin)           "Martin")
     (test (symbol->string
            (string->symbol "Malvina"))     
           "Malvina")

     (test (eq? 'mISSISSIppi 'mississippi)              #f)
     (test (string->symbol "mISSISSIppi")               'mISSISSIppi)
     (test (eq? 'bitBlt (string->symbol "bitBlt"))      #t)
     (test (eq? 'JollyWog
                (string->symbol
                 (symbol->string 'JollyWog)))
           #t)
     (test (string=? "K. Harper, M.D."
                     (symbol->string
                      (string->symbol "K. Harper, M.D.")))   
           #t)

     ;;     char?                                   ; R7RS 6.6
     ;;     char=?
     ;;     char<?
     ;;     char>?
     ;;     char<=?
     ;;     char>=?
     ;;     char->integer
     ;;     integer->char

     ;; R6RS 11.11

     (test (char? #\a) #t)
     (test (char? "a") #f)
     (test (char? 'a)  #f)
     (test (char? 65)  #f)
    
     (test (char=? #\z #\xDF)  #f)
     (test (char=? #\z #\z)    #t)
     (test (char<? #\z #\z)    #f)
     (test (char<? #\z #\xDF)  #t)
     (test (char<? #\xDF #\z)  #f)
     (test (char<? #\z #\Z)    #f)
     (test (char<=? #\z #\z)   #t)
     (test (char<=? #\z #\xDF) #t)
     (test (char<=? #\xDF #\z) #f)
     (test (char<=? #\z #\Z)   #f)
     (test (char>? #\z #\z)    #f)
     (test (char>? #\z #\xDF)  #f)
     (test (char>? #\xDF #\z)  #t)
     (test (char>? #\z #\Z)    #t)
     (test (char>=? #\z #\z)   #t)
     (test (char>=? #\z #\xDF) #f)
     (test (char>=? #\xDF #\z) #t)
     (test (char>=? #\z #\Z)   #t)

     (let* ((w #\A)
            (x #\N)
            (y #\z)
            (z (integer->char (+ 13 (char->integer w)))))

       (test (char=? x y z)                             #f)
       (test (char=? x x z)                             #t)
       (test (char=? w x y)                             #f)
       (test (char=? y x w)                             #f)

       (test (char<? x y z)                             #f)
       (test (char<? x x z)                             #f)
       (test (char<? w x y)                             #t)
       (test (char<? y x w)                             #f)

       (test (char>? x y z)                             #f)
       (test (char>? x x z)                             #f)
       (test (char>? w x y)                             #f)
       (test (char>? y x w)                             #t)

       (test (char<=? x y z)                            #f)
       (test (char<=? x x z)                            #t)
       (test (char<=? w x y)                            #t)
       (test (char<=? y x w)                            #f)

       (test (char>=? x y z)                            #f)
       (test (char>=? x x z)                            #t)
       (test (char>=? w x y)                            #f)
       (test (char>=? y x w)                            #t)


       (test (char=? x x)                               #t)
       (test (char=? w x)                               #f)
       (test (char=? y x)                               #f)

       (test (char<? x x)                               #f)
       (test (char<? w x)                               #t)
       (test (char<? y x)                               #f)

       (test (char>? x x)                               #f)
       (test (char>? w x)                               #f)
       (test (char>? y x)                               #t)

       (test (char<=? x x)                              #t)
       (test (char<=? w x)                              #t)
       (test (char<=? y x)                              #f)

       (test (char>=? x x)                              #t)
       (test (char>=? w x)                              #f)
       (test (char>=? y x)                              #t))

     (test (integer->char 32) #\space)

     (cond-expand
      (full-unicode
       (test (integer->char #xDF) #\xDF)
       (test (integer->char #x10AAAA) #\x10AAAA)
       (test (char->integer (integer->char 5000))
             5000))
      (else))

     (test (char->integer #\alarm)     #x0007)
     (test (char->integer #\backspace) #x0008)
     (test (char->integer #\delete)    #x007f)
     (test (char->integer #\escape)    #x001b)
     (test (char->integer #\newline)   #x000a)
     (test (char->integer #\null)      #x0000)
     (test (char->integer #\return)    #x000d)
     (test (char->integer #\space)     #x0020)
     (test (char->integer #\tab)       #x0009)

     ;;     string?                                 ; R7RS 6.7
     ;;     make-string
     ;;     string
     ;;     string-length
     ;;     string-ref
     ;;     string-set!
     ;;     string=?
     ;;     string<?
     ;;     string>?
     ;;     string<=?
     ;;     string>=?
     ;;     substring
     ;;     string-append
     ;;     string->list
     ;;     list->string
     ;;     string-copy
     ;;     string-copy!
     ;;     string-fill!

     ;; R6RS 11.12

     (test (string? "apple") #t)
     (test (string? #u8(1 2)) #f)
     (test (string? #\a) #f)
     (test (string? 77) #f)

     (test (string-length (make-string 15))   15)
     (test (make-string 0 #\c)                "")
     (test (make-string 5 #\c)                "ccccc")
     (test (eqv? (make-string 5 #\c) "ccccc") #f)

     (test (string)                           "")
     (test (string #\c #\b #\a)               "cba")
     (test (eqv? (string #\c #\b #\a) "cba")  #f)

     (test (string-length (make-string 10)    ) 10)
     (test (string-length (make-string 10 #\a)) 10)
     (test (string-ref (make-string 10 #\a) 0)  #\a)
     (test (string-ref (make-string 10 #\a) 5)  #\a)
     (test (string-ref (make-string 10 #\a) 9)  #\a)
     (test (string-ref "abc" 0)                 #\a)
     (test (string-ref "abc" 2)                 #\c)

     (let ()
       (define (f) (make-string 3 #\*))
       (define (g) "***")
       (test/unspec (string-set! (f) 0 #\?))
       ;(string-set! (g) 0 #\?) ; is an error
       ;(string-set! (symbol->string 'immutable) 0 #\?) ; is an error
       (let ((s (f)))
         (test/unspec (string-set! s 0 #\?))
         (test (string-ref s 0) #\?)
         (test (string-ref s 1) #\*)
         (test (string-ref s 2) #\*)))

     (test (string=? "Strasse" "Strasse") #t)

     (test (string=? "Strasse" "Strasse" "Strasse") #t)

     (test (string<? "z" "z") #f)
     (test (string<? "z" "zz") #t)
     (test (string<? "z" "Z") #f)
     (test (string<=? "z" "zz") #t)
     (test (string<=? "z" "Z") #f)
     (test (string<=? "z" "z") #t)

     (test (string<? "z" "z") #f)
     (test (string>? "z" "zz") #f)
     (test (string>? "z" "Z") #t)
     (test (string>=? "z" "zz") #f)
     (test (string>=? "z" "Z") #t)
     (test (string>=? "z" "z") #t)

     ;; The full-unicode feature doesn't imply full Unicode in strings,
     ;; so these tests might fail even in a conforming implementation.

     (cond-expand
      (full-unicode-strings
       (test (string=? "Stra\xDF;e" "Strasse") #f)
       (test (string=? "Strasse" "Strasse" "Stra\xDF;e") #f)
       (test (string=? "Strasse" "Stra\xDF;e" "Strasse") #f)
       (test (string=? "Stra\xDF;e" "Strasse" "Strasse") #f)
       (test (string<? "z" "\xDF;") #t)
       (test (string<? "\xDF;" "z") #f)
       (test (string<=? "z" "\xDF;") #t)
       (test (string<=? "\xDF;" "z") #f)
       (test (string>? "z" "\xDF;") #f)
       (test (string>? "\xDF;" "z") #t)
       (test (string>=? "z" "\xDF;") #f)
       (test (string>=? "\xDF;" "z") #t))
      (else))

     ;; R7RS 6.7 says "These procedures compare strings in an
     ;; implementation-defined way.  One approach is to make
     ;; them the lexicographic extensions to strings of the
     ;; corresponding ordering on characters.  In that case,
     ;; string<? would be the lexicographic ordering on strings
     ;; induced by the ordering char<? on characters, and if
     ;; the two strings differ in length but are the same up
     ;; to the length of the shorter string, the shorter string
     ;; would be considered to be lexicographically less than
     ;; the longer string.  However, it is also permitted to
     ;; use the natural ordering imposed by the implementation's
     ;; internal representation of strings, or a more complex
     ;; locale-specific ordering."
     ;;
     ;; Which means a conforming implementation might not pass
     ;; the tests immediately below.  On the other hand, any
     ;; implementation that fails to pass these particular tests
     ;; is incompatible with both the R5RS and R6RS standards,
     ;; and is likely to surprise any R7RS programmers who need
     ;; to compare strings.

     (let* ((w "a")
            (x "abc")
            (y "def")
            (z (string #\a #\b #\c)))

       (test (string=? x y z)                           #f)
       (test (string=? x x z)                           #t)
       (test (string=? w x y)                           #f)
       (test (string=? y x w)                           #f)

       (test (string<? x y z)                           #f)
       (test (string<? x x z)                           #f)
       (test (string<? w x y)                           #t)
       (test (string<? y x w)                           #f)

       (test (string>? x y z)                           #f)
       (test (string>? x x z)                           #f)
       (test (string>? w x y)                           #f)
       (test (string>? y x w)                           #t)

       (test (string<=? x y z)                          #f)
       (test (string<=? x x z)                          #t)
       (test (string<=? w x y)                          #t)
       (test (string<=? y x w)                          #f)

       (test (string>=? x y z)                          #f)
       (test (string>=? x x z)                          #t)
       (test (string>=? w x y)                          #f)
       (test (string>=? y x w)                          #t)


       (test (string=? x x)                             #t)
       (test (string=? w x)                             #f)
       (test (string=? y x)                             #f)

       (test (string<? x x)                             #f)
       (test (string<? w x)                             #t)
       (test (string<? y x)                             #f)

       (test (string>? x x)                             #f)
       (test (string>? w x)                             #f)
       (test (string>? y x)                             #t)

       (test (string<=? x x)                            #t)
       (test (string<=? w x)                            #t)
       (test (string<=? y x)                            #f)

       (test (string>=? x x)                            #t)
       (test (string>=? w x)                            #f)
       (test (string>=? y x)                            #t))

     (test (substring "apple" 0 3) "app")
     (test (substring "apple" 1 3) "pp")
     (test (substring "apple" 3 5) "le")

     (test (string-append) "")
     (test (string-append "apple") "apple")
     (test (string-append "apple" "banana") "applebanana")
     (test (string-append "apple" "banana" "cherry") "applebananacherry")

     (test (string->list "apple")     (list #\a #\p #\p #\l #\e))
     (test (string->list "apple" 1)   (list #\p #\p #\l #\e))
     (test (string->list "apple" 0 3) (list #\a #\p #\p))
     (test (string->list "apple" 1 3) (list #\p #\p))
     (test (string->list "apple" 3 5) (list #\l #\e))
     (test (list->string (list #\a #\p #\p #\l #\e)) "apple")

     (test "apple" (string-copy "apple"))
     (let ((s "apple"))
       (test (eq? s (string-copy s)) #f))

     (test (eqv? (string-copy "apple") "apple") #f)
     (test (string-copy "apple")                "apple")
     (test (string-copy "apple" 1)              "pple")
     (test (string-copy "apple" 0 3)            "app")
     (test (string-copy "apple" 1 3)            "pp")
     (test (string-copy "apple" 3 5)            "le")

     ;; R7RS 6.7 says "It is an error if at is less than zero or greater than
     ;; the length of to.  It is also an error if (- (string-length to) at)
     ;; is less than (- end start)."
     ;; The R7RS does not say what the last argument (end) defaults to if
     ;; omitted.  If end is not specified, Larceny uses the largest index
     ;; that will work.

     (let ((s (make-string 6 #\!)))
       (test/unspec (string-copy! s 0 "apple"))
       (test s "apple!")
       (test/unspec (string-copy! s 2 "pears are nice too" 0 4))
       (test s "appear")
       (test/unspec (string-copy! s 0 "blink" 1 4))
       (test s "linear")
       (test/unspec (string-copy! s 4 "  "))
       (test s "line  ")
       (test/unspec (string-copy! s 2 "past" 2))
       (test s "list  ")
       (test/unspec (string-copy! s 2 s 0 4))
       (test s "lilist")
       (test/unspec (string-copy! s 0 s 1))
       (test s "ilistt"))

     (let ((s (make-string 6 #\*)))
       (test/unspec (string-copy! s 0 "apple!"))
       (test s "apple!")
       (test/unspec (string-copy! s 2 "pears are nice too" 0 4))
       (test s "appear")
       (test/unspec (string-copy! s 1 "fiction" 2))
       (test s "action")
       (test/unspec (string-copy! s 4 "ve"))
       (test s "active")
       (test/unspec (string-copy! s 1 s 0 5))
       (test s "aactiv"))

     (let ((s (make-string 6 #\!)))
       (test/unspec (string-fill! s #\space))
       (test s "      ")
       (test/unspec (string-fill! s #\o 2))
       (test s "  oooo")
       (test/unspec (string-fill! s #\k 3))
       (test s "  okkk")
       (test/unspec (string-fill! s #\space 4 6))
       (test s "  ok  ")
       (test/unspec (string-fill! s #\! 4 5))
       (test s "  ok! "))

     ;;     vector?                                 ; R7RS 6.8
     ;;     make-vector
     ;;     vector
     ;;     vector-length
     ;;     vector-ref
     ;;     vector-set!
     ;;     vector->list
     ;;     list->vector
     ;;     vector->string
     ;;     string->vector
     ;;     vector-copy
     ;;     vector-copy!
     ;;     vector-append
     ;;     vector-fill!

     ;; R6RS 11.13

     (test (vector? '#(1 2 3)) #t)
     (test (vector? "apple") #f)

     (test (vector-length (make-vector 10)) 10)
     (test (vector-length (make-vector 10 'x)) 10)
     (test (vector-ref (make-vector 10 'x) 0) 'x)
     (test (vector-ref (make-vector 10 'x) 5) 'x)
     (test (vector-ref (make-vector 10 'x) 9) 'x)

     (test '#(0 (2 2 2 2) "Anna")  (vector 0 '(2 2 2 2) "Anna"))
     (test (vector 'a 'b 'c)       '#(a b c))
     (test (vector-ref '#(1 1 2 3 5 8 13 21) 5)   8)

     (cond-expand
      ((library (scheme inexact))
       (test (vector-ref '#(1 1 2 3 5 8 13 21)
                         (exact (round (* 2 (acos -1)))))
             13))
      (else))

     (test (let ((vec (vector 0 '(2 2 2 2) "Anna")))
             (vector-set! vec 1 '("Sue" "Sue"))
             vec)
           '#(0 ("Sue" "Sue") "Anna"))

     ; (vector-set! '#(0 1 2) 1 "doe") ; is an error

     (test (vector->list '#(dah dah didah))      '(dah dah didah))
     (test (vector->list '#(dah dah didah) 1)    '(dah didah))
     (test (vector->list '#(dah dah didah) 1 2)  '(dah))
     (test (list->vector '(dididit dah))         '#(dididit dah))

     (test (vector->string '#(#\b #\a #\n #\a #\c #\h))      "banach")
     (test (vector->string '#(#\b #\a #\n #\a #\c #\h) 2)    "nach")
     (test (vector->string '#(#\b #\a #\n #\a #\c #\h) 1 4)  "ana")
     (test (string->vector "")                               '#())
     (test (string->vector "ABC")                            '#(#\A #\B #\C))
     (test (vector->string '#(#\1 #\2 #\3))                  "123")

     (let ()
       (define a #(1 8 2 8))        ; a may be immutable
       (define b (vector-copy a))
       (vector-set! b 0 3)          ; b is mutable
       (test b '#(3 8 2 8))
       (test (vector-copy b 1 3) '#(8 2))
       (test (vector-copy b 1)   '#(8 2 8)))

     ;; R7RS 6.7 says "It is an error if at is less than zero or greater than
     ;; the length of to.  It is also an error if (- (vector-length to) at)
     ;; is less than (- end start)."
     ;; The R7RS does not say what the last argument (end) defaults to if
     ;; omitted.  If end is not specified, Larceny uses the largest index
     ;; that will work.

     (let ()
       (define a (vector 1 2 3 4 5))
       (define b (vector 10 20 30 40 50))
       (test/unspec (vector-copy! b 1 a 0 2))
       (test b '#(10 1 2 40 50))
       (test/unspec (vector-copy! b 1 b 2))
       (test b '#(10 2 40 50 50))
       (test/unspec (vector-copy! b 1 b 0 4))
       (test b '#(10 10 2 40 50))
       (test/unspec (vector-copy! a 2 a 0 3))
       (test a '#(1 2 1 2 3)))

     (let ()
       (define a (vector 1 2 3 4 5))
       (define b (vector 10 20 30 40 50))
       (test/unspec (vector-copy! b 1 a 0 4))
       (test b '#(10 1 2 3 4))
       (test/unspec (vector-copy! b 2 b 1 4))
       (test b '#(10 1 1 2 3))
       (test/unspec (vector-copy! a 2 a 0))    ; FIXME: R7RS doesn't say
       (test a '#(1 2 1 2 3)))

     (test (vector-append #(a b c) #(d e f)) '#(a b c d e f))

     (let ()
       (define a (vector 1 2 3 4 5))
       (test/unspec (vector-fill! a 'smash 2 4))
       (test a '#(1 2 smash smash 5))
       (test/unspec (vector-fill! a 'bits 4))
       (test a '#(1 2 smash smash bits))
       (test/unspec (vector-fill! a 2 3 4))
       (test a '#(1 2 smash 2 bits))
       (test/unspec (vector-fill! a 'bits))
       (test a (make-vector 5 'bits)))

     ;;     bytevector?                             ; R7RS 6.9
     ;;     make-bytevector
     ;;     bytevector
     ;;     bytevector-length
     ;;     bytevector-u8-ref
     ;;     bytevector-u8-set!
     ;;     bytevector-copy
     ;;     bytevector-copy!
     ;;     bytevector-append
     ;;     utf8->string
     ;;     string->utf8

     ;; R6RS Standard Libraries, Chapter 2

     (test (bytevector? #u8(1 2 3)) #t)
     (test (bytevector? "123") #f)

     (test (make-bytevector 2 12) '#u8(12 12))

     (test (bytevector 1 3 5 1 3 5) '#u8(1 3 5 1 3 5))
     (test (bytevector)             '#u8())

     (test (bytevector-length #u8(1 2 3)) 3)
     (test (bytevector-length (make-bytevector 10)) 10)
     (test (bytevector-length (make-bytevector 10 3)) 10)
     (test (bytevector-u8-ref (make-bytevector 10 3) 0) 3)
     (test (bytevector-u8-ref (make-bytevector 10 3) 5) 3)
     (test (bytevector-u8-ref (make-bytevector 10 3) 9) 3)
     (test (bytevector-u8-ref (make-bytevector 10 255) 9) 255)
     (test (bytevector-u8-ref (make-bytevector 10 -1) 9) 255)
     (test (bytevector-u8-ref (make-bytevector 10 -128) 9) 128)

     (test (bytevector-u8-ref '#u8(1 1 2 3 5 8 13 21) 5)
           8)

     (let ((bv (bytevector 1 2 3 4)))
       (test/unspec (bytevector-u8-set! bv 1 3))
       (test bv '#u8(1 3 3 4)))

     (let ()
       (define a #u8(1 2 3 4 5))
       (test (bytevector-copy a 2 4) #u8(3 4))
       (test (bytevector-copy a 2)   #u8(3 4 5))
       (test (bytevector-copy a)     #u8(1 2 3 4 5)))

     ;; R7RS 6.7 says "It is an error if at is less than zero or greater than
     ;; the length of to.  It is also an error if (- (bytevector-length to) at)
     ;; is less than (- end start)."
     ;; The R7RS does not say what the last argument (end) defaults to if
     ;; omitted.  If end is not specified, Larceny uses the largest index
     ;; that will work.

     (let ()
       (define a (bytevector 1 2 3 4 5))
       (define b (bytevector 10 20 30 40 50))
       (test/unspec (bytevector-copy! b 2 a 0 3))
       (test b '#u8(10 20 1 2 3))
       (test/unspec (bytevector-copy! b 2 b 0 3))
       (test b '#u8(10 20 10 20 1))
       (test/unspec (bytevector-copy! b 3 a 3))
       (test b '#u8(10 20 10 4 5))
       (test/unspec (bytevector-copy! b 0 a))
       (test b '#u8(1 2 3 4 5)))

     (test (bytevector-append)                       '#u8())
     (test (bytevector-append #u8(0 1 2) #u8(3 4 5)) '#u8(0 1 2 3 4 5))

     (test (string->utf8 "apple")                    #u8(97 112 112 108 101))
     (test (utf8->string (string->utf8 "apple"))     "apple")
     (test (utf8->string (string->utf8 "apple") 3)   "le")
     (test (utf8->string (string->utf8 "apple") 1 3) "pp")
     (test (utf8->string (string->utf8 "apple" 3))   "le")
     (test (utf8->string (string->utf8 "apple" 1 3)) "pp")

     ;; Once again, the full-unicode feature doesn't imply full Unicode
     ;; for strings, so a conforming implementation could fail this test.

     (cond-expand
      (full-unicode-strings
       (test (string->utf8 "app\x3BB;e") #u8(97 112 112 206 187 101))
       (test (utf8->string (string->utf8 "app\x3BB;e")) "app\x3BB;e")
       (test (utf8->string
              (string->utf8
               "\x0;\x1;\x80;\xFF;\xD7FF;\xE000;\x10FFFF;"))
             "\x0;\x1;\x80;\xFF;\xD7FF;\xE000;\x10FFFF;"))
      (else))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Tests originally from Ikarus

     (test (bytevector? (make-bytevector 1)) #t)
     (test (bytevector? (make-bytevector 1 17)) #t)
     (test (bytevector? 'foo) #f)
     (test (bytevector? "hey") #f)
     (test (bytevector? '#(2837 2398 239)) #f)
     (test (bytevector-length (make-bytevector 0)) 0)

     ;;     procedure?                              ; R7RS 6.10
     ;;     apply
     ;;     map
     ;;     string-map
     ;;     vector-map
     ;;     for-each
     ;;     string-for-each
     ;;     vector-for-each
     ;;     call-with-current-continuation
     ;;     call/cc
     ;;     values
     ;;     call-with-values
     ;;     dynamic-wind

     ;; R6RS 11.6

     (test (procedure? car) #t)
     (test (procedure? 'car) #f)
     (test (procedure? (lambda (x) (* x x))) #t)
     (test (procedure? '(lambda (x) (* x x))) #f)

     (test (call-with-current-continuation procedure?) #t)

     ;; R6RS 11.15

     (test (apply + (list 3 4))               7)

     (cond-expand
      ((library (scheme inexact))
       (let ()
         (define compose
           (lambda (f g)
             (lambda args
               (f (apply g args)))))
         (test/approx ((compose sqrt *) 12 75)    30)))
      (else))

     (test (map cadr '((a b) (d e) (g h)))    '(b e h))

     (test (map (lambda (n) (expt n n))
                '(1 2 3 4 5))
           '(1 4 27 256 3125))

     (test (map + '(1 2 3) '(4 5 6))          '(5 7 9))

     (test ((lambda (x)
              (and (member x '((1 2) (2 1)))
                   #t))
            (let ((count 0))
              (map (lambda (ignored)
                     (set! count (+ count 1))
                     count)
                   '(a b))))
           #t)

     (test (string-map
            (lambda (c)
              (integer->char (+ 1 (char->integer c))))
            "HAL")
           "IBM")

     (cond-expand
      ((library (scheme char))
       (test (string-map char-foldcase "AbdEgH")
             "abdegh")
       (test (string-map
              (lambda (c k)
                ((if (eqv? k #\u) char-upcase char-downcase)
                 c))
              "studlycaps xxx"
              "ululululul")
             "StUdLyCaPs"))
      (else))

     (test (vector-map cadr '#((a b) (d e) (g h)))    '#(b e h))

     (test (vector-map (lambda (n) (expt n n))
                       '#(1 2 3 4 5))
           '#(1 4 27 256 3125))

     (test (vector-map + '#(1 2 3) '#(4 5 6))         '#(5 7 9))

     (test ((lambda (x)
              (and (member x '(#(1 2) #(2 1)))
                   #t))
            (let ((count 0))
              (vector-map (lambda (ignored)
                            (set! count (+ count 1))
                            count)
                          '#(a b))))
           #t)

     (test (vector-map (lambda (x) (+ 1 x)) 
                       '#(1 2 3))
           '#(2 3 4))
     (test (vector-map (lambda (x y) (- x y)) 
                       '#(3 4 5) 
                       '#(0 -1 2))
           '#(3 5 3))
     (test (vector-map (lambda (x y f) (f (- x y)))
                       '#(3 4 5) 
                       '#(0 -1 2)
                       (vector - * /))
           '#(-3 5 1/3))

     (test (let ((v (make-vector 5)))
             (for-each (lambda (i)
                         (vector-set! v i (* i i)))
                       '(0 1 2 3 4))
             v) 
           '#(0 1 4 9 16))

     (test/unspec (for-each (lambda (x) x) '(1 2 3 4)))

     (test/unspec (for-each even? '()))

     (let ((accum '()))
       (test/unspec (string-for-each
                     (lambda (a) (set! accum (cons a accum)))
                     "elppa"))
       (test accum '(#\a #\p #\p #\l #\e))
       (test/unspec (string-for-each
                     (lambda (a b) (set! accum (cons (list a b) accum)))
                     "elppa"
                     "ananb"))
       (test accum
             '((#\a #\b) (#\p #\n) (#\p #\a) (#\l #\n) (#\e #\a)
                #\a #\p #\p #\l #\e))
       (test/unspec (string-for-each
                     (lambda (a b c) (set! accum c))
                     "elppa"
                     "ananb"
                     "chery"))
       (test accum #\y))

     (test (let ((v (make-list 5)))
             (vector-for-each (lambda (i)
                                (list-set! v i (* i i)))
                              '#(0 1 2 3 4))
             v)
           '(0 1 4 9 16))

     (let ((accum '()))
       (test/unspec (vector-for-each
                     (lambda (a) (set! accum (cons a accum)))
                     '#(e l p p a)))
       (test accum '(a p p l e))
       (test/unspec (vector-for-each
                     (lambda (a b) (set! accum (cons (list a b) accum)))
                     '#(e l p p a)
                     '#(a n a n b)))
       (test accum '((a b) (p n) (p a) (l n) (e a)
                     a p p l e))
       (test/unspec (vector-for-each
                     (lambda (a b c) (set! accum c))
                     '#(e l p p a)
                     '#(a n a n b)
                     '#(c h e r y)))
       (test accum 'y))

     (test (call-with-current-continuation
            (lambda (exit)
              (for-each (lambda (x)
                          (if (negative? x)
                              (exit x)))
                       '(54 0 37 -3 245 19))
              #t))
           -3)
     (test (call/cc
            (lambda (exit)
              (for-each (lambda (x)
                          (if (negative? x)
                              (exit x)))
                       '(54 0 37 -3 245 19))
              #t))
           -3)

     (let ()
       (define list-length
         (lambda (obj)
           (call-with-current-continuation
            (lambda (return)
              (letrec ((r (lambda (obj)
                            (cond ((null? obj) 0)
                                  ((pair? obj)
                                   (+ (r (cdr obj)) 1))
                                  (else (return #f))))))
                (r obj))))))
       (test (list-length '(1 2 3 4))     4)
       (test (list-length '(a b . c))     #f))

     (test/values (values))
     (test (values 1) 1)
     (test/values (values 1 2 3) 1 2 3)

     (test (call-with-values (lambda () (values 4 5))
            (lambda (a b) b))
           5)

     (test (call-with-values * -) -1)

     (test (let ((path '())
                 (c #f))
             (let ((add (lambda (s)
                          (set! path (cons s path)))))
               (dynamic-wind
                   (lambda () (add 'connect))
                   (lambda ()
                     (add (call-with-current-continuation
                           (lambda (c0)
                             (set! c c0)
                             'talk1))))
                   (lambda () (add 'disconnect)))
               (if (< (length path) 4)
                   (c 'talk2)
                   (reverse path))))
           '(connect talk1 disconnect
             connect talk2 disconnect))
    
     (test (let ((n 0))
             (call-with-current-continuation
              (lambda (k)
                (dynamic-wind
                    (lambda ()
                      (set! n (+ n 1))
                      (k))
                    (lambda ()
                      (set! n (+ n 2)))
                    (lambda ()
                      (set! n (+ n 4))))))
             n) 
           1)

     (test (let ((n 0))
             (call-with-current-continuation
              (lambda (k)
                (dynamic-wind
                   values
                    (lambda ()
                      (dynamic-wind
                         values
                          (lambda ()
                            (set! n (+ n 1))
                            (k))
                          (lambda ()
                            (set! n (+ n 2))
                            (k))))
                    (lambda ()
                      (set! n (+ n 4))))))
             n) 
           7)

     ;;     with-exception-handler                  ; R7RS 6.11
     ;;     raise
     ;;     raise-continuable
     ;;     error
     ;;     error-object?
     ;;     error-object-message
     ;;     error-object-irritants
     ;;     read-error?
     ;;     file-error?

     (test (let* ((q (open-output-string))
                  (x (call-with-current-continuation
                      (lambda (k)
                        (with-exception-handler
                         (lambda (x)
                           (display "condition: " q)
                           (write x q)
                           (newline q)
                           (k 'exception))
                         (lambda ()
                           (+ 1 (raise 'an-error))))))))
             (list x (get-output-string q)))
           '(exception "condition: an-error\n"))

     (test (let* ((q (open-output-string))
                  (x (call-with-current-continuation
                      (lambda (k)
                        (with-exception-handler
                         (lambda (exn)
                           (display "threw second exception\n" q)
                           (k 'done))
                         (lambda ()
                           (with-exception-handler
                            (lambda (x)
                              (display "something went wrong\n" q))
                            (lambda ()
                              (+ 1 (raise 'an-error))))))))))
             (list x (get-output-string q)))
           '(done "something went wrong\nthrew second exception\n"))

     (test (let* ((q (open-output-string))
                  (x
                   (with-exception-handler
                    (lambda (con)
                      (cond ((string? con)
                             (display con q))
                            (else
                             (display "a warning has been issued" q)))
                      42)
                    (lambda ()
                      (+ (raise-continuable "should be a number")
                         23)))))
             (list x (get-output-string q)))
           '(65 "should be a number"))

     (test (let ((exn (call-with-current-continuation
                       (lambda (k)
                         (define (null-list? l)
                           (cond ((pair? l) #f)
                                 ((null? l) #t)
                                 (else
                                  (error
                                   "null-list?: argument out of domain"
                                   l))))
                         (with-exception-handler
                          (lambda (exn) (k exn))
                          (lambda ()
                            (map null-list?
                                 '((a) (b) () bad (e f)))))))))
             (and (error-object? exn)
                  (list (error-object-message exn)
                        (error-object-irritants exn))))
           '("null-list?: argument out of domain"
             (bad)))

     (test (read-error? (call/cc
                         (lambda (k)
                          (with-exception-handler
                           k
                           (lambda () (error "just an error"))))))
           #f)

     (test (file-error? (call/cc
                         (lambda (k)
                          (with-exception-handler
                           k
                           (lambda () (error "just an error"))))))
           #f)

     (cond-expand
      ((library (scheme read))
       (test (read-error? (call/cc
                           (lambda (k)
                             (with-exception-handler
                              k
                              (lambda ()
                                (read (open-input-string "(")))))))
             #t))
      (else))

     (cond-expand
      ((library (scheme file))
       (test (file-error? (call/cc
                           (lambda (k)
                             (with-exception-handler
                              k
                              (lambda ()
                                (open-input-file "probably not there"))))))
             #t))
      (else))

     ;;     call-with-port                          ; R7RS 6.13.1
     ;;     input-port?
     ;;     output-port?
     ;;     textual-port?
     ;;     binary-port?
     ;;     port?
     ;;     input-port-open?
     ;;     output-port-open?
     ;;     current-input-port
     ;;     current-output-port
     ;;     current-error-port
     ;;     close-port
     ;;     close-input-port
     ;;     close-output-port
     ;;     open-input-string
     ;;     open-output-string
     ;;     get-output-string
     ;;     open-input-bytevector
     ;;     open-output-bytevector
     ;;     get-output-bytevector

     ;; The standard idiom for using string and bytevector ports.

     (test (call-with-port
            (open-input-string "Do you read this?")
            read-line)
           "Do you read this?")

     (test (map (lambda (s)
                  (call-with-port (open-input-string s) read-line))
                '("abc\ndef" "abc\rdef" "abc\r\ndef"))
           '("abc" "abc" "abc"))

     (test (call-with-port
            (open-output-string)
            (lambda (p)
              (write-string "No, but I can write." p)
              (get-output-string p)))
           "No, but I can write.")

     (test (call-with-port
            (open-output-string)
            (lambda (p)
              (write-string "No, but I can write." p 4)
              (get-output-string p)))
           "but I can write.")

     (test (call-with-port
            (open-output-string)
            (lambda (p)
              (write-string "No, but I can write." p 4 13)
              (get-output-string p)))
           "but I can")

     (test (call-with-port
            (open-input-bytevector '#u8(115 111 109 101 32 98 121 116 101 115))
            (lambda (p) (read-bytevector 9 p)))
           '#u8(115 111 109 101 32 98 121 116 101))

     (test (call-with-port
            (open-output-bytevector)
            (lambda (p)
              (write-bytevector '#u8(115 111 109 101) p)
              (write-bytevector '#u8(98 121 116 101 115) p)
              (get-output-bytevector p)))
           '#u8(115 111 109 101 98 121 116 101 115))

     (test (call-with-port
            (open-output-bytevector)
            (lambda (p)
              (write-bytevector '#u8(115 111 109 101) p 2)
              (write-bytevector '#u8(98 121 116 101 115) p 1 4)
              (get-output-bytevector p)))
           '#u8(109 101 121 116 101))

     (let* ((catholic-predicates (list input-port?
                                       output-port?
                                       textual-port?
                                       binary-port?
                                       port?))
            (port-predicates (list input-port-open?
                                   output-port-open?))
            (predicates (append catholic-predicates port-predicates)))
       (define (catholic-profile x)
         (map (lambda (f) (f x))
              catholic-predicates))
       (define (port-profile p)
         (map (lambda (f) (f p))
              predicates))
       (define (closed-profile name p)
         (map (lambda (f) (f p))
              predicates))

       ;; The R7RS (small) document allows textual ports to be binary
       ;; as well, and allows binary ports to be textual as well.
       ;; The following hack allows the tests to be written as though
       ;; textual ports are not binary and binary ports are not textual.

       (define textual-port-predicates
         (list input-port?
               output-port?
               textual-port?
               (lambda (x) #f)
               port?
               input-port-open?
               output-port-open?))

       (define binary-port-predicates
         (list input-port?
               output-port?
               (lambda (x) #f)
               binary-port?
               port?
               input-port-open?
               output-port-open?))

       (define (textual-port-profile p)
         (map (lambda (f) (f p))
              textual-port-predicates))

       (define (binary-port-profile p)
         (map (lambda (f) (f p))
              binary-port-predicates))

       (define-syntax ptst
         (syntax-rules (port-profile quote)
          ((_ (port-profile expr)
              '(i? o? #t b? p? ipo? opo?))
           (test (textual-port-profile expr)
                 '(i? o? #t b? p? ipo? opo?)))
          ((_ (port-profile expr)
              '(i? o? t? #t p? ipo? opo?))
           (test (binary-port-profile expr)
                 '(i? o? t? #t p? ipo? opo?)))
          ((_ whatever1
              whatever2)
           (test whatever1
                 whatever2))))

       (test (catholic-profile "not a port")
             '(#f #f #f #f #f))

       (ptst (port-profile (current-input-port))
             '(#t #f #t #f #t #t #f))
       (ptst (port-profile (current-output-port))
             '(#f #t #t #f #t #f #t))
       (ptst (port-profile (current-error-port))
             '(#f #t #t #f #t #f #t))
       (ptst (port-profile (open-input-string "whatever"))
             '(#t #f #t #f #t #t #f))
       (ptst (port-profile (open-output-string))
             '(#f #t #t #f #t #f #t))
       (ptst (port-profile (open-input-bytevector '#u8(0 0 7)))
             '(#t #f #f #t #t #t #f))
       (ptst (port-profile (open-output-bytevector))
             '(#f #t #f #t #t #f #t))

       ;; FIXME: some subsequent tests read from (current-input-port),
       ;; so closing that port here doesn't work.
       ;; FIXME: the test macros write to (current-output-port),
       ;; so closing that port here doesn't work.

#;     (let ((p (current-input-port)))
         (test/unspec (close-port p))
         (ptst (closed-profile 'current-input-port p)
               '(#t #f #t #f #t #f #f)))
#;     (let ((p (current-output-port)))
         (test/unspec (close-port p))
         (ptst (closed-profile 'current-output-port p)
               '(#f #t #t #f #t #f #f)))
       (let ((p (current-error-port)))
         (test/unspec (close-port p))
         (ptst (closed-profile 'current-error-port p)
               '(#f #t #t #f #t #f #f)))
       (let ((p (open-input-string "whatever")))
         (test/unspec (close-port p))
         (ptst (closed-profile 'open-input-string p)
               '(#t #f #t #f #t #f #f)))
       (let ((p (open-output-string)))
         (test/unspec (close-port p))
         (ptst (closed-profile 'open-output-string p)
               '(#f #t #t #f #t #f #f)))
       (let ((p (open-input-bytevector '#u8(0 0 7))))
         (test/unspec (close-port p))
         (ptst (closed-profile 'open-input-bytevector p)
               '(#t #f #f #t #t #f #f)))
       (let ((p (open-output-bytevector)))
         (test/unspec (close-port p))
         (ptst (closed-profile 'open-output-bytevector p)
               '(#f #t #f #t #t #f #f)))

       ;; Closing (current-input-port) twice may not work.

#;     (let ((p (current-input-port)))
         (test/unspec (close-input-port p))
         (ptst (closed-profile 'current-input-port p)
               '(#t #f #t #f #t #f #f)))
       (let ((p (open-input-string "whatever")))
         (test/unspec (close-input-port p))
         (ptst (closed-profile 'open-input-string p)
               '(#t #f #t #f #t #f #f)))
       (let ((p (open-input-bytevector '#u8())))
         (test/unspec (close-input-port p))
         (ptst (closed-profile 'open-input-bytevector p)
               '(#t #f #f #t #t #f #f)))

#;     (let ((p (current-output-port)))
         (test/unspec (close-output-port p))
         (ptst (closed-profile 'current-output-port p)
               '(#f #t #t #f #t #f #f)))
       (let ((p (current-error-port)))
         (test/unspec (close-output-port p))
         (ptst (closed-profile 'current-error-port p)
               '(#f #t #t #f #t #f #f)))
       (let ((p (open-output-string)))
         (test/unspec (close-output-port p))
         (ptst (closed-profile 'open-output-string p)
               '(#f #t #t #f #t #f #f)))
       (let ((p (open-output-bytevector)))
         (test/unspec (close-output-port p))
         (ptst (closed-profile 'open-output-bytevector p)
               '(#f #t #f #t #t #f #f))))

     ;;     read-char                               ; R7RS 6.13.2
     ;;     peek-char
     ;;     read-line
     ;;     eof-object?
     ;;     eof-object
     ;;     char-ready?
     ;;     read-string
     ;;     read-u8
     ;;     peek-u8
     ;;     u8-ready?
     ;;     read-bytevector
     ;;     read-bytevector!

     (call-with-port
      (open-input-string
       (string-append
        "Far out in the uncharted backwaters\n"
        "of the unfashionable end\n"
        "of the Western Spiral arm of the Galaxy\n"
        "lies a small unregarded yellow sun."))
      (lambda (adams)
        (test (read-char adams) #\F)
        (test (peek-char adams) #\a)
        (test (read-char adams) #\a)
        (test (read-char adams) #\r)
        (test (peek-char adams) #\space)
        (test (read-char adams) #\space)
        (test (read-line adams) "out in the uncharted backwaters")
        (test (eof-object? (read-char adams)) #f)
        (test (eof-object? (peek-char adams)) #f)
        (test (eof-object? (eof-object)) #t)
        (test (char-ready? adams) #t)
        (test (read-string 24 adams) "f the unfashionable end\n")
        (test (read-string 25 adams) "of the Western Spiral arm")
        (test (read-line adams) " of the Galaxy")
        (test (read-string 1000 adams)
              "lies a small unregarded yellow sun.")))

     (parameterize
      ((current-input-port 
        (open-input-string
         (string-append
          "Far out in the uncharted backwaters\n"
          "of the unfashionable end\n"
          "of the Western Spiral arm of the Galaxy\n"
          "lies a small unregarded yellow sun."))))
      (test (read-char) #\F)
      (test (peek-char) #\a)
      (test (read-char) #\a)
      (test (read-char) #\r)
      (test (peek-char) #\space)
      (test (read-char) #\space)
      (test (read-line) "out in the uncharted backwaters")
      (test (eof-object? (read-char)) #f)
      (test (eof-object? (peek-char)) #f)
      (test (eof-object? (eof-object)) #t)
      (test (char-ready?) #t)
      (test (read-string 24) "f the unfashionable end\n")
      (test (read-string 25) "of the Western Spiral arm")
      (test (read-line) " of the Galaxy")
      (test (read-line)
            "lies a small unregarded yellow sun."))

     (call-with-port
      (open-input-bytevector
       '#u8(73 39 109 32 97 32 98 97 105 116 109 97 110 46))
      (lambda (zelazny)
        (test (read-u8 zelazny) 73)
        (test (peek-u8 zelazny) 39)
        (test (read-u8 zelazny) 39)
        (test (read-u8 zelazny) 109)
        (test (peek-u8 zelazny) 32)
        (test (read-u8 zelazny) 32)
        (test (read-bytevector 2 zelazny) '#u8(97 32))
        (test (read-bytevector 4 zelazny) '#u8(98 97 105 116))
        (test (eof-object? (read-u8 zelazny)) #f)
        (test (eof-object? (peek-u8 zelazny)) #f)
        (test (eof-object? (eof-object)) #t)
        (test (u8-ready? zelazny) #t)
        (test (let ((bv (make-bytevector 5 0)))
                (test (read-bytevector! bv zelazny) 3)
                bv)
              '#u8(97 110 46 0 0))
        (test (eof-object? (peek-u8 zelazny)) #t)
        (test (eof-object? (read-u8 zelazny)) #t)
       ;(test (eof-object? (read-bytevector 0 zelazny)) #t)    ; FIXME: unclear
        (test (eof-object? (read-bytevector 1000 zelazny)) #t)
        (test (let ((bv (make-bytevector 5 0)))
                (test (eof-object? (read-bytevector! bv zelazny)) #t)
                bv)
              '#u8(0 0 0 0 0))))

     ;; This is a lovely idea, but some implementations may insist
     ;; upon (current-input-port) being a textual input port.
#;
     (parameterize
      ((current-input-port
        (open-input-bytevector
         '#u8(73 39 109 32 97 32 98 97 105 116 109 97 110 46))))
      (test (read-u8) 73)
      (test (peek-u8) 39)
      (test (read-u8) 39)
      (test (read-u8) 109)
      (test (peek-u8) 32)
      (test (read-u8) 32)
      (test (read-bytevector 2) '#u8(97 32))
      (test (read-bytevector 4) '#u8(98 97 105 116))
      (test (eof-object? (read-u8)) #f)
      (test (eof-object? (peek-u8)) #f)
      (test (eof-object? (eof-object)) #t)
      (test (u8-ready?) #t)
      (test (let ((bv (make-bytevector 5 0)))
              (test (read-bytevector! bv) 3)
              bv)
            '#u8(97 110 46 0 0))
      (test (eof-object? (peek-u8)) #t)
      (test (eof-object? (read-u8)) #t)
      (test (eof-object? (read-bytevector 1000)) #t)
      (test (let ((bv (make-bytevector 5 0)))
              (test (eof-object? (read-bytevector! bv)) #t)
              bv)
            '#u8(0 0 0 0 0)))

     (call-with-port
      (open-input-bytevector
       '#u8(84 104 105 115 32 105 115 32 116 104 101 32 109 111
            115 116 32 98 101 97 117 116 105 102 117 108 32 112
            108 97 99 101 32 111 110 32 101 97 114 116 104 46))
      (lambda (abbey)
        (let ((bv (make-bytevector 10 0)))
          (test (read-bytevector! bv abbey) 10)
          (test bv '#u8(84 104 105 115 32 105 115 32 116 104))
          (test (read-bytevector! bv abbey 2 9) 7)
          (test bv '#u8(84 104 101 32 109 111 115 116 32 104))
          (test (read-bytevector! bv abbey) 10)
          (test bv '#u8(98 101 97 117 116 105 102 117 108 32))
          (test (read-bytevector! bv abbey 0 9) 9)
          (test bv '#u8(112 108 97 99 101 32 111 110 32 32))
          (test (read-bytevector! bv abbey 0) 6)
          (test bv '#u8(101 97 114 116 104 46 111 110 32 32))
          (test (eof-object? (read-bytevector! bv abbey 9)) #t))))

     ;;     newline                                 ; R7RS 6.13.3
     ;;     write-char
     ;;     write-string
     ;;     write-u8
     ;;     write-bytevector
     ;;     flush-output-port

     (test (call-with-port
            (open-output-string)
            (lambda (hesse)
              (test/unspec (write-string "Du kannst reiten und fahren" hesse))
              (test/unspec (flush-output-port hesse))
              (test (get-output-string hesse) "Du kannst reiten und fahren")
              (test/unspec (write-char #\, hesse))
              (test/unspec (write-char #\space hesse))
              (test/unspec (write-string "zu zweien und zu dreien" hesse))
              (test/unspec (write-char #\. hesse))
              (test/unspec (write-char #\. hesse))
              (test/unspec (write-char #\. hesse))
              (test/unspec (newline hesse))
              (test/unspec (flush-output-port hesse))
              (test/unspec (write-string "Den letzten Schritt " hesse))
              (test/unspec (write-string "musst du gehen allein." hesse))
              (test/unspec (newline hesse))
              (test/unspec (flush-output-port hesse))
              (get-output-string hesse)))
           (string-append
            "Du kannst reiten und fahren, zu zweien und zu dreien...\n"
            "Den letzten Schritt musst du gehen allein.\n"))

     (test (call-with-port
            (open-output-bytevector)
            (lambda (melville)
              (test/unspec (write-bytevector '#u8(67 97 108 108 32 109 101)
                                             melville))
              (test/unspec (write-u8 32 melville))
              (flush-output-port melville)
              (test/unspec (write-bytevector '#u8(73 115 104 109 97 101 108)
                                             melville))
              (test/unspec (write-u8 46 melville))
              (flush-output-port melville)
              (utf8->string (get-output-bytevector melville))))
           "Call me Ishmael.")

     (test (char-ready? (open-input-string "abc")) #t)
     (test (u8-ready? (open-input-bytevector '#u8(1 2 3))) #t)

     ;;     features                                ; R7RS 6.14

     (test (and (memq 'r7rs (features)) #t) #t)
     (test (list? (features)) #t)

     ;; These were tested along the way:
     ;;
     ;;     ...                                     ; syntactic keywords
     ;;     =>                                      ;   recognized by
     ;;     _                                       ;   various R7RS
     ;;     else                                    ;   syntaxes

     ;;;
     )))
