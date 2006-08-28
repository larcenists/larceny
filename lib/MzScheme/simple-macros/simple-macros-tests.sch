
;;;===========================================================
;;;
;;; Portable Hygienic Macros: Examples and Tests
;;;
;;;   Andr? van Tonder
;;;
;;;===========================================================

;(load "simple-macros.scm")
;(load "simple-syntax-case.scm")


;(repl '
; (
  (import syntax-case-module)

  ;;=========================================================================
  ;;
  ;; Introductory examples:
  ;;
  ;;=========================================================================

  (define-syntax (swap! a b)
    (quasisyntax
     (let ((temp ,a))
       (set! ,a ,b)
       (set! ,b temp))))


  (let ((temp 1)
        (set! 2))
    (swap! set! temp)
    (values temp set!))   ;==> 2 1


  ;; Simplified COND:

  (define-syntax (my-cond c . cs)
    (or (and (list? c) (>= (length c) 2))
        (syntax-error))
    (cond ((literal-identifier=?
            (car c)
            (syntax else)) (quasisyntax (begin ,@(cdr c))))
          ((null? cs)      (quasisyntax (if ,(car c) (begin ,@(cdr c)))))
          (else            (quasisyntax (if ,(car c)
                                            (begin ,@(cdr c))
                                            (my-cond ,@cs))))))


  (my-cond (#f 1) (else 2))                ;==> 2
  (let ((else #f)) (my-cond (else 2)))     ;==> unspecified


  ;; Equivalently, with SYNTAX-CASE:

  (define-syntax my-cond
    (lambda (form)
      (syntax-case form (else)
        ((_ (else e1 e2 ...))         (syntax (begin e1 e2 ...)))
        ((_ (e0 e1 e2 ...))           (syntax (if e0 (begin e1 e2 ...))))
        ((_ (e0 e1 e2 ...) c1 c2 ...) (syntax (if e0
                                                  (begin e1 e2 ...)
                                                  (my-cond c1 c2 ...)))))))

  (my-cond (#f 1) (else 2))                 ;==> 2
  (let ((else #f)) (my-cond (else 2)))      ;==> unspecified


  ;; Another nontrivial macro using only primitives, to be done
  ;; further down also with syntax-case:

  (define-syntax case
    (lambda (form)
      (or (and (list? form)
               (>= (length form) 3))
          (syntax-error))
      (let ((e (cadr form))
            (c1 (caddr form))
            (c2... (cdddr form)))
        (quasisyntax
         (let ((t ,e))
           ,(let f ((c1 c1) (cmore c2...))
              (or (and (list? c1)
                       (>= (length c1) 2))
                  (syntax-error))
              (if (null? cmore)
                  (cond ((literal-identifier=? (car c1) (syntax else))
                         (quasisyntax (begin ,@(cdr c1))))
                        ((list? (car c1))
                         (quasisyntax (if (memv t ',(car c1))
                                          (begin ,@(cdr c1)))))
                        (else (syntax-error)))
                  (cond ((list? (car c1))
                         (quasisyntax
                          (if (memv t ',(car c1))
                              (begin ,@(cdr c1))
                              ,(f (car cmore) (cdr cmore)))))
                        (else (syntax-error))))))))))

  (case 'a
    ((b c) 'no)
    ((d a) 'yes))   ;==> yes


  ;;=========================================================================
  ;;
  ;; Syntax and quasisyntax semantics:
  ;;
  ;;=========================================================================


  (bound-identifier=? (syntax x) (syntax x))                ;==> #f
  (bound-identifier=? (quasisyntax x) (quasisyntax x))      ;==> #f

  (quasisyntax ,(bound-identifier=? (syntax x) (syntax x))) ;==> #t

  (let-syntax ((f (lambda (form) (syntax (syntax x)))))
    (quasisyntax ,(bound-identifier=? (f)
                                      (f))))                ;==> #f

  (let ((y (syntax (x x))))
    (bound-identifier=? (car y) (cadr y)))                  ;==> #t

  (let ((top-car (syntax car)))
    (let ((car 2))
      (free-identifier=? top-car (syntax car))))            ;==> #f

  (let ((x 1))
    (let-syntax ((m (lambda (form)
                      (quasisyntax
                       (let ((x 2))
                         (let-syntax ((n (lambda (form)
                                           (free-identifier=? (cadr form)
                                                              (syntax x)))))
                           (n ,(cadr form))))))))
      (m x)))  ;==> #f

  ;; SYNTAX inside QUASISYNTAX preserves bound-identifier=?

  (let-syntax ((m (lambda (_)
                    (quasisyntax
                     (let ((,(syntax x) 1)) ,(syntax x))))))
    (m))   ;==> 1

  ;; With this semantics, macro-generating macros work correctly.

  (let-syntax ((m (lambda (form)
                    (let ((x (cadr form)))
                      (quasisyntax
                       (let-syntax ((n (lambda (_)
                                         (quasisyntax (let ((,(syntax ,x) 4)) ,(syntax ,x))))))
                         (n)))))))
    (m z))  ;==> 4

  ;; and are exactly equivalent to the corresponding SYNTAX-CASE forms:

  (let-syntax ((m (lambda (form)
                    (syntax-case form ()
                      ((_ x) (syntax
                              (let-syntax ((n (lambda (_)
                                                (syntax (let ((x 4)) x)))))
                                (n))))))))
    (m z))   ;==> 4


  ;; Using more generative SYNTAX semantics to define generate-temporaries:

  (define-syntax letrec0
    (lambda (x)

      (define (generate-temporaries ls)
        (map (lambda (x) (syntax temp)) ls))

      (syntax-case x ()
        ((_ ((i v) ...) e1 e2 ...)
         (with-syntax (((t ...) (generate-temporaries (syntax (i ...)))))
           (syntax (let ((i #f) ...)
                     (let ((t v) ...)
                       (set! i t) ...
                       (let () e1 e2 ...)))))))))

  (letrec0 ((x (lambda () y))
            (y (lambda () 1)))
    (y))                       ;==> 1


  ;;==========================================================================
  ;;
  ;; Examples of improved hygiene:
  ;;
  ;;==========================================================================

  ;; In traditional SYNTAX-CASE, the procedural forms of the macros
  ;; below would have accidental variable capture and/or violate
  ;; referential transparency:

  (letrec-syntax ((help (syntax-rules () ((help) (list 1 2))))
                  (main (syntax-rules () ((main) (let ((list +)) (help))))))
    (main)) ;==> (1 2)

  ;; equivalent to:

  (let-syntax ((main (lambda (_)
                       (define (help) (syntax (list 1 2)))
                       (with-syntax ((rest (help)))
                         (syntax (let ((list +)) rest))))))
    (main)) ;==> (1 2)

  ;;============

  (let ((x 1))
    (letrec-syntax ((help (syntax-rules () ((help) x)))
                    (main (syntax-rules () ((main) (let ((x 2)) (help))))))
      (main))) ;==> 1

  ;; equivalent to:

  (let ((x 1))
    (let-syntax ((main (lambda (form)
                         (define (help) (syntax x))
                         (with-syntax ((rest (help))) (syntax (let ((x 2)) rest))))))
      (main))) ;==> 1

  ;; or more concisely:

  (let ((x 1))
    (let-syntax ((main (lambda (form)
                         (define (help) (syntax x))
                         (quasisyntax (let ((x 2)) ,(help))))))
      (main))) ;==> 1

  ;; but not equivalent to:

  (let ((x 1))
    (let-syntax ((main (lambda (form)
                         (quasisyntax (let ((x 2)) ,(syntax x))))))
      (main))) ;==> 2

  ;============

  (letrec-syntax ((help (syntax-rules () ((help y) (let ((x 2)) y))))
                  (main (syntax-rules () ((main)   (let ((x 1)) (help x))))))
    (main))

  ;; equivalent to:

  (let ((x 1))
    (let-syntax ((main (lambda (form)
                         (define (help y)
                           (with-syntax ((y y))
                             (syntax (let ((x 2)) y))))
                         (with-syntax ((x (syntax x)))
                           (with-syntax ((rest (help (syntax x))))
                             (let ((x 1)) rest))))))
      (main)))

  ;; or more concisely:

  (let ((x 1))
    (let-syntax ((main (lambda (form)
                         (define (help y)
                           (quasisyntax (let ((x 2)) ,y)))
                         (quasisyntax (let ((x 1)) ,(help (syntax x)))))))
      (main)))


  ;; A more practical example:
  ;; The following macro does not suffer from accidental variable capture:

  (define-syntax let-in-order
    (lambda (form)
      (syntax-case form ()
        ((_ ((i e) ...) e0 e1 ...)
         (let f ((ies (syntax ((i e) ...)))
                 (its '()))
           (syntax-case ies ()
             (()            (with-syntax ((its its))
                              (syntax (let its e0 e1 ...))))
             (((i e) . ies) (with-syntax ((t (syntax t)))
                              (with-syntax ((rest (f (syntax ies)
                                                     (cons (syntax (i t)) its))))
                                (syntax (let ((t e)) rest)))))))))))

  (let-in-order ((x 1)
                 (y 2))
    (+ x y))                ;==> 3

  ;; ... whereas the same macro would give the wrong answer 4 in
  ;; traditional SYNTAX-CASE implementations.

  ;; A bit shorter with QUASISYNTAX, which does not invert the
  ;; structure of the code as above.  Also, the embedded occurrence
  ;; of SYNTAX is regarded as a continuation of the outer QUASISYNTAX
  ;; for the purpose of bound-identifier=? equivalence,
  ;; so we do not need to introduce t separately.

  (define-syntax let-in-order
    (lambda (form)
      (syntax-case form ()
        ((_ ((i e) ...) e0 e1 ...)
         (let f ((ies (syntax ((i e) ...)))
                 (its (syntax ())))
           (syntax-case ies ()
             (()            (quasisyntax (let ,its e0 e1 ...)))
             (((i e) . ies) (quasisyntax
                             (let ((t e))
                               ,(f (syntax ies)
                                   (quasisyntax ((i t) ,@its))))))))))))

  (let-in-order ((x 1)
                 (y 2))
    (+ x y))                ;==> 3


  ;; The same macro expressed without syntax-case:

  (define-syntax (let-in-order bindings . body)
    (let f ((ies bindings)
            (its (syntax ())))
      (cond ((null? ies) (quasisyntax (let ,its ,@body)))
            ((pair? ies) (quasisyntax
                          (let ((t ,(cadar ies)))
                            ,(f (cdr ies)
                                (quasisyntax ((,(caar ies) t) ,@its)))))))))

  (let-in-order ((x 1)
                 (y 2))
    (+ x y))                ;==> 3


  ;;=========================================================================
  ;;
  ;; Composing macros with intentional variable capture using DATUM->SYNTAX
  ;;
  ;;=========================================================================


  (define-syntax if-it
    (lambda (x)
      (syntax-case x ()
        ((k e1 e2 e3)
         (with-syntax ((it (datum->syntax-object (syntax k) 'it)))
           (syntax (let ((it e1))
                     (if it e2 e3))))))))

  (define-syntax when-it
    (lambda (x)
      (syntax-case x ()
        ((k e1 e2)
         (with-syntax ((it* (datum->syntax-object (syntax k) 'it)))
           (syntax (if-it e1
                          (let ((it* it)) e2)
                          (if #f #f))))))))

  (define-syntax my-or
    (lambda (x)
      (syntax-case x ()
        ((k e1 e2)
         (syntax (if-it e1 it e2))))))

  (if-it 2 it 3)    ;==> 2
  (when-it 42 it)   ;==> 42
  (my-or 2 3)       ;==> 2
  ; (my-or #f it)   ;==> undefined identifier: it


  (let ((it 1)) (if-it 42 it #f))   ;==> 42
  (let ((it 1)) (when-it 42 it))    ;==> 42
  (let ((it 1)) (my-or #f it))      ;==> 1
  (let ((if-it 1)) (when-it 42 it)) ;==> 42



  ;;=========================================================================
  ;;
  ;; Composing macros with intentional variable capture using
  ;; MAKE-FLUID-IDENTIFIER:
  ;;
  ;;=========================================================================

  (define-syntax if-it
    (lambda (x)
      (syntax-case x ()
        ((k e1 e2 e3)
         (with-syntax ((it (make-fluid-identifier (syntax here) 'it)))
           (syntax (let ((it e1))
                     (if it e2 e3))))))))

  (define-syntax when-it
    (lambda (x)
      (syntax-case x ()
        ((k e1 e2)
         (syntax (if-it e1 e2 (if #f #f)))))))

  (define-syntax my-or
    (lambda (x)
      (syntax-case x ()
        ((k e1 e2)
         (syntax (let ((thunk (lambda () e2)))
                   (if-it e1 it (thunk))))))))

  (if-it 2 it 3)    ;==> 2
  (when-it 42 it)   ;==> 42
  (my-or 2 3)       ;==> 2
  ; (my-or #f it)   ;==> undefined identifier: it


  (let ((it 1)) (if-it 42 it #f))   ;==> 1
  (let ((it 1)) (when-it 42 it))    ;==> 1
  (let ((it 1)) (my-or 42 it))      ;==> 42
  (let ((it 1)) (my-or #f it))      ;==> 1
  (let ((if-it 1)) (when-it 42 it)) ;==> 42


  ;;=========================================================================
  ;;
  ;; Loop macro - see Petrofsky discussion at
  ;; http://groups-beta.google.com/group/comp.lang.scheme/msg/5438d13dae4b9f71
  ;;
  ;;=========================================================================

  (define-syntax loop
    (lambda (x)
      (syntax-case x ()
        ((k e ...)
         (with-syntax ((break (make-fluid-identifier (syntax here) 'break)))
           (syntax
            (call-with-current-continuation
             (lambda (break)
               (let f () e ... (f))))))))))

  (loop
   (break 'foo))     ;==> foo

  (loop
   (loop
    (break 'foo))
   (break 'bar))     ;==> bar

  (call-with-current-continuation
   (lambda (break)
     (loop
      (break 'foo))
     (break 'bar)))  ;==> foo (as in Petrofsky's example,
                     ;         explicit binding takes precedence over implicit)

  (define-syntax loop-while
    (lambda (form)
      (syntax-case form ()
        ((_ test exp ...)
         (syntax
          (loop (if (not test) (break #f))
                exp ...))))))

  (let ((n 0))
    (loop-while (< n 5)
                (set! n (+ n 1)))
    n)
                        ;==>  5

  (loop
   (let ((n 0))
     (loop-while (< n 5)
                 (set! n (+ n 1))
                 (if (= n 2)
                     (break 'foo)))
     (break 'bar)))
                        ;==>  bar

  (let ((loop #f))
    (loop-while #t (break 42)))  ;==> 42


  ;;=========================================================================
  ;;
  ;; An example of using make-fluid-identifier to implement fluid-let-syntax:
  ;;
  ;;=========================================================================


  (define-syntax fluid-let-syntax
    (lambda (form)
      (syntax-case form ()
        ((k ((i e) ...) e1 e2 ...)
         (with-syntax (((fi ...)
                        (map (lambda (i)
                               (make-fluid-identifier i
                                                      (syntax-object->datum i)))
                             (syntax (i ...)))))
           (syntax
            (let-syntax ((fi e) ...) e1 e2 ...)))))))


  (let ((f (lambda (x) (+ x 1))))
    (let-syntax ((g (syntax-rules ()
                      ((_ x) (f x)))))
      (let-syntax ((f (syntax-rules ()
                        ((_ x) x))))
        (g 1))))  ;==> 2


  (let ((f (lambda (x) (+ x 1))))
    (let-syntax ((g (syntax-rules ()
                      ((_ x) (f x)))))
      (fluid-let-syntax ((f (syntax-rules ()
                              ((_ x) x))))
        (g 1))))  ;==> 1


  ;;=========================================================================
  ;;
  ;; Escaping ellipses:
  ;;
  ;;=========================================================================


  (let-syntax ((m (lambda (form)
                    (syntax-case form ()
                      ((_ x ...)
                       (with-syntax ((::: (syntax ...)))
                         (syntax
                          (let-syntax ((n (lambda (form)
                                            (syntax-case form ()
                                              ((_ x ... :::)
                                               (syntax `(x ... :::)))))))
                            (n a b c d)))))))))
      (m u v))
                                        ;==> (a b c d)


  ;;========================================================================
  ;;
  ;; EMBEDDED-SYNTAX:
  ;;
  ;;========================================================================

  ;; Embedded-syntax is useful for certain macro-generating macros
  ;; that have to compose pieces of code preserving bound-identifier=?
  ;; where not all the pieces are passed via the same chain of calls.
  ;; For example, in the following fragment, z is passed to the inner
  ;; macro by two paths, one of them via x and then y, and the other
  ;; via only x:

  ;; Although we could have avoided the problem here, there are legitimate
  ;; cases where we cannot (cf. the very implementation of SYNTAX-CASE).
  ;; Instead, using EMBEDDED-SYNTAX, we can "tunnel" a piece of syntax
  ;; x as follows:

  (let ((z 2))
    (let-syntax ((m (lambda (form)
                      (syntax-case form ()
                        ((_ x)
                         (syntax
                          (let-syntax ((n (lambda (form*)
                                            (syntax-case form* ()
                                              ((_ y)
                                               (with-syntax ((x (embedded-syntax x)))
                                                 (syntax (let ((y 1))
                                                           x))))))))
                            (n x))))))))
      (m z)))   ;==> 1

  ;; compared to:

  (let ((z 2))
    (let-syntax ((m (lambda (form)
                      (syntax-case form ()
                        ((_ x)
                         (syntax
                          (let-syntax ((n (lambda (form*)
                                            (syntax-case form* ()
                                              ((_ y)
                                               (syntax (let ((y 1))
                                                         x)))))))
                            (n x))))))))
      (m z)))   ;==> 2

  ;; or equivalently:

  (let ((z 2))
    (let-syntax ((m (lambda (form)
                      (quasisyntax
                       (let-syntax ((n (lambda (form*)
                                         (quasisyntax
                                          (let ((,(cadr form*) 1))
                                            ,(embedded-syntax ,(cadr form)))))))
                         (n ,(cadr form)))))))
      (m z)))   ;==> 1

  ;; compared to:

  (let ((z 2))
    (let-syntax ((m (lambda (form)
                      (quasisyntax
                       (let-syntax ((n (lambda (form*)
                                         (quasisyntax
                                          (let ((,(cadr form*) 1))
                                            ,(syntax ,(cadr form)))))))
                         (n ,(cadr form)))))))
      (m z)))   ;==> 2



  ;;====================================================================
  ;;
  ;; MISCELLANEOUS TESTS, CORNER CASES, ETC.
  ;;
  ;; Collected from various sources by (I believe) Scott Miller, and
  ;; supplemented with some of my own.
  ;;
  ;;====================================================================

  ;; From R5RS:

  (define-syntax or
    (syntax-rules ()
      ((or)          #f)
      ((or e)        e)
      ((or e1 e ...) (let ((temp e1))
                       (if temp temp (or e ...))))))

  (or #f #f 1)

  (define-syntax or
    (lambda (form)
      (syntax-case form ()
        ((or)          (syntax #f))
        ((or e)        (syntax e))
        ((or e1 e ...) (syntax (let ((temp e1))
                                 (if temp temp (or e ...))))))))

  (or #f #f 1)


  (let-syntax ((when (syntax-rules ()
                       ((when test stmt1 stmt2 ...)
                        (if test
                            (begin stmt1
                                   stmt2 ...))))))
    (let ((if #t))
      (when if (set! if 'now))
      if))                                  ;===>  now

  (let ((x 'outer))
    (let-syntax ((m (syntax-rules () ((m) x))))
      (let ((x 'inner))
        (m))))                              ;===>  outer

  (letrec-syntax
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
             y)))                ;===>  7


  (define-syntax cond
    (syntax-rules (else =>)
      ((cond (else result1 result2 ...))
       (begin result1 result2 ...))
      ((cond (test => result))
       (let ((temp test))
         (if temp (result temp))))
      ((cond (test => result) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             (result temp)
             (cond clause1 clause2 ...))))
      ((cond (test)) test)
      ((cond (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             temp
             (cond clause1 clause2 ...))))
      ((cond (test result1 result2 ...))
       (if test (begin result1 result2 ...)))
      ((cond (test result1 result2 ...)
             clause1 clause2 ...)
       (if test
           (begin result1 result2 ...)
           (cond clause1 clause2 ...)))))

  (let ((=> #f))
    (cond (#t => 'ok)))                   ;===> ok

  (cond ('(1 2) => cdr))                  ;===> (2)

  (cond ((> 3 2) 'greater)
        ((< 3 2) 'less))                 ;===>  greater
  (cond ((> 3 3) 'greater)
        ((< 3 3) 'less)
        (else 'equal))                   ;===>  equal


  ;; Eli Barzilay
  ;; In thread:
  ;; R5RS macros...
  ;; http://groups.google.com/groups?selm=skitsdqjq3.fsf%40tulare.cs.cornell.edu

  (let-syntax ((foo
                (syntax-rules ()
                  ((_ expr) (+ expr 1)))))
    (let ((+ *))
      (foo 3)))               ;==> 4

  ;; Al Petrofsky again
  ;; In thread:
  ;; Buggy use of begin in r5rs cond and case macros.
  ;; http://groups.google.com/groups?selm=87bse3bznr.fsf%40radish.petrofsky.org

  (let-syntax ((foo (syntax-rules ()
                      ((_ var) (define var 1)))))
    (let ((x 2))
      (begin (define foo +))
      (cond (else (foo x)))
      x))                    ;==> 2

  ;; Al Petrofsky
  ;; In thread:
  ;; An Advanced syntax-rules Primer for the Mildly Insane
  ;; http://groups.google.com/groups?selm=87it8db0um.fsf@radish.petrofsky.org

  (let ((x 1))
    (let-syntax
        ((foo (syntax-rules ()
                ((_ y) (let-syntax
                           ((bar (syntax-rules ()
                                   ((_) (let ((x 2)) y)))))
                         (bar))))))
      (foo x)))                        ;==> 1

  ;; which would be done as follows using quasisyntax:

  (let ((x 1))
    (let-syntax
        ((foo (lambda (form)
                (quasisyntax
                 (let-syntax
                     ((bar (lambda (_)
                             (syntax (let ((x 2)) ,(cadr form))))))
                    (bar))))))
      (foo x)))                 ;==> 1

  ;; another example:

  (let ((x 1))
    (let-syntax
        ((foo (syntax-rules ()
                ((_ y) (let-syntax
                           ((bar (syntax-rules ()
                                   ((_ x) y))))
                         (bar 2))))))
      (foo x)))                         ;==> 1

  ;; Al Petrofsky

  (let-syntax ((x (syntax-rules ()))) 1)    ;==> 1

  ;; This example actually illustrates a bug in R5RS.  If a Scheme system
  ;; follows the letter of the standard, 1 should be returned, but
  ;; the general agreement is that 2 should instead be returned.
  ;; The reason is that in R5RS, let-syntax always introduces new scope, thus
  ;; in the following test, the let-syntax breaks the definition section
  ;; and begins the expression section of the let.
  ;;
  ;; The general agreement by the implementors in 1998 was that the following
  ;; should be possible, but isn't:
  ;;
  ;;   (define ---)
  ;;   (let-syntax (---)
  ;;     (define ---)
  ;;     (define ---))
  ;;   (define ---)
  ;;
  ;; Scheme systems based on the Portable syntax-case expander by Dybvig
  ;; and Waddell do allow the above, and thus often violate the letter of
  ;; R5RS.  In such systems, the following will produce a local scope:
  ;;
  ;;   (define ---)
  ;;   (let-syntax ((a ---))
  ;;     (let ()
  ;;       (define ---)
  ;;       (define ---)))
  ;;   (define ---)
  ;;
  ;; Credits to Matthias Radestock and thanks to R. Kent Dybvig for the
  ;; explanation and background

  (let ((x 1))
    (let-syntax ((foo (syntax-rules () ((_) 2))))
      (define x (foo))
      3)
    x)                 ;==> 2

  ;; Al Petrofsky

  (let ((a 1))
    (letrec-syntax
        ((foo (syntax-rules ()
                ((_ b)
                 (bar a b))))
         (bar (syntax-rules ()
                ((_ c d)
                 (cons c (let ((c 3))
                           (list d c 'c)))))))
      (let ((a 2))
        (foo a))))                ;==> (1 2 3 a)



  ;;==============================================================================
  ;;
  ;; Syntax-case: Examples and tests:
  ;;
  ;;==============================================================================

  ;; Various examples from the Chez Scheme documentation
  ;; Only the CASE macro needed a minor change

  (define-syntax cond       ; no change:
    (lambda (x)
      (syntax-case x ()
        ((_ c1 c2 ...)
         (let f ((c1 (syntax c1))
                 (cmore (syntax (c2 ...))))
           (if (null? cmore)
               (syntax-case c1 (else =>)
                 ((else e1 e2 ...) (syntax (begin e1 e2 ...)))
                 ((e0)             (syntax (let ((t e0)) (if t t))))
                 ((e0 => e1)       (syntax (let ((t e0)) (if t (e1 t)))))
                 ((e0 e1 e2 ...)   (syntax (if e0 (begin e1 e2 ...)))))
               (with-syntax ((rest (f (car cmore) (cdr cmore))))
                 (syntax-case c1 (=>)
                   ((e0)           (syntax (let ((t e0)) (if t t rest))))
                   ((e0 => e1)     (syntax (let ((t e0)) (if t (e1 t) rest))))
                   ((e0 e1 e2 ...) (syntax (if e0 (begin e1 e2 ...) rest)))))))))))

  (let ((=> #f))
    (cond (#t => 'ok)))                   ;===> ok

  (cond ('(1 2) => cdr))                  ;===> (2)

  (cond ((< 3 2) 'less)
        ((> 3 2) 'greater))               ;===>  greater

  (cond ((> 3 3) 'greater)
        ((< 3 3) 'less)
        (else 'equal))                    ;===>  equal


  (define-syntax let0     ; no change
    (lambda (x)
      (define ids?
        (lambda (ls)
          (or (null? ls)
              (and (identifier? (car ls))
                   (ids? (cdr ls))))))
      (define unique-ids?
        (lambda (ls)
          (or (null? ls)
              (and (let notmem? ((x (car ls)) (ls (cdr ls)))
                     (or (null? ls)
                         (and (not (bound-identifier=? x (car ls)))
                              (notmem? x (cdr ls)))))
                   (unique-ids? (cdr ls))))))
      (syntax-case x ()
        ((_ ((i v) ...) e1 e2 ...)
         (and (ids? (syntax (i ...)))
              (unique-ids? (syntax (i ...))))
         (syntax ((lambda (i ...) e1 e2 ...) v ...))))))

  (let0 ((x 1) (y 2) (z 3)) (+ x y z))     ;==> 6

  ; (let0 ((x 1) (y 2) (x 3)) (+ x y z))   ;==> syntax-error


  (define-syntax case   ;; slight modification necessary:
    (lambda (x)
      (syntax-case x ()
        ((_ e c1 c2 ...)
         (with-syntax ((t (syntax t)))  ;; modification
           (with-syntax ((body
                          (let f ((c1 (syntax c1)) (cmore (syntax (c2 ...))))
                            (if (null? cmore)
                                (syntax-case c1 (else)
                                  ((else e1 e2 ...)    (syntax (begin e1 e2 ...)))
                                  (((k ...) e1 e2 ...) (syntax (if (memv t '(k ...))
                                                                   (begin e1 e2 ...)))))
                                (with-syntax ((rest (f (car cmore) (cdr cmore))))
                                  (syntax-case c1 ()
                                    (((k ...) e1 e2 ...)
                                     (syntax (if (memv t '(k ...))
                                                 (begin e1 e2 ...)
                                                 rest)))))))))
             (syntax (let ((t e)) body))))))))


  (case 'a
    ((b c) 'no)
    ((d a) 'yes))

  ;; Quasisyntax slightly more readable since it does not invert the
  ;; structure of the code.  Also, since embedded occurrences of
  ;; syntax or quasisyntax are regarded as continuations of the outer
  ;; quasisyntax for the purpose of bound-identifier=? equivalence,
  ;; we do not need to first bind t separately.

  (define-syntax case
    (lambda (x)
      (syntax-case x ()
        ((_ e c1 c2 ...)
         (quasisyntax
          (let ((t e))
            ,(let f ((c1 (syntax c1)) (cmore (syntax (c2 ...))))
               (if (null? cmore)
                   (syntax-case c1 (else)
                     ((else e1 e2 ...)    (syntax (begin e1 e2 ...)))
                     (((k ...) e1 e2 ...) (syntax (if (memv t '(k ...))
                                                      (begin e1 e2 ...)))))
                   (syntax-case c1 ()
                     (((k ...) e1 e2 ...)
                      (quasisyntax
                       (if (memv t '(k ...))
                           (begin e1 e2 ...)
                           ,(f (car cmore) (cdr cmore))))))))))))))

  (case 'a
    ((b c) 'no)
    ((d a) 'yes))


  (define-syntax loop     ; no change
    (lambda (x)
      (syntax-case x ()
        ((k e ...)
         (with-syntax ((break (datum->syntax-object (syntax k) 'break)))
           (syntax (call-with-current-continuation
                    (lambda (break)
                      (let f () e ... (f))))))))))

  (let ((n 3) (ls '()))
    (loop
     (if (= n 0) (break ls))
     (set! ls (cons 'a ls))
     (set! n (- n 1))))    ;==> (a a a)



  (define-syntax do        ; no change
    (lambda (x)
      (syntax-case x ()
        ((_ (binding ...) (test res ...) exp ...)
         (with-syntax ((((var val update) ...)
                        (map (lambda (b)
                               (syntax-case b ()
                                 ((var val)
                                  (syntax (var val var)))
                                 ((var val update)
                                  (syntax (var val update)))))
                             (syntax (binding ...)))))
           (syntax (let doloop ((var val) ...)
                     (if test
                         (begin (if #f #f) res ...)
                         (begin exp ... (doloop update ...))))))))))

  (let ((x '(1 3 5 7 9)))
    (do ((x x (cdr x))
         (sum 0 (+ sum (car x))))
      ((null? x) sum)))                ;==>  25


  (define-syntax define-structure   ; no change
    (lambda (x)
      (define gen-id
        (lambda (template-id . args)
          (datum->syntax-object template-id
                                (string->symbol
                                 (apply string-append
                                        (map (lambda (x)
                                               (if (string? x)
                                                   x
                                                   (symbol->string
                                                    (syntax-object->datum x))))
                                             args))))))
      (syntax-case x ()
        ((_ name field ...)
         (with-syntax
             ((constructor (gen-id (syntax name) "make-" (syntax name)))
              (predicate (gen-id (syntax name) (syntax name) "?"))
              ((access ...)
               (map (lambda (x) (gen-id x (syntax name) "-" x))
                    (syntax (field ...))))
              ((assign ...)
               (map (lambda (x) (gen-id x "set-" (syntax name) "-" x "!"))
                    (syntax (field ...))))
              (structure-length (+ (length (syntax (field ...))) 1))
              ((index ...) (let f ((i 1) (ids (syntax (field ...))))
                             (if (null? ids)
                                 '()
                                 (cons i (f (+ i 1) (cdr ids)))))))
           (syntax (begin
                     (define constructor
                       (lambda (field ...)
                         (vector 'name field ...)))
                     (define predicate
                       (lambda (x)
                         (and (vector? x)
                              (= (vector-length x) structure-length)
                              (eq? (vector-ref x 0) 'name))))
                     (define access (lambda (x) (vector-ref x index))) ...
                     (define assign
                       (lambda (x update)
                         (vector-set! x index update)))
                     ...)))))))

  (define-structure tree left right)
  (define t
    (make-tree
     (make-tree 0 1)
     (make-tree 2 3)))

  t                     ;==> #(tree #(tree 0 1) #(tree 2 3))
  (tree? t)             ;==> #t
  (tree-left t)         ;==>#(tree 0 1)
  (tree-right t)        ;==> #(tree 2 3)
  (set-tree-left! t 0)
  t                     ;==> #(tree 0 #(tree 2 3))

;  )) ; repl
