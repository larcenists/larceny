; Copyright 1991 William D Clinger (for SIMPLIFY-CONDITIONAL)
; Copyright 1999 William D Clinger (for everything else)
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
; 
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 14 September 2000.
;

; Thresholds that determine how CASE expressions are compiled.
;
; On the SPARC, sequential search is faster than binary search
; if there are fewer than 8 constants, and sequential search uses
; less than half the space if there are fewer than 10 constants.
; Most target machines should similar.

(define *sequential-threshold* 12)
;(define *memq-threshold* 20)
;(define *memv-threshold* 4)


; Some source transformations on IF expressions:
;
; (if '#f E1 E2)                      E2
; (if 'K  E1 E2)                      E1                    K != #f
; (if (if B0 '#f '#f) E1 E2)          (begin B0 E2)
; (if (if B0 '#f 'K ) E1 E2)          (if B0 E2 E1)         K != #f
; (if (if B0 'K  '#f) E1 E2)          (if B0 E1 E2)         K != #f
; (if (if B0 'K1 'K2) E1 E2)          (begin B0 E1)         K1, K2 != #f
; (if (if B0 (if B1 #t #f) B2) E1 E2) (if (if B0 B1 B2) E1 E2)
; (if (if B0 B1 (if B2 #t #f)) E1 E2) (if (if B0 B1 B2) E1 E2)
; (if (if X  X   B0 ) E1 E2)          (if (if X #t B0) E1 E2)   X a variable
; (if (if X  B0  X  ) E1 E2)          (if (if X B0 #f) E1 E2)   X a variable
; (if ((lambda (X)                    (if ((lambda (X)
;        (if X X B2)) B0)                    (if X #t (if B2 #t #f))) B0)
;     E1 E2)                              E1 E2)
; (if (begin ... B0) E1 E2)           (begin ... (if B0 E1 E2))
; (if (not E0) E1 E2)                 (if E0 E2 E1)         not is integrable
;
; FIXME:  Three of the transformations above are intended to clean up
; the output of the OR macro.  It isn't yet clear how well this works.

(define (simplify-conditional exp notepad)
  (define (coercion-to-boolean? exp)
    (and (conditional? exp)
         (let ((E1 (if.then exp))
               (E2 (if.else exp)))
           (and (constant? E1)
                (eq? #t (constant.value E1))
                (constant? E2)
                (eq? #f (constant.value E2))))))
  (if (not (control-optimization))
      (begin (if.test-set! exp (simplify (if.test exp) notepad))
             (if.then-set! exp (simplify (if.then exp) notepad))
             (if.else-set! exp (simplify (if.else exp) notepad))
             exp)
      (let* ((test (if.test exp)))
        (if (and (call? test)
                 (lambda? (call.proc test))
                 (let* ((L (call.proc test))
                        (body (lambda.body L)))
                   (and (conditional? body)
                        (let ((R (lambda.R L))
                              (B0 (if.test body))
                              (B1 (if.then body)))
                          (and (variable? B0)
                               (variable? B1)
                               (let ((x (variable.name B0)))
                                 (and (eq? x (variable.name B1))
                                      (local? R x)
                                      (= 1 (length R))
                                      (= 1 (length (call.args test))))))))))
            (let* ((L (call.proc test))
                   (R (lambda.R L))
                   (body (lambda.body L))
                   (ref (if.then body))
                   (x (variable.name ref))
                   (entry (R-entry R x)))
              (if.then-set! body (make-constant #t))
              (if.else-set! body
                            (make-conditional (if.else body)
                                              (make-constant #t)
                                              (make-constant #f)))
              (R-entry.references-set! entry
                                       (remq ref
                                             (R-entry.references entry)))
              (simplify-conditional exp notepad))
            (let loop ((test (simplify (if.test exp) notepad)))
              (if.test-set! exp test)
              (cond ((constant? test)
                     (simplify (if (constant.value test)
                                   (if.then exp)
                                   (if.else exp))
                               notepad))
                    ((and (conditional? test)
                          (constant? (if.then test))
                          (constant? (if.else test)))
                     (cond ((and (constant.value (if.then test))
                                 (constant.value (if.else test)))
                            (post-simplify-begin
                             (make-begin (list (if.test test)
                                               (simplify (if.then exp)
                                                         notepad)))
                             notepad))
                           ((and (not (constant.value (if.then test)))
                                 (not (constant.value (if.else test))))
                            (post-simplify-begin
                             (make-begin (list (if.test test)
                                               (simplify (if.else exp)
                                                         notepad)))
                             notepad))
                           (else (if (not (constant.value (if.then test)))
                                     (let ((temp (if.then exp)))
                                       (if.then-set! exp (if.else exp))
                                       (if.else-set! exp temp)))
                                 (if.test-set! exp (if.test test))
                                 (loop (if.test exp)))))
                    ((and (conditional? test)
                          (or (coercion-to-boolean? (if.then test))
                              (coercion-to-boolean? (if.else test))))
                     (if (coercion-to-boolean? (if.then test))
                         (if.then-set! test (if.test (if.then test)))
                         (if.else-set! test (if.test (if.else test))))
                     (loop test))
                    ((and (conditional? test)
                          (variable? (if.test test))
                          (let ((x (variable.name (if.test test))))
                            (or (and (variable? (if.then test))
                                     (eq? x (variable.name (if.then test)))
                                     1)
                                (and (variable? (if.else test))
                                     (eq? x (variable.name (if.else test)))
                                     2))))
                     =>
                     (lambda (n)
                       (case n
                         ((1) (if.then-set! test (make-constant #t)))
                         ((2) (if.else-set! test (make-constant #f))))
                       (loop test)))
                    ((begin? test)
                     (let ((exprs (reverse (begin.exprs test))))
                       (if.test-set! exp (car exprs))
                       (post-simplify-begin
                        (make-begin (reverse (cons (loop (car exprs))
                                                   (cdr exprs))))
                        notepad)))
                    ((and (call? test)
                          (variable? (call.proc test))
                          (eq? (variable.name (call.proc test)) name:NOT)
                          (integrable? name:NOT)
                          (= (length (call.args test)) 1))
                     (let ((temp (if.then exp)))
                       (if.then-set! exp (if.else exp))
                       (if.else-set! exp temp))
                     (loop (car (call.args test))))
                    (else
                     (simplify-case exp notepad))))))))

; Given a conditional expression whose test has been simplified,
; simplifies the then and else parts while applying optimizations
; for CASE expressions.
; Precondition: (control-optimization) is true.

(define (simplify-case exp notepad)
  (let ((E0 (if.test exp)))
    (if (and (call? E0)
             (variable? (call.proc E0))
             (let ((name (variable.name (call.proc E0))))
               ; FIXME: Should ensure that the name is integrable,
               ; but MEMQ and MEMV probably aren't according to the
               ; INTEGRABLE? predicate.
               (or (eq? name name:EQ?)
                   (eq? name name:EQV?)
                   (eq? name name:MEMQ)
                   (eq? name name:MEMV)))
             (= (length (call.args E0)) 2)
             (variable? (car (call.args E0)))
             (constant? (cadr (call.args E0))))
        (simplify-case-clauses (car (call.args E0))
                               exp
                               notepad)
        (begin (if.then-set! exp (simplify (if.then exp) notepad))
               (if.else-set! exp (simplify (if.else exp) notepad))
               exp))))

; Code generation for case expressions.
;
; A case expression turns into a conditional expression
; of the form
;
; CASE{I}  ::=  E  |  (if (PRED I K) E CASE{I})
; PRED  ::=  memv  |  memq  |  eqv?  |  eq?
;
; The memq and eq? predicates are used when the constant
; is a (list of) boolean, fixnum, char, empty list, or symbol.
; The constants will almost always be of these types.
;
; The first step is to remove duplicated constants and to
; collect all the case clauses, sorting them into the following
; categories based on their simplified list of constants:
;     constants are fixnums
;     constants are characters
;     constants are symbols
;     constants are of mixed or other type
; After duplicated constants have been removed, the predicates
; for these clauses can be tested in any order.

; Given an arbitrary variable reference, an expression that
; has not yet been simplified or can safely be simplified again,
; and a notepad, returns the expression after simplification.
; If the expression is equivalent to a case expression that dispatches
; on the given variable, then case-optimization will be applied.
;
; These optimizations could generate many new references to the variable,
; which would violate the referencing invariants for Twobit pass 2.
; In particular, assignment elimination would no longer work correctly.
; To avoid this, the original reference is inserted instead of a copy.

(define (simplify-case-clauses ref0 E notepad)
  
  (define notepad2 (make-notepad (notepad.parent notepad)))
  
  (define (collect-clauses E fix chr sym other constants)
    (if (not (conditional? E))
        (analyze (simplify E notepad2)
                 fix chr sym other constants)
        (let ((test (simplify (if.test E) notepad2))
              (code (simplify (if.then E) notepad2)))
          (if.test-set! E test)
          (if.then-set! E code)
          (if (not (call? test))
              (finish E fix chr sym other constants)
              (let ((proc (call.proc test))
                    (args (call.args test)))
                (if (not (and (variable? proc)
                              (let ((name (variable.name proc)))
                                ; FIXME: See note above.
                                (or (eq? name name:EQ?)
                                    (eq? name name:EQV?)
                                    (eq? name name:MEMQ)
                                    (eq? name name:MEMV)))
                              (= (length args) 2)
                              (variable? (car args))
                              (eq? (variable.name (car args))
                                   (variable.name ref0))
                              (constant? (cadr args))))
                    (finish E fix chr sym other constants)
                    (let ((pred (variable.name proc))
                          (datum (constant.value (cadr args))))
                      ; FIXME
                      (if (or (and (or (eq? pred name:MEMV)
                                       (eq? pred name:MEMQ))
                                   (not (list? datum)))
                              (and (eq? pred name:EQ?)
                                   (not (eqv-is-ok? datum)))
                              (and (eq? pred name:MEMQ)
                                   (not (every? (lambda (datum)
                                                  (eqv-is-ok? datum))
                                                datum))))
                          (finish E fix chr sym other constants)
                          (call-with-values
                           (lambda ()
                             (remove-duplicates (if (or (eq? pred name:EQV?)
                                                        (eq? pred name:EQ?))
                                                    (list datum)
                                                    datum)
                                                constants))
                           (lambda (data constants)
                             (let ((clause (list data code))
                                   (E2 (if.else E)))
                               (cond ((every? smallint? data)
                                      (collect-clauses E2
                                                       (cons clause fix)
                                                       chr
                                                       sym
                                                       other
                                                       constants))
                                     ((every? char? data)
                                      (collect-clauses E2
                                                       fix
                                                       (cons clause chr)
                                                       sym
                                                       other
                                                       constants))
                                     ((every? symbol? data)
                                      (collect-clauses E2
                                                       fix
                                                       chr
                                                       (cons clause sym)
                                                       other
                                                       constants))
                                     (else
                                      (collect-clauses E2
                                                       fix
                                                       chr
                                                       sym
                                                       (cons clause other)
                                                       constants))))))))))))))
  
  (define (remove-duplicates data set)
    (let loop ((originals data)
               (data '())
               (set set))
      (if (null? originals)
          (values data set)
          (let ((x (car originals))
                (originals (cdr originals)))
            (if (memv x set)
                (loop originals data set)
                (loop originals (cons x data) (cons x set)))))))
  
  (define (finish E fix chr sym other constants)
    (if.else-set! E (simplify (if.else E) notepad2))
    (analyze E fix chr sym other constants))
  
  (define (analyze default fix chr sym other constants)
    (notepad-var-add! notepad2 (variable.name ref0))
    (for-each (lambda (L)
                (notepad-lambda-add! notepad L))
              (notepad.lambdas notepad2))
    (for-each (lambda (L)
                (notepad-nonescaping-add! notepad L))
              (notepad.nonescaping notepad2))
    (for-each (lambda (var)
                (notepad-var-add! notepad var))
              (append (list name:FIXNUM?
                            name:CHAR?
                            name:SYMBOL?
                            name:FX<
                            name:FX-
                            name:CHAR->INTEGER
                            name:VECTOR-REF)
                      (notepad.vars notepad2)))
    (analyze-clauses (notepad.vars notepad2)
                     ref0
                     default
                     (reverse fix)
                     (reverse chr)
                     (reverse sym)
                     (reverse other)
                     constants))
  
  (collect-clauses E '() '() '() '() '()))

; Returns true if EQ? and EQV? behave the same on x.

(define (eqv-is-ok? x)
  (or (smallint? x)
      (char? x)
      (symbol? x)
      (boolean? x)))

; Returns true if EQ? and EQV? behave the same on x.

(define (eq-is-ok? x)
  (eqv-is-ok? x))

; Any case expression that dispatches on a variable var0 and whose
; constants are disjoint can be compiled as
;
; (let ((n (cond ((eq? var0 'K1) ...)   ; miscellaneous constants
;                ...
;                ((fixnum? var0)
;                 <dispatch-on-fixnum>)
;                ((char? var0)
;                 <dispatch-on-char>)
;                ((symbol? var0)
;                 <dispatch-on-symbols>)
;                (else 0))))
;   <dispatch-on-case-number>)
;
; where the <dispatch-on-case-number> uses binary search within
; the interval [0, p+1), where p is the number of non-default cases.

(define (analyze-clauses F ref0 default fix chr sym other constants)
  (cond ((or (and (null? fix)
                  (null? chr))
             (< (length constants) *sequential-threshold*))
         (implement-clauses-by-sequential-search ref0
                                                 default
                                                 (append fix chr sym other)))
        (else
         (implement-clauses F ref0 default fix chr sym other constants))))

; Implements the general technique described above.

(define (implement-clauses F ref0 default fix chr sym other constants)
  (let* ((name:n ((make-rename-procedure) 'n))
         (ref1 (make-variable name:n))
         (entry (make-R-entry name:n (list ref1) '() '()))
         (F (union (make-set (list name:n)) F))
         (L (make-lambda
             (list name:n)
             '()
             (list entry)
             F
             '()
             '()
             #f
             (implement-case-dispatch
              ref1
              (cons default
                    (map cadr
                         ; The order here must match the order
                         ; used by IMPLEMENT-DISPATCH.
                         (append other fix chr sym)))))))
    (make-call L
               (list (implement-dispatch 0
                                         ref0
                                         (map car other)
                                         (map car fix)
                                         (map car chr)
                                         (map car sym))))))

(define (implement-case-dispatch ref0 exprs)
  (implement-intervals ref0
                       (map (lambda (n code)
                              (list n (+ n 1) code))
                            (iota (length exprs))
                            exprs)))

; Given the number of prior clauses,
; the variable on which to dispatch,
; a list of constant lists for mixed or miscellaneous clauses,
; a list of constant lists for the fixnum clauses,
; a list of constant lists for the character clauses, and
; a list of constant lists for the symbol clauses,
; returns code that computes the index of the selected clause.
; The mixed/miscellaneous clauses must be tested first because
; Twobit's SMALLINT? predicate might not be true of all fixnums
; on the target machine, which means that Twobit might classify
; some fixnums as miscellaneous.

(define (implement-dispatch prior ref0 other fix chr sym)
  (cond ((not (null? other))
         (implement-dispatch-other
          (implement-dispatch (+ prior (length other))
                              ref0 fix chr sym '())
          prior ref0 other))
        ((not (null? fix))
         (make-conditional (make-call (make-variable name:FIXNUM?)
                                      (list ref0))
                           (implement-dispatch-fixnum prior ref0 fix)
                           (implement-dispatch (+ prior (length fix))
                                               ref0 '() chr sym other)))
        ((not (null? chr))
         (make-conditional (make-call (make-variable name:CHAR?)
                                      (list ref0))
                           (implement-dispatch-char prior ref0 chr)
                           (implement-dispatch (+ prior (length chr))
                                               ref0 fix '() sym other)))
        ((not (null? sym))
         (make-conditional (make-call (make-variable name:SYMBOL?)
                                      (list ref0))
                           (implement-dispatch-symbol prior ref0 sym)
                           (implement-dispatch (+ prior (length sym))
                                               ref0 fix chr '() other)))
        (else
         (make-constant 0))))

; The value of ref0 will be known to be a fixnum.
; Can use table lookup, binary search, or sequential search.
; FIXME: Never uses sequential search, which is best when
; there are only a few constants, with gaps between them.

(define (implement-dispatch-fixnum prior ref0 lists)
  
  (define (calculate-intervals n lists)
    (define (loop n lists intervals)
      (if (null? lists)
          (twobit-sort (lambda (interval1 interval2)
                         (< (car interval1) (car interval2)))
                       intervals)
          (let ((constants (twobit-sort < (car lists))))
            (loop (+ n 1)
                  (cdr lists)
                  (append (extract-intervals n constants)
                          intervals)))))
    (loop n lists '()))
  
  (define (extract-intervals n constants)
    (if (null? constants)
        '()
        (let ((k0 (car constants)))
          (do ((constants (cdr constants) (cdr constants))
               (k1 (+ k0 1) (+ k1 1)))
              ((or (null? constants)
                   (not (= k1 (car constants))))
               (cons (list k0 k1 (make-constant n))
                     (extract-intervals n constants)))))))
  
  (define (complete-intervals intervals)
    (cond ((null? intervals)
           intervals)
          ((null? (cdr intervals))
           intervals)
          (else
           (let* ((i1 (car intervals))
                  (i2 (cadr intervals))
                  (end1 (cadr i1))
                  (start2 (car i2))
                  (intervals (complete-intervals (cdr intervals))))
             (if (= end1 start2)
                 (cons i1 intervals)
                 (cons i1
                       (cons (list end1 start2 (make-constant 0))
                             intervals)))))))
  
  (let* ((intervals (complete-intervals
                     (calculate-intervals (+ prior 1) lists)))
         (lo (car (car intervals)))
         (hi (car (car (reverse intervals))))
         (p (length intervals)))
    (make-conditional
     (make-call (make-variable name:FX<)
                (list ref0
                      (make-constant lo)))
     (make-constant 0)
     (make-conditional
      (make-call (make-variable name:FX<)
                 (list ref0
                       (make-constant (+ hi 1))))
      ; The static cost of table lookup is about hi - lo words.
      ; The static cost of binary search is about 5 SPARC instructions
      ; per interval.
      (if (< (- hi lo) (* 5 p))
          (implement-table-lookup ref0 (+ prior 1) lists lo hi)
          (implement-intervals ref0 intervals))
      (make-constant 0)))))

(define (implement-dispatch-char prior ref0 lists)
  (let* ((lists (map (lambda (constants)
                       (map compat:char->integer constants))
                     lists))
         (name:n ((make-rename-procedure) 'n))
         ; Referencing information is destroyed by pass 2.
         ;(entry (make-R-entry name:n '() '() '()))
         (F (list name:n name:EQ? name:FX< name:FX- name:VECTOR-REF))
         (L (make-lambda
             (list name:n)
             '()
             '()  ; entry
             F
             '()
             '()
             #f
             (implement-dispatch-fixnum prior
                                        (make-variable name:n)
                                        lists))))
    (make-call L
               (make-call (make-variable name:CHAR->INTEGER)
                          (list ref0)))))

(define (implement-dispatch-symbol prior ref0 lists)
  (implement-dispatch-other (make-constant 0) prior ref0 lists))

(define (implement-dispatch-other default prior ref0 lists)
  (if (null? lists)
      default
      (let* ((constants (car lists))
             (lists (cdr lists))
             (n (+ prior 1)))
      (make-conditional (make-call-to-memv ref0 constants)
                        (make-constant n)
                        (implement-dispatch-other default n ref0 lists)))))

(define (make-call-to-memv ref0 constants)
  (cond ((null? constants)
         (make-constant #f))
        ((null? (cdr constants))
         (make-call-to-eqv ref0 (car constants)))
        (else
         (make-conditional (make-call-to-eqv ref0 (car constants))
                           (make-constant #t)
                           (make-call-to-memv ref0 (cdr constants))))))

(define (make-call-to-eqv ref0 constant)
  (make-call (make-variable
              (if (eq-is-ok? constant)
                  name:EQ?
                  name:EQV?))
             (list ref0
                   (make-constant constant))))

; Given a variable whose value is known to be a fixnum,
; the clause index for the first fixnum clause,
; an ordered list of lists of constants for fixnum-only clauses,
; and the least and greatest constants in those lists,
; returns code for a table lookup.

(define (implement-table-lookup ref0 index lists lo hi)
  (let ((v (make-vector (+ 1 (- hi lo)) 0)))
    (do ((index index (+ index 1))
         (lists lists (cdr lists)))
        ((null? lists))
        (for-each (lambda (k)
                    (vector-set! v (- k lo) index))
                  (car lists)))
    (make-call (make-variable name:VECTOR-REF)
               (list (make-constant v)
                     (make-call (make-variable name:FX-)
                                (list ref0
                                      (make-constant lo)))))))

; Given a variable whose value is known to lie within the
; half-open interval [m0, mk), and an ordered complete
; list of intervals of the form
;
;     ((m0 m1 code0)
;      (m1 m2 code1)
;      ...
;      (m{k-1} mk code{k-1})
;     )
;
; returns an expression that finds the unique i such that
; ref0 lies within [mi, m{i+1}), and then executes code{i}.

(define (implement-intervals ref0 intervals)
  (if (null? (cdr intervals))
      (caddr (car intervals))
      (let ((n (quotient (length intervals) 2)))
        (do ((n n (- n 1))
             (intervals1 '() (cons (car intervals2) intervals1))
             (intervals2 intervals (cdr intervals2)))
            ((zero? n)
             (let ((intervals1 (reverse intervals1))
                   (m (car (car intervals2))))
               (make-conditional (make-call (make-variable name:FX<)
                                            (list ref0
                                                  (make-constant m)))
                                 (implement-intervals ref0 intervals1)
                                 (implement-intervals ref0 intervals2))))))))

; The brute force approach.
; Given the variable on which the dispatch is being performed, and
; actual (simplified) code for the default clause and
; for all other clauses,
; returns code to perform the dispatch by sequential search.

(define (implement-clauses-by-sequential-search ref0 default clauses)
  (if (null? clauses)
      default
      (let* ((case1 (car clauses))
             (clauses (cdr clauses))
             (constants1 (car case1))
             (code1 (cadr case1)))
        (make-conditional (make-call-to-memv ref0 constants1)
                          code1
                          (implement-clauses-by-sequential-search
                           ref0 default clauses)))))
