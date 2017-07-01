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
; collect all the case clauses.
; After duplicated constants have been removed, the predicates
; for these clauses can be tested in any order.

; Given an arbitrary variable reference, an expression that
; has not yet been simplified or can safely be simplified again,
; and a notepad, returns the expression after simplification.
; If the expression is equivalent to a case expression that dispatches
; on the given variable, then case optimization will be applied.
;
; These optimizations could generate many new references to the variable,
; which would violate the referencing invariants for Twobit pass 2.
; In particular, assignment elimination would no longer work correctly.
; To avoid this, the original reference is inserted instead of a copy.

(define (simplify-case-clauses ref0 E notepad)
  
  (define notepad2 (make-notepad (notepad.parent notepad)))
  
  (define (collect-clauses E clauses constants)
    (if (not (conditional? E))
        (analyze (simplify E notepad2) clauses constants)
        (let ((test (simplify (if.test E) notepad2))
              (code (simplify (if.then E) notepad2)))
          (if.test-set! E test)
          (if.then-set! E code)
          (if (not (call? test))
              (finish E clauses constants)
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
                    (finish E clauses constants)
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
                          (finish E clauses constants)
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
                               (collect-clauses E2
                                                (cons clause clauses)
                                                constants))))))))))))
  
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
  
  (define (finish E clauses constants)
    (if.else-set! E (simplify (if.else E) notepad2))
    (analyze E clauses constants))
  
  (define (analyze default clauses constants)
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
                            name:VECTOR-REF
                            name:FIXNUM-AND
                            name:FIXNUM-ARITHMETIC-SHIFT-LEFT)
                      (notepad.vars notepad2)))
    (analyze-clauses (notepad.vars notepad2)
                     ref0
                     default
                     (reverse clauses)
                     constants))
  
  (collect-clauses E '() '()))

; Returns true if EQ? and EQV? behave the same on x.

(define (eqv-is-ok? x)
  (or (smallint? x)
      (char? x)
      (symbol? x)
      (boolean? x)))

; Returns true if EQ? and EQV? behave the same on x.

(define (eq-is-ok? x)
  (eqv-is-ok? x))

(define (analyze-clauses F ref0 default clauses constants)
  (cond ((< (length constants) *sequential-threshold*)
         (implement-clauses-by-sequential-search ref0
                                                 default
                                                 clauses))
        (else
         (implement-clauses F ref0 default clauses constants))))

; Any case expression that dispatches on a variable var0 and whose
; constants are disjoint can be compiled as
;
; (let ((n (cond ((char? var0)
;                 <dispatch-on-char>)
;                ((symbol? var0)
;                 <dispatch-on-symbols>)
;                ; miscellaneous constants
;                ((eq? var0 'K1) ...)
;                ...
;                ; must come after miscellaneous, because some
;                ; cross-compilers might classify some fixnums
;                ; as miscellaneous
;                ((fixnum? var0)
;                 <dispatch-on-fixnum>)
;                (else 0))))
;   <dispatch-on-case-number>)
;
; where the <dispatch-on-case-number> uses binary search within
; the interval [0, p+1), where p is the number of non-default cases.

; Implements the general technique described above.

(define (implement-clauses F ref0 default clauses constants)
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
                    (map cadr clauses))))))
    (make-call L
               (list (implement-dispatch ref0
                                         (map car clauses))))))

(define (implement-case-dispatch ref0 exprs)
  (implement-intervals ref0
                       (map (lambda (n code)
                              (list n (+ n 1) code))
                            (iota (length exprs))
                            exprs)))

; Given the variable on which to dispatch and
; a list of constant lists for the clauses,
; returns code that computes the index of the selected clause.
; The mixed/miscellaneous clauses must be tested first because
; Twobit's SMALLINT? predicate might not be true of all fixnums
; on the target machine, which means that Twobit might classify
; some fixnums as miscellaneous.

(define (implement-dispatch ref0 selectors)

  (let* ((selectors:chr (map (lambda (x) (filter char? x))
                             selectors))
         (selectors:sym (map (lambda (x) (filter symbol? x))
                             selectors))
         (selectors:fix (map (lambda (x) (filter smallint? x))
                             selectors))
         (selectors:other (map (lambda (x)
                                 (filter (lambda (y)
                                            (and (not (char? y))
                                                 (not (symbol? y))
                                                 (not (smallint? y))))
                                         x))
                               selectors))
         (clause-indexes (cdr (iota (+ 1 (length selectors)))))
         (chr (filter (lambda (x) (not (null? (car x))))
                      (map list selectors:chr clause-indexes)))
         (sym (filter (lambda (x) (not (null? (car x))))
                      (map list selectors:sym clause-indexes)))
         (fix (filter (lambda (x) (not (null? (car x))))
                      (map list selectors:fix clause-indexes)))
         (other (filter (lambda (x) (not (null? (car x))))
                        (map list selectors:other clause-indexes)))
         (exp (make-constant 0))
         (exp (if (null? fix)
                  exp
                  (make-conditional (make-call (make-variable name:FIXNUM?)
                                               (list ref0))
                                    (implement-dispatch-fixnum ref0 fix)
                                    exp)))
         (exp (if (null? other)
                  exp
                  (implement-dispatch-other ref0 other exp)))
         (exp (if (null? sym)
                  exp
                  (make-conditional (make-call (make-variable name:SYMBOL?)
                                               (list ref0))
                                    (implement-dispatch-symbol ref0 sym)
                                    exp)))
         (exp (if (null? chr)
                  exp
                  (make-conditional (make-call (make-variable name:CHAR?)
                                               (list ref0))
                                    (implement-dispatch-char ref0 chr)
                                    exp))))
    exp))

; The value of ref0 will be known to be a fixnum.
; Each of the clauses is a list
; whose car is a list of fixnums and
; whose cadr is the fixnum index of a clause.
;
; Can use table lookup, binary search, or sequential search.
; FIXME: Never uses sequential search, which is best when
; there are only a few constants, with gaps between them.

(define (implement-dispatch-fixnum ref0 clauses)

  ; Given a list of clauses (each represented by a list of fixnum
  ; constants and the index of a clause they select), returns an
  ; equivalent sorted list of half-open intervals represented as
  ; (<lo> <hi> <exp>), where <exp> is an expression that
  ; evaluates to the index of a clause.
  
  (define (calculate-intervals clauses)
    (do ((clauses clauses (cdr clauses))
         (intervals '()
                    (let* ((clause (car clauses))
                           (constants (twobit-sort < (car clause)))
                           (index (cadr clause)))
                      (extract-intervals constants index intervals))))
        ((null? clauses)
         (sort-intervals intervals))))
  
  (define (extract-intervals constants index intervals)
    (if (null? constants)
        intervals
        (let ((k0 (car constants)))
          (do ((constants (cdr constants) (cdr constants))
               (k1 (+ k0 1) (+ k1 1)))
              ((or (null? constants)
                   (not (= k1 (car constants))))
               (extract-intervals constants
                                  index
                                  (cons (list k0 k1 (make-constant index))
                                        intervals)))))))
  
  ; Given a list of disjoint intervals represented as above,
  ; sorts them into increasing order.

  (define (sort-intervals intervals)
    (twobit-sort (lambda (interval1 interval2)
                   (< (car interval1) (car interval2)))
                 intervals))

  ; Given a sorted list of disjoint intervals as above,
  ; completes them by inserting intervals for the default.

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
                     (calculate-intervals clauses)))
         (lo (car (car intervals)))
         (hi (cadr (car (reverse intervals))))          ; exclusive upper bound
         (p (length intervals)))
    (make-conditional
     (make-call (make-variable name:FX<)
                (list ref0
                      (make-constant lo)))
     (make-constant 0)
     (make-conditional
      (make-call (make-variable name:FX<)
                 (list ref0
                       (make-constant hi)))
      ; The static cost of table lookup is about hi - lo words.
      ; The static cost of binary search is about 5 SPARC instructions
      ; per interval.
      (if (< (- hi lo) (* 5 p))
          (implement-table-lookup ref0 intervals lo hi)
          (implement-intervals ref0 intervals))
      (make-constant 0)))))

(define (implement-dispatch-char ref0 clauses)
  (let* ((clauses (map (lambda (clause)
                         (cons (map compat:char->integer (car clause))
                               (cdr clause)))
                       clauses))
         (name:n ((make-rename-procedure) 'n))
         (ref1 (make-variable name:n))
         (entry (make-R-entry name:n (list ref1) '() '()))
         (F (list name:n name:EQ? name:FX< name:FX- name:VECTOR-REF))
         (L (make-lambda
             (list name:n)
             '()
             (list entry)
             F
             '()
             '()
             #f
             (implement-dispatch-fixnum ref1 clauses))))
    (make-call L
               (list
                (make-call (make-variable name:CHAR->INTEGER)
                           (list ref0))))))

; The symbol dispatch cannot be reduced to the fixnum dispatch,
; because distinct symbols may hash to the same fixnum.
;
; The symbol dispatch is always implemented by hash lookup:
;
; ((lambda (symtable valtable i)
;    (if (eq? <ref0> (vector-ref symtable i))
;        (vector-ref valtable i)
;        (if (eq? <ref0> (vector-ref <symtable> (+ i 1)))
;            (vector-ref valtable (+ i 1))
;            ; this pattern above is repeated as many times as necessary
;            0)))
;  <symtable> <valtable> (fixnum-and <mask> (symbol-hash <ref0>)))

(define (implement-dispatch-symbol ref0 clauses)
  (let* ((n (length (apply append (map car clauses))))
         (bits (inexact->exact
                (floor (+ 1.3 (/ (log (+ n 1)) (log 2.0))))))
         (m (expt 2 bits))
         (mask (- m 1))
         (vec0 (make-vector (* 2 m) #f))
         (vec1 (make-vector (* 2 m) 0))
         ; the maximum distance between the hash index
         ; and the actual location of a symbol
         (maxdistance 0))

    ; vec0 and vec1 are larger than necessary,
    ; and combining them should improve cache performance.

    (define (combined-and-trimmed-vector)
      (let* ((v0length/2 (+ m maxdistance 1))
             (v0 (make-vector (* 2 v0length/2) #f)))
        (do ((i 0 (+ i 1)))
            ((= i v0length/2)
             v0)
          (vector-set! v0 (+ i i) (vector-ref vec0 i))
          (vector-set! v0 (+ i i 1) (vector-ref vec1 i)))))

    (define (make-fetch symtable i d)
      (if (> d maxdistance)
          (make-constant 0)
          (let* ((index (if (zero? d)
                            i
                            (make-call (make-variable name:FX+)
                                       (list i (make-constant (+ d d))))))
                 (index+1 (make-call (make-variable name:FX+)
                                     (list i (make-constant (+ d d 1)))))
                 (exp (make-call (make-variable name:VECTOR-REF)
                                 (list symtable index)))
                 (exp (make-call (make-variable name:EQ?)
                                 (list ref0 exp))))
            (make-conditional exp
                              (make-call (make-variable name:VECTOR-REF)
                                         (list symtable index+1))
                              (make-fetch symtable i (+ d 1))))))

    (for-each (lambda (clause)
                (let ((syms (car clause))
                      (index (cadr clause)))
                  (for-each (lambda (sym)
                              (let loop ((h (fxlogand
                                             mask
                                             (twobit-symbol-hash sym)))
                                         (d 0))
                                (if (vector-ref vec0 h)
                                    (loop (+ h 1) (+ d 1))
                                    (begin
                                     (if (> d maxdistance)
                                         (set! maxdistance d))
                                     (vector-set! vec0 h sym)
                                     (vector-set! vec1 h index)))))
                            syms)))
              clauses)

    (let* ((rename (make-rename-procedure))
           (name:symtable (rename 'symtable))
           (name:i (rename 'i))
           (ref:symtable (make-variable name:symtable))
           (ref:i (make-variable name:i))
           (entry:st (make-R-entry name:symtable (list ref:symtable) '() '()))
           (entry:i (make-R-entry name:i (list ref:i) '() '()))
           (F (list name:symtable name:i
                    name:EQ? name:VECTOR-REF name:FX+))
           (L (make-lambda
               (list name:symtable name:i)
               '()
               (list entry:st entry:i)
               F
               '()
               '()
               #f
               (make-fetch ref:symtable ref:i 0)))
           ; FIXME: (symbol-hash x) = (.vector-ref:trusted x 1)
           (exp (make-call (make-variable name:VECTOR-REF)
                           (list ref0 (make-constant 1))))
           (exp (make-call (make-variable name:FIXNUM-AND)
                           (list (make-constant mask) exp)))
           (exp (make-call (make-variable name:FIXNUM-ARITHMETIC-SHIFT-LEFT)
                           (list exp (make-constant 1))))
           (vec (combined-and-trimmed-vector)))

      (make-call L (list (make-constant vec) exp)))))

(define (implement-dispatch-other ref0 clauses default)
  (if (null? clauses)
      default
      (let* ((clause (car clauses))
             (clauses (cdr clauses))
             (constants (car clause))
             (index (cadr clause)))
      (make-conditional (make-call-to-memv ref0 constants)
                        (make-constant index)
                        (implement-dispatch-other ref0 clauses default)))))

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
; a sorted list of intervals of the form (<lo> <high> '<index>),
; where <index> is the index of the selected clause,
; and the least and greatest constants in those intervals,
; returns code for a table lookup.

(define (implement-table-lookup ref0 intervals lo hi)
  (let ((v (make-vector (+ 1 (- hi lo)) 0)))
    (for-each (lambda (interval)
                (let ((k0 (car interval))
                      (k1 (cadr interval))
                      (index (constant.value (caddr interval))))
                  (do ((i k0 (+ i 1)))
                      ((= i k1))
                    (vector-set! v (- i lo) index))))
              intervals)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Ugly things.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FIXME:  This code must be kept in sync with the definition of
; string-hash in Lib/Common/string.sch.
; Any change to this code must be made there also, and vice versa.

(define (twobit-string-hash string)

  (define (string-hash-step code byte)
    (fxlogxor code
              ;; Avoid consing fixnums
              (let* ((code (fxlogand code #x3FFFFF)) ; 22 bits
                     (l (fxlsh code 5)))             ; 27 bits
                (fxlogxor l byte))))

  (define (string-hash-loop string limit i code)
    (if (= i limit)
        code
        (string-hash-loop
         string limit (+ i 1)
         (string-hash-step code
                           (fxlogand #xFFFF
                                     (char->integer (string-ref string i)))))))

  (let ((n (string-length string)))
    (string-hash-loop string n 0 (fxlogxor n #x1aa5))))

; FIXME: This procedure must compute the same hash value
; as the symbol-hash procedure used at runtime.

(define (twobit-symbol-hash sym)
  (twobit-string-hash (symbol->string sym)))
