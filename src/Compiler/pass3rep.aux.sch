; Representations, which form a subtype hierarchy.
;
; <rep>  ::=  <fixnum>  |  (<fixnum> <datum> ...)
;
; (<rep> <datum> ...) is a subtype of <rep>, but the non-fixnum
; representations are otherwise interpreted by arbitrary code.

(define *nreps* 0)
(define *rep-encodings* '())
(define *rep-decodings* '())
(define *rep-subtypes* '())
(define *rep-joins* (make-bytevector 0))
(define *rep-meets* (make-bytevector 0))
(define *rep-joins-special* '#())
(define *rep-meets-special* '#())

(define (representation-error msg . stuff)
  (apply error
         (if (string? msg)
             (string-append "Bug in flow analysis: " msg)
             msg)
         stuff))

(define (symbol->rep sym)
  (let ((probe (assq sym *rep-encodings*)))
    (if probe
        (cdr probe)
        (let ((rep *nreps*))
          (set! *nreps* (+ *nreps* 1))
          (if (> *nreps* 255)
              (representation-error "Too many representation types"))
          (set! *rep-encodings*
                (cons (cons sym rep)
                      *rep-encodings*))
          (set! *rep-decodings*
                (cons (cons rep sym)
                      *rep-decodings*))
          rep))))

(define (rep->symbol rep)
  (if (pair? rep)
      (cons (rep->symbol (car rep)) (cdr rep))
      (let ((probe (assv rep *rep-decodings*)))
        (if probe
            (cdr probe)
            'unknown))))

(define (representation-table table)
  (map (lambda (row)
         (map (lambda (x)
                (if (list? x)
                    (map symbol->rep x)
                    x))
              row))
       table))

; DEFINE-SUBTYPE is how representation types are defined.

(define (define-subtype sym1 sym2)
  (let* ((rep2 (symbol->rep sym2))
         (rep1 (symbol->rep sym1)))
    (set! *rep-subtypes*
          (cons (cons rep1 rep2)
                *rep-subtypes*))
    sym1))

; COMPUTE-TYPE-STRUCTURE! must be called before DEFINE-INTERSECTION.

(define (define-intersection sym1 sym2 sym3)
  (let ((rep1 (symbol->rep sym1))
        (rep2 (symbol->rep sym2))
        (rep3 (symbol->rep sym3)))
    (representation-aset! *rep-meets* rep1 rep2 rep3)
    (representation-aset! *rep-meets* rep2 rep1 rep3)))

;

(define (representation-aref bv i j)
  (bytevector-ref bv (+ (* *nreps* i) j)))

(define (representation-aset! bv i j x)
  (bytevector-set! bv (+ (* *nreps* i) j) x))

(define (compute-unions!)
  
  ; Always define a bottom element.
  
  (for-each (lambda (sym)
              (define-subtype 'bottom sym))
            (map car *rep-encodings*))
  
  (let* ((debugging? #f)
         (n *nreps*)
         (n^2 (* n n))
         (matrix (make-bytevector n^2)))
    
    ; This code assumes there will always be a top element.
    
    (define (lub rep1 rep2 subtype?)
      (do ((i 0 (+ i 1))
           (bounds '()
                   (if (and (subtype? rep1 i)
                            (subtype? rep2 i))
                       (cons i bounds)
                       bounds)))
          ((= i n)
           (car (twobit-sort subtype? bounds)))))
    
    (define (join i j)
      (lub i j (lambda (rep1 rep2)
                 (= 1 (representation-aref matrix rep1 rep2)))))
    
    (define (compute-transitive-closure!)
      (let ((changed? #f))
        (define (loop)
          (do ((i 0 (+ i 1)))
              ((= i n))
              (do ((k 0 (+ k 1)))
                  ((= k n))
                  (do ((j 0 (+ j 1))
                       (sum 0
                            (fxlogior sum
                                    (fxlogand
                                     (representation-aref matrix i j)
                                     (representation-aref matrix j k)))))
                      ((= j n)
                       (if (> sum 0)
                           (let ((x (representation-aref matrix i k)))
                             (if (zero? x)
                                 (begin
                                  (set! changed? #t)
                                  (representation-aset! matrix i k 1)))))))))
          (if changed?
              (begin (set! changed? #f)
                     (loop))))
        (loop)))
    
    (define (compute-joins!)
      (let ((default (lambda (x y)
                       (error "Compiler bug: special meet or join" x y))))
        (set! *rep-joins-special* (make-vector n default))
        (set! *rep-meets-special* (make-vector n default)))
      (set! *rep-joins* (make-bytevector n^2))
      (set! *rep-meets* (make-bytevector n^2))
      (do ((i 0 (+ i 1)))
          ((= i n))
          (do ((j 0 (+ j 1)))
              ((= j n))
              (representation-aset! *rep-joins*
                                    i
                                    j
                                    (join i j)))))
    
    (do ((i 0 (+ i 1)))
        ((= i n))
        (do ((j 0 (+ j 1)))
            ((= j n))
            (representation-aset! matrix i j 0))
        (representation-aset! matrix i i 1))
    (for-each (lambda (subtype)
                (let ((rep1 (car subtype))
                      (rep2 (cdr subtype)))
                  (representation-aset! matrix rep1 rep2 1)))
              *rep-subtypes*)
    (compute-transitive-closure!)
    (if debugging?
        (do ((i 0 (+ i 1)))
            ((= i n))
            (do ((j 0 (+ j 1)))
                ((= j n))
                (write-char #\space)
                (write (representation-aref matrix i j)))
            (newline)))
    (compute-joins!)
    (set! *rep-subtypes* '())))

; Intersections are not dual to unions because a conservative analysis
; must always err on the side of the larger subtype.
; COMPUTE-UNIONS! must be called before COMPUTE-INTERSECTIONS!.

(define (compute-intersections!)
  (let ((n *nreps*))
    
    (define (meet i j)
      (let ((k (representation-union i j)))
        (if (= i k)
            j
            i)))
    
    (do ((i 0 (+ i 1)))
        ((= i n))
        (do ((j 0 (+ j 1)))
            ((= j n))
            (representation-aset! *rep-meets*
                                  i
                                  j
                                  (meet i j))))))

(define (compute-type-structure!)
  (compute-unions!)
  (compute-intersections!))

(define (representation-subtype? rep1 rep2)
  (equal? rep2 (representation-union rep1 rep2)))

(define (representation-union rep1 rep2)
  (if (fixnum? rep1)
      (if (fixnum? rep2)
          (representation-aref *rep-joins* rep1 rep2)
          (representation-union rep1 (car rep2)))
      (if (fixnum? rep2)
          (representation-union (car rep1) rep2)
          (let ((r1 (car rep1))
                (r2 (car rep2)))
            (if (= r1 r2)
                ((vector-ref *rep-joins-special* r1) rep1 rep2)
                (representation-union r1 r2))))))

(define (representation-intersection rep1 rep2)
  (if (fixnum? rep1)
      (if (fixnum? rep2)
          (representation-aref *rep-meets* rep1 rep2)
          (representation-intersection rep1 (car rep2)))
      (if (fixnum? rep2)
          (representation-intersection (car rep1) rep2)
          (let ((r1 (car rep1))
                (r2 (car rep2)))
            (if (= r1 r2)
                ((vector-ref *rep-meets-special* r1) rep1 rep2)
                (representation-intersection r1 r2))))))

; For debugging.

(define (display-unions-and-intersections)
  (let* ((column-width 10)
         (columns/row (- (quotient 80 column-width) 1)))
    
    (define (display-symbol sym)
      (let* ((s (symbol->string sym))
             (n (string-length s)))
        (if (< n column-width)
            (begin (display s)
                   (display (make-string (- column-width n) #\space)))
            (begin (display (substring s 0 (- column-width 1)))
                   (write-char #\space)))))
    
    ; Display columns i to n.
    
    (define (display-matrix f i n)
      (display (make-string column-width #\space))
      (do ((i i (+ i 1)))
          ((= i n))
          (display-symbol (rep->symbol i)))
      (newline)
      (newline)
      (do ((k 0 (+ k 1)))
          ((= k *nreps*))
          (display-symbol (rep->symbol k))
          (do ((i i (+ i 1)))
              ((= i n))
              (display-symbol (rep->symbol (f k i))))
          (newline))
      (newline)
      (newline))
    
    (display "Unions:")
    (newline)
    (newline)
    
    (do ((i 0 (+ i columns/row)))
        ((>= i *nreps*))
        (display-matrix representation-union
                        i
                        (min *nreps* (+ i columns/row))))
    
    (display "Intersections:")
    (newline)
    (newline)
    
    (do ((i 0 (+ i columns/row)))
        ((>= i *nreps*))
        (display-matrix representation-intersection
                        i
                        (min *nreps* (+ i columns/row))))))

; Operations that can be specialized.
;
; Format: (<name> (<arg-rep> ...) <specific-name>)

(define (rep-specific? f rs)
  (rep-match f rs rep-specific caddr))

; Operations whose result has some specific representation.
;
; Format: (<name> (<arg-rep> ...) (<result-rep>))

(define (rep-result? f rs)
  (rep-match f rs rep-result caaddr))

; Unary predicates that give information about representation.
;
; Format: (<name> <rep-if-true> <rep-if-false>)

(define (rep-if-true f rs)
  (rep-match f rs rep-informing caddr))

(define (rep-if-false f rs)
  (rep-match f rs rep-informing cadddr))

; Given the name of an integrable primitive,
; the representations of its arguments,
; a representation table, and a selector function
; finds the most type-specific row of the table that matches both
; the name of the primitive and the representations of its arguments,
; and returns the result of applying the selector to that row.
; If no row matches, then REP-MATCH returns #f.
;
; FIXME:  This should be more efficient, and should prefer the most
; specific matches.

(define (rep-match f rs table selector)
  (let ((n (length rs)))
    (let loop ((entries table))
      (cond ((null? entries)
             #f)
            ((eq? f (car (car entries)))
             (let ((rs0 (cadr (car entries))))
               (if (and (= n (length rs0))
                        (every? (lambda (r1+r2)
                                  (let ((r1 (car r1+r2))
                                        (r2 (cdr r1+r2)))
                                    (representation-subtype? r1 r2)))
                                (map cons rs rs0)))
                   (selector (car entries))
                   (loop (cdr entries)))))
            (else
             (loop (cdr entries)))))))

; To prevent representation analysis from taking an inordinately
; long time, we keep track of the number of calls to aeval, and
; abandon representation analysis by widening everything to
; rep:object if the number of calls exceeds a threshold computed
; by multiplying the expression size by a multiplier.
;
; Widening is crude, but effective.
;
; The multiplier isn't critical.  A multiplier of 1000 would be
; small enough to cause widening on pathological cases, but
; would waste a lot of time first.  A multiplier of 12 is just
; large enough to avoid widening on Larceny's current code.
; A multiplier of 5 would be large enough to avoid widening on
; all but 3 expressions.

(define aeval:multiplier 25)

(define aeval:calls 0)
(define aeval:threshold 0)

; Abstract interpretation with respect to types and constraints.
; Returns a representation type.

(define (aeval E types constraints)
  (set! aeval:calls (+ aeval:calls 1))
  (cond ((> aeval:calls aeval:threshold)
         rep:object)
        ((call? E)
         (let ((proc (call.proc E)))
           (if (variable? proc)
               (let* ((op (variable.name proc))
                      (argtypes (map (lambda (E)
                                       (aeval E types constraints))
                                     (call.args E)))
                      (type (rep-result? op argtypes)))
                 (if type
                     type
                     rep:object))
               rep:object)))
        ((variable? E)
         (representation-typeof (variable.name E) types constraints))
        ((constant? E)
         (representation-of-value (constant.value E)))
        (else
         rep:object)))

; If x has representation type t0 in the hash table,
; and some further constraints
;
;     x = (op y1 ... yn)
;     x : t1
;      ...
;     x : tk
;
; then
;
;     typeof (x) = op (typeof (y1), ..., typeof (yn))
;                  &  t0  &  t1  &  ...  &  tk
;
; where & means intersection and op is the abstraction of op.
;
; Also if T : true and T = E then E may give information about
; the types of other variables.  Similarly for T : false.

(define (representation-typeof name types constraints)
  (let ((t0 (hashtable-fetch types name rep:object))
        (cs (hashtable-fetch (constraints.table constraints) name '())))
    (define (loop type cs)
      (if (null? cs)
          type
          (let* ((c (car cs))
                 (cs (cdr cs))
                 (E (constraint.rhs c)))
            (cond ((constant? E)
                   (loop (representation-intersection type
                                                      (constant.value E))
                         cs))
                  ((call? E)
                   (loop (representation-intersection
                          type (aeval E types constraints))
                         cs))
                  (else
                   (loop type cs))))))
    (loop t0 cs)))

; Constraints.
;
; The constraints used by this analysis consist of type constraints
; together with the available expressions used for commoning.
;
; (T E      K)   T = E     until killed by an effect in K
; (T '<rep> K)   T : <rep> until killed by an effect in K

(define (make-constraint T E K)
  (list T E K))

(define (constraint.lhs c)
  (car c))

(define (constraint.rhs c)
  (cadr c))

(define (constraint.killer c)
  (caddr c))

(define (make-type-constraint T type K)
  (make-constraint T
                   (make-constant type)
                   K))

; If the new constraint is of the form T = E until killed by K,
; then there shouldn't be any prior constraints.
;
; Otherwise the new constraint is of the form T : t until killed by K.
; Suppose the prior constraints are
;     T = E  until killed by K
;     T : t1 until killed by K1
;      ...
;     T : tn until killed by Kn
;
; If there exists i such that ti is a subtype of t and Ki a subset of K,
; then the new constraint adds no new information and should be ignored.
; Otherwise compute t' = t1 & ... & tn and K' = K1 | ... | Kn, where
; & indicates intersection and | indicates union.
; If K = K' then add the new constraint T : t' until killed by K;
; otherwise add two new constraints:
;     T : t' until killed by K'
;     T : t  until killed by K

(define (constraints-add! types constraints new)
  (let* ((debugging? #f)
         (T (constraint.lhs new))
         (E (constraint.rhs new))
         (K (constraint.killer new))
         (cs (constraints-for-variable constraints T)))
    
    (define (loop type K cs newcs)
      (if (null? cs)
          (cons (make-type-constraint T type K) newcs)
          (let* ((c2 (car cs))
                 (cs (cdr cs))
                 (E2 (constraint.rhs c2))
                 (K2 (constraint.killer c2)))
            (if (constant? E2)
                (let* ((type2 (constant.value E2))
                       (type3 (representation-intersection type type2)))
                  (cond ((eq? type2 type3)
                         (if (= K2 (fxlogand K K2))
                             (append newcs cs)
                             (loop (representation-intersection type type2)
                                   (available:killer-combine K K2)
                                   cs
                                   (cons c2 newcs))))
                        ((representation-subtype? type type3)
                         (if (= K (fxlogand K K2))
                             (loop type K cs newcs)
                             (loop type K cs (cons c2 newcs))))
                        (else
                         (loop type3
                               (available:killer-combine K K2)
                               cs
                               (cons c2 newcs)))))
                (let* ((op (variable.name (call.proc E2)))
                       (args (call.args E2))
                       (argtypes (map (lambda (exp)
                                        (aeval exp types constraints))
                                      args)))
                  (cond ((representation-subtype? type rep:true)
                         (let ((reps (rep-if-true op argtypes)))
                           (if reps
                               (record-new-reps! args argtypes reps K2))))
                        ((representation-subtype? type rep:false)
                         (let ((reps (rep-if-false op argtypes)))
                           (if reps
                               (record-new-reps! args argtypes reps K2)))))
                  (loop type K cs (cons c2 newcs)))))))
    
    (define (record-new-reps! args argtypes reps K2)
      (if debugging?
          (begin (write (list (map make-readable args)
                              (map rep->symbol argtypes)
                              (map rep->symbol reps)))
                 (newline)))
      (for-each (lambda (arg type0 type1)
                  (if (not (representation-subtype? type0 type1))
                      (if (variable? arg)
                          (let ((name (variable.name arg)))

                            ; FIXME:  In this context, a variable
                            ; should always be local so the hashtable
                            ; operation shouldn't be necessary, but
                            ; some previous pass appears to break
                            ; that invariant.

                            (if (hashtable-get types name)
                                (constraints-add!
                                 types
                                 constraints
                                 (make-type-constraint
                                  name
                                  type1 
                                  (available:killer-combine K K2)))
                                (if debugging?
                                    (cerror
                                     "Compiler bug: unexpected global: "
                                     name)))))))
                args argtypes reps))
    
    (if (not (zero? K))
        (constraints-add-killedby! constraints T K))
    
    (let* ((table (constraints.table constraints))
           (cs (hashtable-fetch table T '())))
      (cond ((constant? E)
             ; It's a type constraint.
             (let ((type (constant.value E)))
               (if debugging?
                   (begin (display T)
                          (display " : ")
                          (display (rep->symbol type))
                          (newline)))
               (let ((cs (loop type K cs '())))
                 (hashtable-put! table T cs)
                 constraints)))
            (else
             (if debugging?
                 (begin (display T)
                        (display " = ")
                        (display (make-readable E #t))
                        (newline)))
             (if (not (null? cs))
                 (begin
                  (display "Compiler bug: ")
                  (write T)
                  (display " has unexpectedly nonempty constraints")
                  (newline)))
             (hashtable-put! table T (list (list T E K)))
             constraints)))))

; Sets of constraints.
;
; The set of constraints is represented as (<hashtable> <killedby>),
; where <hashtable> is a hashtable mapping variables to lists of
; constraints as above, and <killedby> is a vector mapping basic killers
; to lists of variables that need to be examined for constraints that
; are killed by that basic killer.

(define number-of-basic-killers
  (do ((i 0 (+ i 1))
       (k 1 (+ k k)))
      ((> k available:killer:dead)
       i)))

(define (constraints.table  constraints) (car constraints))
(define (constraints.killed constraints) (cadr constraints))

(define (make-constraints-table)
  (list (make-oldstyle-hashtable symbol-hash assq)
        (make-vector number-of-basic-killers '())))

(define (copy-constraints-table constraints)
  (list (hashtable-copy (constraints.table constraints))
        (list->vector (vector->list (constraints.killed constraints)))))

(define (constraints-for-variable constraints T)
  (hashtable-fetch (constraints.table constraints) T '()))

(define (constraints-add-killedby! constraints T K0)
  (if (not (zero? K0))
      (let ((v (constraints.killed constraints)))
        (do ((i 0 (+ i 1))
             (k 1 (+ k k)))
            ((= i number-of-basic-killers))
            (if (not (zero? (fxlogand k K0)))
                (vector-set! v i (cons T (vector-ref v i))))))))

(define (constraints-kill! constraints K)
  (if (not (zero? K))
      (let ((table (constraints.table constraints))
            (killed (constraints.killed constraints)))
        (define (examine! T)
          (let ((cs (filter (lambda (c)
                              (zero? (fxlogand (constraint.killer c) K)))
                            (hashtable-fetch table T '()))))
            (if (null? cs)
                (hashtable-remove! table T)
                (hashtable-put! table T cs))))
        (do ((i 0 (+ i 1))
             (j 1 (+ j j)))
            ((= i number-of-basic-killers))
            (if (not (zero? (fxlogand j K)))
                (begin (for-each examine! (vector-ref killed i))
                       (vector-set! killed i '())))))))

(define (constraints-intersect! constraints0 constraints1 constraints2)
  (let ((table0 (constraints.table constraints0))
        (table1 (constraints.table constraints1))
        (table2 (constraints.table constraints2)))
    (if (eq? table0 table1)
        ; FIXME:  Which is more efficient: to update the killed vector,
        ; or not to update it?  Both are safe.
        (hashtable-for-each (lambda (T cs)
                              (if (not (null? cs))
                                  (hashtable-put!
                                   table0
                                   T
                                   (cs-intersect
                                    (hashtable-fetch table2 T '())
                                    cs))))
                            table1)
        ; This case shouldn't ever happen, so it can be slow.
        (begin
         (constraints-intersect! constraints0 constraints0 constraints1)
         (constraints-intersect! constraints0 constraints0 constraints2)))))

(define (cs-intersect cs1 cs2)
  (define (loop cs init rep Krep)
    (if (null? cs)
        (values init rep Krep)
        (let* ((c (car cs))
               (cs (cdr cs))
               (E2 (constraint.rhs c))
               (K2 (constraint.killer c)))
          (cond ((constant? E2)
                 (loop cs
                       init
                       (representation-intersection rep (constant.value E2))
                       (available:killer-combine Krep K2)))
                ((call? E2)
                 (if init
                     (begin (display "Compiler bug in cs-intersect")
                            (larceny-break))
                     (loop cs c rep Krep)))
                (else
                 (error "Compiler bug in cs-intersect"))))))
  (call-with-values
   (lambda ()
     (loop cs1 #f rep:object available:killer:none))
   (lambda (c1 rep1 Krep1)
     (call-with-values
      (lambda ()
        (loop cs2 #f rep:object available:killer:none))
      (lambda (c2 rep2 Krep2)
        (let ((c (if (equal? c1 c2) c1 #f))
              (rep (representation-union rep1 rep2))
              (Krep (available:killer-combine Krep1 Krep2)))
          (if (eq? rep rep:object)
              (if c (list c) '())
              (let ((T (constraint.lhs (car cs1))))
                (if c
                    (list c (make-type-constraint T rep Krep))
                    (list (make-type-constraint T rep Krep)))))))))))
