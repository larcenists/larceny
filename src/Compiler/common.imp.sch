; Copyright 1991 William Clinger
;
; $Id$
;
; Relatively target-independent information for Twobit's backend.
;
; Most of the definitions in this file can be extended or overridden by
; target-specific definitions.
;
; Twobit currently assumes the following hidden, specialized,
; or trusted primops, which every target should support:
;
; .check!         (aka name:CHECK!)
; .list           (aka name:LIST)
; .cons           (aka name:CONS)
; .car            (aka name:CAR)
; .cdr            (aka name:CDR)
; .make-cell      (aka name:MAKE-CELL)
; .cell-ref       (aka name:CELL-REF)
; .cell-set!      (aka name:CELL-SET!)
; .fixnum?        (aka name:FIXNUM?)
; .char?          (aka name:CHAR?)
; .symbol?        (aka name:SYMBOL?)
; 
; .=:fix:fix
; .<:fix:fix
; .<=:fix:fix     (might not be necessary)
; .>:fix:fix      (might not be necessary)
; .>=:fix:fix
; 
; .--             (might not be necessary)
; .+:idx:idx
; .-:idx:idx
;
; .fxlognot
; .fxlogand
; .fxlogior
; .fxlogxor
; .fxlsh
; .fxrsha
; .fxrshl
; 
; .integer->char:trusted
; .char->integer:chr
; 
; .car:pair
; .cdr:pair
; 
; .string-length:str
; .string-ref:trusted
; .string-set!:trusted
; 
; .ustring-length:str   (deprecated, might not be necessary)
; .ustring-ref:trusted  (deprecated, might not be necessary)
; .ustring-set!:trusted (deprecated, might not be necessary)
; 
; .vector-length:vec
; .vector-ref:trusted
; .vector-set!:trusted
; .vector-set!:trusted:nwb

(define twobit-sort
  (lambda (less? list) (compat:sort list less?)))

;; A hook for backends to override if they have a register preference
;; Produces #t only if r1 preferred over r2 (otherwise inconclusive).

(define (arch-prefers-reg? r1 r2) #f)

(define renaming-prefix ".")

; The prefix used for cells introduced by the compiler.

(define cell-prefix (string-append renaming-prefix "CELL:"))

; Names of global procedures that cannot be redefined or assigned
; by ordinary code.
; The expansion of quasiquote uses .cons and .list directly, so these
; should not be changed willy-nilly.
; Others may be used directly by a DEFINE-INLINE.

(define name:CHECK!  '.check!)
(define name:LIST '.list)
(define name:CONS '.cons)
(define name:CAR '.car)
(define name:CDR '.cdr)
(define name:MAKE-CELL '.make-cell)
(define name:CELL-REF '.cell-ref)
(define name:CELL-SET! '.cell-set!)
(define name:CALL '.call)
(define name:IGNORED (string->symbol "IGNORED"))

;(begin (eval `(define ,name:CONS cons))
;       (eval `(define ,name:LIST list))
;       (eval `(define ,name:MAKE-CELL list))
;       (eval `(define ,name:CELL-REF car))
;       (eval `(define ,name:CELL-SET! set-car!)))

; If (INTEGRATE-PROCEDURES) is anything but null, then control optimization
; recognizes calls to these procedures.

(define name:NOT 'not)
(define name:MEMQ 'memq)
(define name:MEMV 'memv)

; If (INTEGRATE-PROCEDURES) is anything but null, then control optimization
; recognizes calls to these procedures and also creates calls to them.

(define name:EQ? 'eq?)
(define name:EQV? 'eqv?)

; Control optimization creates calls to these procedures,
; which do not need to check their arguments.

(define name:FIXNUM?       '.fixnum?)
(define name:CHAR?         '.char?)
(define name:SYMBOL?       '.symbol?)
(define name:FX<           '.<:fix:fix)
(define name:FX+           '.+:idx:idx)
(define name:FX-           '.-:idx:idx)
(define name:CHAR->INTEGER '.char->integer:chr)
(define name:VECTOR-REF    '.vector-ref:trusted)

; FIXME: should be fixnum-and:trusted, but that isn't implemented yet

(define name:FIXNUM-AND    'fxlogand)

; FIXME: should be fixnum-arithmetic-shift-left:trusted

(define name:FIXNUM-ARITHMETIC-SHIFT-LEFT 'fxlsh)

; If (INTEGRATE-PROCEDURES) is anything but null, then special optimization
; recognizes calls to these procedures and also creates calls to them.

(define name:VALUES           'values)
(define name:CALL-WITH-VALUES 'call-with-values)
(define op:UNSPECIFIED        'unspecified)
(define op:FIXNUM?            'fixnum?)


; Constant folding.
; Prototype, will probably change in the future.

(define (constant-folding-entry name)
  (if (eq? (integrate-procedures) 'none)
      (assq name $minimal-constant-folding-procedures$)
      (assq name $usual-constant-folding-procedures$)))

(define constant-folding-predicates cadr)
(define constant-folding-folder caddr)

; FIXME: This table should hold more of the procedures that
; Twobit inserts prior to constant folding.

(define $minimal-constant-folding-procedures$
  (let ((always? (lambda (x) #t))
        (smallint? (lambda (n) (smallint? n)))
        (ratnum? (lambda (n)
                   (and (number? n)
                        (exact? n)
                        (rational? n)))))
    `(
      ; This makes some assumptions about the host system,
      ; notably that its char->integer procedure is compatible.
      
      (.fixnum? (,smallint?) ,smallint?)
      (.char? (,always?) ,char?)
      (.symbol? (,always?) ,symbol?)

      (.+:idx:idx  (,smallint? ,smallint?) ,+)
      (.-:idx:idx  (,smallint? ,smallint?) ,-)

      (.=:fix:fix  (,smallint? ,smallint?) ,=)
      (.<:fix:fix  (,smallint? ,smallint?) ,<)
      (.<=:fix:fix (,smallint? ,smallint?) ,<=)
      (.>:fix:fix  (,smallint? ,smallint?) ,>)
      (.>=:fix:fix (,smallint? ,smallint?) ,>=)

      (.-- (,ratnum?) ,(lambda (x) (- 0 x)))

      (.char->integer:chr (,char?) ,char->integer)

      (.car:pair   (,pair?) ,car)
      (.cdr:pair   (,pair?) ,cdr)
      )))

(define $usual-constant-folding-procedures$
  (append $minimal-constant-folding-procedures$
    (let ((always? (lambda (x) #t))
          (charcode? (lambda (n)
                       (and (number? n)
                            (exact? n)
                            (<= 0 n)
                            (< n 128))))
          (ratnum? (lambda (n)
                     (and (number? n)
                          (exact? n)
                          (rational? n))))
          ; smallint? is defined later.
          (smallint? (lambda (n) (smallint? n))))
      `(
        ; This makes some assumptions about the host system.
        
        (integer->char (,charcode?) ,integer->char)
        (char->integer (,char?) ,char->integer)
        (zero? (,ratnum?) ,zero?)
        (< (,ratnum? ,ratnum?) ,<)
        (<= (,ratnum? ,ratnum?) ,<=)
        (= (,ratnum? ,ratnum?) ,=)
        (>= (,ratnum? ,ratnum?) ,>=)
        (> (,ratnum? ,ratnum?) ,>)
        (+ (,ratnum? ,ratnum?) ,+)
        (- (,ratnum? ,ratnum?) ,-)
        (* (,ratnum? ,ratnum?) ,*)
        (eq? (,always? ,always?) ,eq?)
        (eqv? (,always? ,always?) ,eqv?)
        (equal? (,always? ,always?) ,equal?)
        (memq (,always? ,list?) ,memq)
        (memv (,always? ,list?) ,memv)
        (member (,always? ,list?) ,member)
        (assq (,always? ,list?) ,assq)
        (assv (,always? ,list?) ,assv)
        (assoc (,always? ,list?) ,assoc)
        (length (,list?) ,length)
        (-- (,ratnum?) ,(lambda (x) (- 0 x)))   ; FIXME: Larceny-specific
        (fixnum? (,smallint?) ,smallint?)       ; FIXME: Larceny-specific
        ))))

; Compiler macros.
;
; Order matters.  If f and g are both inlined, and the definition of g
; uses f, then f should be defined before g.

; For now there's only one inline environment, though there might be
; others later.
;
; Consequences:  A compiler macro can assume that all inlined R4RS
; procedures have their usual values, but cannot assume that non-R4RS
; procedures are intact.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FIXME:  This part is obsolete.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'
(define inline-syntactic-environment
  (syntactic-copy (the-usual-syntactic-environment)))

'
(define non-inline-names
  (syntactic-environment-names (the-usual-syntactic-environment)))

; This is contorted, for the following reason: it's important not to
; have standard macros be in the inline environment that's presented to
; the compiler, because the syntactic environment that is in the
; environment that the compiler is operating under may not have those
; standard macros present.  Yet, I believe that the inline procedures
; must be defined in an environment where the standard macros are
; present.  So what I do here is construct a smaller environment,
; containing only inline definitions, to be used by the compiler.
;
; When the .CALL macro is implemented much of this cruft will probably
; go away, because inlines will be defined using a different mechanism.

; The definition of COMPILER-MACROS has been commented out.

'
(define (compiler-macros)
  (let ((names (if (eq? (integrate-procedures) 'none)
                   '()
                   (difference (syntactic-environment-names 
                                inline-syntactic-environment)
                               non-inline-names))))
    (syntactic-extend (make-minimal-syntactic-environment)
                      names
                      (map (lambda (n)
                             (syntactic-lookup inline-syntactic-environment n))
                           names))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End of obsolete code.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The list of compiler macros has been rewritten to avoid the
; use of quasiquote on large structures.  Larceny's quasiquote
; apparently takes quadratic time, so this rewrite improved
; Larceny's build times.

(define common-compiler-macros
  (list

'
(define-syntax .rewrite-eqv?
  (transformer
   (lambda (exp rename compare)
     (let ((exp (cadr exp)))
       (if (= (length exp) 3)
           (let ((arg1 (cadr exp))
                 (arg2 (caddr exp)))
             (define (constant? exp)
               (or (boolean? exp)
                   (char? exp)
                   (and (pair? exp)
                        (= (length exp) 2)
                        (identifier? (car exp))
                        (compare (car exp) (rename 'quote))
                        (symbol? (cadr exp)))))
             (if (or (constant? arg1)
                     (constant? arg2))
                 (cons (rename 'eq?) (cdr exp))
                 exp))
           exp)))))

'
(define-syntax .rewrite-memv
  (transformer
   (lambda (exp rename compare)
     (let ((exp (cadr exp)))
       (if (= (length exp) 3)
           (let ((arg1 (cadr exp))
                 (arg2 (caddr exp)))
             (if (or (boolean? arg1)
                     (fixnum? arg1)
                     (char? arg1)
                     (and (pair? arg1)
                          (= (length arg1) 2)
                          (identifier? (car arg1))
                          (compare (car arg1) (rename 'quote))
                          (symbol? (cadr arg1)))
                     (and (pair? arg2)
                          (= (length arg2) 2)
                          (identifier? (car arg2))
                          (compare (car arg2) (rename 'quote))
                          (every1? (lambda (x)
                                     (or (boolean? x)
                                         (fixnum? x)
                                         (char? x)
                                         (symbol? x)))
                                   (cadr arg2))))
                 (cons (rename 'memq) (cdr exp))
                 exp))
           exp)))))

'
(define-syntax .rewrite-assv
  (transformer
   (lambda (exp rename compare)
     (let ((exp (cadr exp)))
       (if (= (length exp) 3)
           (let ((arg1 (cadr exp))
                 (arg2 (caddr exp)))
             (if (or (boolean? arg1)
                     (char? arg1)
                     (and (pair? arg1)
                          (= (length arg1) 2)
                          (identifier? (car arg1))
                          (compare (car arg1) (rename 'quote))
                          (symbol? (cadr arg1)))
                     (and (pair? arg2)
                          (= (length arg2) 2)
                          (identifier? (car arg2))
                          (compare (car arg2) (rename 'quote))
                          (every1? (lambda (y)
                                     (and (pair? y)
                                          (let ((x (car y)))
                                            (or (boolean? x)
                                                (char? x)
                                                (symbol? x)))))
                                   (cadr arg2))))
                 (cons (rename 'assq) (cdr exp))
                 exp))
           exp)))))

(list 'define-syntax name:CALL
      (list 'syntax-rules
            '   (r4rs r5rs larceny quote lambda
                 boolean?
                 car cdr
                 vector-length vector-ref vector-set!
                 string-length string-ref string-set!
                 make-ustring ustring-length ustring-ref ustring-set!
                 list vector
                 cadddr cddddr cdddr caddr cddr cdar cadr caar
                 make-vector make-bytevector make-string
                 endianness big little
                 bytevector-u8-ref bytevector-u8-set!
                 = < > <= >= + * - /
                 abs negative? positive? min max
                 fx=? fx<? fx>? fx<=? fx>=?
                 fxzero? fxpositive? fxnegative?
                 fxmin fxmax
                 fx+ fx- fx*
                 fxnot fxand fxior fxxor fxif
                 fxeven? fxodd?
                 fl=? fl<? fl>? fl<=? fl>=?
                 flzero? flpositive? flnegative?
                 flmin flmax
                 fl+ fl- fl* fl/
                 eqv? memv assv memq
                 map for-each
                 lookahead-u8 get-u8
                 lookahead-char get-char put-char
                 peek-char read-char write-char
                 )

   ; FIXME: Eliminating these next two should fix ticket #37.

;  ((,name:CALL r4rs ?proc ?exp)
;   (,name:CALL r5rs ?proc ?exp))

;  ((,name:CALL r5rs ?proc ?exp)
;   (,name:CALL larceny ?proc ?exp))

   ; FIXME

;`  ((_ quote integer->char (integer->char k0))
;    (let ((k k0))
;      (.check! (.fixnum? k) ,$ex.int2char k)
;      (.check! (.<:fix:fix k #x110000) ,$ex.int2char k)
;      (.check! (.>=:fix:fix k 0) ,$ex.int2char k)
;      (.check! (not (.=:fix:fix #x0000d800 (logand #x00fff800 k))))
;      (.integer->char:trusted k)))
   
`  ((_ larceny boolean? (boolean? ?x))
    (let ((x ?x)) (or (eq? x #t) (eq? x #f))))

`  ((_ larceny car (car x0))
    (let ((x x0))
      (.check! (pair? x) ,$ex.car x)
      (.car:pair x)))
   
`  ((_ larceny cdr (cdr x0))
    (let ((x x0))
      (.check! (pair? x) ,$ex.cdr x)
      (.cdr:pair x)))

`  ((_ larceny vector-length (vector-length v0))
    (let ((v v0))
      (.check! (vector? v) ,$ex.vlen v)
      (.vector-length:vec v)))
   
`  ((_ larceny vector-ref (vector-ref v0 i0))
    (let ((v v0)
          (i i0))
      (.check! (.fixnum? i) ,$ex.vref v i)
      (.check! (vector? v) ,$ex.vref v i)
      (.check! (.<:fix:fix i (.vector-length:vec v)) ,$ex.vref v i)
      (.check! (.>=:fix:fix i 0) ,$ex.vref  v i)
      (.vector-ref:trusted v i)))
   
`  ((_ larceny vector-set! (vector-set! v0 i0 x0))
    (let ((v v0)
          (i i0)
          (x x0))
      (.check! (.fixnum? i) ,$ex.vset v i x)
      (.check! (vector? v) ,$ex.vset v i x)
      (.check! (.<:fix:fix i (.vector-length:vec v)) ,$ex.vset v i x)
      (.check! (.>=:fix:fix i 0) ,$ex.vset v i x)
      (.vector-set!:trusted v i x)))
   
`  ((_ larceny string-length (string-length v0))
    (let ((v v0))
      (.check! (string? v) ,$ex.slen v)
      (.string-length:str v)))
   
`  ((_ larceny string-ref (string-ref v0 i0))
    (let ((v v0)
          (i i0))
      (.check! (.fixnum? i) ,$ex.sref v i)
      (.check! (string? v) ,$ex.sref v i)
      (.check! (.<:fix:fix i (.string-length:str v)) ,$ex.sref v i)
      (.check! (.>=:fix:fix i 0) ,$ex.sref  v i)
      (.string-ref:trusted v i)))
   
`  ((_ larceny string-set! (string-set! v0 i0 x0))
    (let ((v v0)
          (i i0)
          (x x0))
      (.check! (.fixnum? i) ,$ex.sset v i x)
      (.check! (string? v) ,$ex.sset v i x)
      (.check! (.<:fix:fix i (.string-length:str v)) ,$ex.sset v i x)
      (.check! (.>=:fix:fix i 0) ,$ex.sset v i x)
      (.check! (.char? x) ,$ex.sset v i x)
      (.string-set!:trusted v i x)))
   
`  ((_ larceny make-ustring (make-ustring ?n))
    (make-ustring ?n #\space))

`  ((_ larceny ustring-length (ustring-length v0))
    (let ((v v0))
      (.check! (ustring? v) ,$ex.slen v)
      (.ustring-length:str v)))
   
`  ((_ larceny ustring-ref (ustring-ref v0 i0))
    (let ((v v0)
          (i i0))
      (.check! (.fixnum? i) ,$ex.sref v i)
      (.check! (ustring? v) ,$ex.sref v i)
      (.check! (.<:fix:fix i (.ustring-length:str v)) ,$ex.sref v i)
      (.check! (.>=:fix:fix i 0) ,$ex.sref  v i)
      (.ustring-ref:trusted v i)))
   
`  ((_ larceny ustring-set! (ustring-set! v0 i0 x0))
    (let ((v v0)
          (i i0)
          (x x0))
      (.check! (.fixnum? i) ,$ex.sset v i x)
      (.check! (ustring? v) ,$ex.sset v i x)
      (.check! (.<:fix:fix i (.ustring-length:str v)) ,$ex.sset v i x)
      (.check! (.>=:fix:fix i 0) ,$ex.sset v i x)
      (.check! (.char? x) ,$ex.sset v i x)
      (.ustring-set!:trusted v i x)))
   
; This transformation must make sure the entire list is freshly
; allocated when an argument to LIST returns more than once.

`  ((_ larceny list (list))
    '())
`  ((_ larceny list (list ?e))
    (cons ?e '()))
`  ((_ larceny list (list ?e1 ?e2 ...))
    (let* ((t1 ?e1)
           (t2 (list ?e2 ...)))
      (cons t1 t2)))

; This transformation must make sure the entire vector is freshly
; allocated when an argument to VECTOR returns more than once.

`  ((_ larceny vector (vector))
    '#())
`  ((_ larceny vector (vector ?e))
    (make-vector 1 ?e))
`  ((_ larceny vector (vector ?e1 ?e2 ...))
    (letrec-syntax
      ((vector-aux1
        (... (syntax-rules ()
              ((vector-aux1 () ?n ?exps ?indexes ?temps)
               (vector-aux2 ?n ?exps ?indexes ?temps))
              ((vector-aux1 (?exp1 ?exp2 ...) ?n ?exps ?indexes ?temps)
               (vector-aux1 (?exp2 ...)
                            (+ ?n 1)
                            (?exp1 . ?exps)
                            (?n . ?indexes)
                            (t . ?temps))))))
       (vector-aux2
        (... (syntax-rules ()
              ((vector-aux2 ?n (?exp1 ?exp2 ...) (?n1 ?n2 ...) (?t1 ?t2 ...))
               (let* ((?t1 ?exp1)
                      (?t2 ?exp2)
                      ...
                      (v (make-vector ?n ?t1)))
                 (.vector-set!:trusted v ?n2 ?t2)
                 ...
                 v))))))
      (vector-aux1 (?e1 ?e2 ...) 0 () () ())))

`  ((_ larceny cadddr (cadddr ?e))
    (car (cdr (cdr (cdr ?e)))))

`  ((_ larceny cddddr (cddddr ?e))
    (cdr (cdr (cdr (cdr ?e)))))

`  ((_ larceny cdddr (cdddr ?e))
    (cdr (cdr (cdr ?e))))

`  ((_ larceny caddr (caddr ?e))
    (car (cdr (cdr ?e))))

`  ((_ larceny cddr (cddr ?e))
    (cdr (cdr ?e)))

`  ((_ larceny cdar (cdar ?e))
    (cdr (car ?e)))

`  ((_ larceny cadr (cadr ?e))
    (car (cdr ?e)))

`  ((_ larceny caar (caar ?e))
    (car (car ?e)))

`  ((_ larceny make-vector (make-vector ?n))
    (make-vector ?n '()))

`  ((_ larceny make-bytevector (make-bytevector ?n ?fill))
    (let ((bv (make-bytevector ?n)))
      (bytevector-fill! bv ?fill)
      bv))

`  ((_ larceny make-string (make-string ?n))
    (make-string ?n #\space))

`  ((_ larceny bytevector-u8-ref (bytevector-u8-ref x y))
    (bytevector-ref x y))

`  ((_ larceny bytevector-u8-set! (bytevector-u8-set! x y z))
    (bytevector-set! x y z))

   ; FIXME: These are incorrect because ?e3 ... might not return.

`  ((_ larceny = (= ?e1 ?e2 ?e3 ?e4 ...))
    (let* ((t1 ?e1)
           (t2 ?e2)
           (t3 (= t2 ?e3 ?e4 ...)))
      (if (= t1 t2) t3 #f)))

`  ((_ larceny < (< ?e1 ?e2 ?e3 ?e4 ...))
    (let* ((t1 ?e1)
           (t2 ?e2)
           (t3 (< t2 ?e3 ?e4 ...)))
      (if (< t1 t2) t3 #f)))

`  ((_ larceny > (> ?e1 ?e2 ?e3 ?e4 ...))
    (let* ((t1 ?e1)
           (t2 ?e2)
           (t3 (> t2 ?e3 ?e4 ...)))
      (if (> t1 t2) t3 #f)))

`  ((_ larceny <= (<= ?e1 ?e2 ?e3 ?e4 ...))
    (let* ((t1 ?e1)
           (t2 ?e2)
           (t3 (<= t2 ?e3 ?e4 ...)))
      (if (<= t1 t2) t3 #f)))

`  ((_ larceny >= (>= ?e1 ?e2 ?e3 ?e4 ...))
    (let* ((t1 ?e1)
           (t2 ?e2)
           (t3 (>= t2 ?e3 ?e4 ...)))
      (if (>= t1 t2) t3 #f)))

`  ((_ larceny + (+))
    0)
`  ((_ larceny + (+ ?e))
    (+ ?e 0))
`  ((_ larceny + (+ ?e1 ?e2 ?e3 ?e4 ...))
    (let* ((t1 ?e1)
           (t2 (+ ?e2 ?e3 ?e4 ...)))
      (+ t1 t2)))

`  ((_ larceny * (*))
    1)
`  ((_ larceny * (* ?e))
    (+ ?e 0))
`  ((_ larceny * (* ?e1 ?e2 ?e3 ?e4 ...))
    (let* ((t1 ?e1)
           (t2 (* ?e2 ?e3 ?e4 ...)))
      (* t1 t2)))

`  ((_ larceny - (- ?e))
    (- 0 ?e))
`  ((_ larceny - (- ?e1 ?e2 ?e3 ?e4 ...))
    (let* ((t1 ?e1)
           (t2 (+ ?e2 ?e3 ?e4 ...)))
      (- t1 t2)))

`  ((_ larceny / (/ ?e))
    (/ 1 ?e))
`  ((_ larceny / (/ ?e1 ?e2 ?e3 ?e4 ...))
    (let* ((t1 ?e1)
           (t2 (* ?e2 ?e3 ?e4 ...)))
      (/ t1 t2)))

`  ((_ larceny abs (abs ?z))
    (let ((temp ?z))
      (if (< temp 0)
          (.-- temp)
          temp)))

`  ((_ larceny negative? (negative? ?x))
    (< ?x 0))

`  ((_ larceny positive? (positive? ?x))
    (> ?x 0))

`  ((_ larceny min (min ?x ?y))
    (let ((x ?x) (y ?y))
      (let ((r (if (<= x y) x y)))
        (if (or (inexact? x) (inexact? y))
            (+ r 0.0)
            r))))
`  ((_ larceny min (min ?x ?y ?z ...))
    (let ((x ?x) (y (min ?y ?z ...)))
      (min x y)))

`  ((_ larceny max (max ?x ?y))
    (let ((x ?x) (y ?y))
      (let ((r (if (>= x y) x y)))
        (if (or (inexact? x) (inexact? y))
            (+ r 0.0)
            r))))
`  ((_ larceny max (max ?x ?y ?z ...))
    (let ((x ?x) (y (max ?y ?z ...)))
      (max x y)))

   ; Special cases for two or three arguments.

`  ((_ larceny fx=? (fx=? ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (.fixnum? x) ,$ex.fx= x)
      (.check! (.fixnum? y) ,$ex.fx= y)
      (.=:fix:fix x y)))
`  ((_ larceny fx=? (fx=? ?x ?y ?z))
    (let* ((x ?x)
           (y ?y)
           (z ?z))
      (and (fx=? x y) (fx=? y z))))

`  ((_ larceny fx<? (fx<? ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (.fixnum? x) ,$ex.fx< x)
      (.check! (.fixnum? y) ,$ex.fx< y)
      (.<:fix:fix x y)))
`  ((_ larceny fx<? (fx<? ?x ?y ?z))
    (let* ((x ?x)
           (y ?y)
           (z ?z))
      (and (fx<? x y) (fx<? y z))))

`  ((_ larceny fx>? (fx>? ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (.fixnum? x) ,$ex.fx> x)
      (.check! (.fixnum? y) ,$ex.fx> y)
      (.>:fix:fix x y)))
`  ((_ larceny fx>? (fx>? ?x ?y ?z))
    (let* ((x ?x)
           (y ?y)
           (z ?z))
      (and (fx>? x y) (fx>? y z))))

`  ((_ larceny fx<=? (fx<=? ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (.fixnum? x) ,$ex.fx<= x)
      (.check! (.fixnum? y) ,$ex.fx<= y)
      (.<=:fix:fix x y)))
`  ((_ larceny fx<=? (fx<=? ?x ?y ?z))
    (let* ((x ?x)
           (y ?y)
           (z ?z))
      (and (fx<=? x y) (fx<=? y z))))

`  ((_ larceny fx>=? (fx>=? ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (.fixnum? x) ,$ex.fx>= x)
      (.check! (.fixnum? y) ,$ex.fx>= y)
      (.>=:fix:fix x y)))
`  ((_ larceny fx>=? (fx>=? ?x ?y ?z))
    (let* ((x ?x)
           (y ?y)
           (z ?z))
      (and (fx>=? x y) (fx>=? y z))))

`  ((_ larceny fxzero? (fxzero? ?x))
    (fx=? ?x 0))

`  ((_ larceny fxpositive? (fxpositive? ?x))
    (fx>? ?x 0))

`  ((_ larceny fxnegative? (fxnegative? ?x))
    (fx<? ?x 0))

   ; Special cases for two or three arguments.

`  ((_ larceny fxmin (fxmin ?x ?y))
    (let ((x ?x)
          (y ?y))
      (if (fx<=? x y) x y)))
`  ((_ larceny fxmin (fxmin ?x ?y ?z))
    (let ((x ?x)
          (y ?y)
          (z ?z))
      (fxmin (if (fx<=? x y) x y) z)))

`  ((_ larceny fxmax (fxmax ?x ?y))
    (let ((x ?x)
          (y ?y))
      (if (fx>=? x y) x y)))
`  ((_ larceny fxmax (fxmax ?x ?y ?z))
    (let ((x ?x)
          (y ?y)
          (z ?z))
      (fxmax (if (fx>=? x y) x y) z)))

   ; These procedures accept only two arguments.

`  ((_ larceny fx+ (fx+ ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (fixnum? x) ,$ex.fx+ x y)
      (.check! (fixnum? y) ,$ex.fx+ x y)
      (let ((z (+ x y)))
        (.check! (fixnum? z) ,$ex.fx+ x y)
        z)))

`  ((_ larceny fx* (fx* ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (fixnum? x) ,$ex.fx* x y)
      (.check! (fixnum? y) ,$ex.fx* x y)
      (let ((z (* x y)))
        (.check! (fixnum? z) ,$ex.fx* x y)
        z)))

`  ((_ larceny fx- (fx- ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (fixnum? x) ,$ex.fx- x y)
      (.check! (fixnum? y) ,$ex.fx- x y)
      (let ((z (- x y)))
        (.check! (fixnum? z) ,$ex.fx- x y)
        z)))

`  ((_ larceny fx- (fx- ?x))
    (let ((x ?x))
      (.check! (fixnum? x) ,$ex.fx-- x)
      (let ((z (- 0 x)))
        (.check! (fixnum? z) ,$ex.fx-- x)
        z)))

`  ((_ larceny fxnot (fxnot ?x))
    (.fxlognot ?x))

`  ((_ larceny fxand (fxand ?x ?y))
    (.fxlogand ?x ?y))
`  ((_ larceny fxand (fxand ?x ?y ?z ...))
    (let* ((x ?x)
           (y (fxand ?y ?z ...)))
      (.fxlogand x y)))

`  ((_ larceny fxior (fxior ?x ?y))
    (.fxlogior ?x ?y))
`  ((_ larceny fxior (fxior ?x ?y ?z ...))
    (let* ((x ?x)
           (y (fxior ?y ?z ...)))
      (.fxlogior x y)))

`  ((_ larceny fxxor (fxxor ?x ?y))
    (.fxlogxor ?x ?y))
`  ((_ larceny fxxor (fxxor ?x ?y ?z ...))
    (let* ((x ?x)
           (y (fxxor ?y ?z ...)))
      (.fxlogxor x y)))

`  ((_ larceny fxif (fxif ?x ?y ?z))
    (let ((x ?x)
          (y ?y)
          (z ?z))
      (fxior (fxand x y)
             (fxand (fxnot x) z))))

`  ((_ larceny fxeven? (fxeven? ?x))
    (fxzero? (fxand ?x 1)))

`  ((_ larceny fxodd? (fxodd? ?x))
    (not (fxzero? (fxand ?x 1))))

   ; Special cases for two or three arguments.
   ; FIXME: should be faster.

`  ((_ larceny fl=? (fl=? ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (flonum? x) ,$ex.fl= x)
      (.check! (flonum? y) ,$ex.fl= y)
      (= x y)))
`  ((_ larceny fl=? (fl=? ?x ?y ?z))
    (let* ((x ?x)
           (y ?y)
           (z ?z))
      (and (fl=? x y) (fl=? y z))))

`  ((_ larceny fl<? (fl<? ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (flonum? x) ,$ex.fl< x)
      (.check! (flonum? y) ,$ex.fl< y)
      (< x y)))
`  ((_ larceny fl<? (fl<? ?x ?y ?z))
    (let* ((x ?x)
           (y ?y)
           (z ?z))
      (and (fl<? x y) (fl<? y z))))

`  ((_ larceny fl>? (fl>? ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (flonum? x) ,$ex.fl> x)
      (.check! (flonum? y) ,$ex.fl> y)
      (> x y)))
`  ((_ larceny fl>? (fl>? ?x ?y ?z))
    (let* ((x ?x)
           (y ?y)
           (z ?z))
      (and (fl>? x y) (fl>? y z))))

`  ((_ larceny fl<=? (fl<=? ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (flonum? x) ,$ex.fl<= x)
      (.check! (flonum? y) ,$ex.fl<= y)
      (<= x y)))
`  ((_ larceny fl<=? (fl<=? ?x ?y ?z))
    (let* ((x ?x)
           (y ?y)
           (z ?z))
      (and (fl<=? x y) (fl<=? y z))))

`  ((_ larceny fl>=? (fl>=? ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (flonum? x) ,$ex.fl>= x)
      (.check! (flonum? y) ,$ex.fl>= y)
      (>= x y)))
`  ((_ larceny fl>=? (fl>=? ?x ?y ?z))
    (let* ((x ?x)
           (y ?y)
           (z ?z))
      (and (fl>=? x y) (fl>=? y z))))

`  ((_ larceny flzero? (flzero? ?x))
    (fl=? ?x 0.0))

`  ((_ larceny flpositive? (flpositive? ?x))
    (fl>? ?x 0.0))

`  ((_ larceny flnegative? (flnegative? ?x))
    (fl<? ?x 0.0))

   ; Special cases for two or three arguments.

`  ((_ larceny flmin (flmin ?x ?y))
    (let ((x ?x)
          (y ?y))
      (if (fl<=? x y) x y)))
`  ((_ larceny flmin (flmin ?x ?y ?z))
    (let ((x ?x)
          (y ?y)
          (z ?z))
      (flmin (if (fl<=? x y) x y) z)))

`  ((_ larceny flmax (flmax ?x ?y))
    (let ((x ?x)
          (y ?y))
      (if (fl>=? x y) x y)))
`  ((_ larceny flmax (flmax ?x ?y ?z))
    (let ((x ?x)
          (y ?y)
          (z ?z))
      (flmax (if (fl>=? x y) x y) z)))

   ; Special cases for two, three, or more arguments.

`  ((_ larceny fl+ (fl+))
    0.0)
`  ((_ larceny fl+ (fl+ ?x))
    (fl+ ?x 0.0))
`  ((_ larceny fl+ (fl+ ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (flonum? x) ,$ex.fl+ x y)
      (.check! (flonum? y) ,$ex.fl+ x y)
      (+ x y)))
`  ((_ larceny fl+ (fl+ ?x ?y ?z ...))
    (let* ((x ?x) (y ?y) (z (fl+ ?z ...)))
      (fl+ x (fl+ y z))))

`  ((_ larceny fl* (fl*))
    1.0)
`  ((_ larceny fl* (fl* ?x))
    (fl* ?x 1.0))
`  ((_ larceny fl* (fl* ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (flonum? x) ,$ex.fl* x y)
      (.check! (flonum? y) ,$ex.fl* x y)
      (* x y)))
`  ((_ larceny fl* (fl* ?x ?y ?z ...))
    (let* ((x ?x) (y ?y) (z (fl* ?z ...)))
      (fl* x (fl* y z))))

`  ((_ larceny fl- (fl- ?x))
    (let ((x ?x))
      (.check! (flonum? x) ,$ex.fl-- x)
      (- 0.0 x)))
`  ((_ larceny fl- (fl- ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (flonum? x) ,$ex.fl- x y)
      (.check! (flonum? y) ,$ex.fl- x y)
      (- x y)))
`  ((_ larceny fl- (fl- ?x ?y ?z ...))
    (let* ((x ?x) (y ?y) (z (fl+ ?z ...)))
      (fl- x (fl+ y z))))

`  ((_ larceny fl/ (fl/ ?x))
    (let ((x ?x))
      (.check! (flonum? x) ,$ex.fl/ x)
      (/ 1.0 x)))
`  ((_ larceny fl/ (fl/ ?x ?y))
    (let ((x ?x)
          (y ?y))
      (.check! (flonum? x) ,$ex.fl/ x y)
      (.check! (flonum? y) ,$ex.fl/ x y)
      (/ x y)))
`  ((_ larceny fl/ (fl/ ?x ?y ?z ...))
    (let* ((x ?x) (y ?y) (z (fl* ?z ...)))
      (fl/ x (fl* y z))))

   ; These three compiler macros cannot be expressed using SYNTAX-RULES.

;   ((_ larceny eqv? exp)
;    (.rewrite-eqv? exp))

;   ((_ larceny memv exp)
;    (.rewrite-memv exp))

;   ((_ larceny assv exp)
;    (.rewrite-assv exp))

`  ((_ larceny memq (memq ?expr '(?datum ...)))
    (letrec-syntax
      ((memq0
        (... (syntax-rules (quote)
              ((memq0 '?xx '(?d ...))
               (let ((t1 '(?d ...)))
                 (memq1 '?xx t1 (?d ...))))
              ((memq0 ?e '(?d ...))
               (let ((t0 ?e)
                     (t1 '(?d ...)))
                 (memq1 t0 t1 (?d ...)))))))
       (memq1
        (... (syntax-rules ()
              ((memq1 ?t0 ?t1 ())
               #f)
              ((memq1 ?t0 ?t1 (?d1 ?d2 ...))
               (if (eq? ?t0 '?d1)
                   ?t1
                   (let ((?t1 (cdr ?t1)))
                     (memq1 ?t0 ?t1 (?d2 ...)))))))))
      (memq0 ?expr '(?datum ...))))

`  ((_ larceny map (map ?proc ?exp1 ?exp2 ...))
    (letrec-syntax
      ((loop
        (... (syntax-rules (lambda)
              ((loop 1 () (?y1 ?y2 ...) ?f ?exprs)
               (loop 2 (?y1 ?y2 ...) ?f ?exprs))
              ((loop 1 (?a1 ?a2 ...) (?y2 ...) ?f ?exprs)
               (loop 1 (?a2 ...) (y1 ?y2 ...) ?f ?exprs))
              
              ((loop 2 ?ys (lambda ?formals ?body) ?exprs)
               (loop 3 ?ys (lambda ?formals ?body) ?exprs))
              ((loop 2 ?ys (?f1 . ?f2) ?exprs)
               (let ((f (?f1 . ?f2)))
                 (loop 3 ?ys f ?exprs)))
              ; ?f must be a constant or variable.
              ((loop 2 ?ys ?f ?exprs)
               (loop 3 ?ys ?f ?exprs))
              
              ((loop 3 (?y1 ?y2 ...) ?f (?e1 ?e2 ...))
               (do ((?y1 ?e1 (cdr ?y1))
                    (?y2 ?e2 (cdr ?y2))
                    ...
                    (results '() (cons (?f (car ?y1) (car ?y2) ...)
                                       results)))
                   ((or (null? ?y1) (null? ?y2) ...)
                    (reverse results))))))))
      
      (loop 1 (?exp1 ?exp2 ...) () ?proc (?exp1 ?exp2 ...))))

`  ((_ larceny for-each (for-each ?proc ?exp1 ?exp2 ...))
    (letrec-syntax
      ((loop
        (... (syntax-rules (lambda)
              ((loop 1 () (?y1 ?y2 ...) ?f ?exprs)
               (loop 2 (?y1 ?y2 ...) ?f ?exprs))
              ((loop 1 (?a1 ?a2 ...) (?y2 ...) ?f ?exprs)
               (loop 1 (?a2 ...) (y1 ?y2 ...) ?f ?exprs))
              
              ((loop 2 ?ys (lambda ?formals ?body) ?exprs)
               (loop 3 ?ys (lambda ?formals ?body) ?exprs))
              ((loop 2 ?ys (?f1 . ?f2) ?exprs)
               (let ((f (?f1 . ?f2)))
                 (loop 3 ?ys f ?exprs)))
              ; ?f must be a constant or variable.
              ((loop 2 ?ys ?f ?exprs)
               (loop 3 ?ys ?f ?exprs))
              
              ((loop 3 (?y1 ?y2 ...) ?f (?e1 ?e2 ...))
               (do ((?y1 ?e1 (cdr ?y1))
                    (?y2 ?e2 (cdr ?y2))
                    ...)
                   ((or (null? ?y1) (null? ?y2) ...)
                    (if #f #f))
                   (?f (car ?y1) (car ?y2) ...)))))))
      
      (loop 1 (?exp1 ?exp2 ...) () ?proc (?exp1 ?exp2 ...))))

   ; The fast path for lookahead-u8 and get-u8.
   ; FIXME:  This can be bummed further.

`  ((_ larceny lookahead-u8 (lookahead-u8 p0))
    (let ((p p0))
      (.check! (port? p) ,$ex.get-u8 p)
      (let ((type (vector-like-ref p 0))           ; 0 = port.type
            (buf  (vector-like-ref p 1))           ; 1 = port.mainbuf
            (ptr  (vector-like-ref p 2))           ; 2 = port.mainptr
            (lim  (vector-like-ref p 3)))          ; 3 = port.mainlim
        (cond ((and (eq? type 2)
                    (.<:fix:fix ptr lim))
               (bytevector-ref buf ptr))
              (else
               (io/get-u8 p #t))))))

`  ((_ larceny get-u8 (get-u8 p0))
    (let ((p p0))
      (.check! (port? p) ,$ex.get-u8 p)
      (let ((type (vector-like-ref p 0))           ; 0 = port.type
            (buf  (vector-like-ref p 1))           ; 1 = port.mainbuf
            (ptr  (vector-like-ref p 2))           ; 2 = port.mainptr
            (lim  (vector-like-ref p 3)))          ; 3 = port.mainlim
        (cond ((and (eq? type 2)
                    (.<:fix:fix ptr lim))
               (let ((byte (bytevector-ref buf ptr)))
                 (vector-like-set! p
                                   2               ; 2 = port.mainptr
                                   (.+:idx:idx ptr 1))
                 byte))
              (else
               (io/get-u8 p #f))))))

   ; The fast path for lookahead-char.
   ; FIXME:  This can be bummed further.

`  ((_ larceny lookahead-char (lookahead-char p0))
    (let ((p p0))
      (.check! (port? p) ,$ex.get-char p)
      (let ((type (.vector-ref:trusted p 0))       ; 0 = port.type
            (buf  (.vector-ref:trusted p 1))       ; 1 = port.mainbuf
            (ptr  (.vector-ref:trusted p 2)))      ; 2 = port.mainptr
        (let ((unit (if (eq? type 3)               ; 3 = input, textual
                        (bytevector-ref buf ptr)   ; FIXME: should be trusted
                        255)))
          (if (and (.<:fix:fix 13 unit)            ; 13 = #\return
                   (.<:fix:fix unit 128))
              (.integer->char:trusted unit)
              (io/get-char p #t))))))

   ; The fast path for get-char.
   ; FIXME:  This can be bummed further.

`  ((_ larceny get-char (get-char p0))
    (let ((p p0))
      (.check! (port? p) ,$ex.get-char p)
      (let ((type (.vector-ref:trusted p 0))       ; 0 = port.type
            (buf  (.vector-ref:trusted p 1))       ; 1 = port.mainbuf
            (ptr  (.vector-ref:trusted p 2)))      ; 2 = port.mainptr
        (let ((unit (if (eq? type 3)               ; 3 = input, textual
                        (bytevector-ref buf ptr)   ; FIXME: should be trusted
                        255)))

          (if (and (.<:fix:fix 13 unit)            ; 13 = #\return
                   (.<:fix:fix unit 128))          ; 2 = port.mainptr

              (begin (.vector-set!:trusted:nwb p 2 (.+:idx:idx ptr 1))
                     (.integer->char:trusted unit))

              (io/get-char p #f))))))

   ; The fast path for put-char.
   ; FIXME:  This can be bummed further.

`  ((_ larceny put-char (put-char p0 c0))
    (let ((p p0)
          (c c0))
      (.check! (port? p) ,$ex.put-char p c)
      (let ((type (.vector-ref:trusted p 0))       ; 0 = port.type
            (buf  (.vector-ref:trusted p 1))       ; 1 = port.mainbuf
            (lim  (.vector-ref:trusted p 3))       ; 3 = port.mainlim
            (sv   (char->integer c0)))
        (if (and (eq? type 5)                      ; 5 = output, textual
                 (.<:fix:fix
                  lim
                  (bytevector-length buf))         ; FIXME: should be trusted
                 (.<:fix:fix 10 sv)                ; 10 = #\newline
                 (.<:fix:fix sv 128))
            (begin (bytevector-set! buf lim sv)    ; 3 = port.mainlim
                   (.vector-set!:trusted:nwb p 3 (.+:idx:idx lim 1))
                   (unspecified))
            (io/put-char p c)))))

   ; The fast path for peek-char and read-char.

`  ((_ larceny peek-char (peek-char))
    (lookahead-char (current-input-port)))

`  ((_ larceny peek-char (peek-char p))
    (lookahead-char p))

`  ((_ larceny read-char (read-char))
    (get-char (current-input-port)))

`  ((_ larceny read-char (read-char p))
    (get-char p))

`  ((_ larceny write-char (write-char c))
    (put-char (current-output-port) c))

`  ((_ larceny write-char (write-char c p))
    (put-char p c))

   ; Default case: expand into the original expression.

`  ((_ ?anything ?proc ?exp)
    ?exp)

   ))

))

(for-each (lambda (x) 
            (twobit-expand x (the-usual-syntactic-environment)))
          common-compiler-macros)

; MacScheme machine assembly instructions.

(define instruction.op car)
(define instruction.arg1 cadr)
(define instruction.arg2 caddr)
(define instruction.arg3 cadddr)

; Opcode table.

(define *mnemonic-names* '())           ; For readify-lap
(begin
 '
 (define *last-reserved-mnemonic* 32767)	; For consistency check
 '
 (define make-mnemonic
   (let ((count 0))
     (lambda (name)
       (set! count (+ count 1))
       (if (= count *last-reserved-mnemonic*)
           (error "Error in make-mnemonic: conflict: " name))
       (set! *mnemonic-names* (cons (cons count name) *mnemonic-names*))
       count)))
 '
 (define (reserved-mnemonic name value)
   (if (and (> value 0) (< value *last-reserved-mnemonic*))
       (set! *last-reserved-mnemonic* value))
   (set! *mnemonic-names* (cons (cons value name) *mnemonic-names*))
   value)
 #t)

(define make-mnemonic
   (let ((count 0))
     (lambda (name)
       (set! count (+ count 1))
       (set! *mnemonic-names* (cons (cons count name) *mnemonic-names*))
       count)))

(define (reserved-mnemonic name ignored)
  (make-mnemonic name))

(define $.linearize (reserved-mnemonic '.linearize -1))  ; unused?
(define $.label (reserved-mnemonic '.label 63))
(define $.proc (reserved-mnemonic '.proc 62))    ; proc entry point
(define $.cont (reserved-mnemonic '.cont 61))    ; return point
(define $.align (reserved-mnemonic '.align 60))  ; align code stream
(define $.asm (reserved-mnemonic '.asm 59))      ; in-line native code
(define $.proc-doc                               ; internal def proc info
  (reserved-mnemonic '.proc-doc 58))
(define $.end                                    ; end of code vector
  (reserved-mnemonic '.end 57))                  ; (asm internal)
(define $.singlestep                             ; insert singlestep point
  (reserved-mnemonic '.singlestep 56))           ; (asm internal)
(define $.entry (reserved-mnemonic '.entry 55))  ; procedure entry point 
                                                 ; (asm internal)

(define $op1 (make-mnemonic 'op1))               ; op      prim
(define $op2 (make-mnemonic 'op2))               ; op2     prim,k
(define $op3 (make-mnemonic 'op3))               ; op3     prim,k1,k2
(define $op2imm (make-mnemonic 'op2imm))         ; op2imm  prim,x
(define $const (make-mnemonic 'const))           ; const   x
(define $global (make-mnemonic 'global))         ; global  x
(define $setglbl (make-mnemonic 'setglbl))       ; setglbl x
(define $lexical (make-mnemonic 'lexical))       ; lexical m,n
(define $setlex (make-mnemonic 'setlex))         ; setlex  m,n
(define $stack (make-mnemonic 'stack))           ; stack   n
(define $setstk (make-mnemonic 'setstk))         ; setstk  n
(define $load (make-mnemonic 'load))             ; load    k,n
(define $store (make-mnemonic 'store))           ; store   k,n
(define $reg (make-mnemonic 'reg))               ; reg     k
(define $setreg (make-mnemonic 'setreg))         ; setreg  k
(define $movereg (make-mnemonic 'movereg))       ; movereg k1,k2
(define $lambda (make-mnemonic 'lambda))         ; lambda  x,n,doc
(define $lexes (make-mnemonic 'lexes))           ; lexes   n,doc
(define $args= (make-mnemonic 'args=))           ; args=   k
(define $args>= (make-mnemonic 'args>=))         ; args>=  k
(define $invoke (make-mnemonic 'invoke))         ; invoke  k
(define $save (make-mnemonic 'save))             ; save    L,k
(define $setrtn (make-mnemonic 'setrtn))         ; setrtn  L
(define $restore (make-mnemonic 'restore))       ; restore n    ; deprecated
(define $pop (make-mnemonic 'pop))               ; pop     k
(define $popstk (make-mnemonic 'popstk))         ; popstk       ; for students
(define $return (make-mnemonic 'return))         ; return
(define $mvrtn (make-mnemonic 'mvrtn))           ; mvrtn        ; NYI
(define $apply (make-mnemonic 'apply))           ; apply
(define $nop (make-mnemonic 'nop))               ; nop
(define $jump (make-mnemonic 'jump))             ; jump    m,o
(define $skip (make-mnemonic 'skip))             ; skip    L    ; forward
(define $branch (make-mnemonic 'branch))         ; branch  L
(define $branchf (make-mnemonic 'branchf))       ; branchf L
(define $check (make-mnemonic 'check))           ; check   k1,k2,k3,L
(define $trap (make-mnemonic 'trap))             ; trap    k1,k2,k3,exn

; A peephole optimizer may define more instructions in some
; target-specific file.

; eof
