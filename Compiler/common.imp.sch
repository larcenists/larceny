; Copyright 1991 William Clinger
;
; Relatively target-independent information for Twobit's backend.
;
; 24 April 1999 / wdc
;
; Most of the definitions in this file can be extended or overridden by
; target-specific definitions.

(define twobit-sort
  (lambda (less? list) (compat:sort list less?)))

(define renaming-prefix ".")

; The prefix used for cells introduced by the compiler.

(define cell-prefix (string-append renaming-prefix "CELL:"))

; Names of global procedures that cannot be redefined or assigned
; by ordinary code.
; The expansion of quasiquote uses .cons and .list directly, so these
; should not be changed willy-nilly.
; Others may be used directly by a DEFINE-INLINE.

(define name:CHECK!  '.check!)
(define name:CONS '.cons)
(define name:LIST '.list)
(define name:MAKE-CELL '.make-cell)
(define name:CELL-REF '.cell-ref)
(define name:CELL-SET! '.cell-set!)
(define name:IGNORED (string->symbol "IGNORED"))
(define name:CAR '.car)
(define name:CDR '.cdr)

;(begin (eval `(define ,name:CONS cons))
;       (eval `(define ,name:LIST list))
;       (eval `(define ,name:MAKE-CELL list))
;       (eval `(define ,name:CELL-REF car))
;       (eval `(define ,name:CELL-SET! set-car!)))

; If (INTEGRATE-USUAL-PROCEDURES) is true, then control optimization
; recognizes calls to these procedures.

(define name:NOT 'not)
(define name:MEMQ 'memq)
(define name:MEMV 'memv)

; If (INTEGRATE-USUAL-PROCEDURES) is true, then control optimization
; recognizes calls to these procedures and also creates calls to them.

(define name:EQ? 'eq?)
(define name:EQV? 'eqv?)

; Control optimization creates calls to these procedures,
; which do not need to check their arguments.

(define name:FIXNUM?       'fixnum?)
(define name:CHAR?         'char?)
(define name:SYMBOL?       'symbol?)
(define name:FX<           '<:fix:fix)
(define name:FX-           'fx-)                   ; non-checking version
(define name:CHAR->INTEGER 'char->integer)         ; non-checking version
(define name:VECTOR-REF    'vector-ref:trusted)


; Constant folding.
; Prototype, will probably change in the future.

(define (constant-folding-entry name)
  (assq name $usual-constant-folding-procedures$))

(define constant-folding-predicates cadr)
(define constant-folding-folder caddr)

(define $usual-constant-folding-procedures$
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
      (-- (,ratnum?) ,(lambda (x) (- 0 x)))
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
      (fixnum? (,smallint?) ,smallint?)
      (=:fix:fix  (,smallint? ,smallint?) ,=)
      (<:fix:fix  (,smallint? ,smallint?) ,<)
      (<=:fix:fix (,smallint? ,smallint?) ,<=)
      (>:fix:fix  (,smallint? ,smallint?) ,>)
      (>=:fix:fix (,smallint? ,smallint?) ,>=)
      )))

(begin '
       (define (.check! flag exn . args)
         (if (not flag)
             (apply error "Runtime check exception: " exn args)))
       #t)

; Order matters.  If f and g are both inlined, and the definition of g
; uses f, then f should be defined before g.

(for-each pass1
          `(

(define-inline car
  (syntax-rules ()
   ((car x0)
    (let ((x x0))
      (.check! (pair? x) ,$ex.car x)
      (car:pair x)))))
   
(define-inline cdr
  (syntax-rules ()
   ((car x0)
    (let ((x x0))
      (.check! (pair? x) ,$ex.cdr x)
      (cdr:pair x)))))

(define-inline vector-length
  (syntax-rules ()
   ((vector-length v0)
    (let ((v v0))
      (.check! (vector? v) ,$ex.vlen v)
      (vector-length:vec v)))))
   
(define-inline vector-ref
  (syntax-rules ()
   ((vector-ref v0 i0)
    (let ((v v0)
          (i i0))
      (.check! (fixnum? i) ,$ex.vref v i)
      (.check! (vector? v) ,$ex.vref v i)
      (.check! (<:fix:fix i (vector-length:vec v)) ,$ex.vref v i)
      (.check! (>=:fix:fix i 0) ,$ex.vref  v i)
      (vector-ref:trusted v i)))))
   
(define-inline vector-set!
  (syntax-rules ()
   ((vector-set! v0 i0 x0)
    (let ((v v0)
          (i i0)
          (x x0))
      (.check! (fixnum? i) ,$ex.vset v i x)
      (.check! (vector? v) ,$ex.vset v i x)
      (.check! (<:fix:fix i (vector-length:vec v)) ,$ex.vset v i x)
      (.check! (>=:fix:fix i 0) ,$ex.vset v i x)
      (vector-set!:trusted v i x)))))
   
; This transformation must make sure the entire list is freshly
; allocated when an argument to LIST returns more than once.

(define-inline list
  (syntax-rules ()
   ((list)
    '())
   ((list ?e)
    (cons ?e '()))
   ((list ?e1 ?e2 ...)
    (let* ((t1 ?e1)
           (t2 (list ?e2 ...)))
      (cons t1 t2)))))

; This transformation must make sure the entire list is freshly
; allocated when an argument to VECTOR returns more than once.

(define-inline vector
  (syntax-rules ()
   ((vector)
    '#())
   ((vector ?e)
    (make-vector 1 ?e))
   ((vector ?e1 ?e2 ...)
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
                 (vector-set! v ?n2 ?t2)
                 ...
                 v))))))
      (vector-aux1 (?e1 ?e2 ...) 0 () () ())))))

(define-inline cadddr
  (syntax-rules ()
   ((cadddr ?e)
    (car (cdr (cdr (cdr ?e)))))))

(define-inline cddddr
  (syntax-rules ()
   ((cddddr ?e)
    (cdr (cdr (cdr (cdr ?e)))))))

(define-inline cdddr
  (syntax-rules ()
   ((cdddr ?e)
    (cdr (cdr (cdr ?e))))))

(define-inline caddr
  (syntax-rules ()
   ((caddr ?e)
    (car (cdr (cdr ?e))))))

(define-inline cddr
  (syntax-rules ()
   ((cddr ?e)
    (cdr (cdr ?e)))))

(define-inline cdar
  (syntax-rules ()
   ((cdar ?e)
    (cdr (car ?e)))))

(define-inline cadr
  (syntax-rules ()
   ((cadr ?e)
    (car (cdr ?e)))))

(define-inline caar
  (syntax-rules ()
   ((caar ?e)
    (car (car ?e)))))

(define-inline make-vector
  (syntax-rules ()
   ((make-vector ?n)
    (make-vector ?n '()))))

(define-inline make-string
  (syntax-rules ()
   ((make-string ?n)
    (make-string ?n #\space))))

(define-inline =
  (syntax-rules ()
   ((= ?e1 ?e2 ?e3 ?e4 ...)
    (let ((t ?e2))
      (and (= ?e1 t)
           (= t ?e3 ?e4 ...))))))

(define-inline <
  (syntax-rules ()
   ((< ?e1 ?e2 ?e3 ?e4 ...)
    (let ((t ?e2))
      (and (< ?e1 t)
           (< t ?e3 ?e4 ...))))))

(define-inline >
  (syntax-rules ()
   ((> ?e1 ?e2 ?e3 ?e4 ...)
    (let ((t ?e2))
      (and (> ?e1 t)
           (> t ?e3 ?e4 ...))))))

(define-inline <=
  (syntax-rules ()
   ((<= ?e1 ?e2 ?e3 ?e4 ...)
    (let ((t ?e2))
      (and (<= ?e1 t)
           (<= t ?e3 ?e4 ...))))))

(define-inline >=
  (syntax-rules ()
   ((>= ?e1 ?e2 ?e3 ?e4 ...)
    (let ((t ?e2))
      (and (>= ?e1 t)
           (>= t ?e3 ?e4 ...))))))

(define-inline +
  (syntax-rules ()
   ((+)
    0)
   ((+ ?e)
    ?e)
   ((+ ?e1 ?e2 ?e3 ?e4 ...)
    (+ (+ ?e1 ?e2) ?e3 ?e4 ...))))

(define-inline *
  (syntax-rules ()
   ((*)
    1)
   ((* ?e)
    ?e)
   ((* ?e1 ?e2 ?e3 ?e4 ...)
    (* (* ?e1 ?e2) ?e3 ?e4 ...))))

(define-inline -
  (syntax-rules ()
   ((- ?e)
    (- 0 ?e))
   ((- ?e1 ?e2 ?e3 ?e4 ...)
    (- (- ?e1 ?e2) ?e3 ?e4 ...))))

(define-inline /
  (syntax-rules ()
   ((/ ?e)
    (/ 1 ?e))
   ((/ ?e1 ?e2 ?e3 ?e4 ...)
    (/ (/ ?e1 ?e2) ?e3 ?e4 ...))))

(define-inline abs
  (syntax-rules ()
   ((abs ?z)
    (let ((temp ?z))
      (if (< temp 0)
          (-- temp)
          temp)))))

(define-inline negative?
  (syntax-rules ()
   ((negative? ?x)
    (< ?x 0))))

(define-inline positive?
  (syntax-rules ()
   ((positive? ?x)
    (> ?x 0))))

(define-inline eqv?
  (transformer
   (lambda (exp rename compare)
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
           exp)))))

(define-inline memq
  (syntax-rules (quote)
   ((memq ?expr '(?datum ...))
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
      (memq0 ?expr '(?datum ...))))))

(define-inline memv
  (transformer
   (lambda (exp rename compare)
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
           exp)))))

(define-inline assv
  (transformer
   (lambda (exp rename compare)
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
           exp)))))

(define-inline map
  (syntax-rules (lambda)
   ((map ?proc ?exp1 ?exp2 ...)
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
      
      (loop 1 (?exp1 ?exp2 ...) () ?proc (?exp1 ?exp2 ...))))))

(define-inline for-each
  (syntax-rules (lambda)
   ((for-each ?proc ?exp1 ?exp2 ...)
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
      
      (loop 1 (?exp1 ?exp2 ...) () ?proc (?exp1 ?exp2 ...))))))

))

(define extended-syntactic-environment
  (syntactic-copy global-syntactic-environment))

(define (make-extended-syntactic-environment)
  (syntactic-copy extended-syntactic-environment))

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
