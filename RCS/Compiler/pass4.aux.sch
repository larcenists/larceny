; Copyright 1991 Lightship Software, Incorporated.
;
; Parameters determined by the target machine.

;(define *nregs* 8)
(define *nregs* 32)
(define *lastreg* (- *nregs* 1))
(define *fullregs* (quotient *nregs* 2))

; Procedures for fetching parts of assembly instructions.

(define instruction.op car)
(define instruction.arg1 cadr)
(define instruction.arg2 caddr)
(define instruction.arg3 cadddr)

; Procedures for emitting instructions.
;
; To save consing, instructions are accumulated as a list in
; reverse order.  Lists may be embedded using the cg-ops and
; cg-linearize procedures, which insert *ops and *linearize
; as indicators:
;
;    (... *ops (x1 ... xk) ...) is equivalent to
;    (... xk ... x1 ...)
; and
;    (... *linearize (x1 ... xk) ...) is equivalent to
;    (... x1 ... xk ...).
;
; These procedures ought to be macros so they can perform
; the following transformations:
;
;    (cg-linearize
;     (cg-ops E1 .... Ek)
;     E)
;    =>
;    (cons Ek (cons ... (cons E1 E)))
; and
;    (cg-ops E1 ... Ek) => (list Ek ... E1)

(define $.ops '*ops)
(define $.linearize '*linearize)

(define cg-op list)

(define (cg-ops . ops)               ; this oughta be a macro
  (cond ((null? ops) ops)
        ((null? (cdr ops)) ops)
        (else (cons $.ops ops))))

(define (cg-linearize t1 t2)
  (list $.linearize t1 t2))

(define (cg-make-linear tree)
  (define (f tree code)
    (cond ((null? tree) code)
          ((eq? (car tree) $.linearize)
           (f (cadr tree)
              (f (caddr tree)
                 code)))
          ((eq? (car tree) $.ops)
           (append (cdr tree) code))
          (else (f (cdr tree) (cons (car tree) code)))))
  (f tree '()))

(define (make-label)
  (set! cg-label-counter (+ cg-label-counter 1))
  cg-label-counter)

(define cg-label-counter 1000)

; Instructions and pseudo-instructions.

; (define (make-mnemonic symbol)
;   (let ((s (symbol->string symbol)))
;     (string->symbol
;      (string-append
;       "    "
;       s
;       (make-string (max 0 (- 8 (string-length s)))
;                    #\space)))))
; 
; (define $.label (string->symbol "L"))
; 
; (define $op1 (make-mnemonic 'op1))               ; op      prim
; (define $op2 (make-mnemonic 'op2))               ; op2     prim,k
; (define $op3 (make-mnemonic 'op3))               ; op3     prim,k1,k2
; (define $op2imm (make-mnemonic 'opx))            ; op2imm  prim,x
; (define $const (make-mnemonic 'const))           ; const   x
; (define $global (make-mnemonic 'global))         ; global  x
; (define $setglbl (make-mnemonic 'setglbl))       ; setglbl x
; (define $lexical (make-mnemonic 'lexical))       ; lexical m,n
; ;(define $setlex (make-mnemonic 'setlex))         ; setlex  m,n
; (define $stack (make-mnemonic 'stack))           ; stack   n
; (define $setstk (make-mnemonic 'setstk))         ; setstk  n
; (define $reg (make-mnemonic 'reg))               ; reg     k
; (define $setreg (make-mnemonic 'setreg))         ; setreg  k
; (define $movereg (make-mnemonic 'movereg))       ; movereg k1,k2
; (define $lambda (make-mnemonic 'lambda))         ; lambda  x,k
; (define $lexes (make-mnemonic 'lexes))           ; lexes   k
; (define $chain (make-mnemonic 'chain))           ; chain   m
; (define $args= (make-mnemonic 'args=))           ; args=   k
; (define $args>= (make-mnemonic 'args>=))         ; args>=  k
; (define $invoke (make-mnemonic 'invoke))         ; invoke  k
; (define $save (make-mnemonic 'save))             ; save    L,k
; ;(define $setrtn (make-mnemonic 'setrtn))         ; setrtn  L
; (define $restore (make-mnemonic 'restore))       ; restore k
; (define $pop (make-mnemonic 'pop))               ; pop     k
; (define $return (make-mnemonic 'return))         ; return
; ;(define $mvrtn (make-mnemonic 'mvrtn))           ; mvrtn
; ;(define $apply (make-mnemonic 'apply))           ; apply
; (define $skip (make-mnemonic 'skip))             ; skip    L    ;forward
; (define $branch (make-mnemonic 'branch))         ; branch  L
; (define $branchf (make-mnemonic 'branchf))       ; branchf L
; 
; (define $cons 'cons)

; Environments.
;
; Each identifier has one of the following kinds of entry.
;
;    (<name> register <number> (object))
;    (<name> procedure <rib> <label> (object))
;    (<name> lexical <rib> <offset> (object))
;    (<name> integrable <arity> <op> <imm> (object))
;    (<name> global (object))
;
; Implementation.
;
; An environment is represented as a list of the form
;
;    ((<name-or-#t> ...)                     ; registers
;     (<entry> ...)                          ; lexical rib
;     ...)
;
; where each <name-or-#t> is either an identifier or #t,
; and where each <entry> has one of the forms
;
;    (<name> procedure <rib> <label> (object))
;    (<name> lexical <offset> (object))
;    (<name> integrable <arity> <op> <imm>)

(define entry.name car)
(define entry.kind cadr)
(define entry.rib caddr)
(define entry.offset cadddr)
(define entry.label cadddr)
(define entry.regnum caddr)
(define entry.arity caddr)
(define entry.op cadddr)
(define (entry.imm entry) (car (cddddr entry)))

(define (cgenv-initial integrable)
  (list '((#t register 0 (object)))
        (map (lambda (x)
               (list (car x)
                     'integrable
                     (cadr x)
                     (caddr x)
                     (cadddr x)
                     '((object))))
             integrable)))

(define (cgenv-lookup-reg env reg)
  (let* ((registers (car env))
         (n (length registers)))
    (if (>= reg n)
        #t
        (entry.name (list-ref registers (- n reg 1))))))

(define (cgenv-lookup env id)
  (define (loop ribs m)
    (if (null? ribs)
        (cons id '(global (object)))
        (let ((x (assq id (car ribs))))
          (if x
              (case (cadr x)
                ((lexical) (cons id
                                 (cons (cadr x)
                                       (cons m (cddr x)))))
                ((procedure) (cons id
                                   (cons (cadr x)
                                         (cons m (cddr x)))))
                ((integrable) x)
                (else ???))
              (loop (cdr ribs) (+ m 1))))))
  (let ((x (assq id (car env))))
    (if x
        x
        (loop (cdr env) 0))))

(define (cgenv-extend env vars procs)
  (cons (car env)
        (cons (do ((n 0 (+ n 1))
                   (vars vars (cdr vars))
                   (rib (map (lambda (id)
                               (list id 'procedure (make-label) '(object)))
                             procs)
                        (cons (list (car vars) 'lexical n '(object)) rib)))
                  ((null? vars) rib))
              (cdr env))))

(define (cgenv-bindreg env var)
  (cgenv-bindregs env (list var)))

(define (cgenv-bindregs env vars)
  (cons (do ((k (length (car env)) (+ k 1))
             (vars vars (cdr vars))
             (regs (car env)
                   (cons (list (car vars) 'register k '(object))
                         regs)))
            ((null? vars) regs))
        (cdr env)))

(define (cgenv-bindprocs env procs)
  (cons (car env)
        (cons (append (map (lambda (id)
                             (list id 'procedure (make-label) '(object)))
                           procs)
                      (cadr env))
              (cddr env))))

(define (cgenv-tos env)
  (- (length (car env)) 1))

(define (cgenv-tos-1 env)
  (- (length (car env)) 2))

(define (cgenv-push env n)
  (if (zero? n)
      env
      (cgenv-push
       (cons (cons (list #t 'register (length (car env)) '(object))
                   (car env))
             (cdr env))
       (- n 1))))

(define (cgenv-pop env n)
  (if (zero? n)
      env
      (cgenv-pop (cons (cdr (car env))
                       (cdr env))
                 (- n 1))))

(define (cgenv-regvars env)
  (do ((rib (car env) (cdr rib))
       (vars '() (cons (car (car rib)) vars)))
      ((null? rib) vars)))

(define (cgenv-liveregs env)
  (length (car env)))

; For testing.

(define foo
  (lambda (x)
    (pretty-print
     (pass4 (pass2 (pass1 x)) $usual-integrable-procedures$))))

(define compile
  (lambda (x)
    (pass4 (pass2 (pass1 x)) $usual-integrable-procedures$)))

; Find the smallest number of registers such that
; adding more registers does not affect the code
; generated for x (from 4 to 32 registers).

(define (minregs x)
  (define (defregs R)
    (set! *nregs* R)
    (set! *lastreg* (- *nregs* 1))
    (set! *fullregs* (quotient *nregs* 2)))
  (defregs 32)
  (let ((code (assemble (compile x))))
    (define (binary-search m1 m2)
      (if (= (+ m1 1) m2)
          m2
          (let ((midpt (quotient (+ m1 m2) 2)))
            (defregs midpt)
            (if (equal? code (assemble (compile x)))
                (binary-search m1 midpt)
                (binary-search midpt m2)))))
    (defregs 4)
    (let ((newcode (assemble (compile x))))
      (if (equal? code newcode)
          4
          (binary-search 4 32)))))

; Minimums:
;  browse     10
;  triangle    5
;  traverse   10
;  destruct    6
;  puzzle      8,8,10,7
;  tak         6
;  fft        28   (changing the named lets to macros didn't matter)
