; Copyright 1991 William Clinger
;
; $Id$
;
; Larceny -- target-specific information for Twobit's SPARC backend.
;
; 18 December 1998 / wdc

(define twobit-sort
  (lambda (less? list) (compat:sort list less?)))

(define renaming-prefix ".")

; The prefix used for cells introduced by the compiler.

(define cell-prefix (string-append renaming-prefix "CELL:"))

; Names of global procedures that cannot be redefined or assigned
; by ordinary code.
; The expansion of quasiquote uses .cons and .list directly, so these
; should not be changed willy-nilly.

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

; The maximum number of fixed arguments that may be followed by a rest
; argument.  This limitation is removed by the macro expander.

(define @maxargs-with-rest-arg@ 30)

; The number of MacScheme machine registers.
; (They do not necessarily correspond to hardware registers.)

(define *nregs* 32)
(define *lastreg* (- *nregs* 1))
(define *fullregs* (quotient *nregs* 2))

; The number of argument registers that are represented by hardware
; registers.

(define *nhwregs* 8)

; Variable names that indicate register targets.

(define *regnames*
  (do ((alist '() (cons (cons (string->symbol
                               (string-append ".REG" (number->string r)))
                              r)
                        alist))
       (r (- *nhwregs* 1) (- r 1)))
      ((<= r 0)
       alist)))

; A non-inclusive upper bound for the instruction encodings.

(define *number-of-mnemonics* 72)

; Integrable procedures and procedure-specific source code transformations.
; Every integrable procedure that takes a varying number of arguments must
; supply a transformation procedure to map calls into the fixed arity
; required by the MacScheme machine instructions.

; The table of integrable procedures.
; Each entry is a list of the following items:
;
;    procedure name
;    arity
;    procedure name to be used by the disassembler
;    predicate for immediate operands (or #f)
;    primop code in the MacScheme machine (not used by Larceny, may be absent)

(define (prim-entry name)
  (assq name $usual-integrable-procedures$))

(define prim-arity cadr)
(define prim-opcodename caddr)
(define prim-immediate? cadddr)
(define (prim-primcode entry)
  (car (cddddr entry)))

; This predicate returns #t iff its argument will be represented
; as a fixnum on the target machine.

(define smallint?
  (let* ((least (- (expt 2 28)))
         (greatest (- (- least) 1)))
    (lambda (x)
      (and (number? x)
           (exact? x)
           (integer? x)
           (<= least x greatest)))))

(define (sparc-imm? x)
  (and (fixnum? x)
       (<= -1024 x 1023)))

(define (sparc-eq-imm? x)
  (or (sparc-imm? x)
      (eq? x #t)
      (eq? x #f)
      (eq? x '())))

(define (valid-typetag? x)
  (and (fixnum? x)
       (<= 0 x 7)))

(define (fixnum-primitives) #t)

(define $usual-integrable-procedures$
  `((break 0 break #f 3)
    (creg 0 creg #f 7)
    (unspecified 0 unspecified #f -1)
    (undefined 0 undefined #f 8)
    (eof-object 0 eof-object #f -1)
    (enable-interrupts 1 enable-interrupts #f -1)
    (disable-interrupts 0 disable-interrupts #f -1)

    (typetag 1 typetag #f #x11)
    (not 1 not #f #x18)
    (null? 1 null? #f #x19)
    (pair? 1 pair? #f #x1a)
    (eof-object? 1 eof-object? #f -1)
    (port? 1 port? #f -1)
    (structure? 1 structure? #f -1)
    (car 1 car #f #x1b)
    (,name:CAR 1 car #f #x1b)
    (cdr 1 cdr #f #x1c)
    (,name:CDR 1 cdr #f #x1c)
    (symbol? 1 symbol? #f #x1f)
    (number? 1 complex? #f #x20)
    (complex? 1 complex? #f #x20)
    (real? 1 rational? #f #x21)
    (rational? 1 rational? #f #x21)
    (integer? 1 integer? #f #x22)
    (fixnum? 1 fixnum? #f #x23)
    (flonum? 1 flonum? #f -1)
    (compnum? 1 compnum? #f -1)
    (exact? 1 exact? #f #x24)
    (inexact? 1 inexact? #f #x25)
    (exact->inexact 1 exact->inexact #f #x26)
    (inexact->exact 1 inexact->exact #f #x27)
    (round 1 round #f #x28)
    (truncate 1 truncate #f #x29)
    (zero? 1 zero? #f #x2c)
    (-- 1 -- #f #x2d)
    (lognot 1 lognot #f #x2f)
    (real-part 1 real-part #f #x3e)
    (imag-part 1 imag-part #f #x3f)
    (char? 1 char? #f #x40)
    (char->integer 1 char->integer #f #x41)
    (integer->char 1 integer->char #f #x42)
    (string? 1 string? #f #x50)
    (string-length 1 string-length #f #x51)
    (vector? 1 vector? #f #x52)
    (vector-length 1 vector-length #f #x53)
    (bytevector? 1 bytevector? #f #x54)
    (bytevector-length 1 bytevector-length #f #x55)
    (bytevector-fill! 2 bytevector-fill! #f -1)
    (make-bytevector 1 make-bytevector #f #x56)
    (procedure? 1 procedure? #f #x58)
    (procedure-length 1 procedure-length #f #x59)
    (make-procedure 1 make-procedure #f #x5a)
    (creg-set! 1 creg-set! #f #x71)
    (,name:MAKE-CELL 1 make-cell #f #x7e)
    (,name:CELL-REF 1 cell-ref #f #x7f)
    (,name:CELL-SET! 2 cell-set! #f #xdf)
    (typetag-set! 2 typetag-set! ,valid-typetag? #xa0)
    (eq? 2 eq? ,sparc-eq-imm? #xa1)
    (eqv? 2 eqv? #f #xa2)
    (cons 2 cons #f #xa8)
    (,name:CONS 2 cons #f #xa8)
    (set-car! 2 set-car! #f #xa9)
    (set-cdr! 2 set-cdr! #f #xaa)
    (+ 2 + ,sparc-imm? #xb0)
    (- 2 - ,sparc-imm? #xb1)
    (* 2 * ,sparc-imm? #xb2)
    (/ 2 / #f #xb3)
    (quotient 2 quotient #f #xb4)
    (< 2 < ,sparc-imm? #xb5)
    (<= 2 <= ,sparc-imm? #xb6)
    (= 2 = ,sparc-imm? #xb7)
    (> 2 > ,sparc-imm? #xb8)
    (>= 2 >= ,sparc-imm? #xb9)
    (logand 2 logand #f #xc0)
    (logior 2 logior #f #xc1)
    (logxor 2 logxor #f #xc2)
    (lsh 2 lsh #f #xc3)
    (rsha 2 rsha #f -1)
    (rshl 2 rshl #f -1)
    (rot 2 rot #f #xc4)
    (make-string 2 make-string #f -1)
    (string-ref 2 string-ref ,sparc-imm? #xd1)
    (string-set! 3 string-set! ,sparc-imm? -1)
    (make-vector 2 make-vector #f #xd2)
    (vector-ref 2 vector-ref ,sparc-imm? #xd3)
    (bytevector-ref 2 bytevector-ref ,sparc-imm? #xd5)
    (procedure-ref 2 procedure-ref #f #xd7)
    (char<? 2 char<? ,char? #xe0)
    (char<=? 2 char<=? ,char? #xe1)
    (char=? 2 char=? ,char? #xe2)
    (char>? 2 char>? ,char? #xe3)
    (char>=? 2 char>=? ,char? #xe4)
    
    (sys$partial-list->vector 2 sys$partial-list->vector #f -1)
    (vector-set! 3 vector-set! #f #xf1)
    (bytevector-set! 3 bytevector-set! #f #xf2)
    (procedure-set! 3 procedure-set! #f #xf3)
    (bytevector-like? 1 bytevector-like? #f -1)
    (vector-like? 1 vector-like? #f -1)
    (bytevector-like-ref 2 bytevector-like-ref #f -1)
    (bytevector-like-set! 3 bytevector-like-set! #f -1)
    (sys$bvlcmp 2 sys$bvlcmp #f -1)
    (vector-like-ref 2 vector-like-ref #f -1)
    (vector-like-set! 3 vector-like-set! #f -1)
    (vector-like-length 1 vector-like-length #f -1)
    (bytevector-like-length 1 bytevector-like-length #f -1)
    (remainder 2 remainder #f -1)
    (sys$read-char 1 sys$read-char #f -1)
    (gc-counter 0 gc-counter #f -1)
    ,@(if (fixnum-primitives)
	  `((most-positive-fixnum 0 most-positive-fixnum #f)
	    (most-negative-fixnum 0 most-negative-fixnum #f)
	    (fx+                  2 fx+                  ,sparc-imm?)
	    (fx-                  2 fx-                  ,sparc-imm?)
	    (fx--                 1 fx--                 #f)
	    (fx*                  2 fx*                  #f)
	    (fx=                  2 fx=                  ,sparc-imm?)
	    (fx<                  2 fx<                  ,sparc-imm?)
	    (fx<=                 2 fx<=                 ,sparc-imm?)
	    (fx>                  2 fx>                  ,sparc-imm?)
	    (fx>=                 2 fx>=                 ,sparc-imm?)
	    (fxzero?              1 fxzero?              #f)
	    (fxpositive?          1 fxpositive?          #f)
	    (fxnegative?          1 fxnegative?          #f))
	  '())
    ))

; Not used by the Sparc assembler; for information only.

(define $immediate-primops$
  '((typetag-set! #x80)
    (eq? #x81)
    (+ #x82)
    (- #x83)
    (< #x84)
    (<= #x85)
    (= #x86)
    (> #x87)
    (>= #x88)
    (char<? #x89)
    (char<=? #x8a)
    (char=? #x8b)
    (char>? #x8c)
    (char>=? #x8d)
    (string-ref #x90)
    (vector-ref #x91)
    (bytevector-ref #x92)
    (bytevector-like-ref -1)
    (vector-like-ref -1)
    (fx+ -1)
    (fx- -1)
    (fx-- -1)
    (fx= -1)
    (fx< -1)
    (fx<= -1)
    (fx> -1)
    (fx>= -1)))


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
                        (rational? n)))))
    `(
      ; This makes some assumptions about the host system.
      
      (integer->char (,charcode?) ,integer->char)
      (char->integer (,char?) ,char->integer)
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
      )))

(for-each pass1
          '(

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

(define *mnemonic-names* '())		; For readify-lap
(define *last-reserved-mnemonic* 32767)	; For consistency check

(define make-mnemonic
  (let ((count 0))
    (lambda (name)
      (set! count (+ count 1))
      (if (= count *last-reserved-mnemonic*)
          (error "Error in make-mnemonic: conflict: " name))
      (set! *mnemonic-names* (cons (cons count name) *mnemonic-names*))
      count)))

(define (reserved-mnemonic name value)
  (if (and (> value 0) (< value *last-reserved-mnemonic*))
      (set! *last-reserved-mnemonic* value))
  (set! *mnemonic-names* (cons (cons value name) *mnemonic-names*))
  value)

(define $.linearize (reserved-mnemonic '.linearize -1))
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
(define $load (make-mnemonic 'load))             ; load    k,n  ; @@ Will
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
(define $restore (make-mnemonic 'restore))       ; restore n
(define $pop (make-mnemonic 'pop))               ; pop     k
(define $return (make-mnemonic 'return))         ; return
(define $mvrtn (make-mnemonic 'mvrtn))           ; mvrtn
(define $apply (make-mnemonic 'apply))           ; apply
(define $nop (make-mnemonic 'nop))               ; nop
(define $jump (make-mnemonic 'jump))             ; jump    m,o
(define $skip (make-mnemonic 'skip))             ; skip    L    ;forward
(define $branch (make-mnemonic 'branch))         ; branch  L
(define $branchf (make-mnemonic 'branchf))       ; branchf L


; Operations introduced by peephole optimizer.

(define $reg/op1/branchf                  ; reg/op1/branchf    prim,k1,L
  (make-mnemonic 'reg/op1/branchf))
(define $reg/op2/branchf                  ; reg/op2/branchf    prim,k1,k2,L
  (make-mnemonic 'reg/op2/branchf))
(define $reg/op2imm/branchf               ; reg/op2imm/branchf prim,k1,x,L
  (make-mnemonic 'reg/op2imm/branchf))
(define $reg/op1/setreg                   ; reg/op1/setreg     prim,k1,kr
  (make-mnemonic 'reg/op1/setreg))
(define $reg/op2/setreg                   ; reg/op2/setreg     prim,k1,k2,kr
  (make-mnemonic 'reg/op2/setreg))
(define $reg/op2imm/setreg                ; reg/op2imm/setreg  prim,k1,x,kr
  (make-mnemonic 'reg/op2imm/setreg))
(define $reg/branchf                      ; reg/branchf        k, L
  (make-mnemonic 'reg/branchf))
(define $reg/return                       ; reg/return         k
  (make-mnemonic 'reg/return))
(define $reg/setglbl                      ; reg/setglbl        k,x
  (make-mnemonic 'reg/setglbl))
(define $reg/op3                          ; reg/op3            prim,k1,k2,k3
  (make-mnemonic 'reg/op3))
(define $const/setreg                     ; const/setreg       const,k
  (make-mnemonic 'const/setreg))
(define $const/return                     ; const/return       const
  (make-mnemonic 'const/return))
(define $global/setreg                    ; global/setreg      x,k
  (make-mnemonic 'global/setreg))
(define $setrtn/branch                    ; setrtn/branch      L,doc
  (make-mnemonic 'setrtn/branch))
(define $setrtn/invoke                    ; setrtn/invoke      L
  (make-mnemonic 'setrtn/invoke))
(define $global/invoke                    ; global/invoke      global,n
  (make-mnemonic 'global/invoke))

; misc

(define $cons     'cons)
(define $car:pair 'car)
(define $cdr:pair 'cdr)

; eof
