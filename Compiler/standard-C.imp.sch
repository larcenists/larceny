; Copyright 1991 William Clinger
;
; $Id$
;
; Larceny -- target-specific information for Twobit's standard-C backend.

(define twobit-sort
  (lambda (less? list)
    (compat:sort list less?)))

(define renaming-prefix ".")

; The prefix used for cells introduced by the compiler.

(define cell-prefix (string-append renaming-prefix "CELL:"))

; Names of global procedures that cannot be redefined or assigned
; by ordinary code.

(define name:CONS '.cons)
(define name:CAR '.car)
(define name:CDR '.cdr)
(define name:LIST '.list)
(define name:MAKE-CELL '.make-cell)
(define name:CELL-REF '.cell-ref)
(define name:CELL-SET! '.cell-set!)
(define name:IGNORED (string->symbol "IGNORED"))

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
;    primop code used to name macros
;    flag for whether the primitive may create a continuation

(define (prim-entry name)
  (assq name $usual-integrable-procedures$))

(define (prim-entry-by-opcodename name)
  (let loop ((x $usual-integrable-procedures$))
    (cond ((null? x) #f)
	  ((eq? (prim-opcodename (car x)) name)
	   (car x))
	  (else 
	   (loop (cdr x))))))

(define prim-arity cadr)
(define prim-opcodename caddr)
(define prim-immediate? cadddr)
(define (prim-primcode entry) (car (cddddr entry)))
(define (prim-implicit-continuation? entry) (cadr (cddddr entry)))

(define (op1-primcode name)
  (prim-primcode (prim-entry-by-opcodename name)))

(define op2-primcode op1-primcode)
(define op3-primcode op1-primcode)

(define (op1-implicit-continuation? name)
  (prim-implicit-continuation? (prim-entry-by-opcodename name)))

(define op2-implicit-continuation? op1-implicit-continuation?)
(define op3-implicit-continuation? op1-implicit-continuation?)

(define (op2imm-primcode name)
  (cadr (assq name $immediate-primops$)))

(define (op2imm-implicit-continuation? name)
  (caddr (assq name $immediate-primops$)))

; This predicate returns #t iff its argument will be represented
; as an immediate fixnum on the target machine.

(define smallint?
  (let* ((least (- (expt 2 28)))
         (greatest (- (- least) 1)))
    (lambda (x)
      (and (number? x)
           (exact? x)
           (integer? x)
           (<= least x greatest)))))

(define (byte? x)
  (and (fixnum? x)
       (<= 0 x)
       (< x 256)))

(define (typetag-arg? x)
  (and (fixnum? x) (<= 0 x 7)))

(define $usual-integrable-procedures$
  `((break 0 break #f 1 #f)
    ; 2 missing
    (unspecified 0 unspecified #f 3 #f)
    (undefined 0 undefined #f 4 #f)
    (eof-object 0 eof-object #f 5 #f)
    (enable-interrupts 1 enable-interrupts #f 6 #t)
    (disable-interrupts 0 disable-interrupts #f 7 #t)
    (typetag 1 typetag #f 8 #f)
    (not 1 not #f 9 #f)
    (null? 1 null? #f 10 #f)
    (pair? 1 pair? #f 11 #f)
    (eof-object? 1 eof-object? #f 12 #f)
    (port? 1 port? #f 13 #f)
    (structure? 1 structure? #f 14 #f)
    (car 1 car #f 15 #f)
    (cdr 1 cdr #f 16 #f)
    (symbol? 1 symbol? #f 17 #f)
    (number? 1 complex? #f 18 #f)
    (complex? 1 complex? #f 19 #f)
    (real? 1 rational? #f 20 #f)
    (rational? 1 rational? #f 20 #f)
    (compnum? 1 compnum? #f 21 #f)
    (integer? 1 integer? #f 22 #f)
    (fixnum? 1 fixnum? #f 23 #f)
    (flonum? 1 flonum? #f 24 #f)
    (exact? 1 exact? #f 25 #f)
    (inexact? 1 inexact? #f 26 #f)
    (exact->inexact 1 exact->inexact #f 27 #t)
    (inexact->exact 1 inexact->exact #f 28 #t)
    (round 1 round #f 29 #t)
    (truncate 1 truncate #f 30 #t)
    (zero? 1 zero? #f 31 #t)
    (-- 1 -- #f 32 #t)
    (lognot 1 lognot #f 33 #f)
    (real-part 1 real-part #f 34 #f)
    (imag-part 1 imag-part #f 35 #f)
    (char? 1 char? #f 36 #f)
    (char->integer 1 char->integer #f 37 #f)
    (integer->char 1 integer->char #f 38 #f)
    (string? 1 string? #f 39 #f)
    (string-length 1 string-length #f 40 #f)
    (vector? 1 vector? #f 41 #f)
    (vector-length 1 vector-length #f 42 #f)
    (bytevector? 1 bytevector? #f 43 #f)
    (bytevector-length 1 bytevector-length #f 44 #f)
    (bytevector-fill! 2 bytevector-fill! #f 45 #f)
    (make-bytevector 1 make-bytevector #f 46 #f)
    (procedure? 1 procedure? #f 47 #f)
    (procedure-length 1 procedure-length #f 48 #f)
    (make-procedure 1 make-procedure #f 49 #f)
    ; 50 missing
    ; 51 missing
    (,name:MAKE-CELL 1 ,name:MAKE-CELL #f 52 #f)
    ; 53 missing
    (,name:CELL-REF 1 ,name:CELL-REF #f 54 #f)
    (typetag-set! 2 typetag-set! ,typetag-arg? 55 #f)
    (eq? 2 eq? ,smallint? 56 #f)
    (eqv? 2 eqv? #f 57 #t)
    (cons 2 cons #f 58 #f)
    (.cons 2 cons #f 58 #f)
    (set-car! 2 set-car! #f 59 #f)
    (set-cdr! 2 set-cdr! #f 60 #f)
    (+ 2 + ,smallint? 61 #t)
    (- 2 - ,smallint? 62 #t)
    (* 2 * #f 63 #t)
    (/ 2 / #f 64 #t)
    (quotient 2 quotient #f 65 #t)
    (< 2 < ,smallint? 66 #t)
    (<= 2 <= ,smallint? 67 #t)
    (= 2 = ,smallint? 68 #t)
    (> 2 > ,smallint? 69 #t)
    (>= 2 >= ,smallint? 70 #t)
    (logand 2 logand #f 71 #f)
    (logior 2 logior #f 72 #f)
    (logxor 2 logxor #f 73 #f)
    (lsh 2 lsh #f 74 #f)
    (rsha 2 rsha #f 75 #f)
    (rshl 2 rshl #f 76 #f)
    (rot 2 rot #f 77 #f)
;    (string-ref 2 string-ref ,smallint? 78 #f)
    (string-ref 2 string-ref #f 78 #f)
    (string-set! 3 string-set! ,smallint? 79 #f)
    (make-vector 2 make-vector #f 80 #f)
;    (vector-ref 2 vector-ref ,smallint? 81 #f)
    (vector-ref 2 vector-ref #f 81 #f)
;    (bytevector-ref 2 bytevector-ref ,smallint? 82 #f)
    (bytevector-ref 2 bytevector-ref #f 82 #f)
    (procedure-ref 2 procedure-ref #f 83 #f)
    (,name:CELL-SET! 2 ,name:CELL-SET! #f 84 #f)
    (char<? 2 char<? ,char? 85 #f)
    (char<=? 2 char<=? ,char? 86 #f)
    (char=? 2 char=? ,char? 87 #f)
    (char>? 2 char>? ,char? 88 #f)
    (char>=? 2 char>=? ,char? 89 #f)
    (sys$partial-list->vector 2 sys$partial-list->vector #f 90 #f)
    (vector-set! 3 vector-set! #f 91 #f)
    (bytevector-set! 3 bytevector-set! #f 92 #f)
    (procedure-set! 3 procedure-set! #f 93 #f)
    (bytevector-like? 1 bytevector-like? #f 94 #f)
    (vector-like? 1 vector-like? #f 95 #f)
    (bytevector-like-ref 2 bytevector-like-ref #f 96 #f)
    (bytevector-like-set! 3 bytevector-like-set! #f 97 #f)
    (sys$bvlcmp 2 sys$bvlcmp #f 98 #f)
    (vector-like-ref 2 vector-like-ref #f 99 #f)
    (vector-like-set! 3 vector-like-set! #f 100 #f)
    (vector-like-length 1 vector-like-length #f 101 #f)
    (bytevector-like-length 1 bytevector-like-length #f 102 #f)
    (remainder 2 remainder #f 103 #t)
    (#f 1 petit-patch-boot-code #f 104 #f)
    (#f 1 syscall #f 105 #t)
    (creg 0 creg #f 106 #f)
    (creg-set! 1 creg-set! #f 107 #f)
    (gc-counter 0 gc-counter #f 108 #f)
    (make-string 2 make-string #f 109 #f)
    ))

(define $immediate-primops$
  '((typetag-set! 128 #f)
    (eq? 129 #f)
    (+ 130 #t)
    (- 131 #t)
    (< 132 #t)
    (<= 133 #t)
    (= 134 #t)
    (> 135 #t)
    (>= 136 #t)
    (char<? 137 #f)
    (char<=? 138 #f)
    (char=? 139 #f)
    (char>? 140 #f)
    (char>=? 141 #f)
; Not currently, although eventually.
;    (string-ref 142 #f)
;    (vector-ref 143 #f)
;    (bytevector-ref 144 #f)
    ))


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

(define $.linearize -1)
(define $.label 63)
(define $.proc 62)
(define $.cont 61)        ; return point
(define $.align 60)       ; align code stream
(define $.asm 59)         ; in-line native code
(define $.proc-doc 58)    ; internal definition procedure info
(define $.end 57)         ; end of code vector (asm internal)
(define $.singlestep 56)  ; insert singlestep point (asm internal)
(define $.entry 55)       ; entry point for procedure (asm internal)

(define make-mnemonic
  (let ((count 0))
    (lambda (ignored)
      (set! count (+ count 1))
      count)))

(define $op1 (make-mnemonic 'op1))               ; op      prim
(define $op2 (make-mnemonic 'op2))               ; op2     prim,k
(define $op3 (make-mnemonic 'op3))               ; op3     prim,k1,k2
(define $op2imm (make-mnemonic 'opx))            ; op2imm  prim,x
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

; misc

(define $cons 'cons)
(define $car:pair 'car)
(define $cdr:pair 'cdr)

; eof
