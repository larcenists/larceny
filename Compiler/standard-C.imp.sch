; Compiler/sparc.imp.sch
; Larceny -- target-specific information for Twobit's standard-C backend.
;
; $Id$
;
; Copyright 1991 William Clinger
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
; 09 April 1998 / lth

(define twobit-sort
  (lambda (less? list)
    (compat:sort list less?)))

(define renaming-prefix ".")

; The prefix used for cells introduced by the compiler.

(define cell-prefix (string-append renaming-prefix "CELL:"))

; Names of global procedures that cannot be redefined or assigned
; by ordinary code.

(define name:IGNORED (string->symbol "IGNORED"))
(define name:CONS '.cons)
(define name:LIST '.list)
(define name:MAKE-CELL (string->symbol "MAKE-CELL"))
(define name:CELL-REF (string->symbol "CELL-REF"))
(define name:CELL-SET! (string->symbol "CELL-SET!"))

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

;(define *nregs* 8)
(define *nregs* 32)
(define *lastreg* (- *nregs* 1))
(define *fullregs* (quotient *nregs* 2))

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

(define prim-arity cadr)
(define prim-opcodename caddr)
(define prim-immediate? cadddr)
(define (prim-primcode entry) (car (cddddr entry)))
(define (prim-implicit-continuation? entry) (cadr (cddddr entry)))

(define (op1-primcode name)
  (prim-primcode (prim-entry name)))

(define op2-primcode op1-primcode)
(define op3-primcode op1-primcode)

(define (op1-implicit-continuation? name)
  (prim-implicit-continuation? (prim-entry name)))

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
    ; 21 missing
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
    ; (make-cell 1 make-cell #f 51 #f)
    ; (cell-ref 1 cell-ref #f 53 #f)
    (,name:MAKE-CELL 1 make-cell #f 52 #f)
    (,name:CELL-REF 1 cell-ref #f 54 #f)
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
    (string-ref 2 string-ref ,smallint? 78 #f)
    (string-set! 3 string-set! ,smallint? 79 #f)
    (make-vector 2 make-vector #f 80 #f)
    (vector-ref 2 vector-ref ,smallint? 81 #f)
    (bytevector-ref 2 bytevector-ref ,smallint? 82 #f)
    (procedure-ref 2 procedure-ref #f 83 #f)
    ; (cell-set! 2 cell-set! #f 84 #f)
    (,name:CELL-SET! 2 cell-set! #f 84 #f)
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
    (string-ref 142 #f)
    (vector-ref 143 #f)
    (bytevector-ref 144 #f)
    (bytevector-like-ref 145 #f)
    (vector-like-ref 146 #f)))


; FIXME: must deal with these (used by MAL only).

(define $secret-sacred-primops$
  '((syscall 192 #t)
    (creg 193 #f)
    (creg-set! 194 #f)))


; Primitive name used by compiler for generating certain consing code.

(define $cons 'cons)


; These values are used in Chez/compat.ss and in any other compat libraries
; where the hosting compiler does not support (unspecified) and (undefined).
; The extra level of quoting removes any dependence on these values being
; self-quoting or not.

(define-inline 'undefined
  (lambda (exp env) (list 'quote (undefined))))

(define-inline 'unspecified
  (lambda (exp env) (list 'quote (unspecified))))


; Bugs: Many of these should be checking env but aren't.
; These bugs should disappear when this is rewritten to coexist
; with hygienic macros.

(define-inline 'abs
  (lambda (exp env)
    (cond ((not (= 2 (length exp)))
           (inline-error exp))
          ((and (not (assq '< env))
                (not (assq '-- env)))
           (m-scan `(let ((temp ,(cadr exp)))
                         (if (< temp 0)
                             (-- temp)
                             temp))
                   env))
          (else (make-call (make-variable 'abs)
                           (m-scan (cadr exp) env))))))

(define-inline 'negative?
  (lambda (exp env)
    (cond ((not (= 2 (length exp)))
           (inline-error exp))
          (else (make-call (make-variable '<)
                           (list (m-scan (cadr exp) env)
                                 (make-constant 0)))))))

(define-inline 'positive?
  (lambda (exp env)
    (cond ((not (= 2 (length exp)))
           (inline-error exp))
          (else (make-call (make-variable '>)
                           (list (m-scan (cadr exp) env)
                                 (make-constant 0)))))))

; This transformation must make sure the entire list is freshly
; allocated when an argument to LIST returns more than once.

(define-inline 'list
  (lambda (exp env)
    (cond ((null? (cdr exp)) (make-constant '()))
          ((null? (cddr exp))
           (make-call (make-variable 'cons)
                      (list (m-scan (cadr exp) env)
                            (make-constant '()))))
          (else (m-scan `((lambda x x) ,@(cdr exp)) env)))))

(define-inline 'cadddr
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'car)
                 (list (make-call
                        (make-variable 'cdr)
                        (list (make-call
                               (make-variable 'cdr)
                               (list (make-call
                                      (make-variable 'cdr)
                                      (list (m-scan (cadr exp) env)))))))))))))

(define-inline 'cdddr
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'cdr)
                 (list (make-call
                        (make-variable 'cdr)
                        (list (make-call
                               (make-variable 'cdr)
                               (list (m-scan (cadr exp) env)))))))))))

(define-inline 'caddr
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'car)
                 (list (make-call
                        (make-variable 'cdr)
                        (list (make-call
                               (make-variable 'cdr)
                               (list (m-scan (cadr exp) env)))))))))))

(define-inline 'cddr
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'cdr)
                 (list (make-call
                        (make-variable 'cdr)
                        (list (m-scan (cadr exp) env)))))))))

(define-inline 'cdar
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'cdr)
                 (list (make-call
                        (make-variable 'car)
                        (list (m-scan (cadr exp) env)))))))))

(define-inline 'cadr
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'car)
                 (list (make-call
                        (make-variable 'cdr)
                        (list (m-scan (cadr exp) env)))))))))

(define-inline 'caar
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          (else (make-call
                 (make-variable 'car)
                 (list (make-call
                        (make-variable 'car)
                        (list (m-scan (cadr exp) env)))))))))

(define-inline 'make-vector
  (lambda (exp env)
    (cond ((= 2 (length exp))
           (m-scan `(make-vector ,(cadr exp) '()) env))
          ((= 3 (length exp))
           (make-call (make-variable 'make-vector)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (inline-error exp)))))

(define-inline 'make-string
  (lambda (exp env)
    (cond ((= 2 (length exp))
           (m-scan `(make-string ,(cadr exp) #\space) env))
          ((= 3 (length exp))
           (make-call (make-variable 'make-string)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (inline-error exp)))))

(define-inline 'integer->char
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          ((and (smallint? (cadr exp))
                (<= 0 (cadr exp) 255))
           (make-constant (integer->char (cadr exp))))
          (else (make-call (make-variable 'integer->char)
                           (list (m-scan (cadr exp) env)))))))

(define-inline 'char->integer
  (lambda (exp env)
    (cond ((not (= 2 (length exp))) (inline-error exp))
          ((char? (cadr exp))
           (make-constant (char->integer (cadr exp))))
          (else (make-call (make-variable 'char->integer)
                           (list (m-scan (cadr exp) env)))))))

(define-inline '=
  (lambda (exp env)
    (cond ((< (length exp) 3) (inline-error exp))
          ((= (length exp) 3)
           (make-call (make-variable '=)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (let ((TEMP (gensym "t")))
                  (m-scan `(let ((,TEMP ,(caddr exp)))
                                (and (= ,(cadr exp) ,TEMP)
                                     (= ,TEMP ,@(cdddr exp))))
                          env))))))

(define-inline '<
  (lambda (exp env)
    (cond ((< (length exp) 3) (inline-error exp))
          ((= (length exp) 3)
           (make-call (make-variable '<)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (let ((TEMP (gensym "t")))
                  (m-scan `(let ((,TEMP ,(caddr exp)))
                                (and (< ,(cadr exp) ,TEMP)
                                     (< ,TEMP ,@(cdddr exp))))
                          env))))))

(define-inline '>
  (lambda (exp env)
    (cond ((< (length exp) 3) (inline-error exp))
          ((= (length exp) 3)
           (make-call (make-variable '>)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (let ((TEMP (gensym "t")))
                  (m-scan `(let ((,TEMP ,(caddr exp)))
                                (and (> ,(cadr exp) ,TEMP)
                                     (> ,TEMP ,@(cdddr exp))))
                          env))))))

(define-inline '<=
  (lambda (exp env)
    (cond ((< (length exp) 3) (inline-error exp))
          ((= (length exp) 3)
           (make-call (make-variable '<=)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (let ((TEMP (gensym "t")))
                  (m-scan `(let ((,TEMP ,(caddr exp)))
                                (and (<= ,(cadr exp) ,TEMP)
                                     (<= ,TEMP ,@(cdddr exp))))
                          env))))))

(define-inline '>=
  (lambda (exp env)
    (cond ((< (length exp) 3) (inline-error exp))
          ((= (length exp) 3)
           (make-call (make-variable '>=)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (let ((TEMP (gensym "t")))
                  (m-scan `(let ((,TEMP ,(caddr exp)))
                                (and (>= ,(cadr exp) ,TEMP)
                                     (>= ,TEMP ,@(cdddr exp))))
                          env))))))

(define-inline '+
  (lambda (exp env)
    (define (fold args val)
      (cond ((null? args) (make-constant val))
            ((and (constant? (car args))
                  (smallint? (constant.value (car args)))
                  (smallint? (+ val (constant.value (car args)))))
             (fold (cdr args) (+ val (constant.value (car args)))))
            ((and (null? (cdr args))
                  (zero? val))
             (car args))
            ((null? (cdr args))
             (make-call (make-variable '+)
                        (list (car args) (make-constant val))))
            ((and (constant? (cadr args))
                  (smallint? (constant.value (cadr args)))
                  (smallint? (+ val (constant.value (cadr args)))))
             (fold (cons (car args) (cddr args))
                   (+ val (constant.value (cadr args)))))
            (else (fold (cons (make-call (make-variable '+)
                                         (list (car args) (cadr args)))
                              (cddr args))
                        val))))
    (fold (map (lambda (x) (m-scan x env)) (cdr exp)) 0)))

(define-inline '*
  (lambda (exp env)
    (define (fold args val)
      (cond ((null? args) (make-constant val))
            ((and (constant? (car args))
                  (smallint? (constant.value (car args)))
                  (smallint? (* val (constant.value (car args)))))
             (fold (cdr args) (* val (constant.value (car args)))))
            ((and (null? (cdr args))
                  (= 1 val))
             (car args))
            ((null? (cdr args))
             (make-call (make-variable '*)
                        (list (car args) (make-constant val))))
            ((and (constant? (cadr args))
                  (smallint? (constant.value (cadr args)))
                  (smallint? (* val (constant.value (cadr args)))))
             (fold (cons (car args) (cddr args))
                   (* val (constant.value (cadr args)))))
            (else (fold (cons (make-call (make-variable '*)
                                         (list (car args) (cadr args)))
                              (cddr args))
                        val))))
    (fold (map (lambda (x) (m-scan x env)) (cdr exp)) 1)))

(define-inline '-
  (lambda (exp env)
    (define (fold args val)
      (cond ((and (null? (cdr args))
                  (zero? val))
             (car args))
            ((and (null? (cdr args))
                  (constant? (car args))
                  (smallint? (constant.value (car args)))
                  (smallint? (- (constant.value (car args)) val)))
             (make-constant (- (constant.value (car args)) val)))
            ((null? (cdr args))
             (make-call (make-variable '-)
                        (list (car args) (make-constant val))))
            ((and (constant? (cadr args))
                  (smallint? (constant.value (cadr args)))
                  (smallint? (+ val (constant.value (cadr args)))))
             (fold (cons (car args) (cddr args))
                   (+ val (constant.value (cadr args)))))
            (else (fold (cons (make-call (make-variable '-)
                                         (list (car args) (cadr args)))
                              (cddr args))
                        val))))
    (cond ((null? (cdr exp)) (inline-error exp))
          ((null? (cdr (cdr exp)))
           (make-call (make-variable '--)
                      (list (m-scan (cadr exp) env))))
          (else (fold (map (lambda (x) (m-scan x env)) (cdr exp)) 0)))))

(define-inline '/
  (lambda (exp env)
    (cond ((null? (cdr exp))
           (inline-error exp))
          ((null? (cddr exp))
           (m-scan `(/ 1 ,(cadr exp)) env))
          ((null? (cdddr exp))
           (make-call (make-variable '/)
                      (list (m-scan (cadr exp) env)
                            (m-scan (caddr exp) env))))
          (else (m-scan `(/ (/ ,(cadr exp) ,(caddr exp)) ,@(cdddr exp))
                        env)))))

(define-inline 'eq?
  (lambda (exp env)
    (cond ((not (= 3 (length exp)))
           (inline-error exp))
          ((and (smallint? (cadr exp))
                (smallint? (caddr exp)))
           (make-constant (eqv? (cadr exp) (caddr exp))))
          ((smallint? (cadr exp))
           (m-scan `(eq? ,(caddr exp) ,(cadr exp)) env))
          (else (make-call (make-variable 'eq?)
                           (list (m-scan (cadr exp) env)
                                 (m-scan (caddr exp) env)))))))

(define-inline 'eqv?
  (lambda (exp env)
    (cond ((not (= 3 (length exp)))
           (inline-error exp))
          (else (make-call
                 (let ((arg1 (cadr exp))
                       (arg2 (caddr exp)))
                   (if (or (boolean? arg1)
                           (boolean? arg2)
                           (smallint? arg1)
                           (smallint? arg2)
                           (char? arg1)
                           (char? arg2)
                           (and (pair? arg1)
                                (eq? (car arg1) 'quote)
                                (pair? (cdr arg1))
                                (let ((x (cadr arg1)))
                                  (or (boolean? x)
                                      (smallint? x)
                                      (char? x)
                                      (null? x)
                                      (symbol? x))))
                           (and (pair? arg2)
                                (eq? (car arg2) 'quote)
                                (pair? (cdr arg2))
                                (let ((x (cadr arg2)))
                                  (or (boolean? x)
                                      (smallint? x)
                                      (char? x)
                                      (null? x)
                                      (symbol? x)))))
                       (make-variable 'eq?)
                       (make-variable 'eqv?)))
                 (list (m-scan (cadr exp) env)
                       (m-scan (caddr exp) env)))))))

(define-inline 'memv
  (lambda (exp env)
    (cond ((not (= 3 (length exp)))
           (inline-error exp))
          (else (let ((e1 (m-scan (cadr exp) env))
                      (e2 (m-scan (caddr exp) env)))
                  (if (and (constant? e2)
                           (not (assq 'memq env))
                           (list? (constant.value e2))
                           (every? (lambda (x)
                                     (or (boolean? x)
                                         (smallint? x)
                                         (char? x)
                                         (null? x)
                                         (symbol? x)))
                                   (constant.value e2)))
                      (if (constant? e1)
                          (make-constant
                           (memv (constant.value e1)
                                 (constant.value e2)))
                          (make-call (make-variable 'memq)
                                     (list e1 e2)))
                      (make-call (make-variable 'memv)
                                 (list e1 e2))))))))


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

; Unused, but some system code (printlap) depend on their existence.

; Backward compatible peephole stuff for old optimizations.

(define $optb2 (make-mnemonic 'optb2))           ; optb2   prim,L
(define $optb3 (make-mnemonic 'optb3))           ; optb3   prim,x,L

; Operations created by new peephole optimizations.

(define $optbreg1 (make-mnemonic 'optbreg1))       ; optbreg1    prim,k1,L
(define $optbreg2 (make-mnemonic 'optbreg2))       ; optbreg2    prim,k1,k2,L
(define $optbreg2imm (make-mnemonic 'optbreg2imm)) ; optbreg2imm prim,k1,x,L

(define $dresop1 (make-mnemonic 'dresop1))         ; dresop1     prim,k1,kr
(define $dresop2 (make-mnemonic 'dresop2))         ; dresop2     prim,k1,k2,kr
(define $dresop2imm (make-mnemonic 'dresop2imm))   ; dresop2imm  prim,k1,x,kr

(define $constreg (make-mnemonic 'constreg))       ; constreg    const,k

(define $branchfreg (make-mnemonic 'branchfreg))   ; branchfreg k, L

; eof
