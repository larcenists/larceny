; Compiler/sparc.imp.sch
; Larceny -- target-specific information for Twobit's SPARC backend.
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
; 26 February 1998

(define twobit-sort (lambda (less? list) (compat:sort list less?)))

(define renaming-prefix ".")

; The prefix used for cells introduced by the compiler.

(define cell-prefix (string-append renaming-prefix "CELL:"))

; Names of global procedures that cannot be redefined or assigned
; by ordinary code.

(define name:IGNORED (string->symbol "IGNORED"))
(define name:CONS (string->symbol "CONS"))
(define name:LIST '%list)
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
;    primop code in the MacScheme machine

(define (prim-entry name)
  (assq name $usual-integrable-procedures$))

(define prim-arity cadr)
(define prim-opcodename caddr)
(define prim-immediate? cadddr)
(define (prim-primcode entry)
  (car (cddddr entry)))

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

; BEGIN code snarfed from Larceny/Compiler/pass1.imp.sch.

; MacScheme v4 compiler; Larceny version
; Implementation-defined stuff for pass 1.
;
; $Id: twobit.imp.sch,v 1.2 1997/02/11 20:21:58 lth Exp $
;
; Changed by lth on 950627: added sys$bvlcmp, removed several others.

; This is more conservative than it needs to be.

(define (sparc-imm? x)
  (and (fixnum? x)
       (<= 0 x)
       (< x 256)))

; (define hash-bang-unspecified '#&(a))

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
    (cdr 1 cdr #f #x1c)
    (symbol? 1 symbol? #f #x1f)
    (number? 1 complex? #f #x20)
    (complex? 1 complex? #f #x20)
    (real? 1 rational? #f #x21)
    (rational? 1 rational? #f #x21)
    (integer? 1 integer? #f #x22)
    (fixnum? 1 fixnum? #f #x23)
    (flonum? 1 flonum? #f -1)
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
    (make-cell 1 make-cell #f #x7e)
    (,(string->symbol "MAKE-CELL") 1 make-cell #f #x7e)
    (cell-ref 1 cell-ref #f #x7f)
    (,(string->symbol "CELL-REF") 1 cell-ref #f #x7f)
    
    ; These next few entries are for the disassembler only.
    ; [Not used by the Larceny disassembler.]

    (#f 2 typetag-set! #f #x80)
    (#f 2 eq? #f #x81)
    (#f 2 + #f #x82)
    (#f 2 - #f #x83)
    (#f 2 < #f #x84)
    (#f 2 <= #f #x85)
    (#f 2 = #f #x86)
    (#f 2 > #f #x87)
    (#f 2 >= #f #x88)
    (#f 2 char<? #f #x89)
    (#f 2 char<=? #f #x8a)
    (#f 2 char=? #f #x8b)
    (#f 2 char>? #f #x8c)
    (#f 2 char>=? #f #x8d)
    (#f 2 string-ref #f #x90)
    (#f 2 vector-ref #f #x91)
    (#f 2 bytevector-ref #f #x92)
    
    (typetag-set! 2 typetag-set! ,(lambda (x)
                                          (and (fixnum? x)
                                               (<= 0 x 7)))   ; used to be 31
                                 #xa0)
    (eq? 2 eq? ,sparc-imm? #xa1)
    (eqv? 2 eqv? #f #xa2)
    (cons 2 cons #f #xa8)
    (%cons 2 cons #f #xa8)          ; for the benefit of macro expansion...
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
    (string-ref 2 string-ref ,sparc-imm? #xd1)
    (string-set! 3 string-set! ,sparc-imm? -1)
    (make-vector 2 make-vector #f #xd2)
    (vector-ref 2 vector-ref ,sparc-imm? #xd3)
    (bytevector-ref 2 bytevector-ref ,sparc-imm? #xd5)
    (procedure-ref 2 procedure-ref #f #xd7)
    (cell-set! 2 cell-set! #f #xdf)
    (,(string->symbol "CELL-SET!") 2 cell-set! #f #xdf)
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
;    (modulo 2 modulo #f -1)
    (sys$read-char 1 sys$read-char #f -1)
    ))

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
    (vector-like-ref -1)))

; END code snarfed from Larceny/Compiler/pass1.imp.sch.

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

; @@Lars
; Precomputes (length (quote (x ...)))

;(define-inline 'length
;  (lambda (exp env)
;    (cond ((not (= 2 (length exp)))
;	   (inline-error exp))
;	  ((and (pair? (cadr exp))
;		(eq? (car (cadr exp)) 'quote)
;		(list? (cadr (cadr exp))))
;	   (make-constant (length (cadr (cadr exp)))))
;	  (else
;	   (make-call (make-variable 'length)
;		      (list (m-scan (cadr exp) env)))))))

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

; BEGIN code snarfed from Larceny/Compiler/pass4.imp.sch.

; Opcode table.
;
; $Id: twobit.imp.sch,v 1.2 1997/02/11 20:21:58 lth Exp $

(define $.linearize -1)
(define $.label 63)
(define $.proc 62)        ; entry point for procedure
(define $.cont 61)        ; return point
(define $.align 60)       ; align code stream
(define $.asm 59)         ; in-line native code
(define $.proc-doc 58)    ; internal definition procedure info
(define $.end 57)         ; end of code vector (asm internal)
(define $.singlestep 56)  ; insert singlestep point (asm internal)
(define $.entry 55)       ; procedure entry point (asm internal)

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

; misc

(define $cons 'cons)

; END code snarfed from Larceny/Compiler/pass4.imp.sch.
