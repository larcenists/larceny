; Copyright 1991 Lightship Software
;
; 29 August 1991
;
; Integrable procedures and procedure-specific source code transformations.
; Every integrable procedure that takes a varying number of arguments must
; supply a transformation procedure to map calls into the fixed arity
; required by the MacScheme machine instructions.

; The maximum number of fixed arguments that may be followed by a rest
; argument.  This limitation is removed by the macro expander.

(define @maxargs-with-rest-arg@ 30)

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

(define (byte? x)
  (and (fixnum? x)
       (<= 0 x)
       (< x 256)))

; primop tables used to be here.

; Procedure-specific source code transformations.
; The transformer is passed a source code expression and a predicate
; and returns one of:
;
;    the original source code expression
;    a new source code expression to use in place of the original
;    #f to indicate that the procedure is being called
;      with an incorrect number of arguments or
;      with an incorrect operand
;
; The original source code expression is guaranteed to be a list whose
; car is the name associated with the transformer.
; The predicate takes an identifier (a symbol) and returns true iff
; that identifier is bound to something other than its global binding.

(define @integrable@ '())

(define (integrable? name)
  (and (integrate-usual-procedures)
       (or (assq name @integrable@)
           (prim-entry name))))

(define define-integrable)

(define (define-integrable name transformer)
  (if (assq name @integrable@)
      (begin (set! @integrable@
                   (remq! (assq name @integrable@)
                          @integrable@))
             (define-integrable name transformer))
      (begin (set! @integrable@
                   (cons (list name transformer)
                         @integrable@))
             name)))

(define-integrable 'abs
  (lambda (exp bound?)
    (and (= 2 (length exp))
         (not (bound? '<))
         (not (bound? '--))
         `(let ((temp ,(cadr exp)))
               (if (< temp 0)
                   (-- temp)
                   temp)))))

(define-integrable 'negative?
  (lambda (exp bound?)
    (and (= 2 (length exp))
         (not (bound? '<))
         `(< ,(cadr exp) 0))))

(define-integrable 'positive?
  (lambda (exp bound?)
    (and (= 2 (length exp))
         (not (bound? '>))
         `(> ,(cadr exp) 0))))

(define-integrable 'list
  (lambda (exp bound?)
    (cond ((null? (cdr exp)) ''())
          ((bound? 'cons) exp)
          (else `(cons ,(cadr exp)
                       (list ,@(cddr exp)))))))

(define-integrable 'cadddr
  (lambda (exp bound?)
    (and (= 2 (length exp))
         (not (bound? 'car))
         (not (bound? 'cdr))
         `(car (cdr (cdr (cdr ,(cadr exp))))))))

(define-integrable 'cdddr
  (lambda (exp bound?)
    (and (= 2 (length exp))
         (not (bound? 'cdr))
         `(cdr (cdr (cdr ,(cadr exp)))))))

(define-integrable 'caddr
  (lambda (exp bound?)
    (and (= 2 (length exp))
         (not (bound? 'car))
         (not (bound? 'cdr))
         `(car (cdr (cdr ,(cadr exp)))))))

(define-integrable 'cddr
  (lambda (exp bound?)
    (and (= 2 (length exp))
         (not (bound? 'cdr))
         `(cdr (cdr ,(cadr exp))))))

(define-integrable 'cdar
  (lambda (exp bound?)
    (and (= 2 (length exp))
         (not (bound? 'car))
         (not (bound? 'cdr))
         `(cdr (car ,(cadr exp))))))

(define-integrable 'cadr
  (lambda (exp bound?)
    (and (= 2 (length exp))
         (not (bound? 'car))
         (not (bound? 'cdr))
         `(car (cdr ,(cadr exp))))))

(define-integrable 'caar
  (lambda (exp bound?)
    (and (= 2 (length exp))
         (not (bound? 'car))
         `(car (car ,(cadr exp))))))

(define-integrable 'make-vector
  (lambda (exp bound?)
    (cond ((= 2 (length exp))
           `(make-vector ,(cadr exp) '()))
          ((= 3 (length exp)) exp)
          (else #f))))

; (define-integrable 'make-string
;   (lambda (exp bound?)
;     (cond ((= 2 (length exp))
;            `(make-string ,(cadr exp) #\space))
;           ((= 3 (length exp)) exp)
;           (else #f))))

(define-integrable 'integer->char
  (lambda (exp bound?)
    (cond ((not (= 2 (length exp))) #f)
          ((and (fixnum? (cadr exp))
                (<= 0 (cadr exp) 255))
           (list 'quote (integer->char (cadr exp))))
          (else exp))))

(define-integrable 'char->integer
  (lambda (exp bound?)
    (cond ((not (= 2 (length exp))) #f)
          ((char? (cadr exp))
           (list 'quote (char->integer (cadr exp))))
          (else exp))))

(define-integrable '=
  (lambda (exp bound?)
    (cond ((< (length exp) 3) #f)
          ((= (length exp) 3) exp)
          (else (let ((TEMP (gensym "T")))
                  `(let ((,TEMP ,(caddr exp)))
                        (and (= ,(cadr exp) ,TEMP)
                             (= ,TEMP ,@(cdddr exp)))))))))

(define-integrable '<
  (lambda (exp bound?)
    (cond ((< (length exp) 3) #f)
          ((= (length exp) 3) exp)
          (else (let ((TEMP (gensym "T")))
                  `(let ((,TEMP ,(caddr exp)))
                        (and (< ,(cadr exp) ,TEMP)
                             (< ,TEMP ,@(cdddr exp)))))))))

(define-integrable '>
  (lambda (exp bound?)
    (cond ((< (length exp) 3) #f)
          ((= (length exp) 3) exp)
          (else (let ((TEMP (gensym "T")))
                  `(let ((,TEMP ,(caddr exp)))
                        (and (> ,(cadr exp) ,TEMP)
                             (> ,TEMP ,@(cdddr exp)))))))))

(define-integrable '<=
  (lambda (exp bound?)
    (cond ((< (length exp) 3) #f)
          ((= (length exp) 3) exp)
          (else (let ((TEMP (gensym "T")))
                  `(let ((,TEMP ,(caddr exp)))
                        (and (<= ,(cadr exp) ,TEMP)
                             (<= ,TEMP ,@(cdddr exp)))))))))

(define-integrable '>=
  (lambda (exp bound?)
    (cond ((< (length exp) 3) #f)
          ((= (length exp) 3) exp)
          (else (let ((TEMP (gensym "T")))
                  `(let ((,TEMP ,(caddr exp)))
                        (and (>= ,(cadr exp) ,TEMP)
                             (>= ,TEMP ,@(cdddr exp)))))))))

(define-integrable '+
  (lambda (exp bound?)
    (cond ((null? (cdr exp)) '0)
          ((null? (cddr exp)) (cadr exp))
          ((null? (cdddr exp))
           (cond ((and (fixnum? (cadr exp))
                       (fixnum? (caddr exp))
                       (fixnum? (+ (cadr exp) (caddr exp))))
                  (+ (cadr exp) (caddr exp)))
                 ((fixnum? (cadr exp))
                  `(+ ,(caddr exp) ,(cadr exp)))
                 (else exp)))
          (else `(+ (+ ,(cadr exp) ,(caddr exp)) ,@(cdddr exp))))))

(define-integrable '*
  (lambda (exp bound?)
    (cond ((null? (cdr exp)) '1)
          ((null? (cddr exp)) (cadr exp))
          ((null? (cdddr exp))
           (cond ((and (fixnum? (cadr exp))
                       (fixnum? (caddr exp))
                       (fixnum? (* (cadr exp) (caddr exp))))
                  (* (cadr exp) (caddr exp)))
                 ((fixnum? (cadr exp))
                  `(* ,(caddr exp) ,(cadr exp)))
                 (else exp)))
          (else `(* (* ,(cadr exp) ,(caddr exp)) ,@(cdddr exp))))))

(define-integrable '-
  (lambda (exp bound?)
    (cond ((null? (cdr exp)) #f)
          ((null? (cddr exp))
           (if (bound? '--)
               `(- 0 ,(cadr exp))
               `(-- ,(cadr exp))))
          ((null? (cdddr exp))
           (cond ((and (fixnum? (cadr exp))
                       (fixnum? (caddr exp))
                       (fixnum? (- (cadr exp) (caddr exp))))
                  (- (cadr exp) (caddr exp)))
                 (else exp)))
          (else `(- (- ,(cadr exp) ,(caddr exp)) ,@(cdddr exp))))))

(define-integrable '/
  (lambda (exp bound?)
    (cond ((null? (cdr exp)) #f)
          ((null? (cddr exp)) `(/ 1 ,(cadr exp)))
          ((null? (cdddr exp))
           (cond ((and (fixnum? (cadr exp))
                       (fixnum? (caddr exp))
                       (fixnum? (/ (cadr exp) (caddr exp))))
                  (/ (cadr exp) (caddr exp)))
                 (else exp)))
          (else `(/ (/ ,(cadr exp) ,(caddr exp)) ,@(cdddr exp))))))

(define-integrable 'eq?
  (lambda (exp bound?)
    (cond ((not (= 3 (length exp))) #f)
          ((fixnum? (caddr exp)) exp)
          ((fixnum? (cadr exp))
           `(eq? ,(caddr exp) ,(cadr exp)))
          (else exp))))

(define-integrable 'eqv?
  (lambda (exp bound?)
    (cond ((not (= 3 (length exp))) #f)
          ((bound? 'eq?) exp)
          (else (let ((arg1 (cadr exp))
                      (arg2 (caddr exp)))
                  (if (or (boolean? arg1)
                          (boolean? arg2)
                          (fixnum? arg1)
                          (fixnum? arg2)
                          (char? arg1)
                          (char? arg2)
                          (and (pair? arg1)
                               (pair? (cdr arg1))
                               (let ((x (cadr arg1)))
                                 (or (boolean? x)
                                     (fixnum? x)
                                     (char? x)
                                     (null? x)
                                     (symbol? x))))
                          (and (pair? arg2)
                               (pair? (cdr arg2))
                               (let ((x (cadr arg2)))
                                 (or (boolean? x)
                                     (fixnum? x)
                                     (char? x)
                                     (null? x)
                                     (symbol? x)))))
                      (cons 'eq? (cdr exp))
                      exp))))))
