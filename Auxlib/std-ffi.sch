; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Simple C callout interface to the FFI system.
;
; This file must be loaded from the Larceny build directory, typically
; as part of building the standard heap image.
;
;
; (foreign-file filename)  =>  unspecified
;   Adds a foreign file to the list of files to load procedures from.
;
; (foreign-procedure name params type)  =>  procedure
;   name           a string: the foreign procedure's name
;   param-types    a list of symbols: the formal parameter types
;   ret-type       a symbol: the return type
;
; (foreign-procedure-pointer addr params type)  =>  procedure
;   addr           a number: the foreign procedure's address
;   param-types    a list of symbols: the formal parameter types
;   ret-type       a symbol: the return type
;
; (foreign-null-pointer)  =>  integer
;   Returns a foreign null pointer.
;
; (foreign-null-pointer? integer)  =>  boolean
;   Tests whether the argument is a foreign null pointer.
;
; FIXME: the definitions of ffi/rename-type, foreign-null-pointer?, and
; foreign-null-pointer are compiler specific and should be cleaned up.

;;; Initialization

(define *ffi-path* "Ffi/")

(load (string-append *ffi-path* "ffi-load.sch"))

(define *ffi-architecture*)
(define *ffi-callout-abi*)

(call-with-values
 (lambda ()
   (load-ffi *ffi-path*))
 (lambda (architecture callout-abi callback-abi)
   (set! *ffi-architecture* architecture)
   (set! *ffi-callout-abi* callout-abi)
   #t))


;;; Support code that is really compiler specific.  These definitions are OK
;;; for SunOS 4, SunOS 5 with the compilers I've tried.

(define *ffi-attributes*
  (let ()

    (define _strlen
      (ffi/foreign-procedure *ffi-callout-abi* "strlen"
			     '(unsigned32)
			     'signed32))
					   
    (define (integer-check x name)
      (if (or (fixnum? x)
	      (<= -2147483648 x 2147483647))
	  x
	  (error "Foreign-procedure " name ": " x 
		 " is out of range for a signed integer type.")))

    (define (unsigned-integer-check x name)
      (if (or (and (fixnum? x)
		   (>= x 0))
	      (<= 0 x 4294967295))
	  x
	  (error "Foreign-procedure " name ": " x 
		 " is out of range for a unsigned integer type.")))

    (define (character->char x name)
      (if (char? x)
	  (let ((c (char->integer x)))
	    (if (> c 127)
		(- 256 c)
		c))
	  (error "Foreign-procedure " name ": " x
		 " is not a character.")))

    (define (character->uchar x name)
      (if (char? x)
	  (char->integer x)
	  (error "Foreign-procedure " name ": " x
		 " is not a character.")))

    (define (flonum-check x name)
      (if (and (rational? x)
	       (inexact? x))
	  x
	  (error "Foreign-procedure " name ": " x
		 " is not a floating-point number.")))

    (define (boxed->pointer x name)
      (cond ((or (pair? x)
		 (vector-like? x)
		 (bytevector-like? x)
		 (procedure? x))
	     x)
	    ((eq? x #f)
	     (foreign-null-pointer))
	    (else
	     (error "Foreign-proceduer " name ": " x
		    "is not a valid value for a boxed-object type."))))

    (define (id x name) x)

    (define (object->bool x name)
      (if x 1 0))

    (define (int->boolean x name)
      (if (zero? x) #f #t))

    (define (string->asciiz x name)
      (cond ((eq? x #f)
	     (foreign-null-pointer))
	    ((string? x)
	     (ffi/string->asciiz x))
	    (else
	     (error "Foreign-procedure " name ": " x
		    " is not a valid value for a string type."))))

    (define (char->character x name)
      (if (<= -128 x 127)
	  (if (< x 0)
	      (integer->char (+ 256 x))
	      (integer->char x))
	  (error "Foreign-procedure " name ": " x
		 " is not a valid return value for a character.")))

    (define (uchar->character x name)
      (if (<= 0 x 255)
	  (integer->char x)
	  (error "Foreign-procedure " name ": " x
		 " is not a valid return value for a character.")))

    (define (asciiz->string x name)
      (if (foreign-null-pointer? x) 
	  #f
          (%peek-string x)))

    `((int      signed32   ,integer-check           ,id)
      (short    signed32   ,integer-check           ,id)
      (char     signed32   ,character->char         ,char->character)
      (unsigned unsigned32 ,unsigned-integer-check  ,id) ; avoid
      (uint     unsigned32 ,unsigned-integer-check  ,id)
      (ushort   unsigned32 ,unsigned-integer-check  ,id)
      (uchar    unsigned32 ,character->uchar        ,uchar->character)
      (long     signed32   ,integer-check           ,id)
      (ulong    unsigned32 ,unsigned-integer-check  ,id)
      (float    ieee32     ,flonum-check            ,id)
      (double   ieee64     ,flonum-check            ,id)
      (bool     signed32   ,object->bool            ,int->boolean)
      (void     void       ,#f                      ,id)
      (boxed    pointer    ,boxed->pointer          ,#f)
      (string   pointer    ,string->asciiz          ,asciiz->string))))

(define (ffi/rename-arg-type t)
  (let ((probe (assq t *ffi-attributes*)))
    (if probe
	(cadr probe)
	(error "FFI: " t " is not a valid type name."))))

(define (ffi/rename-ret-type t)
  (if (eq? t 'string)
      'unsigned32
      (ffi/rename-arg-type t)))

(define (ffi/arg-converter t)
  (caddr (assq t *ffi-attributes*)))

(define (ffi/ret-converter t)
  (cadddr (assq t *ffi-attributes*)))

;;; Interface

(define (foreign-file x)
  (ffi/libraries (cons x (ffi/libraries))))

(define (foreign-procedure name param-types ret-type)
  (stdffi/make-foreign-procedure name param-types ret-type 
                                 ffi/foreign-procedure))

(define (foreign-procedure-pointer addr param-types ret-type)
  (stdffi/make-foreign-procedure addr param-types ret-type
                                 ffi/foreign-procedure-pointer))

; Name can be a string or an address.

(define (stdffi/make-foreign-procedure name param-types ret-type maker)

  (define (call0 p r)
    (lambda ()
      (r (p) name)))

  (define (call1 p a1 r)
    (lambda (f1)
      (r (p (a1 f1 name)) name)))

  (define (call2 p a1 a2 r)
    (lambda (f1 f2)
      (r (p (a1 f1 name) (a2 f2 name)) name)))

  (define (call3 p a1 a2 a3 r)
    (lambda (f1 f2 f3)
      (r (p (a1 f1 name) (a2 f2 name) (a3 f3 name)) name)))

  (define (call4 p a1 a2 a3 a4 r)
    (lambda (f1 f2 f3 f4)
      (r (p (a1 f1 name) (a2 f2 name) (a3 f3 name) (a4 f4 name)) name)))

  (define (calln p as r)
    (lambda args
      (r (apply p (map (lambda (ac arg) (ac arg name)) as args)) name)))

  (define (param-conversion p)
    (case (length param-types)
      ((0) (call0 p (ffi/ret-converter ret-type)))
      ((1) (call1 p (ffi/arg-converter (car param-types))
		  (ffi/ret-converter ret-type)))
      ((2) (call2 p (ffi/arg-converter (car param-types))
		  (ffi/arg-converter (cadr param-types))
		  (ffi/ret-converter ret-type)))
      ((3) (call3 p (ffi/arg-converter (car param-types))
		  (ffi/arg-converter (cadr param-types))
		  (ffi/arg-converter (caddr param-types))
		  (ffi/ret-converter ret-type)))
      ((4) (call4 p (ffi/arg-converter (car param-types))
		  (ffi/arg-converter (cadr param-types))
		  (ffi/arg-converter (caddr param-types))
		  (ffi/arg-converter (cadddr param-types))
		  (ffi/ret-converter ret-type)))
      (else (calln p (map ffi/arg-converter param-types)
		   (ffi/ret-converter ret-type)))))

  (cond ((memq 'void param-types)
	 (error "FFI: \"void\" is not a valid parameter type."))
	((eq? 'boxed ret-type)
	 (error "FFI: \"boxed\" is not a valid return type."))
	(else
	 (param-conversion 
	  (maker *ffi-callout-abi* 
                 name
                 (map ffi/rename-arg-type param-types)
                 (ffi/rename-ret-type ret-type))))))

(define (foreign-null-pointer? x)
  (eq? x 0))

(define (foreign-null-pointer)
  0)


;;; Memory access utility functions.

(define sizeof:short 2)
(define sizeof:int 4)
(define sizeof:long 4)
(define sizeof:pointer 4)

; %peek* and %poke*: convenient access to values in memory.

(define (%peek8 addr)
  (let ((x (make-bytevector 1)))
    (peek-bytes addr x 1)
    (let ((v (bytevector-ref x 0)))
      (if (> v 127)
	  (- (- 256 v))
	  v))))

(define (%peek16 addr)
  (let ((x (make-bytevector 2)))
    (peek-bytes addr x 2)
    (%get16 x 0)))

(define (%peek32 addr)
  (let ((x (make-bytevector 4)))
    (peek-bytes addr x 4)
    (%get32 x 0)))

(define (%peek8u addr)
  (let ((x (make-bytevector 1)))
    (peek-bytes addr x 1)
    (bytevector-ref x 0)))

(define (%peek16u addr)
  (let ((x (make-bytevector 2)))
    (peek-bytes addr x 2)
    (bytevector-ref x 0)))

(define (%peek32u addr)
  (let ((x (make-bytevector 4)))
    (peek-bytes addr x 4)
    (%get32u x 0)))

(define (%poke8 addr val)
  (let ((x (make-bytevector 1)))
    (if (< x 0)
	(bytevector-set! x 0 (+ 256 val))
	(bytevector-set! x 0 val))
    (poke-bytes addr x 1)))

(define (%poke16 addr val)
  (let ((x (make-bytevector 2)))
    (%set16 x 0 val)
    (poke-bytes addr x 2)))

(define (%poke32 addr val)
  (let ((x (make-bytevector 4)))
    (%set32 x 0 val)
    (poke-bytes addr x 4)))

(define (%poke8u addr val)
  (let ((x (make-bytevector 1)))
    (bytevector-set! x 0 val)
    (poke-bytes addr x 1)))

(define (%poke16u addr val)
  (let ((x (make-bytevector 2)))
    (%set16u x 0 val)
    (poke-bytes addr x 2)))

(define (%poke32u addr val)
  (let ((x (make-bytevector 4)))
    (%set32u x 0 val)
    (poke-bytes addr x 4)))

(define %peek-int %peek32)
(define %peek-long %peek32)
(define %peek-unsigned %peek32u)        ; avoid using
(define %peek-uint %peek32u)
(define %peek-ulong %peek32u)
(define %peek-short %peek16)
(define %peek-ushort %peek16u)
(define %peek-pointer %peek32u)

(define %poke-int %poke32)
(define %poke-long %poke32)
(define %poke-unsigned %poke32u)        ; avoid using
(define %poke-uint %poke32u)
(define %poke-ulong %poke32u)
(define %poke-short %poke16)
(define %poke-ushort %poke16u)
(define %poke-pointer %poke32u)


; %get* and %set*: reading and writing multibyte values from bytevectors.

(define (%get16u x offs)
  (+ (* (bytevector-ref x offs) 256) (bytevector-ref x (+ offs 1))))

(define (%set16u x offs val)
  (bytevector-set! x offs (quotient val 256))
  (bytevector-set! x (+ offs 1) (remainder val 256)))

(define (%get32u x offs)
  (+ (* (bytevector-ref x offs) 16777216)
     (* (bytevector-ref x (+ offs 1)) 65536)
     (* (bytevector-ref x (+ offs 2)) 256)
     (bytevector-ref x (+ offs 3))))

(define (%set32u x offs val)
  (bytevector-set! x offs (quotient val 16777216))
  (bytevector-set! x (+ offs 1) (remainder (quotient val 65536) 256))
  (bytevector-set! x (+ offs 2) (remainder (quotient val 256) 256))
  (bytevector-set! x (+ offs 3) (remainder val 256)))

(define (%get16 x offs)
  (let ((v (%get16u x offs)))
    (if (>= v 32767) (- (- 65536 v)) v)))

(define (%get32 x offs)
  (let ((v (%get32u x offs)))
    (if (>= v 2147483648) (- (- 4294967296 v)) v)))

(define (%set16 x offs v)
  (let ((v (if (< v 0) (+ 65536 v) v)))
    (%set16u x offs v)))

(define (%set32 x offs v)
  (let ((v (if (< v 0) (+ 4294967296 v) v)))
    (%set32u x offs v)))

; %get-* and %set-*: get and set values in bytevectors in C language terms.

(define %get-int %get32)
(define %get-unsigned %get32u)          ; inconsistent name -- avoid
(define %get-uint %get32u)
(define %get-short %get16)
(define %get-ushort %get16u)
(define %get-long %get32)
(define %get-ulong %get32u)
(define %get-pointer %get32u)

(define %set-int %set32)
(define %set-unsigned %set32u)          ; inconsistent name -- avoid
(define %set-uint %set32u)
(define %set-short %set16)
(define %set-ushort %set16u)
(define %set-pointer %set32u)

; Given the address of a C string, get the string.

(define %peek-string 
  (let ((_strlen (foreign-procedure "strlen" '(uint) 'int)))
    (lambda (ptr)
      (let* ((l (_strlen ptr))
	     (s (make-bytevector l)))
	(peek-bytes ptr s l)
	(typetag-set! s (typetag ""))
	s))))

; Steps through a pointer array terminated by a 0 pointer
; and gets each pointed-to element.

(define (%peek-pointer-array ptr getter)
  (let ((p (%peek-pointer ptr)))
    (if (foreign-null-pointer? p)
	'()
	(cons (getter p) (%peek-pointer-array (+ ptr 4) getter)))))

; eof
