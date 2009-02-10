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
; (find-foreign-file string+(list of string) ...)  =>  unspecified
;   Searches product spaces of path components until a file is found,
;   then adds it to the list of files.
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
; (foreign-variable name type)  =>  procedure
;   name           a string: the name of the variable
;   type           a symbol: the variables's type
;
;   Calling the procedure with no argument fetches the value and
;   converts it to a suitable Scheme representation.  Calling it
;   with a Scheme value stores a converted value in the variable.
;   
; (foreign-wrap-procedure procedure params type)  =>  trampoline
;   procedure      a scheme procedure: trampoline will invoke this
;   params         a list of symbols: the formal parameter types
;   type           a symbol: the return type
;   
;   Returns a trampoline object suitable for passing as a tramp
;   parameters to foreign procedures.  (We can't return the 
;   integer address of the generated codevector; if we did, the
;   codevector might be garbage collected.)
; 
; FIXME: the definitions of ffi/rename-type, foreign-null-pointer?, and
; foreign-null-pointer are compiler specific and should be cleaned up.
;
; FIXME: the implementation of foreign-variable is not complete!

;;; Initialization

(require 'ffi-load)

(define *ffi-architecture*)
(define *ffi-callout-abi*)
(define *ffi-callback-abi*)
(define *ffi-stdcall-callout-abi*)
(define *ffi-stdcall-callback-abi*)

(call-with-values
 load-ffi
 (lambda (architecture callout-abi callback-abi
          stdcall-callout-abi stdcall-callback-abi)
   (set! *ffi-architecture* architecture)
   (set! *ffi-callout-abi* callout-abi)
   (set! *ffi-callback-abi* callback-abi)
   (set! *ffi-stdcall-callout-abi* stdcall-callout-abi)
   (set! *ffi-stdcall-callback-abi* stdcall-callback-abi)))

;; ffi-get-abi: (callout | callback) x (Maybe (cdecl | stdcall | ...)) -> Abi
;; Given a calling direction and optional convention, find an abi.
(define ffi-get-abi
  (let ((abi-table `(((callout  cdecl)   ,*ffi-callout-abi*)
                     ((callback cdecl)   ,*ffi-callback-abi*)
                     ((callout  stdcall) ,*ffi-stdcall-callout-abi*)
                     ((callback stdcall) ,*ffi-stdcall-callback-abi*))))
    (lambda (direction maybe-conv)
      (let* ((convention (if (null? maybe-conv) 'cdecl (car maybe-conv)))
             (key        (list direction convention)))
        (cond
          ((assoc key abi-table)
             => cadr)
          (else (error 'ffi-get-abi ": Unknown ABI: " key)))))))

;;; Support code that is really compiler specific.  These definitions are OK
;;; for SunOS 4, SunOS 5 with the compilers I've tried.

(define (trampoline->pointer x name)
  (cond ((trampoline? x)
         (let ((address (ffi/handle->address (tr-code x))))
           (+ 4 address))) ;; XXX should not hard code bv header length...
        ((integer? x)
         x)
        (else
         (error "Foreign procedure " name ": " x
                "is not a valid value for a trampoline type."))))

(define void*-rt (make-record-type "void*" '(ptr) #f))
(define a-void*-printer 
  (lambda (obj port)
    (display "#<" port)
    (display (record-type-name (record-type-descriptor obj)) port)
    (display " 0x" port)
    (display (number->string ((record-accessor void*-rt 'ptr) obj) 16) port)
    (display ">" port)))
((record-updater (record-type-descriptor void*-rt) 'printer)
 void*-rt
 a-void*-printer)

(define void*? (record-predicate void*-rt))
(define (void*->address x)
  ((record-accessor void*-rt 'ptr) x))
(define (void*-byte-ref x idx)
  (%peek8  (+ ((record-accessor void*-rt 'ptr) x) idx)))
(define (void*-byte-set! x idx val)
  (%poke8  (+ ((record-accessor void*-rt 'ptr) x) idx) val))
(define (void*-word-ref x idx)
  (%peek32u (+ ((record-accessor void*-rt 'ptr) x) idx)))
(define (void*-word-set! x idx val)
  (%poke32 (+ ((record-accessor void*-rt 'ptr) x) idx) val))
(define (void*-void*-ref x idx)
  ((record-constructor void*-rt) (void*-word-ref x idx)))
(define (void*-void*-set! x idx val)
  (let* ((void*-ptr (record-accessor void*-rt 'ptr))
         (dest-addr (+ (void*-ptr x) idx))
         (hex (lambda (n) (string-append "0x" (number->string n 16))))
         (source-val (void*-ptr val)))
    (%poke32 dest-addr source-val)
    ))

(define (void*-double-ref x idx)
  (let ((bv (make-bytevector 8))
        (addr (+ ((record-accessor void*-rt 'ptr) x) idx)))
    (peek-bytes addr bv 8)
    (bytevector-ieee-double-native-ref bv 0)))
(define (void*-double-set! x idx val)
  (let ((bv (make-bytevector 8))
        (addr (+ ((record-accessor void*-rt 'ptr) x) idx)))
    (bytevector-ieee-double-native-set! bv 0 val)
    (poke-bytes addr bv 8)))

(define (void*-float-ref x idx)
  (let ((bv (make-bytevector 4))
        (addr (+ ((record-accessor void*-rt 'ptr) x) idx)))
    (peek-bytes addr bv 4)
    (bytevector-ieee-single-native-ref bv 0)))
(define (void*-float-set! x idx val)
  (let ((bv (make-bytevector 4))
        (addr (+ ((record-accessor void*-rt 'ptr) x) idx)))
    (bytevector-ieee-single-native-set! bv 0 val)
    (poke-bytes addr bv 4)))
         
(define *ffi-attributes*
  (let ()

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

    (define (longlong-check x name)
      (if (or (fixnum? x)
	      (<= -9223372036854775808 x 9223372036854775807))
	  x
	  (error "Foreign-procedure " name ": " x 
		 " is out of range for a signed integer type.")))

    (define (ulonglong-check x name)
      (if (or (and (fixnum? x)
		   (>= x 0))
	      (<= 0 x 18446744073709551615))
	  x
	  (error "Foreign-procedure " name ": " x 
		 " is out of range for a signed integer type.")))

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

    (define (void*->unsigned x name)
      (cond ((not (void*? x))
             (error 'void*->unsigned ": received object " x " not of void* type")))
      ((record-accessor void*-rt 'ptr) x))
    (define (unsigned->void* x name)
      (if (foreign-null-pointer? x)
          #f
          ((record-constructor void*-rt) x)))
    (define (boxed->pointer x name)
      (cond ((or (pair? x)
		 (vector-like? x)
		 (bytevector-like? x)
		 (procedure? x))
	     x)
	    ((eq? x #f)
	     (foreign-null-pointer))
	    (else
	     (error "Foreign-procedure " name ": " x
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

    `((byte     signed32   ,integer-check           ,id)
      (int      signed32   ,integer-check           ,id)
      (short    signed32   ,integer-check           ,id)
      (char     signed32   ,character->char         ,char->character)
      (unsigned unsigned32 ,unsigned-integer-check  ,id) ; avoid
      (uint     unsigned32 ,unsigned-integer-check  ,id)
      (ushort   unsigned32 ,unsigned-integer-check  ,id)
      (uchar    unsigned32 ,character->uchar        ,uchar->character)
      (long     signed32   ,integer-check           ,id)
      (ulong    unsigned32 ,unsigned-integer-check  ,id)
      (size_t   unsigned32 ,unsigned-integer-check  ,id)
      (float    ieee32     ,flonum-check            ,id)
      (double   ieee64     ,flonum-check            ,id)
      (longlong  signed64   ,longlong-check         ,id)
      (ulonglong unsigned64 ,ulonglong-check        ,id)
      (bool     signed32   ,object->bool            ,int->boolean)
      (void     void       ,#f                      ,id)
      (boxed    pointer    ,boxed->pointer          ,#f)
      (void*    unsigned32 ,void*->unsigned          ,unsigned->void*)
      (tramp    unsigned32 ,trampoline->pointer     ,#f)
      (string   pointer    ,string->asciiz          ,asciiz->string))))

(define (ffi-attribute-core-entry t)
  (let ((probe (assq t *ffi-attributes*)))
    (if probe
        probe
	(error "FFI: " t " is not a valid core type name."))))

(define (ffi-add-attribute-core-entry! high-level-name low-level-name high->low low->high)
  (set! *ffi-attributes*
        (cons
         (list high-level-name low-level-name high->low low->high)
         *ffi-attributes*)))

(define *ffi-attribute-aliases*
  '())

(define (ffi-add-alias-of-attribute-entry! new-name t)
  (set! *ffi-attribute-aliases*
        (cons (cons new-name t)
              *ffi-attribute-aliases*)))

;; This now handles the function type constructor -> to convert
;; between C function pointers and Scheme closures.  It might be good
;; to also add an array type constructor that converts between C
;; arrays and Scheme vectors (or maybe lists?  doesn't really
;; matter... the question is more whether the idea of creating such
;; arrays on the fly is actually sound.
;; (Though the boxed descriptor might handle a lot of this, at least
;;  for passing in.)
(define (ffi-attribute-entry t)
  (cond
   ((assq t *ffi-attribute-aliases*) =>
    (lambda (t2) (ffi-attribute-entry (cdr t2))))
   ((pair? t)
    (cond
     ;; (-> (type ...) type)
     ((eq? '-> (car t))
      (let ((param-types (cadr t))
            (ret-type    (caddr t)))
        `(,t unsigned32 
             ,(lambda (proc name) ;; FIXME: space leak!
                (trampoline->pointer 
                 (foreign-wrap-procedure proc param-types ret-type)
                 name))
             ,(lambda (addr name)
                (foreign-procedure-pointer addr param-types ret-type)))))
     ;; (maybe type)
     ((eq? 'maybe (car t))
      (let* ((param-type (cadr t))
             (entry (ffi-attribute-entry param-type))
             (snd (cadr entry))
             (thd (caddr entry))
             (fth (cadddr entry)))
        `(,t ,(cadr entry) 
             ,(lambda (x name) 
                (if x
                    (thd x name)
                    (foreign-null-pointer)))
             ,(lambda (x name)
                (if (foreign-null-pointer? x)
                    #f
                    (fth x name))))))
     ;; (oneof (schemeval-i cint-i) ... type)
     ((eq? 'oneof (car t))
      (let* ((else-type (car (reverse (cdr t))))
             (s2c-vals (reverse (cdr (reverse (cdr t)))))
             (c2s-vals (map reverse s2c-vals))
             (entry (ffi-attribute-entry else-type))
             (thd (caddr entry))
             (fth (cadddr entry)))
        `(,t signed32 
             ,(lambda (sval name) 
                (cond ((assoc sval s2c-vals) => 
                       (lambda (entry)
                         (let ((cval (cadr entry)))
                           cval)))
                      (else (thd sval name))))
             ,(lambda (cval name) 
                (cond ((assoc cval c2s-vals) =>
                       (lambda (entry)
                         (let ((sval (cadr entry)))
                           sval)))
                      (else (fth cval name)))))))
     (else
      (error "FFI: " t " is not a valid type constructor."))))
   (else
    (ffi-attribute-core-entry t))))

(define (ffi/rename-arg-type t)
  (cadr (ffi-attribute-entry t)))

(define (ffi/rename-ret-type t)
  (if (eq? t 'string)
      'unsigned32
      (ffi/rename-arg-type t)))

(define (ffi/arg-converter t)
  (caddr (ffi-attribute-entry t)))

(define (ffi/ret-converter t)
  (cadddr (ffi-attribute-entry t)))
   

;;; Interface

(define (foreign-file x)
  (ffi/libraries (cons x (ffi/libraries))))

(define (foreign-procedure-provided? name . rest)
  (ffi/provides-procedure? (ffi-get-abi 'callout rest) name))

(define (foreign-procedure name param-types ret-type . rest)
  (apply stdffi/make-foreign-procedure name param-types ret-type 
                                       ffi/foreign-procedure rest))

(define (foreign-procedure-pointer addr param-types ret-type . rest)
  (apply stdffi/make-foreign-procedure addr param-types ret-type
                                       ffi/foreign-procedure-pointer rest))

(define (foreign-procedure-tramp name . rest)
  (ffi/link-procedure (ffi-get-abi 'callout rest) name))

(define (find-foreign-file . rest)
  (define (product rest)
    (cond
      ((null? rest) '(()))
      ((pair? (car rest))
       (apply append
       (map
         (lambda (head)
           (map
             (lambda (tail)
               (cons head tail))
             (product (cdr rest))))
         (car rest))))
      (else
        (product (cons (list (car rest)) (cdr rest))))))

  (define (find-file . rest)
    (call-with-current-continuation
      (lambda (done)
        (for-each
          (lambda (possibility)
            (let ((file (apply string-append possibility)))
              (if (file-exists? file)
                (done file))))
          (product rest))
        #f)))

  (cond
    ((apply find-file rest) => foreign-file)
    (else
      (apply error 'find-foreign-file ": Can't find " rest))))


; FIXME, this is not completed!  Also, there should ABI support for
; linking variables (different name mangling, perhaps).

(define (foreign-variable name type . rest)

  (define (var peek poke)
    (let ((addr (ffi/link-procedure (ffi-get-abi 'callout rest) name)))
      (lambda args
	(if (null? args)
	    (peek addr)
	    (poke addr (car args))))))

  (cond ((or (eq? type 'long) (eq? type 'int))
	 (var %peek32 %poke32))
	((or (eq? type 'ulong) (eq? type 'uint))
	 (var %peek32u %poke32u))
	((eq? type 'ushort)
	 (var %peek16u %poke16u))
	((eq? type 'short)
	 (var %peek16 %poke16))
	(else
	 (error "foreign-variable needs support for type " type))))

; Name can be a string or an address.

(define (stdffi/make-foreign-procedure name param-types ret-type maker . rest)

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

  (define (check-for-obvious-type-problems param-types ret-type)
    (cond ((memq 'void param-types)
           (error name "FFI: \"void\" is not a valid parameter type."))
          ((eq? 'boxed ret-type)
           (error name "FFI: \"boxed\" is not a valid return type.")))
    (let ((pointer-objects
           (filter (lambda (x) (or (eq? x 'boxed) (eq? x 'string)))
                   param-types))
          (callbacks
           (filter (lambda (x) (and (pair? x) (eq? '-> (car x))))
                   param-types)))
      (cond ((and (not (null? pointer-objects))
                  (not (null? callbacks)))
             (for-each 
              display
              (list name " FFI warning: boxed objects"
                    ", e.g. " pointer-objects ", can "
                    "become unstable when callbacks"
                    ", e.g. " callbacks ", are invoked."))
             (newline)))))

  (check-for-obvious-type-problems param-types ret-type)
  (param-conversion 
   (maker (ffi-get-abi 'callout rest) 
          name
          (map ffi/rename-arg-type param-types)
          (ffi/rename-ret-type ret-type))))

(define (foreign-null-pointer? x)
  (eq? x 0))

(define (foreign-null-pointer)
  0)

(define (foreign-wrap-procedure proc param-types ret-type . rest)
  (ffi/make-callback (ffi-get-abi 'callback rest) 
                     (lambda args
                       (let ((v (apply proc 
                                       (map (lambda (t v)
                                              ((ffi/ret-converter t) v t))
                                            param-types
                                            args)))
                             (convert-ret-val (ffi/arg-converter ret-type)))
                         (if convert-ret-val
                             (convert-ret-val v ret-type)
                             v)))
                     (map ffi/rename-arg-type param-types)
                     (ffi/rename-ret-type ret-type)))

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
    (%get16u x 0)))

(define (%peek32u addr)
  (let ((x (make-bytevector 4)))
    (peek-bytes addr x 4)
    (%get32u x 0)))

(define (%poke8 addr val)
  (let ((x (make-bytevector 1)))
    (if (< val 0)
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

(define %get16u)
(define %set16u)
(define %get32u)
(define %set32u)

(if (eq? 'little (cdr (assq 'arch-endianness (system-features))))
    (begin
      (set! %get16u 
	    (lambda (x offs)
	      (+ (* (bytevector-ref x (+ offs 1)) 256)
		 (bytevector-ref x offs))))

      (set! %set16u 
	    (lambda (x offs val)
	      (bytevector-set! x (+ offs 1) (quotient val 256))
	      (bytevector-set! x offs (remainder val 256))))

      (set! %get32u
	    (lambda (x offs)
	      (+ (* (bytevector-ref x (+ offs 3)) 16777216)
		 (* (bytevector-ref x (+ offs 2)) 65536)
		 (* (bytevector-ref x (+ offs 1)) 256)
		 (bytevector-ref x offs))))

      (set! %set32u
	    (lambda (x offs val)
	      (bytevector-set! x (+ offs 3) (quotient val 16777216))
	      (bytevector-set! x (+ offs 2) 
			       (remainder (quotient val 65536) 256))
	      (bytevector-set! x (+ offs 1)
			       (remainder (quotient val 256) 256))
	      (bytevector-set! x offs (remainder val 256)))))
    (begin
      (set! %get16u 
	    (lambda (x offs)
	      (+ (* (bytevector-ref x offs) 256)
		 (bytevector-ref x (+ offs 1)))))

      (set! %set16u 
	    (lambda (x offs val)
	      (bytevector-set! x offs (quotient val 256))
	      (bytevector-set! x (+ offs 1) (remainder val 256))))

      (set! %get32u
	    (lambda (x offs)
	      (+ (* (bytevector-ref x offs) 16777216)
		 (* (bytevector-ref x (+ offs 1)) 65536)
		 (* (bytevector-ref x (+ offs 2)) 256)
		 (bytevector-ref x (+ offs 3)))))

      (set! %set32u
	    (lambda (x offs val)
	      (bytevector-set! x offs (quotient val 16777216))
	      (bytevector-set! x (+ offs 1) 
			       (remainder (quotient val 65536) 256))
	      (bytevector-set! x (+ offs 2)
			       (remainder (quotient val 256) 256))
	      (bytevector-set! x (+ offs 3) (remainder val 256))))))

(define (%get16 x offs)
  (let ((v (%get16u x offs)))
    (if (>= v 32767) (- (- 65536 v)) v)))

(define (%get32 x offs)
  (let ((v (%get32u x offs)))
    (if (>= v 2147483648) (- (- 4294967296 v)) v)))

;; XXX this probably has to be parameterized as well...
(define (%get64 x offs)
  (let ((lo (%get32u x (+ offs 0)))
        (hi (%get32 x  (+ offs 32))))
    (+ (* hi (expt 2 32)) lo)))

(define (%set16 x offs v)
  (let ((v (if (< v 0) (+ 65536 v) v)))
    (%set16u x offs v)))

(define (%set32 x offs v)
  (let ((v (if (< v 0) (+ 4294967296 v) v)))
    (%set32u x offs v)))

;; (size is in 8-bit bytes, not in bits here)
(define (size->%integer-getter size)
  (if (eq? 'little (cdr (assq 'arch-endianness (system-features))))
      (lambda (x offs)
        (let rec ((accum 0) (size size) (offs offs) (mult 1))
          (if (zero? size) 
              accum
              (rec (+ accum (* mult (bytevector-ref x offs)))
                   (- size 1)
                   (+ offs 1)
                   (* mult 256)))))
      (lambda (x offs)
        (let rec ((accum 0) (size size) (offs (+ offs size -1)) (mult 1))
          (if (zero? size) 
              accum
              (rec (+ accum (* mult (bytevector-ref x offs)))
                   (- size 1)
                   (- offs 1)
                   (* mult 256)))))))
(define (size->%integer-setter size)
  (let ((init-divisor (expt 2 (* (- size 1) 8))))
    (if (eq? 'little (cdr (assq 'arch-endianness (system-features))))
        (lambda (x offs n)
          (let rec ((size size) (offs (+ offs size -1)) (d init-divisor)) 
            (cond ((not (zero? size))
                   (bytevector-set! x offs (remainder (quotient n d) 256))
                   (rec (- size 1) (- offs 1) (/ d 256))))))
        (lambda (x offs n)
          (let rec ((size size) (offs 0) (d init-divisor))
            (cond ((not (zero? size))
                 (bytevector-set! x offs (remainder (quotient n d) 256))
                 (rec (- size 1) (+ offs 1) (/ d 256)))))))))
(define (size->%bytevector-getter size)
  (lambda (x offs)
    (let ((rtn (make-bytevector size)))
      (bytevector-copy! x offs rtn 0 size)
      rtn)))
(define (size->%bytevector-setter size)
  (lambda (x offs src)
    (bytevector-copy! src 0 x offs size)))

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
  (let ((_strlen ; (foreign-procedure "strlen" '(uint) 'int)
	 (lambda (p)
	   (do ((p p (+ p 1))
		(l 0 (+ l 1)))
	       ((zero? (%peek8 p)) l)))))
    (lambda (ptr)
      (let* ((l (_strlen ptr))
	     (s (make-string l))
             (bv (make-bytevector l)))
	(peek-bytes ptr bv l)
        (do ((i 0 (+ i 1)))
            ((= i l) s)
          (string-set! s i (integer->char (bytevector-ref bv i))))))))

; Steps through a pointer array terminated by a 0 pointer
; and gets each pointed-to element.

(define (%peek-pointer-array ptr getter)
  (let ((p (%peek-pointer ptr)))
    (if (foreign-null-pointer? p)
	'()
	(cons (getter p) (%peek-pointer-array (+ ptr 4) getter)))))

; eof
