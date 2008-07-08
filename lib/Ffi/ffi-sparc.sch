; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny FFI -- SPARC ABI implementations
;
; See the file sparc-abi.txt for information about calling conventions
; on the SPARC.

; SPARC specific trampoline fields
;
; Additional fields:
;   name            offset  contents
;   ------          ------  --------
;   next-argreg     base+0  an integer -- the next argument register (callout)
;   next-slot       base+1  an integer -- the next stack slot (both)
;   return-type     base+2  a symbol -- the return type (both)
;   next-local      base+3  an integer -- the next local variable (callback)
;   return-encoding base+4  an integer -- the encoding of the return type (cb)
;   proc-addr       base+5  an integer -- a procedure handle
;   arg-types       base+6  an integer -- a byte* handle
;   invoke-loc      base+7  an integer -- address of invoke instructions (co)

#!no-fold-case

(define ffi/SPARC-C-callout-stdabi)
(define ffi/SPARC-C-callback-stdabi)

(let ()
    
  (define base *trampoline-basis-size*)
  (define field-count (+ *trampoline-basis-size* 8))

  (define (next-argreg tr) (vector-ref tr base))
  (define (next-slot tr) (vector-ref tr (+ base 1)))
  (define (return-type tr) (vector-ref tr (+ base 2)))
  (define (next-local tr) (vector-ref tr (+ base 3)))
  (define (return-encoding tr) (vector-ref tr (+ base 4)))
  (define (proc-addr tr) (vector-ref tr (+ base 5)))
  (define (arg-types tr) (vector-ref tr (+ base 6)))
  (define (invoke-loc tr) (vector-ref tr (+ base 7)))

  (define (set-next-argreg! tr val) (vector-set! tr base val))
  (define (set-next-slot! tr val) (vector-set! tr (+ base 1) val))
  (define (set-return-type! tr val) (vector-set! tr (+ base 2) val))
  (define (set-next-local! tr val) (vector-set! tr (+ base 3) val))
  (define (set-return-encoding! tr val) (vector-set! tr (+ base 4) val))
  (define (set-proc-addr! tr val) (vector-set! tr (+ base 5) val))
  (define (set-arg-types! tr val) (vector-set! tr (+ base 6) val))
  (define (set-invoke-loc! tr val) (vector-set! tr (+ base 7) val))

  (define (alloc-trampoline)
    (let ((tr (make-vector field-count)))
      (tr-init-common tr)
      (set-next-argreg! tr 0)
      (set-next-slot! tr first-available-slot)
      (set-next-local! tr 0)
      (set-return-type! tr 'unknown)
      (set-return-encoding! tr 'unknown)
      (set-proc-addr! tr 'unknown)
      (set-arg-types! tr 'unknown)
      tr))

  (define two^30 (expt 2 30))
  (define two^25 (expt 2 25))
  (define two^24 (expt 2 24))
  (define two^22 (expt 2 22))
  (define two^19 (expt 2 19))
  (define two^16 (expt 2 16))
  (define two^14 (expt 2 14))
  (define two^13 (expt 2 13))
  (define two^12 (expt 2 12))
  (define two^10 (expt 2 10))
  (define two^8 (expt 2 8))

  (define regparams 6)			; Arguments passed in %o registers
  (define first-available-slot 23)	; SPARC ABI "extra" area

  (define (%g x) x)
  (define (%o x) (+ x 8))
  (define (%l x) (+ x 16))
  (define (%i x) (+ x 24))
  (define (%f x) x)

  (define (bits k) (if (>= k 0) k (+ two^13 k)))

  (define %sp 14)			; %o6
  (define %fp 30)			; %i6

  (define (aluop code op3 rs1 rs2/imm rd ibit)
    (+ (* code two^30)
       (* rd two^25)
       (* op3 two^19)
       (* rs1 two^14)
       (* ibit two^13)
       (if (zero? ibit)
	   rs2/imm
	   (bits rs2/imm))))

  (define (sethi n rd)       (+ (* rd two^25) (* 4 two^22) n))
  (define (ldi rs imm rd)    (aluop 3 0 rs imm rd 1))
  (define (sti rd imm rs)    (aluop 3 4 rs imm rd 1))
  (define (addi rs imm rd)   (aluop 2 0 rs imm rd 1))
  (define (subi rs imm rd)   (aluop 2 4 rs imm rd 1))
  (define (ldfi rs imm rfd)  (aluop 3 32 rs imm rfd 1))
  (define (lddfi rs imm rfd) (aluop 3 35 rs imm rfd 1))
  (define (stfi rfd imm rs)  (aluop 3 36 rs imm rfd 1))
  (define (stdfi rfd imm rs) (aluop 3 39 rs imm rfd 1))
  (define (stdi rd imm rs)   (aluop 3 7 rs imm rd 1))
  (define (save n)           (aluop 2 60 %sp n %sp 1))
  (define (restore)          (aluop 2 61 (%g 0) 0 (%g 0) 1))
  (define (jmpli rs imm rd)  (aluop 2 56 rs imm rd 1))
  (define (ori rs imm rd)    (aluop 2 2 rs imm rd 1))
  (define (nop)              (ori (%g 0) 0 (%g 0)))
  (define (ta imm)           (let ((rd 8) (rs 0)) (aluop 2 58 rs imm rd 1)))

  (define (bv4 i)
    (let ((bv (make-bytevector 4)))
      (bytevector-set! bv 3 (remainder i two^8))
      (bytevector-set! bv 2 (remainder (quotient i two^8) two^8))
      (bytevector-set! bv 1 (remainder (quotient i two^16) two^8))
      (bytevector-set! bv 0 (quotient i two^24))
      bv))

  (define (at-end tr i) (tr-at-end tr (bv4 i)))
  (define (at-beginning tr i) (tr-at-beginning tr (bv4 i)))

  (define (hibits x) (quotient x two^10)) ; FIXME: not right for negative #s
  (define (lobits x) (remainder x two^10)) ; ditto

  (define (roundup2 x) (+ x (remainder x 2)))

  (define (align-stack tr)
    (set-next-slot! tr (roundup2 (next-slot tr))))

  (define (align-locals tr)
    (set-next-local! tr (roundup2 (next-local tr))))

  (define (set-reg tr reg val)
    (at-end tr (sethi (hibits val) reg))
    (at-end tr (ori reg (lobits val) reg)))

  (define (invoke tr fptr)
    (set-reg tr (%o 7) fptr)		; Must come first
    (at-end tr (jmpli (%o 7) 0 (%o 7)))
    (at-end tr (nop)))

  (define (return+restore tr)
    (at-end tr (jmpli (%i 7) 8 (%g 0)))
    (at-end tr (restore)))

  (define iflush-machine-code
    ;; Copied from src/Rts/Sparc/memory.s (and objdump -d of its object code)
    '#vu8(
     #x90 #x2a #x20 #x07 ;;	andn	%o0, 0x07, %o0		/* round start down to 8-boundary */
     #x10 #x80 #x00 #x1a ;;	b	1f
     #x92 #x2a #x60 #x07 ;;	andn	%o1, 0x07, %o1		/* ditto for end */
     #x81 #xda #x20 #x08 ;; 0:	iflush	%o0+8
     #x81 #xda #x20 #x10 ;;	iflush	%o0+16
     #x81 #xda #x20 #x18 ;;	iflush	%o0+24
     #x81 #xda #x20 #x20 ;;	iflush	%o0+32
     #x81 #xda #x20 #x28 ;;	iflush	%o0+40
     #x81 #xda #x20 #x30 ;;	iflush  %o0+48
     #x81 #xda #x20 #x38 ;;	iflush	%o0+56
     #x81 #xda #x20 #x40 ;;	iflush	%o0+64
     #x81 #xda #x20 #x48 ;;	iflush	%o0+72
     #x81 #xda #x20 #x50 ;;	iflush	%o0+80
     #x81 #xda #x20 #x58 ;;	iflush	%o0+88
     #x81 #xda #x20 #x60 ;;	iflush	%o0+96
     #x81 #xda #x20 #x68 ;;	iflush	%o0+104
     #x81 #xda #x20 #x70 ;;	iflush	%o0+112
     #x81 #xda #x20 #x78 ;;	iflush	%o0+120
     #x81 #xda #x20 #x80 ;;	iflush	%o0+128
     #x81 #xda #x20 #x88 ;;	iflush	%o0+136
     #x81 #xda #x20 #x90 ;;	iflush	%o0+144
     #x81 #xda #x20 #x98 ;;	iflush	%o0+152
     #x81 #xda #x20 #xa0 ;;	iflush	%o0+160
     #x81 #xda #x20 #xa8 ;;	iflush	%o0+168
     #x81 #xda #x20 #xb0 ;;	iflush	%o0+176
     #x81 #xda #x20 #xb8 ;;	iflush	%o0+184
     #x90 #x02 #x20 #xc0 ;;	add	%o0, 192, %o0
     #x80 #xa2 #x00 #x09 ;; 1:	cmp	%o0, %o1
     #x06 #xbf #xff #xe7 ;;	blt	0b			/* no annull! */
     #x81 #xda #x20 #x00 ;;	iflush	%o0+0			/* must be in slot */
     #x81 #xc3 #xe0 #x08 ;;	retl
     #x01 #x00 #x00 #x00 ;;	nop
     ))

  (define (iflush code)
    (define abi ffi/SPARC-C-callback-stdabi)
    (let* ((%o0 (+ 4 (syscall syscall:object->address code)))
           (%o1 (+ %o0 (* (quotient (+ 3 (bytevector-length code)) 4) 4))))
      (sys$c-ffi-apply iflush-machine-code
                       (ffi/convert-arg-descriptor abi '(unsigned32 unsigned32))
                       (ffi/convert-ret-descriptor abi 'void)
                       (list %o0 %o1))))
  
  ; Callout trampoline
  ;
  ; The callout uses the C calling conventions and implements the following
  ; pseudo-procedure:
  ;
  ; void __callout( word *argv, void *result )
  ; {
  ;   %o0 = *argv++;
  ;   %o1 = *argv++;
  ;   ...
  ;   %o5 = *argv++;
  ;   *(%sp+23) = *argv++;
  ;   ...
  ;   invoke <proc>
  ; #if RETURNS_DOUBLE
  ;   (double*)result = %f0/%f1;
  ; #elif RETURNS_SINGLE
  ;   (float*)result = %f0;
  ; #else
  ;   (word*)result = %o0;
  ; #endif
  ; }

  (define callout-stdabi
    (let ()

      (define (next-arg tr)
	(at-end tr (addi (%i 0) (tr-increment tr) (%i 0))))

      (define (callout-arg-word tr offset)
	(if (< (next-argreg tr) regparams)
	    (begin
	      (at-end tr (ldi (%i 0) (or offset 0) (%o (next-argreg tr))))
	      (if (not offset) (next-arg tr))
	      (set-next-argreg! tr (+ 1 (next-argreg tr))))
	    (begin
	      (at-end tr (ldi (%i 0) (or offset 0) (%l 0)))
	      (at-end tr (sti (%l 0) (* 4 (next-slot tr)) %sp))
	      (if (not offset) (next-arg tr))
	      (set-next-slot! tr (+ 1 (next-slot tr))))))

      (define (callout-arg-ieee64 tr)
	(callout-arg-word tr 0)
	(callout-arg-word tr 4)
	(next-arg tr))

      (define (callout-done tr)
	(align-stack tr)
	(at-beginning tr (save (- (* 4 (next-slot tr)))))

	(set-invoke-loc! tr (length (tr-ilist tr)))

	(invoke tr (tr-fptr tr))
	
	(case (return-type tr)
	  ((word)   (at-end tr (sti (%o 0) 0 (%i 1))))
	  ((ieee64) (at-end tr (stdfi (%f 0) 0 (%i 1))))
	  ((ieee32) (at-end tr (stfi (%f 0) 0 (%i 1))))
	  ((void)   #t)
	  (else ???))

	(return+restore tr))

      (define (change-fptr tr)
	(let ((val (tr-fptr tr))
	      (ilist (list-tail (tr-ilist tr) (invoke-loc tr))))
	  (set-car! ilist       (bv4 (sethi (hibits val) (%o 7))))
	  (set-car! (cdr ilist) (bv4 (ori (%o 7) (lobits val) (%o 7))))))

      (lambda (selector)
	(case selector
	  ((alloc)               alloc-trampoline)
	  ((arg-word arg-ieee32) (lambda (tr) (callout-arg-word tr #f)))
	  ((arg-ieee64)          callout-arg-ieee64)
	  ((ret-word)            (lambda (tr) (set-return-type! tr 'word)))
	  ((ret-ieee64)          (lambda (tr) (set-return-type! tr 'ieee64)))
	  ((ret-ieee32)          (lambda (tr) (set-return-type! tr 'ieee32)))
	  ((ret-void)            (lambda (tr) (set-return-type! tr 'void)))
	  ((change-fptr)         change-fptr)
	  ((done)                callout-done)
	  ((done-pasteup)        (lambda (tr) (iflush (tr-code tr))))
	  (else 
	   (error "sparc-callout: bad selector " selector))))))

  ; Callback trampoline
  ;
  ; The trampoline uses the C calling conventions and implements the
  ; following C procedure:
  ;
  ;  t0 __callback( t1 a1, t2 a2, ..., tn an )
  ;  {
  ;     static HANDLE(word) _proc = ...;        // the scheme procedure
  ;     static HANDLE(byte*) _adesc = ...;      // the argtype descriptors
  ;     void *_args[n];
  ;     t0 _r;
  ;
  ;     _args[0] = (void*)&a1;
  ;     _args[1] = (void*)&a2;
  ;     ...
  ;     _args[n-1] = (void*)&an;
  ;     larceny_convert_and_call( _proc, _args, &_r, _adesc, TDESC(t0), n );
  ;     return r;
  ;  }
  ;
  ; The generated code looks like the following.
  ;
  ; _proc and _adesc are passed in as constants: they are addresses of
  ; fixed data (procedure and bytevector).
  ;
  ; TDESC(t0) (the return type) is passed in as a constant.
  ;
  ; _args is kept in the local stack frame starting at %sp+23; the
  ; trampoline next-slot keeps track of the next stack location to use.
  ;
  ; %o1 points into the caller's stack frame save area, and the caller's
  ; parameters are saved here, and the addresses of the parameters are
  ; stored in _args.

  (define callback-stdabi
    (let ()

      ; Store a pointer to the value in the array.

      (define (callback-arg-word tr)
	(at-end tr (sti (%l 0) (* 4 (next-slot tr)) %sp))  ; push pointer
	(at-end tr (addi (%l 0) 4 (%l 0)))                 ; inc pointer
	(set-next-slot! tr (+ 1 (next-slot tr))))

      ; Copy the element to an aligned local variable and then store
      ; a pointer to the local in the array.

      (define (callback-arg-ieee64 tr)
	(align-locals tr)
	(set-next-local! tr (+ 2 (next-local tr)))
	(at-end tr (ldi (%l 0) 0 (%l 2)))
	(at-end tr (ldi (%l 0) 4 (%l 3)))
	(at-end tr (subi %fp (* 4 (next-local tr)) (%l 1)))
	(at-end tr (stdi (%l 2) 0 (%l 1)))
	(at-end tr (sti (%l 1) (* 4 (next-slot tr)) %sp))
	(at-end tr (addi (%l 0) 8 (%l 0)))
	(set-next-slot! tr (+ 1 (next-slot tr))))

      (define (callback-done tr argc)
	(if (eq? (return-type tr) 'ieee64)
	    (set-next-local! tr (+ 2 (next-local tr)))
	    (set-next-local! tr (+ 1 (next-local tr))))
	(let ((retaddr (* 4 (next-local tr)))
	      (size    (* 4 (roundup2 (+ (next-slot tr) (next-local tr))))))

	  ; At-beginning instructions must be in reverse order (what a crock).

	  (do ((n 0 (+ n 1)))
	      ((= n 6))
	    (at-beginning tr (sti (%i n) (* n 4) (%l 0))))
	  (at-beginning tr (addi %fp (* 23 4) (%l 0))) ; caller's param area
	  (at-beginning tr (save (- size)))

	  (set-reg tr (%o 0) (proc-addr tr))      ; _proc
	  (at-end tr (addi %sp (* 23 4) (%o 1)))  ; _args
	  (at-end tr (subi %fp retaddr (%o 2)))   ; return slot
	  (set-reg tr (%o 3) (arg-types tr))      ; _adesc
          (at-end tr (ldi (%o 3) 0 (%o 3)))
	  (at-end tr (ori (%g 0)                  ; TDESC(t0)
			  (return-encoding tr)
			  (%o 4)))
	  (at-end tr (ori (%g 0) argc (%o 5)))    ; argc
	  
	  (invoke tr (tr-fptr tr))

	  (if (eq? (return-type tr) 'ieee64)
	      (at-end tr (lddfi %fp (- retaddr) (%f 0)))
	      (at-end tr (ldi %fp (- retaddr) (%i 0))))

	  (return+restore tr)))

      (define (callback-ret-type tr type code)
	(set-return-type! tr type)
	(set-return-encoding! tr code))

      (lambda (selector)
	;(display selector) (newline)
	(case selector
	  ((alloc)               alloc-trampoline)
	  ((arg-word arg-ieee32) callback-arg-word)
	  ((arg-ieee64)          callback-arg-ieee64)
	  ((proc-addr)           (lambda (tr addr) (set-proc-addr! tr addr)))
	  ((arg-types)           (lambda (tr bv) (set-arg-types! tr bv)))
	  ((ret-type)            callback-ret-type)
	  ((done)                callback-done)
	  ((done-pasteup)        (lambda (tr) (iflush (tr-code tr))))
	  (else
	   (error "sparc-callback: bad selector " selector))))))


  (set! ffi/SPARC-C-callout-stdabi callout-stdabi)
  (set! ffi/SPARC-C-callback-stdabi callback-stdabi)
  #t)

; eof
