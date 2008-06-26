; Copyright 2001 Lars T Hansen
;
; $Id$
;
; Larceny FFI -- Intel 386 family ABI implementations
;
; See the file i386-abi.txt for information about calling conventions
; on the i386 and later.  This file implements both Windows __stdcall
; and __cdecl conventions.

; Status: callout is done; callback not started yet.

; i386 specific trampoline fields
;
; Additional fields:
;   name            offset  contents
;   ------          ------  --------
;   return-type     base+0  a symbol -- the return type (callout and callback)
;   arg-length      base+1  an integer -- the number of words of arguments so far

(define ffi/i386-C-callout-stdcall)
(define ffi/i386-C-callback-stdcall)
(define ffi/i386-C-callout-cdecl)
(define ffi/i386-C-callback-cdecl)

(let ()

  (define base *trampoline-basis-size*)
  (define field-count (+ *trampoline-basis-size* 6))

  (define (return-type tr) (vector-ref tr base))
  (define (arg-length tr) (vector-ref tr (+ base 1)))
  (define (return-encoding tr) (vector-ref tr (+ base 2)))
  (define (proc-addr tr) (vector-ref tr (+ base 3)))
  (define (arg-types tr) (vector-ref tr (+ base 4)))
  (define (fptr-updater tr) (vector-ref tr (+ base 5)))

  (define (set-return-type! tr val) (vector-set! tr base val))
  (define (set-arg-length! tr val) (vector-set! tr (+ base 1) val))
  (define (set-return-encoding! tr val) (vector-set! tr (+ base 2) val))
  (define (set-proc-addr! tr val) (vector-set! tr (+ base 3) val))
  (define (set-arg-types! tr val) (vector-set! tr (+ base 4) val))
  (define (set-fptr-updater! tr val) (vector-set! tr (+ base 5) val))

  (define two^24 (expt 2 24))
  (define two^16 (expt 2 16))
  (define two^8  (expt 2 8))

  (define (alloc-trampoline)
    (let ((tr (make-vector field-count)))
      (tr-init-common tr)
      (set-return-type! tr 'unknown)
      (set-return-encoding! tr 'unknown)
      (set-arg-length! tr 0)
      tr))

  (define (list->bytevector l)
    (let* ((len (length l))
	   (bv  (make-bytevector len)))
      (do ((i 0 (+ i 1))
	   (l l (cdr l)))
	  ((= i len) bv)
	(bytevector-set! bv i (car l)))))
 
  (define (dword n)
    (if (not (integer? n))
        (error 'dword " non-numeric input: " n))
    (list (remainder n two^8)
	  (remainder (quotient n two^8) two^8)
	  (remainder (quotient n two^16) two^8)
	  (quotient n two^24)))

  ; Callout trampoline
  ;
  ; (see also: larceny_C_ffi_apply in src/Rts/Sys/ffi.c)
  ; The callout uses the __stdcall C calling conventions and implements
  ; the following pseudo-procedure:
  ;
  ; void __callout( word *argv, void *result )
  ; {
  ;   argv = argv + NUMARGS;
  ;   PUSH(*--argv);
  ;   PUSH(*--argv);
  ;   ...
  ;   PUSH(*--argv);
  ;   invoke <proc>
  ; #if RETURNS_DOUBLE
  ;   FSTP_QWORD((double*)result);
  ; #elif RETURNS_SINGLE
  ;   FSTP_DWORD((float*)result);
  ; #else
  ;   (word*)result = EAX;
  ; #endif
  ; }

  (define (make-callout-stdabi stdcall?)
    (let ()

      (define (make-filler tr opcode)
        (vector->list (make-vector (modulo (+ 0 (- (arg-length tr))) 4)
                                   opcode)))
      (define (callout-done tr)
        (define args-size (dword (* 4 (arg-length tr))))
	(tr-at-beginning tr
	  (list->bytevector
	   `(#x55                                       ; PUSH EBP              standard prologue
	     #x89 #xE5                                  ; MOV EBP, ESP
             ,@(make-filler tr #x55) ; PUSH EBP (filler to 16-byte aligned)
	     #x53                                       ; PUSH EBX
	     #x56                                       ; PUSH ESI
	     #x8B #x75 #x08                             ; MOV ESI, [EBP+8]      load argv
	     #x81 #xEC ,@args-size                      ; SUB ESP, 4*argc       allocate space for args
	     #x8B #xFC                                  ; MOV EDI, ESP          copy pointer
	     #xFC)))                                    ; CLD                   copy upward

	((lambda (bv) 
           ;;; to support change-fptr, register a callback that
           ;;; mutates the bytevector below with the new fptr.
           (set-fptr-updater! tr (lambda ()
                                   (for-each (lambda (idx byte) 
                                               (bytevector-set! bv idx byte))
                                             '(1 2 3 4)
                                             (dword (tr-fptr tr)))))
           (tr-at-end tr bv))
	  (list->bytevector
	   `(#xB8 ,@(dword (tr-fptr tr))                ; MOV EAX, <loc>        call the
	     #xFF #xD0                                  ; CALL EAX                procedure
             ,@(if stdcall?
                 `()                                       ; for stdcall, callee pops
                 `(#x81 #xC4 ,@args-size))                 ; ADD ESP, 4*argc       pop args
	     #x8B #x5D #x0C                             ; MOV EBX, [EBP+12]     get return pointer
	     ,@(case (return-type tr)
		 ((word)   '(#x89 #x03))                ; MOV [EBX], EAX
		 ((word2)  '(#x89 #x03                  ; MOV [EBX], EAX
		             #x89 #x53 #x04))           ; MOV [EBX+4], EDX
		 ((ieee64) '(#xDD #x1B))                ; FSTP QWORD PTR [EBX]
		 ((ieee32) '(#xD9 #x1B))                ; FSTP DWORD PTR [EBX]
		 ((void)   '())
		 (else ???))
	     #x5E                                       ; POP ESI               standard epilogue
	     #x5B                                       ; POP EBX
             ,@(make-filler tr #x5D) ; POP EBP (filler to restore old esp)
	     #x5D                                       ; POP EBP
             #xC3                                       ; RETURN                caller pops
;;	     ,@(if stdcall?
;;		   '(#xC2 #x08)                         ; RETURN 8              callee pops
;;		   '(#xC3))                             ; RETURN                caller pops
            )))
	#t)

      (define (copy-dword tr)
	(tr-at-end tr 
	  (list->bytevector
	   `(#xA5			                ; MOVS  DWORD PTR [ESI], DWORD PTR [EDI]
	     #x83 #xC6 #x04))))                         ; ADD   ESI, 4

      (define (copy-qword tr)
	(tr-at-end tr 
	  (list->bytevector
	   `(#xA5			                ; MOVS  DWORD PTR [ESI], DWORD PTR [EDI]
	     #xA5))))			                ; MOVS  DWORD PTR [ESI], DWORD PTR [EDI]
      
      (define (change-fptr tr)
        ((fptr-updater tr)))

      (lambda (selector)
	(case selector
	  ((alloc)               alloc-trampoline)
	  ((arg-word arg-ieee32) (lambda (tr) (copy-dword tr) (set-arg-length! tr (+ (arg-length tr) 1))))
	  ((arg-word2 arg-ieee64) (lambda (tr) (copy-qword tr) (set-arg-length! tr (+ (arg-length tr) 2))))
	  ((ret-word)            (lambda (tr) (set-return-type! tr 'word)))
	  ((ret-ieee64)          (lambda (tr) (set-return-type! tr 'ieee64)))
	  ((ret-ieee32)          (lambda (tr) (set-return-type! tr 'ieee32)))
	  ((ret-void)            (lambda (tr) (set-return-type! tr 'void)))
	  ((ret-word2)           (lambda (tr) (set-return-type! tr 'word2)))
	  ((change-fptr)         change-fptr)
	  ((done)                callout-done)
	  ((done-pasteup)        (lambda (tr) #t))
	  (else 
	   (error "i386-ffi: callout: bad selector " selector))))))
  
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
  ;     larceny_C_ffi_convert_and_call( _proc, _args, &_r, _adesc, TDESC(t0), n );
  ;     return r;
  ;  }
  ;
  ;
  ; callback:                     
  ;     pushl	%ebp
  ;     movl	%esp, %ebp
  ;     subl	$72, %esp
                                  ; 0(ebp) is ebp_old, 4(ebp) is retaddr
                                  ; 8(ebp) is first C invocation argument
;;; FSK added:
  ;     leal      8($ebp), $ecx
  ;     leal    -4-SUMARGSIZE($ebp), $edx ; -4(ebp) is for _ret; rest is for _args
  ;  FOREACH arg in 0 .. args
  ;     movl    $ecx, ($edx)
  ;     addl    $ecx, ARGSIZE(arg)
  ;     addl    $edx, ARGSIZE(arg)
  ;  ENDFOREACH 

  ;     movl	$5, 20(%esp)      ; 6TH = argc;
  ;     movl	$0, 16(%esp)      ; 5TH = TDESC(t0);
  ;     movl	$709468, 12(%esp) ; 4TH = _adesc;
  ;     leal	-4(%ebp), %eax    ; eaxtmp = &_r;
  ;     movl	%eax, 8(%esp)     ; 3RD = &_r;   (eaxtmp)
  ;     leal	-4-SUMARGSIZE(%ebp), %edx   ; edxtmp = _args;
  ;     movl	%edx, 4(%esp)     ; 2ND = _args; (edxtmp)
  ;     movl	$38668, (%esp)    ; 1ST = _proc;
  ;     call	larceny_C_ffi_convert_and_call
  ;     movl	-4(%ebp), %eax
  ;     leave
  ;     ret

  (define (make-callback-stdabi stdcall?)
    (define (callback-arg-words tr word-count)
      (tr-at-end 
       tr
       (list->bytevector
        `(#x89 #xA                       ;; (mov (& edx) ecx)
          #x83 #xC1 ,(* 4 word-count)    ;; (add ecx 4*word-count)
          #x83 #xC2 4)))                 ;; (add edx 4)
      (set-arg-length! tr (+ (arg-length tr) word-count)))
    (define (callback-arg-word tr) 
      (callback-arg-words tr 1))
    (define (callback-arg-ieee64 tr) 
      (callback-arg-words tr 2))
    (define (callback-ret-type tr type code)
      (set-return-type! tr type)
      (set-return-encoding! tr code))
    (define (callback-done tr argc) 
      (if (eq? (return-type tr) 'ieee64)
          (error "i386 callback: ieee64 return not implemented yet."))
      (if (not (integer? argc))
          (error "i386 callback: argc must be non-neg integer."))
      (if (not (integer? (return-encoding tr)))
          (error "i386 callback: return-encoding must be integer."))
      (if (not (integer? (arg-types tr)))
          (error "i386 callback: arg-types must be integer."))

      (let ((arg-size-byte (fxlogand (- (+ 4 (* 4 (arg-length tr)))) #xff)))
        (tr-at-beginning 
         tr
         (list->bytevector
          `(#x55                         ;; (push ebp)
            #x89 #xe5                    ;; (mov ebp esp)
            #x83 #xec 72                 ;; (sub esp 72)
            
            #x8d #x4d 8                  ;; (lea ecx (& 8 ebp))
            #x8d #x55 ,arg-size-byte     ;; (lea edx (& ,arg-size ebp))
            )))

        (tr-at-end
         tr
         (list->bytevector
          `(#xc7 #x44 #x24 #x14 ,@(dword argc) ;; (mov (& 20 esp) ,argc)
                                               ;; (mov (& 16 esp) ,TDESC(t0))
            #xc7 #x44 #x24 #x10 ,@(dword (return-encoding tr))
            #xa1 ,@(dword (arg-types tr))      ;; (mov eax (& ,_adesc))
            #x89 #x44 #x24 #x0c                ;; (mov (& 12 esp) eax)
            #x8d #x45 #xfc                     ;; (lea eax (& -4 ebp)) ;; &_r
            #x89 #x44 #x24 #x08                ;; (mov (& 8 esp) eax)
            #x8d #x55 ,arg-size-byte           ;; (lea edx (& ,arg-size ebp))
            #x89 #x54 #x24 #x04                ;; (mov (& 4 esp) edx)
                                               ;; (mov (& esp) ,_proc)
            #xc7 #x04 #x24 ,@(dword (proc-addr tr))

            #xb8 ,@(dword (tr-fptr tr))        ;; (mov eax ,fptr)
            #xff #xd0                          ;; (call eax)
            
            #x8b #x45 #xfc                     ;; (mov eax (& -4 ebp))
            #xc9                               ;; (leave)
            #xc3                               ;; (ret)
            )))))
    (lambda (selector)
      (cond 
       (stdcall?
        (error "Callback abi not implemented for stdcall."))
       (else
        (case selector
          ((alloc)                 alloc-trampoline)
          ((arg-word arg-ieee32)   callback-arg-word)
          ((arg-ieee64)            callback-arg-ieee64)
          ((proc-addr)             (lambda (tr addr) (set-proc-addr! tr addr)))
          ((arg-types)             (lambda (tr bv) (set-arg-types! tr bv)))
          ((ret-type)              callback-ret-type)
          ((done)                  callback-done)
          ((done-pasteup)          (lambda (tr) 'no-need-to-iflush-on-ia32))
          (else
           (error "i386 callback: bad selector " selector)))))))

  (set! ffi/i386-C-callout-stdcall (make-callout-stdabi #t))
  (set! ffi/i386-C-callback-stdcall (make-callback-stdabi #t))
  (set! ffi/i386-C-callout-cdecl (make-callout-stdabi #f))
  (set! ffi/i386-C-callback-cdecl (make-callback-stdabi #f))
  #t)

; eof


