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
  (define field-count (+ *trampoline-basis-size* 2))

  (define (return-type tr) (vector-ref tr base))
  (define (arg-length tr) (vector-ref tr (+ base 1)))

  (define (set-return-type! tr val) (vector-set! tr base val))
  (define (set-arg-length! tr val) (vector-set! tr (+ base 1) val))

  (define two^24 (expt 2 24))
  (define two^16 (expt 2 16))
  (define two^8  (expt 2 8))

  (define (alloc-trampoline)
    (let ((tr (make-vector field-count)))
      (tr-init-common tr)
      (set-return-type! tr 'unknown)
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
    (list (remainder n two^8)
          (remainder (quotient n two^8) two^8)
          (remainder (quotient n two^16) two^8)
          (quotient n two^24)))

  ; Callout trampoline
  ;
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

      (define (callout-done tr)
        (tr-at-beginning tr
          (list->bytevector
           `(#x55                                       ; PUSH EBP              standard prologue
             #x89 #xE5                                  ; MOV EBP, ESP
             #x53                                       ; PUSH EBX
             #x56                                       ; PUSH ESI
             #x8B #x75 #x08                             ; MOV ESI, [EBP+8]      load argv
             #x81 #xEC ,@(dword (* 4 (arg-length tr)))  ; SUB ESP, 4*argc       allocate space for args
             #x8B #xFC                                  ; MOV EDI, ESP          copy pointer
             #xFC)))                                    ; CLD                   copy upward

        (tr-at-end tr
          (list->bytevector
           `(#xB8 ,@(dword (tr-fptr tr))                ; MOV EAX, <loc>        call the
             #xFF #xD0                                  ; CALL EAX                procedure
             #x81 #xC4 ,@(dword (* 4 (arg-length tr)))  ; ADD ESP, 4*argc       pop args
             #x8B #x5D #x0C                             ; MOV EBX, [EBP+12]     get return pointer
             ,@(case (return-type tr)
                 ((word)   '(#x89 #x03))                ; MOV [EBX], EAX
                 ((ieee64) '(#xDD #x1B))                ; FSTP QWORD PTR [EBX]
                 ((ieee32) '(#xD9 #x1B))                ; FSTP DWORD PTR [EBX]
                 ((void)   '())
                 (else ???))
             #x5E                                       ; POP ESI               standard epilogue
             #x5B                                       ; POP EBX
             #x5D                                       ; POP EBP
             ,@(if stdcall?
                   '(#xC2 #x08)                         ; RETURN 8              callee pops
                   '(#xC3)))))                          ; RETURN                caller pops
        #t)

      (define (copy-dword tr)
        (tr-at-end tr
          (list->bytevector
           `(#xA5                                       ; MOVS  DWORD PTR [ESI], DWORD PTR [EDI]
             #x83 #xC6 #x04))))                         ; ADD   ESI, 4

      (define (copy-qword tr)
        (tr-at-end tr
          (list->bytevector
           `(#xA5                                       ; MOVS  DWORD PTR [ESI], DWORD PTR [EDI]
             #xA5))))                                   ; MOVS  DWORD PTR [ESI], DWORD PTR [EDI]

      (lambda (selector)
        (case selector
          ((alloc)               alloc-trampoline)
          ((arg-word arg-ieee32) (lambda (tr) (copy-dword tr) (set-arg-length! tr (+ (arg-length tr) 1))))
          ((arg-ieee64)          (lambda (tr) (copy-qword tr) (set-arg-length! tr (+ (arg-length tr) 2))))
          ((ret-word)            (lambda (tr) (set-return-type! tr 'word)))
          ((ret-ieee64)          (lambda (tr) (set-return-type! tr 'ieee64)))
          ((ret-ieee32)          (lambda (tr) (set-return-type! tr 'ieee32)))
          ((ret-void)            (lambda (tr) (set-return-type! tr 'void)))
          ((change-fptr)         (lambda (tr) (error "i386-ffi: callout: change-fptr unsupported.")))
          ((done)                callout-done)
          ((done-pasteup)        (lambda (tr) #t))
          (else
           (error "i386-ffi: callout: bad selector " selector))))))

  (define (make-callback-stdabi stdcall?)
    (let ()
      (lambda (selector)
        (error "Callback abi not implemented."))))

  (set! ffi/i386-C-callout-stdcall (make-callout-stdabi #t))
  (set! ffi/i386-C-callback-stdcall (make-callback-stdabi #t))
  (set! ffi/i386-C-callout-cdecl (make-callout-stdabi #f))
  (set! ffi/i386-C-callback-cdecl (make-callback-stdabi #f))
  #t)

; eof

