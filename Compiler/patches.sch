; Temporary patches to use the new (May 1995) version of Twobit.

; Someone has already redefined Chez Scheme's sort procedure.

(define (twobit-sort p? x) (sort x p?))

; Changes to Sparcasm/gen-msi.sch.

; SAVE
;
; Create stack frame.  To avoid confusing the garbage collector, the
; slots must be initialized to something definite unless they will
; immediately be initialized by a MacScheme machine store instruction.
; The creation is done by emit-save0!, and the initialization is done
; by emit-save1!.

(define (emit-save0! as n)
  (let* ((l         (new-label))
	 (l0        (new-label))
	 (framesize (+ 8 (* (+ n 1) 4)))
	 (realsize  (roundup8 (+ framesize 4))))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.subi ,$r.stkp ,realsize ,$r.stkp))
    (emit! as `(,$i.subrcc ,$r.stklim ,$r.stkp ,$r.g0))
    (emit! as `(,$i.ble.a ,l))
    (emit! as `(,$i.ori ,$r.g0 ,framesize ,$r.tmp0))
    (emit! as `(,$i.addi ,$r.stkp ,realsize ,$r.stkp))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.stkoflow ,$r.o7))
    (emit! as `(,$i.subi ,$r.o7 (+ (- $ ,l0) 4) ,$r.o7))
    (emit! as `(,$i.label ,l))
    ; initialize size and return fields of stack frame
    (emit! as `(,$i.sti ,$r.tmp0 0 ,$r.stkp))
    (emit! as `(,$i.sti ,$r.g0 4 ,$r.stkp))))

; Given a vector v of booleans, initializes slot i of the stack frame
; if and only if (vector-ref v i).

(define (emit-save1! as v)
  (let ((n (vector-length v)))
    (let loop ((i 0) (offset 12))
      (cond ((= i n)
             #t)
            ((vector-ref v i)
	     (emit! as `(,$i.sti ,$r.g0 ,offset ,$r.stkp))
	     (loop (+ i 1) (+ offset 4)))
	    (else
	     (loop (+ i 1) (+ offset 4)))))))

; POP
;
; Pop frame.
; If returning?, then emit the return as well and put the pop
; in its delay slot.

(define (emit-pop! as n returning?)
  (let* ((framesize (+ 8 (* (+ n 1) 4)))
	 (realsize  (roundup8 (+ framesize 4))))
    (if returning?
        (begin (emit! as `(,$i.ldi ,$r.stkp ,(+ realsize 4) ,$r.o7))
               (emit! as `(,$i.jmpli ,$r.o7 8 ,$r.g0))
               (emit! as `(,$i.addi ,$r.stkp ,realsize ,$r.stkp)))
        (emit! as `(,$i.addi ,$r.stkp ,realsize ,$r.stkp)))))

