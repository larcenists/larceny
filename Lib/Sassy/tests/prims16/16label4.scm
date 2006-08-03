(bits 16)

; local labels via brute force renaming

(! (begin (define my-gensym  ; assuming eval allows non-expressions (eg define)
	    (let ((count 1))
	      (lambda ()
		(let ((new (string->symbol (string-append
					    "_label"
					    (number->string count)))))
		  (set! count (+ count 1))
		  new))))
	  'void))

(macro my-local (lambda (name . body)
		  (define new-sym (my-gensym))
		  ; do the substitution manually,
		  ; instead of relying on a macro to do it,
		  ; because 'direcs' is a directive
		  ; and we want to be able use this in a nested context
		  (define (deep-replace new old lst)
		    (cond ((pair? lst) (map (lambda (x)
					      (deep-replace new old x))
					    lst))
			  ((eqv? old lst) new)
			  (else lst)))
		  `(label ,new-sym ,@(deep-replace new-sym name body))))


(text
 (label foo (ret))

 (seq (= cx "DO")
      (my-local foo
		(pop ax)
		(cmp ax 0)
		(jnz foo)))
 (nop)
 (jmp foo))

; 00000000  C3                ret
; 00000001  81F9444F          cmp cx,0x4f44
; 00000005  7508              jnz 0xf
; 00000007  58                pop ax
; 00000008  3D0000            cmp ax,0x0
; 0000000B  0F85F8FF          jnz near 0x7
; 0000000F  90                nop
; 00000010  E9EDFF            jmp 0x0

