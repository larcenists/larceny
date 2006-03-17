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

 (seq (= ecx "DOIT")
      (my-local foo
		(pop eax)
		(cmp eax 0)
		(jnz foo)))
 (nop)
 (jmp foo))

; foo:
; 00000000  C3                ret
; 00000001  81F9444F4954      cmp ecx,0x54494f44
; 00000007  750A              jnz 0x13

; _label1:
; 00000009  58                pop eax
; 0000000A  83F800            cmp eax,byte +0x0
; 0000000D  0F85F6FFFFFF      jnz near 0x9
; 00000013  90                nop
; 00000014  E9E7FFFFFF        jmp 0x0

