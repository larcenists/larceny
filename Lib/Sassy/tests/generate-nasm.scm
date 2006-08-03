;loading this file populates the tests directory with .asm files and
;calls nasm to assemble them

;note: the files for seg.scm brt.scm isn't included with this

; ,open c-system-function

(define (andmap p lst)
  (call-with-current-continuation
   (lambda (lose)
     (let iter ((lst lst))
       (cond ((null? lst) '())
	     ((p (car lst)) => (lambda (this) (cons this (iter (cdr lst)))))
	     (else (lose #f)))))))

(define opcode-files
  (list
   "tests/mem-ref.scm"
   "tests/non.scm"
   "tests/alu.scm"
   "tests/bt.scm"
   "tests/shift.scm"
   "tests/setcc.scm"
   "tests/cmovcc.scm"
   "tests/decinc.scm"
   "tests/plier.scm"
   "tests/load.scm"
   "tests/movx.scm"
   "tests/r-rm.scm"
   "tests/rm.scm"
   "tests/rm2.scm"
   "tests/aa.scm"
   "tests/ret.scm"
   "tests/doub-shift.scm"
   "tests/cmpx.scm"
   "tests/misc1.scm"
   "tests/misc2.scm"
   "tests/misc3.scm"
   "tests/jcc.scm"
   "tests/jumps.scm"
   "tests/prefix.scm"
   "tests/fp0.scm"
   "tests/fp1.scm"
   "tests/fp2.scm"
   "tests/fp3.scm"
   "tests/mmx.scm"
   "tests/sse1.scm"
   "tests/sse2.scm"
   "tests/sse3.scm"
   "tests/seg.scm"
   ))

(define opcode16-files
  (list
   "tests/alu.scm"
   "tests/bt.scm"
   "tests/cmpx.scm"
   "tests/decinc.scm"
   "tests/doub-shift.scm"
   "tests/jcc.scm"
   "tests/jumps.scm"
   "tests/load.scm"
   "tests/mem-ref.scm"
   "tests/misc1.scm"
   "tests/misc2.scm"
   "tests/movx.scm"
   "tests/non.scm"
   "tests/plier.scm"
   "tests/prefix.scm"
   "tests/ret.scm"
   "tests/rm2.scm"
   "tests/rm.scm"
   "tests/r-rm.scm"
   "tests/setcc.scm"
   "tests/shift.scm"
   "tests/seg.scm"))

(define (list-fill lst filler)
  (cond ((null? lst) '())
	((null? (cdr lst)) lst)
	(else (cons (car lst) (cons filler (list-fill (cdr lst) filler))))))

(define (sassy->nasm file lst bits)
  (with-output-to-file file
    (lambda ()
      (letrec
	  ((outs (lambda x (for-each display x) (newline)))
	   (i16 (meta-lambda
		 (or ,@num
		     ,@symb
		     (and 'word num)
		     (and 'word symb))))
	   (i32 (meta-lambda
		 (or ,@num
		     ,@symb
		     (and 'dword num)
		     (and 'dword symb))))
	   (prefix? (lambda (x)
		      (and (memq x '(rep repe repne repz repnz lock))
			   (symbol->string x))))

	   (a-sassy
	    (meta-lambda
	     (or
	      (and 'jmp num i32 (lambda (x y) (outs "jmp dword " x ":" y)))
	      (and 'jmp num i16 (lambda (x y) (outs "jmp word " x ":" y)))
	      (and 'call num i32 (lambda (x y) (outs "call dword " x ":" y)))
	      (and 'call num i16 (lambda (x y) (outs "call word " x ":" y)))
	      (and prefix? pair? (lambda (x y) (begin (outs x " ")
						      (a-sassy y))))
	      (and symb 'near rand (lambda (x y) (outs x " near " y)))
	      (and symb 'short rand (lambda (x y) (outs x " short " y)))
	      (and symb 'far rand (lambda (x y) (outs x " far " y)))
	      (and symb (lambda (x) (outs x)))
	      (and symb rand (lambda (x y) (outs x " " y)))
	      (and symb rand rand (lambda (x y z) (outs x " " y ", " z)))
	      (and symb rand rand rand (lambda (u v w x)
					  (outs u " " v ", " w ", " x))))))
	   (reg (lambda (x)
		  (and (memq x '(mm7 mm6 mm5 mm4 mm3 dr7 mm2 dr6 mm1 mm0 cr4
				     dr3 cr3 dr2 cr2 dr1 dr0 cr0 xmm7 xmm6
				     xmm5 xmm4 xmm3 xmm2 xmm1 xmm0 ss sp dx si
				     cx bx gs ax fs es ds cs bp dl cl bl al di
				     dh ch bh ah esp edx esi ecx ebx eax ebp
				     edi st7 st6 st5 st4 st3 st2 st1 st0))
		       (symbol->string x))))
	   (symb (lambda (x) (and (symbol? x) (not (reg x))
				  (symbol->string x))))
	   (num  (lambda (x) (and (number? x) (number->string x))))
	   (segp (lambda (x)
		   (and (memq x '(cs ds ss es fs gs))
			(string-append (symbol->string x) ":"))))
	   (skale
	    (meta-lambda
	     (or (and '* num reg (lambda (x y) (string-append x "*" y)))
		 (and '* reg num (lambda (x y) (string-append x "*" y))))))
	   (mem
	    (meta-lambda
	     (or
	      (and segp mem
		   (lambda (x y)
		     (if (and (> (string-length y) 7)
			      (string=? "[dword " (substring y 0 7)))
			 (string-append "[dword " x
					(substring y 7 (string-length y)))
			 (string-append "[" x
					(substring y 1 (string-length y))))))
	      (and '& __
		   (lambda x
		     (cond ((andmap (lambda (i) (or (num i) (symb i))) x) =>
			    (lambda (lst)
			      (apply string-append
				     "[dword "
				     (append (list-fill lst "+") (list "]")))))
			   (else
			    (let* ((nums (fold (lambda (f r)
						 (if (number? f)
						     (if (number? r)
							 (+ f r)
							 f)
						     r))
					       '()
					       x))
				   (dword (or (and (not (null? nums))
						   (not (u/s-byte nums)))
					      (any symb x)))
				   (itms (fold-right (lambda (x r)
						       (cond
							((or (reg x)
							     (skale x) (symb x))
							 => (lambda (i)
							      (cons i r)))
							(else r)))
						     '()
						     x))
				   (lst (if (null? nums)
					    itms
					    (cons (number->string nums) itms))))
			      (if dword
				  (apply string-append
					 "[dword "
					 (append (list-fill lst "+")
						 (list "]")))
				  (apply string-append
					 "[" (append (list-fill lst "+")
						     (list "]")))
				  )))))))))
	   (sizer (lambda (x)
		    (and (memv x '(byte word dword qword tword dqword))
			 (symbol->string x))))
	   (rand
	    (meta-lambda
	     (or ,@reg
		 ,@num
		 ,@mem
		 ,@symb
		 (and sizer (or num mem reg symb)
		      (lambda (x y)
			(string-append x " " y)))))))
	(display bits)
	(newline)
	(display "section .text")
	(newline)
	(display "foo:")
	(newline)
	(for-each a-sassy lst)))))

(define (gen-file x bits32?)
  (let* ((nasm-asm (string-append
		    (substring x 0 (- (string-length x) 4))
		    (if bits32? ".asm" "16.asm")))
	 (nasm-out (string-append
		    (substring x 0 (- (string-length x) 4))
		    (if bits32? "" "16")))
	 (the-codes (with-input-from-file x (lambda () (read))))
	 (nasm-com (string-append "nasm -f bin " nasm-asm)))
    (and (file-exists? nasm-asm) (delete-file nasm-asm))
    (and (file-exists? nasm-out) (delete-file nasm-out))
    (sassy->nasm nasm-asm the-codes (if bits32? "BITS 32" "BITS 16"))
    (system nasm-com)))

; (for-each (lambda (x) (gen-file x #t)) opcode-files)
; (for-each (lambda (x) (gen-file x #f)) opcode16-files)
 
