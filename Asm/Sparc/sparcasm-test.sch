; Asm/Sparc/sparcasm-test.sch
; Larceny -- Sparc machine assembler test code
;
; $Id: sparcasm-test.sch,v 1.1.1.1 1998/11/19 21:51:59 lth Exp $

(define (sparcasm-test)

  ; Format as hex, with leading zeroes.

  (define (format-instr instr)
    (let ((s (number->string (asm:bv->int instr) 16)))
      (string-insert! s (- 8 (string-length s)) (make-string 8 #\0))))

  ; An 'answer' of #f means that the assembly should fail with a call
  ; to asm-error (tests error handling).  An 'answer' of #t means that
  ; the assembly should fail with a call to asm-value-too-large.  The
  ; answer can also be a list of instructions.  FIXME.

  (define (test-instruction id proc answer)
    (let ((as (make-assembly-structure '() #f)))
      (proc as)
      (let ((segment (assemble-pasteup as)))
	(assemble-finalize! as)
	(let ((instr (bytevector-word-ref (car segment) 0)))
	  (if (not (equal? instr answer))
	      (error "Test failed: #" id
		     "; instruction is " (format-instr instr) 
		     "; answer should be " (format-instr answer)))))))

  (define ibit (asm:bv 0 0 #x20 0))	; immediate bit: 2^13
  (define abit (asm:bv #x20 0 0 0))	; annul bit: 2^29

  (define (rs1-op r) (asm:lsh r 14))
  (define (rs2-op r) r)
  (define (rd-op r)  (asm:lsh r 25))
  (define (imm-op v) (asm:lobits v 13))

  (test-instruction 1
		    (lambda (as) (sparc.sethi as 1234 $r.reg6))
		    (asm:logior (rd-op $r.reg6) (asm:lsh #b100 22) 1234))

  (test-instruction 2
		    (lambda (as)
		      (sparc.ba as 'L1)
		      (sparc.nop as)
		      (sparc.label 'L1))
		    (list (asm:logior (asm:lsh #b1000 25)
				      (asm:lsh #b010 22)
				      2)
			  (asm:lsh #b100 22)))

  ; Here, the branch at L1 should not be lifted into the slot of the first
  ; branch.

  (test-instruction 3
		    (lambda (as)
		      (sparc.ba.a as 'L1)
		      (sparc.slot as)
		      (sparc.label 'L1)
		      (sparc.ba.a as 'L1)
		      (sparc.nop as))
		    (list ...))

  #t)


; Belongs elsewhere.

(define (string-insert! src loc target)
  (let ((l (string-length src)))
    (do ((i 0 (+ i 1))
	 (n loc (+ n 1)))
	((= i l) target)
      (string-set! target n (string-ref src i)))))

; eof



