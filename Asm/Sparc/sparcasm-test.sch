; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Sparc machine assembler test code -- not completed.

(define (sparcasm-test)

  ; Format as hex, with leading zeroes.

  (define (format-instr instr)
    (format-right-justified (number->string (asm:bv->int instr) 16) 8 #\0))

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


(define (sparcasm-floating-point-test)
  (test-asm (lambda (as)
	      (sparc.lddfi as $r.reg6 3 2)
	      (sparc.lddfr as $r.reg6 $r.tmp0 2)
	      (sparc.fnegd as 2 2)
	      (sparc.fnegd as 2 4)
	      (sparc.fmovd as 2 4)
	      (sparc.fabsd as 2 2)
	      (sparc.fabsd as 2 4)
	      (sparc.fcmpd as 2 4)
	      (sparc.nop   as)
	      (sparc.fbne.a as 0)
	      (sparc.slot  as))))

; eof
