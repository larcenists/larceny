; -*- Scheme -*-
;
; Scheme 313 compiler.
; Emitting code for integrables.
;
; $Id$
;
; Temp-register allocation here is completely out of hand. We have to come
; up with a coherent strategy for allocating temporary registers, e.g. a
; set of registers to pick from when one is needed.
; Also, too many implicit registers are assumed (this is definitely related
; to the allocation mess) and should rather be passed/returned as appropriate.
; The key problem is to never load a memory-mapped register or a datum more
; than once. Remember that calling a subroutine which returns a register may
; return a temp which cannot subsequently be allocated.
;
; BUGS
; - Some should go into millicode; the fully checking (byte)vector operations
;   come to mind.
; - Like mentioned above, temp register allocation is gross.
; - Some operations do too much work; for example, vector-ref will load the
;   vector header twice.

(define (emit-primop0! as op)
  ((cdr (assq op primop-list)) as))

(define (emit-primop1! as op r)
  ((cdr (assq op primop-list)) as r))

(define (emit-primop2! as op r1 r2)
  ((cdr (assq op primop-list)) as r1 r2))


; Assoc list of primops with generating procedures.
; This is getting long; a better ordering may be beneficial to performance,
; if anyone cares.

(define primop-list
  (list 

   ; Number predicates

   (cons 'zero?
	 (lambda (as)
	   (emit-cmp-primop! as $i.tsubrcc $i.beq.a $m.zerop $r.g0)))

   (cons '=
	 (lambda (as r)
	   (emit-cmp-primop! as $i.tsubrcc $i.beq.a $m.numeq r)))

   (cons '<
	 (lambda (as r)
	   (emit-cmp-primop! as $i.tsubrcc $i.bl.a $m.numlt r)))

   (cons '<=
	 (lambda (as r)
	   (emit-cmp-primop! as $i.tsubrcc $i.ble.a $m.numle r)))

   (cons '>
	 (lambda (as r)
	   (emit-cmp-primop! as $i.tsubrcc $i.bg.a $m.numgt r)))

   (cons '>=
	 (lambda (as r)
	   (emit-cmp-primop! as $i.tsubrcc $i.bge.a $m.numge r)))

   (cons 'complex?
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.complexp ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'real?
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.realp ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'rational?
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.rationalp ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'integer?
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.integerp ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'exact?
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.exactp ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'inexact?
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.inexactp ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'fixnum?
	 (lambda (as)
	   (emit! as `(,$i.andicc ,$r.result 3 ,$r.g0))
	   (emit-set-bool! as)))

   ; -----------------------------------------------------------------

   ; Generic arithmetic and (bit-)logical operations

   (cons '+
	 (lambda (as r)
	   (emit-arith-primop! as $i.taddrcc $m.add r)))

   (cons '-
	 (lambda (as r)
	   (emit-arith-primop! as $i.tsubrcc $m.subtract r)))

   (cons '*
	 (lambda (as r)
	   (let ((tmp (if (hardware-mapped? r)
			  r
			  (emit-load-reg! as r $r.tmp0))))
	     (emit! as `(,$i.jmpli ,$r.millicode ,$m.multiply ,$r.o7))
	     (emit! as `(,$i.orr ,$r.g0 ,tmp ,$r.argreg2)))))

   (cons '/
	 (lambda (as x y)
	   (let ((tmp (if (hardware-mapped? r)
			  r
			  (emit-load-reg! as r $r.tmp0))))
	     (emit! as `(,$i.jmpli ,$r.millicode ,$m.divide ,$.o7))
	     (emit! as `(,$i.orr ,$r.g0 ,tmp ,$r.argreg2)))))

   (cons 'quotient
	 (lambda (as x y)
	   (let ((tmp (if (hardware-mapped? r)
			  r
			  (emit-load-reg! as r $r.tmp0))))
	     (emit! as `(,$i.jmpli ,$r.millicode ,$m.quotient ,$.o7))
	     (emit! as `(,$i.orr ,$r.g0 ,tmp ,$r.argreg2)))))

   (cons '--
	 (lambda (as)
	   (let ((l1 (new-label)))
	     (emit! as `(,$i.tsubcc ,$r.g0 ,$r.result, $r.tmp0))
	     (emit! as `(,$i.bvc.a ,l1))
	     (emit! as `(,$i.orr ,$r.g0 ,$r.tmp0 ,$r.result))
	     (emit! as `(,$i.jmpli ,$r.millicode ,$m.negate ,$r.o7))
	     (emit! as `(,$i.nop))
	     (emit! as `(,$i.label ,l1)))))

   (cons 'round
	 (lambda (as)
	   (silly 'round)))

   (cons 'truncate
	 (lambda (as)
	   (silly 'truncate)))

   (cons 'sqrt
	 (lambda (as)
	   (silly 'sqrt)))

   (cons 'lognot
	 (lambda (as)
	   (emit-assert-fixnum! as $r.result)
	   (emit! as `(,$i.ornr ,$r.result ,$r.g0 ,$r.result))
	   (emit! as `(,$i.xori ,$r.result 3 ,$r.result))))

   (cons 'logand
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit! as `(,$i.orr ,$r.result ,tmp ,$r.tmp0))
	     (emit-assert-fixnum! as $r.tmp0)
	     (emit! as `(,$i.andr ,$r.result ,x ,$r.result)))))

   (cons 'logior
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit! as `(,$i.orr ,$r.result ,tmp ,$r.tmp0))
	     (emit-assert-fixnum! as $r.tmp0)
	     (emit! as `(,$i.orr ,$r.result ,x ,$r.result)))))

   (cons 'logxor
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit! as `(,$i.orr ,$r.result ,tmp ,$r.tmp0))
	     (emit-assert-fixnum! as $r.tmp0)
	     (emit! as `(,$i.xorr ,$r.result ,tmp ,$r.result)))))

   (cons 'lsh
	 (lambda (as x)
	   (silly 'lsh)))

   (cons 'rot
	 (lambda (as x)
	   (silly 'rot)))

   ; -----------------------------------------------------------------

   ; Various type predicates

   (cons 'null?
	 (lambda (as)
	   (emit! as `(,$i.xoricc ,$r.result ,$imm.null ,$r.g0))
	   (emit-set-boolean! as)))

   (cons 'pair?
	 (lambda (as)
	   (emit-single-tagcheck->bool! as $tag.pair-tag)))

   (cons 'symbol?
	 (lambda (as)
	   (emit-vector-like? as $tag.symbol-typetag)))

   (cons 'char?
	 (lambda (as)
	   (emit! as `(,$i.andi ,$r.result #xFF ,$r.tmp0))
	   (emit! as `(,$i.xoricc ,$r.tmp0 ,$imm.char ,$r.g0))
	   (emit-set-boolean! as)))

   (cons 'string?
	 (lambda (as)
	   (emit-double-tagcheck->bool! as
					$tag.bytevector-tag
					(+ $bytevector-hdr
					   $tag.string-typetag))))

   (cons 'bytevector?
	 (lambda (as)
	   (emit-double-tagcheck->bool! as
					$tag.bytevector-tag
					(+ $tag.bytevector-hdr
					   $tag.bytevector-typetag))))

   (cons 'bytevector-like?
	 (lambda (as)
	   (emit-single-tagcheck->bool as $tag.bytevector-tag)))

   (cons 'vector?
	 (lambda (as)
	   (emit-double-tagcheck->bool! as
					$tag.vector-tag
					(+ $tag.vector-hdr
					   $tag.vector-typetag))))

   (cons 'vector-like?
	 (lambda (as)
	   (emit-single-tagcheck->bool! as $tag.vector-tag)))

   (cons 'procedure?
	 (lambda (as)
	   (emit-single-tagcheck->bool! as $tag.procedure-tag)))

   ; -----------------------------------------------------------------

   ; Pair operations.

   (cons 'cons
	 ; really should be open-coded
	 (lambda (as r)
	   (emit! as `(,$i.orr ,$r.result ,$r.g0 ,$r.argreg2))
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.alloc ,$r.o7))
	   (emit! as `(,$i.ori ,$r.g0 8 ,$r.result))
	   (emit! as `(,$i.sti ,$r.argreg2 0 ,$r.result))
	   (emit! as `(,$i.sti ,r 4 ,$r.result))
	   (emit! as `(,$i.addi ,$r.result ,$tag.pair ,$r.result))))

   (cons 'car
	 (lambda (as)
	   (emit-single-tagcheck-assert! as $tag.pair)
	   (emit! as `(,$i.ldi ,$r.result ,(- 0 $tag.pair) ,$r.result))))

   (cons 'cdr
	 (lambda (as)
	   (emit-single-tagcheck-assert! as $tag.pair)
	   (emit! as `(,$i.ldi ,$r.result ,(- 4 $tag.pair) ,$r.result))))

   (cons 'set-car!
	 (lambda (as x)
	   (emit-single-tagcheck-assert! as $tag.pair-tag)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.setcar ,$r.o7))
	   (if (hardware-mapped? x)
	       (emit! as `(,$i.orr ,$r.g0 ,x ,$r.argreg2))
	       (emit-load-reg as x $r.argreg2))))

   (cons 'set-cdr!
	 (lambda (as x)
	   (emit-single-tagcheck-assert! as $tag.pair-tag)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.setcdr ,$r.o7))
	   (if (hardware-mapped? x)
	       (emit! as `(,$i.orr ,$r.g0 ,x ,$r.argreg2))
	       (emit-load-reg! as x $r.argreg2))))

   ; Cells are internal data structures, represented using pairs.
   ; No error checking is done on cell references.

   (cons 'make-cell
	 ; this really should be open-coded.
	 (lambda (as)
	   (emit! as `(,$i.orr ,$r.result ,$r.g0 ,$r.argreg2))
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.alloc ,$r.o7))
	   (emit! as `(,$i.ori ,$r.g0 8 ,$r.result))
	   (emit! as `(,$i.sti ,$r.argreg2 0 ,$r.result))
	   (emit! as `(,$i.sti ,$r.g0 4 ,$r.result))
	   (emit! as `(,$i.addi ,$r.result ,$tag.pair ,$r.result))))

   (cons 'cell-ref
	 (lambda (as)
	   (emit! as `(,$i.ldi ,$r.result ,(- $tag.pair) ,$r.result))))

   (cons 'cell-set!
	 (lambda (as r)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.setcar ,$r.o7))
	   (if (hardware-mapped? r)
	       (emit! as `(,$i.orr ,$r.g0 ,r ,$r.argreg2))
	       (emit-load-reg! as r $r.argreg2))))

   ;-----------------------------------------------------------------

   ; Hooks to various system services

   (cons 'debugvsm
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.debug ,$ro7))
	   (emit! as `(,$i.nop))))

   (cons 'reset
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.reset ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'exit
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.exit ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'break
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.break ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'time
	 (lambda (as)
	   (silly 'time)))

   (cons 'gc
	 (lambda (as)
	   (silly 'gc)))

   (cons 'dumpheap 
	 (lambda (as)
	   (silly 'dumpheap)))

   ; -----------------------------------------------------------------

   ; Continuations.

   (cons 'creg
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode 
				 ,$m.capture-continuation 
				 ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'creg-set!
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode
				 ,$m.restore-continuation
				 ,$r.o7))
	   (emit! as `(,$i.nop))))

   ; -----------------------------------------------------------------

   ; Typetags

   (cons 'typetag
	 (lambda (as)
	   (emit! as `(,$i.jmpli $r.millicode ,$m.typetag ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'typetag-set!
	 (lambda (as r)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.typetag-set ,$r.o7))
	   (if (hardware-mapped? r)
	       (emit! as `(,$i.orr ,$r.g0 ,r ,$r.argreg2))
	       (emit! as `(,$i.ldi ,$r.globals ,(offsetof r) ,$r.argreg2)))))

   ; -----------------------------------------------------------------

   ; Misc.

   (cons 'exact->inexact
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.exact->inexact ,$r.g0))
	   (emit! as `(,$i.nop))))

   (cons 'inexact->exact
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.inexact->exact ,$r.g0))
	   (emit! as `(,$i.nop))))

   ; These have to work on both compnums and rectnums.

   (cons 'real-part
	 (lambda (as)
	   (silly 'real-part)))

   (cons 'imag-part
	 (lambda (as)
	   (silly 'imag-part)))

   (cons 'char->integer
	 (lambda (as)
	   (emit-assert-char! as $r.result)
	   (emit! as `(,$i.srli ,$r.result 14 ,$r.result))))

   (cons 'integer->char
	 (lambda (as)
	   (emit-assert-fixnum! as $r.result)
	   (emit! as `(,$i.andi ,$r.result #x3FF ,$r.result))
	   (emit! as `(,$i.shli ,$r.result 14 $r.result))
	   (emit! as `(,$i.ori  ,$r.result $imm.char ,$r.result))))

   (cons 'make-rectangular
	 (lambda (as x)
	   (silly 'make-rectangular)))

   (cons 'not
	 (lambda (as)
	   (emit! as `(,$i.subicc ,$r.result ,$imm.false ,$r.g0))
	   (emit-set-bool! as)))

   (cons 'eqv?
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp0)))
		 (l1  (new-label)))
	     (emit! as `(,$i.subicc ,$r.result ,tmp ,$r.g0))
	     (emit! as `(,$i.be.a ,l1))
	     (emit! as `(,$i.ori ,$r.g0 ,$imm.true ,$r.result))
	     (emit! as `(,$i.jmpli ,$r.millicode ,$m.eqv ,$r.o7))
	     (emit! as `(,$i.orr ,$r.g0 ,tmp ,$r.argreg2))
	     (emit! as `(,$i.label ,l1)))))

   ;-----------------------------------------------------------------

   ; String and bytevector operations

   (cons 'make-bytevector
	 (lambda (as)
	   (emit-make-bytevector-like! as
				       '()
				       (+ $tag.bytevector-hdr
					  $tag.bytevector-typetag)
				       $tag.bytevector-tag)))

   (cons 'make-string
	 (lambda (as r)
	   (emit-make-bytevector-like! as
				       r
				       (+ $tag.bytevector-hdr
					  $tag.string-typetag)
				       $tag.bytevector-tag)))

   (cons 'string-length
	 (lambda (as)
	   (emit-get-length! as
			     $tag.bytevector-tag
			     (+ $tag.bytevector-hdr $tag.string-typetag))))

   (cons 'bytevector-length
	 (lambda (as)
	   (emit-get-length! as 
			     $tag.bytevector-tag
			     (+ $tag.bytevector-hdr $tag.bytevector-typetag))))

   (cons 'bytevector-like-length
	 (lambda (as)
	   (emit-get-length! as $tag.bytevector-tag #f)))

   (cons 'string-ref
	 (lambda (as r)
	   (let* ((fault (emit-double-tagcheck-assert! 
			  as
			  $tag.bytevector-tag
			  (+ $tag.bytevector-hdr $tag.string-typetag)))
		  (r (if (not (hardware-mapped? r))
			 (emit-load-reg! as r $r.tmp0)
			 r)))
	     (emit-bytevector-like-ref! as r fault))))

   (cons 'bytevector-ref
	 (lambda (as r)
	   (let* ((fault (emit-double-tagcheck-assert!
			  as
			  $tag.bytevector-tag
			  (+ $tag.bytevector-hdr $tag.bytevector-typetag)))
		  (r (if (not (hardware-mapped? r))
			 (emit-load-reg! as r $r.tmp0)
			 r)))
	     (emit-bytevector-like-ref! as r fault))))

   (cons 'bytevector-like-ref
	 (lambda (as r)
	   (let* ((fault (emit-single-tagcheck-assert! as $tag.bytevector-tag))
		  (r (if (not (hardware-mapped? r))
			 (emit-load-reg! as r $r.tmp0)
			 r)))
	     (emit-bytevector-like-ref! as r fault))))

   (cons 'string-set!

	 ; Wrong! Must check for char, then convert to fixnum.
	 ; FIXME.
	 ; (Don't forget that that fixnum need not be checked. Altogether,
	 ; we should make this a special case completely.)

	 (lambda (as x)
	   (let ((fault (emit-double-tagcheck-assert!
			 as
			 $tag.bytevector-tag
			 (+ $tag.bytevector-hdr $tag.string-typetag))))
	     (display "warning: string-set! is not done")
	     (newline)
	     (emit-bytevector-like-set! as x y fault))))

   (cons 'bytevector-like-set!
	 (lambda (as x y)
	   (let ((fault (emit-single-tagcheck-assert! as $tag.bytevector-tag)))
	     (emit-bytevector-like-set! as x y fault))))

   (cons 'bytevector-set!
	 (lambda (as x y)
	   (let ((fault (emit-double-tagcheck-assert!
			 as
			 $tag.bytevector-tag
			 (+ $tag.bytevector-hdr $tag.bytevector-typetag))))
	     (emit-bytevector-like-set! as x y fault))))

   ;-----------------------------------------------------------------

   ; Vector and procedure operations

   (cons 'make-procedure
	 (lambda (as)
	   (emit-make-vector-like! as
				   '()
				   $tag.procedure-hdr
				   $tag.procedure-tag)))

   (cons 'make-vector
	 (lambda (as r)
	   (emit-make-vector-like! as
				   r
				   (+ $tag.vector-hdr $tag.vector-typetag)
				   $tag.vector-tag)))

   (cons 'vector-length
	 (lambda (as)
	   (emit-get-length! as
			     $tag.vector-tag
			     (+ $tag.vector-hdr $tag.vector-typetag))))

   (cons 'vector-like-length
	 (lambda (as)
	   (emit-get-length! as $tag.vector-tag #f)))

   (cons 'procedure-length
	 (lambda (as)
	   (emit-get-length! as $tag.procedure-tag #f)))

   (cons 'vector-ref
	 (lambda (as r)
	   (let ((fault (emit-double-tagcheck-assert!
			 as
			 $tag.vector-tag
			 (+ $tag.vector-hdr $tag.vector-typetag))))
	     (emit-vector-like-ref! as r fault $tag.vector-tag))))

   (cons 'vector-like-ref
	 (lambda (as r)
	   (let ((fault (emit-single-tagcheck-assert! as $tag.vector-tag)))
	     (emit-vector-like-ref! as r fault $tag.vector-tag))))

   (cons 'procedure-ref
	 (lambda (as x)
	   (let ((fault (emit-single-tagchech-assert! as $tag.procedure-tag)))
	     (emit-vector-like-ref! as r fault $tag.procedure-tag))))

   (cons 'vector-set!
	 (lambda (as r1 r2)
	   (let ((fault (emit-double-tagcheck-assert!
			 as
			 $tag.vector-tag
			 (+ $tag.vector-hdr $tag.vector-typetag))))
	     (emit-vector-like-set! as r1 r2 fault $tag.vector-tag))))

   (cons 'vector-like-set!
	 (lambda (as r1 r2)
	   (let ((fault (emit-single-tagcheck-assert! as $tag.vector-tag)))
	     (emit-vector-like-set! as r1 r2 fault $tag.vector-tag))))

   (cons 'procedure-set!
	 (lambda (as x y)
	   (let ((fault (emit-single-tagcheck-assert! as $tag.procedure-tag)))
	     (emit-vector-like-set! as r1 r2 fault $tag.procedure-tag))))

   ; -----------------------------------------------------------------

   ; Character predicates

   (cons 'char<?
	 (lambda (as x)
	   (emit-char-cmp as x $i.blt.a)))

   (cons 'char<=?
	 (lambda (as x)
	   (emit-char-cmp as x $i.ble.a)))

   (cons 'char=?
	 (lambda (as x)
	   (emit-char-cmp as x $i.be.a)))

   (cons 'char>?
	 (lambda (as x)
	   (emit-char-cmp as x $i.bgt.a)))

   (cons 'char>=?
	 (lambda (as x)
	   (emit-char-cmp as x $i.bge.a)))

   ;-----------------------------------------------------------------
   
   ; These are introduced by peephole optimization; they evaluate a 
   ; boolean expression for control only.
   ; Slots can be filled from the branch target (and use annulls!).
   ; See comments for 'branch', above.

   (cons 'bfnull?
	 (lambda (as label)
	   (emit! as `(,$i.subicc ,$r.result ,$imm.null ,$r.g0))
	   (emit! as `(,$i.bne.a ,(make-label label)))
	   (emit! as `(,$i.slot))))

   (cons 'bfpair?
	 (lambda (as label)
	   (emit! as `(,$i.andi ,$r.result ,$tag.tagmask ,$r.tmp0))
	   (emit! as `(,$i.xoricc ,$r.tmp0 ,$tag.pair ,$r.g0))
	   (emit! as `(,$i.bne.a ,(make-label label)))
	   (emit! as `(,$i.slot))))

   (cons 'bfzero?
	 (lambda (as label)
	   (emit-bcmp-primop! as $i.bne.a $r.g0 label $m.zerop)))

   (cons 'bf=
	 (lambda (as r label)
	   (emit-bcmp-primop! as $i.bne.a r label $m.numeq)))

   (cons 'bf<
	 (lambda (as r label)
	   (emit-bcmp-primop! as $i.bge.a r label $m.numlt)))

   (cons 'bf<=
	 (lambda (as r label)
	   (emit-bcmp-primop! as $i.bg.a r label $m.numle)))

   (cons 'bf>
	 (lambda (as r label)
	   (emit-bcmp-primop! as $i.ble.a r label $m.numgt)))

   (cons 'bf>=
	 (lambda (as r label)
	   (emit-bcmp-primop! as $i.bl.a r label $m.numge)))

   ;-----------------------------------------------------------------

   ; These are used by the mal code for the i/o system, and never
   ; actually generated by the compiler (they should not figure in the
   ; integrables table!)

   (cons 'open-file
	 (lambda (as r)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.open-file ,$r.o7))
	   (emit! as `(,$i.orr ,r ,$r.g0 ,$r.argreg2))))

   (cons 'close-file
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.close-file ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'create-file
	 (lambda (as r)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.create-file ,$r.o7))
	   (emit! as `(,$i.orr ,r ,$r.g0 ,$r.argreg2))))

   (cons 'unlink-file
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.unlink-file ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'read-file
	 (lambda (as r1 r2)
	   (emit! as `(,$i.orr ,r1 ,$r.g0 ,$r.argreg2))
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.read-file ,$r.o7))
	   (emit! as `(,$i.orr ,r2 ,$r.g0 ,$r.argreg3))))

   (cons 'write-file
	 (lambda (as r1 r2)
	   (emit! as `(,$i.orr ,r1 ,$r.g0 ,$r.argreg2))
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.write-file ,$r.o7))
	   (emit! as `(,$i.orr ,r2 ,$r.g0 ,$r.argreg3))))))

; quite temporary

(define (silly name)
  (error 'silly "Silly, silly. (undefined integrable ~a)." name))

(define (emit-set-boolean! as)
  (let ((l1 (new-label)))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.true ,$r.result))
    (emit! as `(,$i.bne.a ,l1))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.false ,$r.result))
    (emit! as `(,$i.label ,l1))))

(define (emit-double-tagcheck->bool! as tag1 tag2)
  (let ((l1 (new-label)))
    (emit! as `(,$i.andi ,$r.result 7 ,$r.tmp0))
    (emit! as `(,$i.xoricc ,$r.tmp0 ,tag1 ,$r.g0))
    (emit! as `(,$i.bne.a ,l1))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.false ,$r.result))
    (emit! as `(,$i.ldbi ,$r.result (+ (- ,tag1) 3) ,$r.tmp0))
    (emit! as `(,$i.ori ,$r.g0 ,$r.true ,$r.result))
    (emit! as `(,$i.subicc ,$r.tmp0 ,tag2 ,$r.g0))
    (emit! as `(,$i.bne.a ,l1))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.false ,$r.result))
    (emit! as `(,$i.label ,l1))))

(define (emit-double-tagcheck-assert! as tag1 tag2)
  (let ((l0 (new-label))
	(l1 (new-label))
	(l2 (new-label)))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.andi ,$r.result 7 ,$r.tmp0))
    (emit! as `(,$i.xoricc ,$r.tmp0 ,tag1 ,$r.g0))
    (emit! as `(,$i.be.a ,l1))
    (emit! as `(,$i.ldbi ,$r.result (+ (- ,tag1) 3) ,$r.tmp0))
    (emit! as `(,$i.label ,l2))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.type-exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l0 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l1))
    (emit! as `(,$i.subicc ,$r.tmp0 ,tag2 ,$r.g0))
    (emit! as `(,$i.bne ,l2))
    (emit! as `(,$i.nop))
    l2))

(define (emit-single-tagcheck->bool! as tag)
  (emit! as `(,$i.andi ,$r.result 7 ,$r.tmp0))
  (emit! as `(,$i.xoricc ,$r.tmp0 ,tag ,$r.g0))
  (emit-set-boolean! as))

(define (emit-single-tagcheck-assert! as tag1)
  (let ((l0 (new-label))
	(l1 (new-label))
	(l2 (new-label)))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.andi ,$r.result 7 ,$r.tmp0))
    (emit! as `(,$i.xoricc ,$r.tmp0 ,tag1 ,$r.g0))
    (emit! as `(,$i.be.a ,l1))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.label ,l2))n
    (emit! as `(,$i.jmpli ,4r.millicode ,$m.type-exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l0 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l1))
    l2))

; Emit single instruction to load sw-mapped reg into another reg, and return
; the destination reg.
;
; This is just a convenient shorthand.

(define (emit-load-reg! as x rd)
  (emit! as `(,$i.ldi ,$r.globals ,(offsetof x) ,$r.tmp0))
  rd)

; Assert that a machine register has a fixnum in it.
; Returns the label of the fault code.

(define (emit-assert-fixnum! as reg)
  (let ((l0 (new-label))
	(l1 (new-label))
	(l2 (new-label)))
    (emit! as `(,$i.label ,l2))
    (emit! as `(,$i.andicc ,reg 3 ,$r.g0))
    (emit! as `(,$i.be.a ,l1))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.type-execption ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l2 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l1))
    l0))

; Assert that RESULT has a character in it.
; Returns the label of the fault code.

(define (emit-assert-char! as)
  (let ((l0 (new-label))
	(l1 (new-label))
	(l2 (new-label)))
    (emit! as `(,$i.label ,l1))
    (emit! as `(,$i.andi ,$r.result #xFF ,$r.tmp0))
    (emit! as `(,$i.subicc ,$r.tmp0 ,$imm.char ,$r.g0))
    (emit! as `(,$i.be.a ,l2))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.type-exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l1 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l2))
    l0))

; This is more expensive than what is good for it (5 cycles in the usual case),
; but there does not seem to be a better way. One could use tsubcc reg, g0, g0
; but the resulting branch chain is no better than what we have.

(define (emit-assert-positive-fixnum! as reg)
  (let ((l1 (new-label))
	(l2 (new-label)))
    (emit! as `(,$i.label ,l2))
    (emit! as `(,$i.sethi (hi #x80000003) ,$r.tmp0))
    (emit! as `(,$i.ori ,$r.tmp0 (lo #x80000003) ,$r.tmp0))
    (emit! as `(,$i.andrcc ,$r.result ,$r.tmp0 ,$r.g0))
    (emit! as `(,$i.be.a ,l1))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.type-exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l2 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l1))))


; Emit code which performs a comparison operation on %result and another
; register and which returns a boolean in %result.
;
; The extra cost of computing the boolean (rather than leaving it implicitly
; in the status register) is 3 cycles.

(define (emit-cmp-primop! as cmp test generic r)
  (let ((l1 (new-label))
	(l2 (new-label))
	(r  (if (not (hardware-mapped? r))
	        (begin (emit! as `(,$i.ldi ,$r.globals ,(offsetof r) ,$r.tmp1))
		       $r.tmp1)
		r)))
    (emit! as `(,cmp ,$r.result ,r ,$r.g0))
    (emit! as `(,$i.bvc.a ,l1))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.false ,$r.result))
    (emit! as `(,$i.orr ,r ,$r.g0 ,$r.argreg2))
    (emit! as `(,$i.jmpli ,$r.millicode ,generic ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l2 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l1))
    (emit! as `(,test ,l2))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.true ,$r.result))
    (emit! as `(,$i.label ,l2))))

; Possibly it would be better to unchain the branches and let slots be filled?

(define (emit-bcmp-primop! as ntest r label generic)
  (let ((l1 (new-label))
	(l2 (make-label label))
	(r  (if (not (hardware-mapped? r))
		(begin (emit! as `(,$i.ldi ,$r.globals ,(offsetof r) ,$r.tmp1))
		       $r.tmp1)
		r)))
    (emit! as `(,$i.tsubrcc ,$r.result ,r ,$r.g0))
    (emit! as `(,$i.bvc.a ,l1))
    (emit! as `(,ntest ,l2))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.jmpli ,$r.millicode ,generic ,$r.o7))
    (emit! as `(,$i.orr ,r ,$r.g0 ,$r.argreg2))
    (emit! as `(,$i.subicc ,$r.result ,$imm.false ,$r.g0))
    (emit! as `(,$i.be ,l2))
    (emit! as `(,$i.label ,l1))
    (emit! as `(,$i.nop))))

; Generic arithmetic.

(define (emit-arith-primop! as i generic r)
  (let ((l1 (new-label))
	(r  (if (not (hardware-mapped? r))
		(begin (emit! as `(,$i.ldi ,$r.globals ,(offsetof r) ,$r.tmp1))
		       $r.tmp1)
		r)))
    (emit! as `(,i ,$r.result ,r ,$r.tmp0))
    (emit! as `(,$i.bvc.a ,l1))
    (emit! as `(,$i.orr ,$r.tmp0 ,$r.g0 ,$r.result))
    (emit! as `(,$i.jmpli ,$r.millicode ,generic ,$r.o7))
    (emit! as `(,$i.orr ,r ,$r.g0 ,$r.argreg2))
    (emit! as `(,$i.label ,l1))))

; Get the length of a vector or bytevector structure, with tag checking
; included.

(define (emit-get-length! as tag1 tag2)
  (if tag2
      (emit-double-tagcheck-assert! as tag1 tag2)
      (emit-single-tagcheck-assert! as tag1))
  (emit! as `(,$i.ldi ,$r.result (- ,tag1) ,$r.tmp0))
  (emit! as `(,$i.srli ,$r.tmp0 8 ,$r.result))
  (if (= tag1 $tag.bytevector)
      (emit! as `(,$i.shli ,$r.result 2 ,$r.result))))


; BYTEVECTORS

; This is not done (must setup header, pointer), and what is here is not
; correct: the byte count must be rounded up to a word count, and the 
; length must be preserved across the call for later use in initialization
; of the header.
;
; The complex case ought to go into millicode; it's too hairy to have in-line.
;
; FIXME

(define (emit-make-bytevector-like as r hdr ptrtag)
  (let ((fault (emit-assert-positive-fixnum! as $r.result)))
    (if (null? r)
      (begin (emit! as `(,$i.jmpli ,$r.millicode ,$m.alloc ,$r.o7))
	     (emit! as `(,$i.nop)))
      (let ((r (if (hardware-mapped? r)
		   r
		   (emit-load-reg! r $r.tmp0))))
	(emit! as `(,$i.andicc ,r #xFF ,$r.tmp1))
	(emit! as `(,$i.subicc ,$r.tmp1 ,$imm.char-hdr ,$r.g0))
	(emit! as `(,$i.bne ,fault))
	(emit! as `(,$i.nop))
	(emit! as `(,$i.and ,r #xFFFFFF00 ,$r.tmp0))
	(emit! as `(,$i.slai ,$r.tmp0 8 ,$r.tmp1))
	(emit! as `(,$i.orr ,$r.tmp0 ,$r.tmp1 ,$r.tmp1))
	(emit! as `(,$i.srai ,$r.tmp1 16 ,$r.tmp0))
	(emit! as `(,$i.orr ,$r.tmp0 ,$r.tmp1 ,$r.argreg2))
	(emit! as `(,$i.jmpli ,$r.millicode ,$m.alloci ,$r.o7))
	(emit! as `(,$i.nop))))))

; FIXME

(define (emit-bytevector-like-ref! as x y fault)
  '())

; FIXME

(define (emit-bytevector-like-set! as x y fault)
  (let ((r1 (if (hardware-mapped? x)
		x
		(emit-load-reg! as x $r.tmp0)))
	(r2 (if (hardware-mapped? y)
		y
		(emit-load-reg! as x $r.tmp1))))
    (emit! as `(,$i.orr ,r1 ,r2 ,$r.tmp2))
    (emit! as `(,$i.andicc ,$r.tmp2 3 ,$r.g0))
    (emit! as `(,$i.bne ,fault))
    (emit! as `(,$i.ldi ,$r.result
			(- ,$tag.bytevector-tag)
			,$r.tmp2))
    (emit! as `(,$i.srai ,r1 2 ,$r.tmp0))
    (emit! as `(,$i.srai ,$r.tmp2 8 ,$r.tmp2))
    (emit! as `(,$i.subicc ,$r.tmp0 ,$r.tmp2 ,$g0))
    (emit! as `(,$i.bgeu ,fault))	       
    ...))


; VECTORS and PROCEDURES

; Not done; not correct (see comments for make-bytevector-like).
; FIXME

(define (emit-make-vector-like! as r hdr ptrtag)
  (emit-assert-positive-fixnum! as $r.result)
  (emit! as `(,$i.jmpli ,$r.millicode ,$m.alloci ,$r.o7))
  (cond ((null? r)
	 (emit! as `(,$i.ori ,$r.g0 ,$imm.null ,$r.argreg2)))
	((hardware-mapped? r)
	 (emit! as `(,$i.orr ,r ,$r.g0 ,$r.argreg2)))
	(else
	 (emit-load-reg! as r $r.argreg2)))
  (emit! as ...))


; `vector-ref' in various guises.
;

(define (emit-vector-like-ref! as r fault tag)
  (let ((r1 (if (hardware-mapped? x)
		x
		(emit-load-reg! as x $r.argreg2))))
    (emit! as `(,$i.andicc ,r1 3 ,$r.g0))
    (emit! as `(,$i.bne.a ,fault))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.ldi ,$r.result (- ,tag) ,$r.tmp2))
    (emit! as `(,$i.srai ,$r.tmp2 8 ,$r.tmp2))
    (emit! as `(,$i.subrcc ,$r.tmp2 ,r1 ,$r.g0))
    (emit! as `(,$i.bgeu ,fault))
    (emit! as `(,$i.addi ,$r.result (- 4 tag) ,$r.tmp0))
    (emit! as `(,$i.ldr ,$r.tmp0 ,r1 ,$r.result))))


; `vector-set!', in its various combination.
;
; It is assumed that the pointer in RESULT is valid. We must check the index
; in register x for validity and then perform the side effect (by calling
; millicode). The tag is the pointer tag to be adjusted for.
;
; If the header was already loaded, it is loaded again. This is wasteful and
; must be fixed, but it *works* for now.

(define (emit-vector-like-set! as x y fault tag)
  (let ((r1 (if (hardware-mapped? x)
		x
		(emit-load-reg! as x $r.argreg2))))
    (emit! as `(,$i.andicc ,r1 3 ,$r.g0))
    (emit! as `(,$i.bne.a ,fault))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.ldi ,$r.result (- ,tag) ,$r.tmp2))
    (emit! as `(,$i.srai ,$r.tmp2 8 ,$r.tmp2))
    (emit! as `(,$i.subrcc ,$r.tmp2 ,r1 ,$r.g0))
    (emit! as `(,$i.bgeu.a ,fault))
    (emit! as `(,$i.slot))
    (if (hardware-mapped? x)
	(emit! as `(,$i.orr ,$r.g0 ,x ,$r.argreg2)))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.vector-set ,$r.o7))
    (if (hardware-mapped? y)
	(emit! as `(,$i.orr ,$r.g0 ,y ,$r.argreg3))
	(emit-load-reg! as y $r.argreg3))))
