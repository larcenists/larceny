; -*- Scheme -*-
;
; Scheme 313 compiler.
; Emitting code for integrables.
;
; $Id: gen-primops.sch,v 1.7 1992/06/10 09:05:49 lth Exp remy $
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

; This switch controls the code generated for primops which side-effect
; data structures. Typically, it is set to #t, but for the systems where we
; use the simpler collector(s), it should be set to #f.

(define register-transactions-for-side-effects #t)

;;

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

   (cons 'unspecified
	 (lambda (as)
	   (emit! as `(,$i.ori ,$r.g0 ,$imm.unspecified ,$r.result))))

   ; Number predicates

   (cons 'zero?
	 (lambda (as)
	   (emit-cmp-primop! as $i.tsubrcc $i.be.a $m.zerop $r.g0)))

   (cons '=
	 (lambda (as r)
	   (emit-cmp-primop! as $i.tsubrcc $i.be.a $m.numeq r)))

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
	   (emit-set-boolean! as)))

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
	 (lambda (as r)
	   (let ((tmp (if (hardware-mapped? r)
			  r
			  (emit-load-reg! as r $r.tmp0))))
	     (emit! as `(,$i.jmpli ,$r.millicode ,$m.divide ,$r.o7))
	     (emit! as `(,$i.orr ,$r.g0 ,tmp ,$r.argreg2)))))

   (cons 'quotient
	 (lambda (as r)
	   (let ((tmp (if (hardware-mapped? r)
			  r
			  (emit-load-reg! as r $r.tmp0))))
	     (emit! as `(,$i.jmpli ,$r.millicode ,$m.quotient ,$r.o7))
	     (emit! as `(,$i.orr ,$r.g0 ,tmp ,$r.argreg2)))))

   (cons 'remainder
	 (lambda (as r)
	   (let ((tmp (if (hardware-mapped? r)
			  r
			  (emit-load-reg! as r $r.tmp0))))
	     (emit! as `(,$i.jmpli ,$r.millicode ,$m.remainder ,$r.o7))
	     (emit! as `(,$i.orr ,$r.g0 ,tmp ,$r.argreg2)))))

   (cons 'modulo
	 (lambda (as r)
	   (let ((tmp (if (hardware-mapped? r)
			  r
			  (emit-load-reg! as r $r.tmp0))))
	     (emit! as `(,$i.jmpli ,$r.millicode ,$m.modulo ,$r.o7))
	     (emit! as `(,$i.orr ,$r.g0 ,tmp ,$r.argreg2)))))

   (cons '--
	 (lambda (as)
	   (let ((l1 (new-label)))
	     (emit! as `(,$i.tsubrcc ,$r.g0 ,$r.result ,$r.tmp0))
	     (emit! as `(,$i.bvc.a ,l1))
	     (emit! as `(,$i.orr ,$r.g0 ,$r.tmp0 ,$r.result))
	     (emit! as `(,$i.jmpli ,$r.millicode ,$m.negate ,$r.o7))
	     (emit! as `(,$i.nop))
	     (emit! as `(,$i.label ,l1)))))

   (cons 'round
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.round ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'truncate
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.truncate ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'sqrt
	 (lambda (as)
	   (silly 'sqrt)))

   ; fixnums only

   (cons 'lognot
	 (lambda (as)
	   (emit-assert-fixnum! as $r.result)
	   (emit! as `(,$i.ornr ,$r.result ,$r.g0 ,$r.result))
	   (emit! as `(,$i.xori ,$r.result 3 ,$r.result))))

   ; fixnums only

   (cons 'logand
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit! as `(,$i.orr ,$r.result ,tmp ,$r.tmp0))
	     (emit-assert-fixnum! as $r.tmp0)
	     (emit! as `(,$i.andr ,$r.result ,tmp ,$r.result)))))

   ; fixnums only

   (cons 'logior
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit! as `(,$i.orr ,$r.result ,tmp ,$r.tmp0))
	     (emit-assert-fixnum! as $r.tmp0)
	     (emit! as `(,$i.orr ,$r.result ,tmp ,$r.result)))))

   ; fixnums only

   (cons 'logxor
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit! as `(,$i.orr ,$r.result ,tmp ,$r.tmp0))
	     (emit-assert-fixnum! as $r.tmp0)
	     (emit! as `(,$i.xorr ,$r.result ,tmp ,$r.result)))))

   ; fixnums only, and only positive shifts are meaningful.
   ; Really ought to reuse fault handler (requires rewrite of asserts).

   (cons 'lsh
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit-assert-fixnum! as $r.result)
	     (emit-assert-positive-fixnum! as tmp)
	     (emit! as `(,$i.srai ,tmp 2 ,$r.tmp1))
	     (emit! as `(,$i.sllr ,$r.result ,$r.tmp1 ,$r.result)))))

   ; Fixnums only, and only positive shifts are meaningful.
   ; Watch the semantics of the shift instruction! We have to do the
   ; original shift first and *then* adjust; no combination is possible
   ; in general.

   (cons 'rshl
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit-assert-fixnum! as $r.result)
	     (emit-assert-positive-fixnum! as tmp)
	     (emit! as `(,$i.srai ,tmp 2 ,$r.tmp1))
	     (emit! as `(,$i.srlr ,$r.result ,$r.tmp1 ,$r.result))
	     (emit! as `(,$i.andni ,$r.result 3 ,$r.result)))))

   ; fixnums only, and only positive shifts are meaningful.

   (cons 'rsha
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit-assert-fixnum! as $r.result)
	     (emit-assert-positive-fixnum! as tmp)
	     (emit! as `(,$i.srai ,tmp 2 ,$r.tmp1))
	     (emit! as `(,$i.srar ,$r.result ,$r.tmp1 ,$r.result))
	     (emit! as `(,$i.andni ,$r.result 3 ,$r.result)))))

   ; fixnums only

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
	   (emit-double-tagcheck->bool! as $tag.vector-tag
					   (+ $imm.vector-header
					      $tag.symbol-typetag))))

   (cons 'char?
	 (lambda (as)
	   (emit! as `(,$i.andi ,$r.result #xFF ,$r.tmp0))
	   (emit! as `(,$i.xoricc ,$r.tmp0 ,$imm.character ,$r.g0))
	   (emit-set-boolean! as)))

   (cons 'string?
	 (lambda (as)
	   (emit-double-tagcheck->bool! as
					$tag.bytevector-tag
					(+ $imm.bytevector-header
					   $tag.string-typetag))))

   (cons 'bytevector?
	 (lambda (as)
	   (emit-double-tagcheck->bool! as
					$tag.bytevector-tag
					(+ $imm.bytevector-header
					   $tag.bytevector-typetag))))

   (cons 'bytevector-like?
	 (lambda (as)
	   (emit-single-tagcheck->bool! as $tag.bytevector-tag)))

   (cons 'vector?
	 (lambda (as)
	   (emit-double-tagcheck->bool! as
					$tag.vector-tag
					(+ $imm.vector-header
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
	   (if (hardware-mapped? r)
	       (emit! as `(,$i.sti ,r 4 ,$r.result))
	       (begin (emit! as `(,$i.ldi ,$r.globals ,(offsetof r) ,$r.tmp0))
		      (emit! as `(,$i.sti ,$r.tmp0 4 ,$r.result))))
	   (emit! as `(,$i.addi ,$r.result ,$tag.pair-tag ,$r.result))))

   (cons 'car
	 (lambda (as)
	   (emit-single-tagcheck-assert! as $tag.pair-tag)
	   (emit! as `(,$i.ldi ,$r.result ,(- 0 $tag.pair-tag) ,$r.result))))

   (cons 'cdr
	 (lambda (as)
	   (emit-single-tagcheck-assert! as $tag.pair-tag)
	   (emit! as `(,$i.ldi ,$r.result ,(- 4 $tag.pair-tag) ,$r.result))))

   (cons 'set-car!
	 (lambda (as x)
	   (emit-single-tagcheck-assert! as $tag.pair-tag)
	   (if register-transactions-for-side-effects
	       (begin
		 (emit! as `(,$i.jmpli ,$r.millicode ,$m.setcar ,$r.o7))
		 (if (hardware-mapped? x)
		     (emit! as `(,$i.orr ,$r.g0 ,x ,$r.argreg2))
		     (emit-load-reg as x $r.argreg2)))
	       (let ((x (if (hardware-mapped? x)
			    x
			    (emit-load-reg! as x $r.argreg2))))
		 (emit! as `(,$i.sti ,x ,(- 0 $tag.pair-tag) ,$r.result))))))

   (cons 'set-cdr!
	 (lambda (as x)
	   (emit-single-tagcheck-assert! as $tag.pair-tag)
	   (if register-transactions-for-side-effects
	       (begin
		 (emit! as `(,$i.jmpli ,$r.millicode ,$m.setcdr ,$r.o7))
		 (if (hardware-mapped? x)
		     (emit! as `(,$i.orr ,$r.g0 ,x ,$r.argreg2))
		     (emit-load-reg! as x $r.argreg2)))
	       (let ((x (if (hardware-mapped? x)
			    x
			    (emit-load-reg! as x $r.argreg2))))
		 (emit! as `(,$i.sti ,x ,(- 4 $tag.pair-tag) ,$r.result))))))

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
	   (emit! as `(,$i.addi ,$r.result ,$tag.pair-tag ,$r.result))))

   (cons 'cell-ref
	 (lambda (as)
	   (emit! as `(,$i.ldi ,$r.result ,(- $tag.pair-tag) ,$r.result))))

   (cons 'cell-set!
	 (lambda (as r)
	   (if register-transactions-for-side-effects
	       (begin
		 (emit! as `(,$i.jmpli ,$r.millicode ,$m.setcar ,$r.o7))
		 (if (hardware-mapped? r)
		     (emit! as `(,$i.orr ,$r.g0 ,r ,$r.argreg2))
		     (emit-load-reg! as r $r.argreg2)))
	       (let ((r (if (hardware-mapped? r)
			    r
			    (emit-load-reg! as r $r.argreg2))))
		 (emit! as `(,$i.sti ,r ,(- 0 $tag.pair-tag) ,$r.result))))))

   ;-----------------------------------------------------------------

   ; Hooks to various system services

   ;; This should arguably not be a primop.

   (cons 'debugvsm
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.debug ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'sys$reset
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.reset ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'sys$exit
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.exit ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'break
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.break ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'sys$gc
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.gcstart ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'sys$dumpheap 
	 (lambda (as r)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.dumpheap ,$r.o7))
	   (if (hardware-mapped? r)
	       (emit! as `(,$i.orr ,$r.g0 ,r ,$r.argreg2))
	       (emit-load-reg! as r $r.argreg2))))

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
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.typetag ,$r.o7))
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
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.exact->inexact ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'inexact->exact
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.inexact->exact ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'real-part
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.real-part ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'imag-part
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.imag-part ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'char->integer
	 (lambda (as)
	   (emit-assert-char! as)
	   (emit! as `(,$i.srli ,$r.result 14 ,$r.result))))

   (cons 'integer->char
	 (lambda (as)
	   (emit-assert-fixnum! as $r.result)
	   (emit! as `(,$i.andi ,$r.result #x3FF ,$r.result))
	   (emit! as `(,$i.slli ,$r.result 14 ,$r.result))
	   (emit! as `(,$i.ori  ,$r.result ,$imm.character ,$r.result))))

   (cons 'make-rectangular
	 (lambda (as x)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.make-rectangular ,$r.o7))
	   (if (hardware-mapped? x)
	       (emit! as `(,$i.orr ,x ,$r.g0 ,$r.argreg2))
	       (emit-load-reg! as x $r.argreg2))))

   (cons 'not
	 (lambda (as)
	   (emit! as `(,$i.subicc ,$r.result ,$imm.false ,$r.g0))
	   (emit-set-boolean! as)))

   (cons 'eq?
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp0))))
	     (emit! as `(,$i.subrcc ,$r.result ,tmp ,$r.g0))
	     (emit-set-boolean! as))))

   (cons 'eqv?
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp0)))
		 (l1  (new-label)))
	     (emit! as `(,$i.subrcc ,$r.result ,tmp ,$r.g0))
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
				       (+ $imm.bytevector-header
					  $tag.bytevector-typetag)
				       $tag.bytevector-tag)))

   (cons 'bytevector-fill!
	 (lambda (as r)
	   (emit-bytevector-like-fill! as 
				       r
				       $imm.bytevector-header)))

   (cons 'string-length
	 (lambda (as)
	   (emit-get-length! as
			     $tag.bytevector-tag
			     (+ $imm.bytevector-header $tag.string-typetag))))

   (cons 'bytevector-length
	 (lambda (as)
	   (emit-get-length! as 
			     $tag.bytevector-tag
			     (+ $imm.bytevector-header $tag.bytevector-typetag))))

   (cons 'bytevector-like-length
	 (lambda (as)
	   (emit-get-length! as $tag.bytevector-tag #f)))

   (cons 'string-ref
	 (lambda (as r)
	   (let* ((fault (emit-double-tagcheck-assert! 
			  as
			  $tag.bytevector-tag
			  (+ $imm.bytevector-header $tag.string-typetag)))
		  (r (if (not (hardware-mapped? r))
			 (emit-load-reg! as r $r.tmp0)
			 r)))
	     (emit-bytevector-like-ref! as r fault #t))))

   (cons 'bytevector-ref
	 (lambda (as r)
	   (let* ((fault (emit-double-tagcheck-assert!
			  as
			  $tag.bytevector-tag
			  (+ $imm.bytevector-header $tag.bytevector-typetag)))
		  (r (if (not (hardware-mapped? r))
			 (emit-load-reg! as r $r.tmp0)
			 r)))
	     (emit-bytevector-like-ref! as r fault #f))))

   (cons 'bytevector-like-ref
	 (lambda (as r)
	   (let* ((fault (emit-single-tagcheck-assert! as $tag.bytevector-tag))
		  (r (if (not (hardware-mapped? r))
			 (emit-load-reg! as r $r.tmp0)
			 r)))
	     (emit-bytevector-like-ref! as r fault #f))))

   (cons 'bytevector-like-set!
	 (lambda (as x y)
	   (let ((fault (emit-single-tagcheck-assert! as $tag.bytevector-tag)))
	     (emit-bytevector-like-set! as x y fault))))

   (cons 'bytevector-set!
	 (lambda (as x y)
	   (let ((fault (emit-double-tagcheck-assert!
			 as
			 $tag.bytevector-tag
			 (+ $imm.bytevector-header $tag.bytevector-typetag))))
	     (emit-bytevector-like-set! as x y fault))))

   ;-----------------------------------------------------------------

   ; Vector and procedure operations

   (cons 'make-procedure
	 (lambda (as)
	   (emit-make-vector-like! as
				   '()
				   $imm.procedure-header
				   $tag.procedure-tag)))

   (cons 'make-vector
	 (lambda (as r)
	   (emit-make-vector-like! as
				   r
				   (+ $imm.vector-header $tag.vector-typetag)
				   $tag.vector-tag)))

   (cons 'vector-length
	 (lambda (as)
	   (emit-get-length! as
			     $tag.vector-tag
			     (+ $imm.vector-header $tag.vector-typetag))))

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
			 (+ $imm.vector-header $tag.vector-typetag))))
	     (emit-vector-like-ref! as r fault $tag.vector-tag))))

   (cons 'vector-like-ref
	 (lambda (as r)
	   (let ((fault (emit-single-tagcheck-assert! as $tag.vector-tag)))
	     (emit-vector-like-ref! as r fault $tag.vector-tag))))

   (cons 'procedure-ref
	 (lambda (as r)
	   (let ((fault (emit-single-tagcheck-assert! as $tag.procedure-tag)))
	     (emit-vector-like-ref! as r fault $tag.procedure-tag))))

   (cons 'vector-set!
	 (lambda (as r1 r2)
	   (let ((fault (emit-double-tagcheck-assert!
			 as
			 $tag.vector-tag
			 (+ $imm.vector-header $tag.vector-typetag))))
	     (emit-vector-like-set! as r1 r2 fault $tag.vector-tag))))

   (cons 'vector-like-set!
	 (lambda (as r1 r2)
	   (let ((fault (emit-single-tagcheck-assert! as $tag.vector-tag)))
	     (emit-vector-like-set! as r1 r2 fault $tag.vector-tag))))

   (cons 'procedure-set!
	 (lambda (as r1 r2)
	   (let ((fault (emit-single-tagcheck-assert! as $tag.procedure-tag)))
	     (emit-vector-like-set! as r1 r2 fault $tag.procedure-tag))))

   ; -----------------------------------------------------------------

   ; Character predicates

   (cons 'char<?
	 (lambda (as x)
	   (emit-char-cmp as x $i.bl.a)))

   (cons 'char<=?
	 (lambda (as x)
	   (emit-char-cmp as x $i.ble.a)))

   (cons 'char=?
	 (lambda (as x)
	   (emit-char-cmp as x $i.be.a)))

   (cons 'char>?
	 (lambda (as x)
	   (emit-char-cmp as x $i.bg.a)))

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
	   (emit! as `(,$i.bne.a ,(make-asm-label label)))
	   (emit! as `(,$i.slot))))

   (cons 'bfpair?
	 (lambda (as label)
	   (emit! as `(,$i.andi ,$r.result ,$tag.tagmask ,$r.tmp0))
	   (emit! as `(,$i.xoricc ,$r.tmp0 ,$tag.pair-tag ,$r.g0))
	   (emit! as `(,$i.bne.a ,(make-asm-label label)))
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
	 (lambda (as r1 r2)
	   (if (hardware-mapped? r1)
	       (emit! as `(,$i.orr ,r1 ,$r.g0 ,$r.argreg2))
	       (emit! as `(,$i.ldi ,$r.globals ,(offsetof r1) ,$r.argreg2)))
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.open-file ,$r.o7))
	   (if (hardware-mapped? r2)
	       (emit! as `(,$i.orr ,r2 ,$r.g0 ,$r.argreg3))
	       (emit! as `(,$i.ldi ,$r.globals ,(offsetof r2) ,$r.argreg3)))))

   (cons 'close-file
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.close-file ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'unlink-file
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.unlink-file ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'read-file
	 (lambda (as r1 r2)
	   (if (hardware-mapped? r1)
	       (emit! as `(,$i.orr ,r1 ,$r.g0 ,$r.argreg2))
	       (emit! as `(,$i.ldi ,$r.globals ,(offsetof r1) ,$r.argreg2)))
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.read-file ,$r.o7))
	   (if (hardware-mapped? r2)
	       (emit! as `(,$i.orr ,r2 ,$r.g0 ,$r.argreg3))
	       (emit! as `(,$i.ldi ,$r.globals ,(offsetof r2) ,$r.argreg3)))))

   (cons 'write-file
	 (lambda (as r1 r2)
	   (if (hardware-mapped? r1)
	       (emit! as `(,$i.orr ,r1 ,$r.g0 ,$r.argreg2))
	       (emit! as `(,$i.ldi ,$r.globals ,(offsetof r1) ,$r.argreg2)))
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.write-file ,$r.o7))
	   (if (hardware-mapped? r2)
	       (emit! as `(,$i.orr ,r2 ,$r.g0 ,$r.argreg3))
	       (emit! as `(,$i.ldi ,$r.globals ,(offsetof r2) ,$r.argreg3)))))

   ; This one is used to get resource data.
   ; It returns a fixnum indicating system time used in milliseconds.

   (cons 'getrusage
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.getrusage ,$r.o7))
	   (emit! as `(,$i.nop))))

   ))

; quite temporary

(define (silly name)
  (error 'silly "Silly, silly. (undefined integrable ~a)." name))

; not so temporary

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
    (emit! as `(,$i.ori ,$r.g0 ,$imm.true ,$r.result))
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
    (emit! as `(,$i.label ,l2))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.type-exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l0 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l1))
    l2))

; Emit single instruction to load sw-mapped reg into another reg, and return
; the destination reg.
;
; This is just a convenient shorthand.

(define (emit-load-reg! as x rd)
  (emit! as `(,$i.ldi ,$r.globals ,(offsetof x) ,rd))
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
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.type-exception ,$r.o7))
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
    (emit! as `(,$i.subicc ,$r.tmp0 ,$imm.character ,$r.g0))
    (emit! as `(,$i.be.a ,l2))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.type-exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l1 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l2))
    l0))

; This is more expensive than what is good for it (5 cycles in the usual case),
; but there does not seem to be a better way.

(define (emit-assert-positive-fixnum! as reg)
  (let ((l1 (new-label))
	(l2 (new-label))
	(l3 (new-label))) 
    (emit! as `(,$i.label ,l2))
    (emit! as `(,$i.tsubrcc ,reg ,$r.g0 ,$r.g0))
    (emit! as `(,$i.bvc ,l1))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.label ,l3))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.type-exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l2 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l1))
    (emit! as `(,$i.bl ,l3))
    (emit! as `(,$i.nop))))

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
    (emit! as `(,$i.ori ,$r.g0 ,$imm.false ,$r.result))
    (emit! as `(,test ,l2))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.true ,$r.result))
    (emit! as `(,$i.label ,l2))))

; Possibly it would be better to unchain the branches and let slots be filled?

(define (emit-bcmp-primop! as ntest r label generic)
  (let ((l1 (new-label))
	(l2 (make-asm-label label))
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
  (if (= tag1 $tag.bytevector-tag)
      (emit! as `(,$i.slli ,$r.result 2 ,$r.result))))


; BYTEVECTORS

; The major problem is that $r.result is clobbered by the call to alloc(i),
; and hence, it must be preserved. Since $r.argreg3 is known not to be touched
; by the allocator (unless there is an exception, which would be fatal 
; (we hope)), it is used for preserving the value.

(define (emit-make-bytevector-like! as hdr ptrtag)
  (let ((fault (emit-assert-positive-fixnum! as $r.result)))

    ; Preserve the length field, then calculate the number of words
    ; to allocate.
    ; The value `28' is an adjustment of 3 (for rounding up) plus another
    ; 4 bytes for the header, all represented as a fixnum.

    (emit! as `(,$i.orr ,$r.result ,$r.g0 ,$r.argreg3))
    (emit! as `(,$i.addi ,$r.result 28 ,$r.result))
    (emit! as `(,$i.andi ,$r.result #xFFFFFFF0 ,$r.result))

    ; Allocate space

    (emit! as `(,$i.jmpli ,$r.millicode ,$m.alloc ,$r.o7))
    (emit! as `(,$i.srai ,$r.result 2 ,$r.result))

    ; Setup header, tag pointer.

    (emit! as `(,$i.slli ,$r.argreg3 6 ,$r.tmp0))
    (emit! as `(,$i.addi ,$r.tmp0 ,hdr ,$r.tmp0))
    (emit! as `(,$i.sti ,$r.tmp0 0 ,$r.result))
    (emit! as `(,$i.addi ,$r.result ,ptrtag ,$r.result))))

; Given a bytevector-like structure and a fixnum, fill the bytevector with the
; fixnum.

(define (emit-bytevector-like-fill! as r hdr)
  (let* ((fault (emit-double-tagcheck-assert! as $tag.bytevector-tag hdr))
	 (l1    (new-label))
	 (l2    (new-label))
	 (r     (if (hardware-mapped? r)
		    r
		    (emit-load-reg! as r $r.tmp2))))

    ; tmp0 is the count/index, tmp1 is the base pointer, r/tmp2 has the byte
    ; to fill with. 
    ; word fill is better, but the word has to be created (takes several
    ; instructions -- add this when/if we move to millicode or have
    ; optimization modes).

    (emit! as `(,$i.andi ,r 3 ,$r.g0))
    (emit! as `(,$i.bne ,fault))
    (emit! as `(,$i.srai ,r 2 ,$r.tmp2))
    (emit! as `(,$i.ldi ,$r.result (- ,$tag.bytevector-tag) ,$r.tmp0))
    (emit! as `(,$i.addi ,$r.result (- 4 ,$tag.bytevector-tag) ,$r.tmp1))
    (emit! as `(,$i.srai ,$r.tmp0 8 ,$r.tmp0))
    (emit! as `(,$i.label ,l2))
    (emit! as `(,$i.subicc ,$r.tmp0 1 ,$r.tmp0))
    (emit! as `(,$i.bge.a ,l2))
    (emit! as `(,$i.stbr ,$r.tmp2 ,$r.tmp0 ,$r.tmp1))
    (emit! as `(,$i.label ,l1))))

; The pointer in $result is known to be bytevector-like. Reference the xth
; element. If the header has been loaded before, it will be loaded again.
; This must be fixed (later).

(define (emit-bytevector-like-ref! as x fault charize?)
  (let ((r (if (hardware-mapped? x)
	       x
	       (emit-load-reg! as x $r.tmp1))))
    (emit! as `(,$i.andicc ,r 3 ,$r.g0))
    (emit! as `(,$i.bne ,fault))
    (emit! as `(,$i.ldi ,$r.result (- ,$tag.bytevector-tag) ,$r.tmp0))
    (emit! as `(,$i.srli ,$r.tmp0 8 ,$r.tmp0))
    (emit! as `(,$i.srai ,r 2 ,$r.tmp1))
;    (emit! as `(,$i.srli ,r 2 ,$r.tmp1))
    (emit! as `(,$i.subrcc ,$r.tmp1 ,$r.tmp0 ,$r.g0))
    (emit! as `(,$i.bgeu.a ,fault))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.subi ,$r.result ,(- $tag.bytevector-tag 4) ,$r.tmp0))
    (emit! as `(,$i.ldbr ,$r.tmp0 ,$r.tmp1 ,$r.tmp0))
    (if (not charize?)
	(emit! as `(,$i.slli ,$r.tmp0 2 ,$r.result))
	(begin (emit! as `(,$i.slli ,$r.tmp0 16 ,$r.result))
	       (emit! as `(,$i.ori ,$r.result ,$imm.character ,$r.result))))))


;

(define (emit-bytevector-like-set! as x y fault)
  (let ((r1 (if (hardware-mapped? x)
		x
		(emit-load-reg! as x $r.tmp0)))
	(r2 (if (hardware-mapped? y)
		y
		(emit-load-reg! as y $r.tmp1))))
    (emit! as `(,$i.orr ,r1 ,r2 ,$r.tmp2))
    (emit! as `(,$i.andicc ,$r.tmp2 3 ,$r.g0))
    (emit! as `(,$i.bne ,fault))
    (emit! as `(,$i.ldi ,$r.result (- ,$tag.bytevector-tag) ,$r.tmp2))
    (emit! as `(,$i.srli ,$r.tmp2 8 ,$r.tmp2))
    (emit! as `(,$i.srai ,r1 2 ,$r.tmp0))
;    (emit! as `(,$i.srli ,r1 2 ,$r.tmp0))
    (emit! as `(,$i.subrcc ,$r.tmp0 ,$r.tmp2 ,$r.g0))
    (emit! as `(,$i.bgeu ,fault))	       
    (emit! as `(,$i.srli ,r2 2 ,$r.tmp1))
    (emit! as `(,$i.subi ,$r.result ,(- $tag.bytevector-tag 4) ,$r.tmp2))
    (emit! as `(,$i.stbr ,$r.tmp1 ,$r.tmp2 ,$r.tmp0))))


; VECTORS and PROCEDURES

;

(define (emit-make-vector-like! as r hdr ptrtag)
  (let ((fault (emit-assert-positive-fixnum! as $r.result)))
    (emit! as `(,$i.orr ,$r.result ,$r.g0 ,$r.argreg3))
    (emit! as `(,$i.addi ,$r.result 4 ,$r.result))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.alloci ,$r.o7))
    (cond ((null? r)
	   (emit! as `(,$i.ori ,$r.g0 ,$imm.null ,$r.argreg2)))
	  ((hardware-mapped? r)
	   (emit! as `(,$i.orr ,r ,$r.g0 ,$r.argreg2)))
	  (else
	   (emit-load-reg! as r $r.argreg2)))
    (emit! as `(,$i.slli ,$r.argreg3 8 ,$r.argreg3))
    (emit! as `(,$i.addi ,$r.argreg3 ,hdr ,$r.argreg3))
    (emit! as `(,$i.sti ,$r.argreg3 0 ,$r.result))
    (emit! as `(,$i.addi ,$r.result ,ptrtag ,$r.result))))


; `vector-ref' in various guises.
;
; Pointer must be valid; header may be reloaded unnecessarily. Fix later.

(define (emit-vector-like-ref! as r fault tag)
  (let ((r1 (if (hardware-mapped? r)
		r
		(emit-load-reg! as r $r.argreg2))))
    (emit! as `(,$i.andicc ,r1 3 ,$r.g0))
    (emit! as `(,$i.bne.a ,fault))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.ldi ,$r.result (- ,tag) ,$r.tmp2))
    (emit! as `(,$i.srai ,$r.tmp2 8 ,$r.tmp2))
    (emit! as `(,$i.subrcc ,$r.tmp2 ,r1 ,$r.g0))
    (emit! as `(,$i.blu ,fault))
    (emit! as `(,$i.addi ,$r.result ,(- 4 tag) ,$r.tmp0))
    (emit! as `(,$i.ldr ,$r.tmp0 ,r1 ,$r.result))))


; `vector-set!', in its various combination.
;
; It is assumed that the pointer in RESULT is valid. We must check the index
; in register x for validity and then perform the side effect (by calling
; millicode). The tag is the pointer tag to be adjusted for.
;
; If the header was already loaded, it is loaded again. This is wasteful and
; must be fixed, but it *works* for now.
;
; The use of vector-set is ok even if it is a procedure.

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
    (emit! as `(,$i.blu.a ,fault))
    (emit! as `(,$i.slot))
    (if register-transactions-for-side-effects
	(begin
	  (if (hardware-mapped? x)
	      (emit! as `(,$i.orr ,$r.g0 ,x ,$r.argreg2)))
	  (emit! as `(,$i.jmpli ,$r.millicode ,$m.vector-set ,$r.o7))
	  (if (hardware-mapped? y)
	      (emit! as `(,$i.orr ,$r.g0 ,y ,$r.argreg3))
	      (emit-load-reg! as y $r.argreg3)))
	(let ((y (if (hardware-mapped? y)
		     y
		     (emit-load-reg! as y $r.argreg3))))
	  (emit! as `(,$i.addr ,$r.result ,r1 ,$r.tmp0))
	  (emit! as `(,$i.sti ,y ,(- 4 $tag.vector-tag) ,$r.tmp0))))))

; We check the tags of both by xoring them and seeing if the low byte is 0.
; If so, then we can subtract one from the other (tag and all) and check the
; condition codes.
;
; The branch-on-true instruction must have the annull bit set.

(define (emit-char-cmp as r btrue)
  (let ((tmp (if (hardware-mapped? r)
		 r
		 (emit-load-reg! as r $r.tmp0)))
	(l0 (new-label))
	(l1 (new-label))
	(l2 (new-label)))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.xorr ,$r.result ,tmp ,$r.tmp1))
    (emit! as `(,$i.andicc ,$r.tmp1 #xFF ,$r.tmp1))
    (emit! as `(,$i.be.a ,l1))
    (emit! as `(,$i.subrcc ,$r.result ,tmp ,$r.g0))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.type-exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l0 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l1))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.false ,$r.result))
    (emit! as `(,btrue ,l2))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.true ,$r.result))
    (emit! as `(,$i.label ,l2))))
