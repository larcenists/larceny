;; -*- Scheme -*-
;;
;; Larceny assembler (Sparc) -- emitting code for integrables.
;;
;; History
;;   March 7, 1995 / lth (v0.23)
;;     Support for inlining of generation check for set-car!/set-cdr!/
;;     cell-set!.
;;
;;   July 5, 1994 / lth (v0.20)
;;     Changed to deal with new list of millicode procs.
;;
;;   July 6, 1994 / lth (v0.20)
;;     Fixed bug which left a vector header behind in argreg3 after making
;;     a vector. This is really evil if the value is propagated into a
;;     GP register and then saved in a stack frame; the collector will have
;;     a fit if implemented in a particular way... Don't know why this was
;;     not a problem in the old collector.
;;
;; Temp-register allocation here is completely out of hand. We have to come
;; up with a coherent strategy for allocating temporary registers, e.g. a
;; set of registers to pick from when one is needed.
;;
;; Also, too many implicit registers are assumed (this is definitely related
;; to the allocation mess) and should rather be passed/returned as appropriate.
;; The key problem is to never load a memory-mapped register or a datum more
;; than once. Remember that calling a subroutine which returns a register may
;; return a temp which cannot subsequently be allocated.
;;
;; BUGS
;; - Like mentioned above, temp register allocation is gross.
;; - Some operations do too much work; for example, vector-ref will load the
;;   vector header twice.

; This switch controls the code generated for primops which side-effect
; data structures. Typically, it is set to #t, but for the systems where we
; use the simpler collector(s), it should be set to #f.

(define register-transactions-for-side-effects #t)

;; these are a bit obsolete but too much effort to rename right now...

(define (emit-primop0! as op)
  ((cdr (assq op primop-list)) as))

(define (emit-primop1! as op r)
  ((cdr (assq op primop-list)) as r))

(define (emit-primop2! as op r1 r2)
  ((cdr (assq op primop-list)) as r1 r2))

(define conv-list '((+ . +imm) (- . -imm)))

(define (emit-primop2imm! as op imm)
  ((cdr (assq (cdr (assq op conv-list)) primop-list)) as imm))


;; These are new and should be used in all cases.

(define (emit-primop.3arg! as a1 a2 a3)
  ((cdr (assq a1 primop-list)) as a2 a3))

(define (emit-primop.4arg! as a1 a2 a3 a4)
  ((cdr (assq a1 primop-list)) as a2 a3 a4))



; Assoc list of primops with generating procedures.
; This is getting long; a better ordering may be beneficial to performance,
; if anyone cares.

(define primop-list
  (list 

   ; I think that this could legally do nothing at all.

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
	   (emit-primop.4arg! as 'internal:+2reg $r.result r $r.result)))

   (cons '-
	 (lambda (as r)
	   (emit-primop.4arg! as 'internal:-2reg $r.result r $r.result)))

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
	   (emit-assert-fixnum! as $r.result $ex.lognot)
	   (emit! as `(,$i.ornr ,$r.result ,$r.g0 ,$r.result))
	   (emit! as `(,$i.xori ,$r.result 3 ,$r.result))))

   ; fixnums only

   (cons 'logand
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit! as `(,$i.orr ,$r.result ,tmp ,$r.tmp0))
	     (emit-assert-fixnum! as $r.tmp0 $ex.logand)
	     (emit! as `(,$i.andr ,$r.result ,tmp ,$r.result)))))

   ; fixnums only

   (cons 'logior
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit! as `(,$i.orr ,$r.result ,tmp ,$r.tmp0))
	     (emit-assert-fixnum! as $r.tmp0 $ex.logior)
	     (emit! as `(,$i.orr ,$r.result ,tmp ,$r.result)))))

   ; fixnums only

   (cons 'logxor
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit! as `(,$i.orr ,$r.result ,tmp ,$r.tmp0))
	     (emit-assert-fixnum! as $r.tmp0 $ex.logxor)
	     (emit! as `(,$i.xorr ,$r.result ,tmp ,$r.result)))))

   ; fixnums only, and only positive shifts are meaningful.
   ; Really ought to reuse fault handler (requires rewrite of asserts).

   (cons 'lsh
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit-assert-fixnum! as $r.result $ex.lsh)
	     (emit-assert-positive-fixnum! as tmp $ex.lsh)
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
	     (emit-assert-fixnum! as $r.result $ex.rshl)
	     (emit-assert-positive-fixnum! as tmp $ex.rshl)
	     (emit! as `(,$i.srai ,tmp 2 ,$r.tmp1))
	     (emit! as `(,$i.srlr ,$r.result ,$r.tmp1 ,$r.result))
	     (emit! as `(,$i.andni ,$r.result 3 ,$r.result)))))

   ; fixnums only, and only positive shifts are meaningful.

   (cons 'rsha
	 (lambda (as x)
	   (let ((tmp (if (hardware-mapped? x)
			  x
			  (emit-load-reg! as x $r.tmp1))))
	     (emit-assert-fixnum! as $r.result $ex.rsha)
	     (emit-assert-positive-fixnum! as tmp $ex.rsha)
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

   ; If peephole optimization is turned on, then internal:cons2reg
   ; will be called directly.

   (cons 'cons
	 (lambda (as r)
	   (emit-primop.4arg! as 'internal:cons2reg $r.result r $r.result)))

   (cons 'car
	 (lambda (as)
	   (emit-primop.3arg! as 'internal:car2reg $r.result $r.result)))

   (cons 'cdr
	 (lambda (as)
	   (emit-primop.3arg! as 'internal:cdr2reg $r.result $r.result)))

   (cons 'set-car!
	 (lambda (as x)
	   (if (not unsafe-mode)
	       (emit-single-tagcheck-assert! as $tag.pair-tag $ex.car))
	   (emit-setcar/setcdr! as x 0)))

   (cons 'set-cdr!
	 (lambda (as x)
	   (if (not unsafe-mode)
	       (emit-single-tagcheck-assert! as $tag.pair-tag $ex.cdr))
	   (emit-setcar/setcdr! as x 4)))

   ; Cells are internal data structures, represented using pairs.
   ; No error checking is done on cell references.

   (cons 'make-cell
	 (lambda (as)
	   (emit-primop.4arg! as 'internal:cons2reg $r.result $r.g0 $r.result)))

   (cons 'cell-ref
	 (lambda (as)
	   (emit-primop.3arg! as 'internal:cellref2reg $r.result $r.result)))

   (cons 'cell-set!
	 (lambda (as r)
	   (emit-setcar/setcdr! as r 0)))

   ;-----------------------------------------------------------------

   ; Hooks to various system services

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
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.gc ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'sys$dumpheap 
	 (lambda (as r)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.dumpheap ,$r.o7))
	   (emit-move2hwreg! as r $r.argreg2)))

   ; -----------------------------------------------------------------

   ; Continuations.

   (cons 'creg
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.creg ,$r.o7))
	   (emit! as `(,$i.nop))))

   (cons 'creg-set!
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.creg-set! ,$r.o7))
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
	   (emit-move2hwreg! as r $r.argreg2)))

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
	   (emit-assert-char! as $ex.char2int)
	   (emit! as `(,$i.srli ,$r.result 14 ,$r.result))))

   (cons 'integer->char
	 (lambda (as)
	   (emit-assert-fixnum! as $r.result $ex.int2char)
	   (emit! as `(,$i.andi ,$r.result #x3FF ,$r.result))
	   (emit! as `(,$i.slli ,$r.result 14 ,$r.result))
	   (emit! as `(,$i.ori  ,$r.result ,$imm.character ,$r.result))))

   (cons 'make-rectangular
	 (lambda (as x)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.make-rectangular ,$r.o7))
	   (emit-move2hwreg! as x $r.argreg2)))

   (cons 'not
	 (lambda (as)
	   (emit! as `(,$i.subicc ,$r.result ,$imm.false ,$r.g0))
	   (emit-set-boolean! as)))

   (cons 'eq?
	 (lambda (as x)
	   (emit-primop.4arg! as 'internal:eq2reg $r.result x $r.result)))

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
			     (+ $imm.bytevector-header $tag.string-typetag)
			     $ex.slen)))

   (cons 'bytevector-length
	 (lambda (as)
	   (emit-get-length! as 
			     $tag.bytevector-tag
			     (+ $imm.bytevector-header $tag.bytevector-typetag)
			     $ex.bvlen)))

   (cons 'bytevector-like-length
	 (lambda (as)
	   (emit-get-length! as $tag.bytevector-tag #f $ex.bvllen)))

   (cons 'string-ref
	 (lambda (as r)
	   (let ((fault (if (not unsafe-mode)
			    (emit-double-tagcheck-assert! 
			     as
			     $tag.bytevector-tag
			     (+ $imm.bytevector-header $tag.string-typetag)
			     $ex.sref)
			    #f)))
	     (emit-bytevector-like-ref! as r fault #t))))

   (cons 'bytevector-ref
	 (lambda (as r)
	   (let ((fault (if (not unsafe-mode)
			    (emit-double-tagcheck-assert!
			     as
			     $tag.bytevector-tag
			     (+ $imm.bytevector-header $tag.bytevector-typetag)
			     $ex.bvref)
			    #f)))
	     (emit-bytevector-like-ref! as r fault #f))))

   (cons 'bytevector-like-ref
	 (lambda (as r)
	   (let ((fault (if (not unsafe-mode)
			    (emit-single-tagcheck-assert! as
							  $tag.bytevector-tag
							  $ex.bvlref)
			    #f)))
	     (emit-bytevector-like-ref! as r fault #f))))

   (cons 'bytevector-like-set!
	 (lambda (as x y)
	   (let ((fault (if (not unsafe-mode)
			    (emit-single-tagcheck-assert! as
							  $tag.bytevector-tag
							  $ex.bvlref)
			    #f)))
	     (emit-bytevector-like-set! as x y fault))))

   (cons 'bytevector-set!
	 (lambda (as x y)
	   (let ((fault (if (not unsafe-mode)
			    (emit-double-tagcheck-assert!
			     as
			     $tag.bytevector-tag
			     (+ $imm.bytevector-header $tag.bytevector-typetag)
			     $ex.bvset)
			    #f)))
	     (emit-bytevector-like-set! as x y fault))))

   ;-----------------------------------------------------------------

   ; Vector and procedure operations

   (cons 'sys$partial-list->vector
	 (lambda (as r)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.partial-list->vector ,$r.o7))
	   (emit-move2hwreg! as r $r.argreg2)))

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
			     (+ $imm.vector-header $tag.vector-typetag)
			     $ex.vlen)))

   (cons 'vector-like-length
	 (lambda (as)
	   (emit-get-length! as $tag.vector-tag #f $ex.vllen)))

   (cons 'procedure-length
	 (lambda (as)
	   (emit-get-length! as $tag.procedure-tag #f $ex.plen)))

   (cons 'vector-ref
	 (lambda (as r)
	   (let ((fault (if (not unsafe-mode)
			    (emit-double-tagcheck-assert!
			     as
			     $tag.vector-tag
			     (+ $imm.vector-header $tag.vector-typetag)
			     $ex.vref)
			    #f)))
	     (emit-vector-like-ref! as r fault $tag.vector-tag))))

   (cons 'vector-like-ref
	 (lambda (as r)
	   (let ((fault (if (not unsafe-mode)
			    (emit-single-tagcheck-assert! as
							  $tag.vector-tag
							  $ex.vlref)
			    #f)))
	     (emit-vector-like-ref! as r fault $tag.vector-tag))))

   (cons 'procedure-ref
	 (lambda (as r)
	   (let ((fault (if (not unsafe-mode)
			    (emit-single-tagcheck-assert! as
							  $tag.procedure-tag
							  $ex.pref)
			    #f)))
	     (emit-vector-like-ref! as r fault $tag.procedure-tag))))

   (cons 'vector-set!
	 (lambda (as r1 r2)
	   (let ((fault (if (not unsafe-mode)
			    (emit-double-tagcheck-assert!
			     as
			     $tag.vector-tag
			     (+ $imm.vector-header $tag.vector-typetag)
			     $ex.vset)
			    #f)))
	     (emit-vector-like-set! as r1 r2 fault $tag.vector-tag))))

   (cons 'vector-like-set!
	 (lambda (as r1 r2)
	   (let ((fault (if (not unsafe-mode)
			    (emit-single-tagcheck-assert! as
							  $tag.vector-tag
							  $ex.vlset)
			    #f)))
	     (emit-vector-like-set! as r1 r2 fault $tag.vector-tag))))

   (cons 'procedure-set!
	 (lambda (as r1 r2)
	   (let ((fault (if (not unsafe-mode)
			    (emit-single-tagcheck-assert! as
							  $tag.procedure-tag
							  $ex.pset)
			    #f)))
	     (emit-vector-like-set! as r1 r2 fault $tag.procedure-tag))))

   ; -----------------------------------------------------------------

   ; Character predicates

   (cons 'char<?
	 (lambda (as x)
	   (emit-char-cmp as x $i.bl.a $ex.char<?)))

   (cons 'char<=?
	 (lambda (as x)
	   (emit-char-cmp as x $i.ble.a $ex.char<=?)))

   (cons 'char=?
	 (lambda (as x)
	   (emit-char-cmp as x $i.be.a $ex.char=?)))

   (cons 'char>?
	 (lambda (as x)
	   (emit-char-cmp as x $i.bg.a $ex.char>?)))

   (cons 'char>=?
	 (lambda (as x)
	   (emit-char-cmp as x $i.bge.a $ex.char>=?)))

   ;-----------------------------------------------------------------
   
   ; These are introduced by peephole optimization. Old (obsolete but still
   ; used, by old code) names are not prefixed by internal: whereas new
   ; procedures are. Old code usually calls new code. Other code (above)
   ; also uses the internal versions.

   (cons 'internal:car2reg
	 (lambda (as src1 dest)
	   (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	       (error "Assembler: internal:car2reg: reg is not HW"))
	   (if (not unsafe-mode)
	       (emit-single-tagcheck-assert-reg! as $tag.pair-tag src1 $ex.car))
	   (emit! as `(,$i.ldi ,src1 ,(- 0 $tag.pair-tag) ,dest))))

   (cons 'internal:cdr2reg
	 (lambda (as src1 dest)
	   (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	       (error "Assembler: internal:cdr2reg: reg is not HW"))
	   (if (not unsafe-mode)
	       (emit-single-tagcheck-assert-reg! as $tag.pair-tag src1 $ex.cdr))
	   (emit! as `(,$i.ldi ,src1 ,(- 4 $tag.pair-tag) ,dest))))

   (cons 'internal:cellref2reg
	 (lambda (as src1 dest)
	   (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	       (error "Assembler: internal:cellref2reg: reg is not HW"))
	   (emit! as `(,$i.ldi ,src1 ,(- $tag.pair-tag) ,dest))))

   (cons 'internal:cons2reg
	 (lambda (as src1 src2 dest)

	   (define (emit-common-case cont)
	     (if (eqv? src1 $r.result)
		 (emit! as `(,$i.orr ,$r.result ,$r.g0 ,$r.argreg2)))
	     (emit! as `(,$i.jmpli ,$r.millicode ,$m.alloc ,$r.o7))
	     (emit! as `(,$i.ori ,$r.g0 8 ,$r.result))
	     (if (eqv? src1 $r.result)
		 (emit! as `(,$i.sti ,$r.argreg2 0 ,$r.result))
		 (emit! as `(,$i.sti ,src1 0 ,$r.result)))
	     (let ((src2 (if (hardware-mapped? src2)
			     src2
			     (emit-load-reg! as src2 $r.tmp1))))
	       (emit! as `(,$i.sti ,src2 4 ,$r.result))
	       (if cont
		   (emit! as `(,$i.b ,cont)))
	       (emit! as `(,$i.addi ,$r.result 1 ,dest))))

	   ; lights-camera-action!
	   (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	       (error "Assembler: internal:cons2reg: reg is not HW"))
	   (if inline-cons
	       (let ((l1 (new-label))
		     (l2 (new-label)))
		 (emit! as `(,$i.addi ,$r.e-top 8 ,$r.e-top))
		 (emit! as `(,$i.subrcc ,$r.e-top ,$r.e-limit ,$r.g0))
		 (emit! as `(,$i.ble.a ,l1))
		 (emit! as `(,$i.sti ,src1 -8 ,$r.e-top))

		 ; overflow: just do an 'alloc' to trigger the correct gc
		 (emit! as `(,$i.subi ,$r.e-top 8 ,$r.e-top))
		 (emit-common-case l2)

		 ; fast case again
		 (emit! as `(,$i.label ,l1))
		 (let ((src2 (if (hardware-mapped? src2)
				 src2
				 (emit-load-reg! as src2 $r.tmp1))))
		   (if (= src2 dest)
		       (begin (emit! as `(,$i.sti ,src2 -4 ,$r.e-top))
			      (emit! as `(,$i.subi ,$r.e-top 7 ,dest)))
		       (begin (emit! as `(,$i.subi ,$r.e-top 7 ,dest))
			      (emit! as `(,$i.sti ,src2 -4 ,$r.e-top))))
		   (emit! as `(,$i.label ,l2))))
	       (begin
		 (emit-common-case #f)))))

   (cons 'internal:+2reg
	 (lambda (as src1 src2 dest)
	   (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	       (error "Assembler: internal:+2reg: reg is not HW"))
	   (emit-arith-primop!
	    as $i.taddrcc $i.subr $m.add src1 src2 dest #t)))

   (cons 'internal:-2reg
	 (lambda (as src1 src2 dest)
	   (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	       (error "Assembler:internal:-2reg: reg is not HW"))
	   (emit-arith-primop!
	    as $i.tsubrcc $i.addr $m.subtract src1 src2 dest #t)))

   (cons 'internal:+imm2reg
	  (lambda (as src1 imm dest)
	   (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	       (error "Assembler: internal:+imm2reg: reg is not HW"))
	   (emit-arith-primop!
	    as $i.taddicc $i.subi $m.add src1 imm dest #f)))

   (cons 'internal:-imm2reg
	  (lambda (as src1 imm dest)
	   (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	       (error "Assembler: internal:-imm2reg: reg is not HW"))
	   (emit-arith-primop!
	    as $i.tsubicc $i.addi $m.subtract src1 imm dest #f)))

   (cons 'internal:bfnull?
	 (lambda (as reg label)
	   (if (not (hardware-mapped? reg))
	       (error "Assembler: internal:bfnull?: reg is not HW"))
	   (emit! as `(,$i.subicc ,reg ,$imm.null ,$r.g0))
	   (emit! as `(,$i.bne.a ,(make-asm-label label)))
	   (emit! as `(,$i.slot))))

   (cons 'internal:bfpair?
	 (lambda (as reg label)
	   (if (not (hardware-mapped? reg))
	       (error "Assembler: internal:bfpair?: reg is not HW"))
	   (emit! as `(,$i.andi ,reg ,$tag.tagmask ,$r.tmp0))
	   (emit! as `(,$i.xoricc ,$r.tmp0 ,$tag.pair-tag ,$r.g0))
	   (emit! as `(,$i.bne.a ,(make-asm-label label)))
	   (emit! as `(,$i.slot))))

   (cons 'internal:bfzero?
	 (lambda (as reg label)
	   (if (not (hardware-mapped? reg))
	       (error "Assembler: internal:bfzero?: reg is not HW"))
	   (emit-bcmp-primop! as $i.bne.a reg $r.g0 label $m.zerop #t)))

   (cons 'internal:bf=
	 (lambda (as src1 src2 label)
	   (emit-bcmp-primop! as $i.bne.a src1 src2 label $m.numeq #t)))

   (cons 'internal:bf<
	 (lambda (as src1 src2 label)
	   (emit-bcmp-primop! as $i.bge.a src1 src2 label $m.numlt #t)))

   (cons 'internal:bf<=
	 (lambda (as src1 src2 label)
	   (emit-bcmp-primop! as $i.bg.a src1 src2 label $m.numle #t)))

   (cons 'internal:bf>
	 (lambda (as src1 src2 label)
	   (emit-bcmp-primop! as $i.ble.a src1 src2 label $m.numgt #t)))

   (cons 'internal:bf>=
	 (lambda (as src1 src2 label)
	   (emit-bcmp-primop! as $i.bl.a src1 src2 label $m.numge #t)))

   (cons 'internal:bf=imm
	 (lambda (as src1 imm label)
	   (emit-bcmp-primop! as $i.bne.a src1 imm label $m.numeq #f)))

   (cons 'internal:bf<imm
	 (lambda (as src1 imm label)
	   (emit-bcmp-primop! as $i.bge.a src1 imm label $m.numlt #f)))

   (cons 'internal:bf<=imm
	 (lambda (as src1 imm label)
	   (emit-bcmp-primop! as $i.bg.a src1 imm label $m.numle #f)))

   (cons 'internal:bf>imm
	 (lambda (as src1 imm label)
	   (emit-bcmp-primop! as $i.ble.a src1 imm label $m.numgt #f)))

   (cons 'internal:bf>=imm
	 (lambda (as src1 imm label)
	   (emit-bcmp-primop! as $i.bl.a src1 imm label $m.numge #f)))

   (cons 'internal:eq2reg
	 (lambda (as src1 src2 dest)
	   (let ((tmp (if (hardware-mapped? src2)
			  src2
			  (emit-load-reg! as src2 $r.tmp0))))
	     (emit! as `(,$i.subrcc ,src1 ,tmp ,$r.g0))
	     (emit-set-boolean-reg! as dest))))

   (cons 'internal:bfeq?
	 (lambda (as src1 src2 label)
	   (emit! as `(,$i.subrcc ,src1 ,src2 ,$r.g0))
	   (emit! as `(,$i.bne.a ,(make-asm-label label)))
	   (emit! as `(,$i.slot))))

   (cons 'internal:bfeq?imm
	 (lambda (as src1 imm label)
	   (emit! as `(,$i.subicc ,src1 ,(thefixnum imm) ,$r.g0))
	   (emit! as `(,$i.bne.a ,(make-asm-label label)))
	   (emit! as `(,$i.slot))))


   ;; old ones; remove when new versions are mature.

   (cons '+imm
	 (lambda (as imm)
	   (emit-primop.4arg! as 'internal:+imm2reg $r.result imm $r.result)))

   (cons '-imm
	 (lambda (as imm)
	   (emit-primop.4arg! as 'internal:-imm2reg $r.result imm $r.result)))

   (cons 'bfnull?
	 (lambda (as label)
	   (emit-primop.3arg! as 'internal:bfnull? $r.result label)))

   (cons 'bfpair?
	 (lambda (as label)
	   (emit-primop.3arg! as 'internal:bfpair? $r.result label)))

   (cons 'bfzero?
	 (lambda (as label)
	   (emit-primop.3arg! as 'internal:bfzero? $r.result label)))

   (cons 'bf=
	 (lambda (as r label)
	   (emit-primop.4arg! as 'internal:bf= $r.result r label)))

   (cons 'bf<
	 (lambda (as r label)
	   (emit-primop.4arg! as 'internal:bf< $r.result r label)))

   (cons 'bf<=
	 (lambda (as r label)
	   (emit-primop.4arg! as 'internal:bf<= $r.result r label)))

   (cons 'bf>
	 (lambda (as r label)
	   (emit-primop.4arg! as 'internal:bf> $r.result r label)))

   (cons 'bf>=
	 (lambda (as r label)
	   (emit-primop.4arg! as 'internal:bf>= $r.result r label)))

   ;-----------------------------------------------------------------

   ; These are used by the mal code for the i/o system, and never
   ; actually generated by the compiler (they should not figure in the
   ; integrables table!)

   (cons 'open-file
	 (lambda (as r1 r2)
	   (emit-move2hwreg! as r1 $r.argreg2)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.open-file ,$r.o7))
	   (emit-move2hwreg! as r2 $r.argreg3)))

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
	   (emit-move2hwreg! as r1 $r.argreg2)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.read-file ,$r.o7))
	   (emit-move2hwreg! as r2 $r.argreg3)))

   (cons 'write-file
	 (lambda (as r1 r2)
	   (emit-move2hwreg! as r1 $r.argreg2)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.write-file ,$r.o7))
	   (emit-move2hwreg! as r2 $r.argreg3)))

   ; This one is used to get resource data.
   ; It takes a vector argument and fills in the slots of the vector with
   ; the resource data.

   (cons 'sys$resource-usage!
	 (lambda (as)
	   (emit! as `(,$i.jmpli ,$r.millicode ,$m.resource-usage ,$r.o7))
	   (emit! as `(,$i.nop))))

   ))

; quite temporary

(define (silly name)
  (error 'silly "Silly, silly. (undefined integrable ~a)." name))

; not so temporary

; set-car!, set-cdr!, cell-set!
; Tag checks must be done by the caller.
; Pair ptr in RESULT, argument in register X.

(define (emit-setcar/setcdr! as x offs)
  (let ((x (if (hardware-mapped? x)
	       x
	       (emit-load-reg! as x $r.argreg2))))
    (emit! as `(,$i.sti ,x ,(- offs $tag.pair-tag) ,$r.result))
    (if register-transactions-for-side-effects
	(let ((l (new-label)))
	  (if inline-assignment
	      (begin
		(emit! as `(,$i.subrcc ,$r.result ,$r.e-top ,$r.g0))
		(emit! as `(,$i.ble.a ,l))
		(emit! as `(,$i.slot))))
	  (emit! as `(,$i.jmpli ,$r.millicode ,$m.addtrans ,$r.o7))
	  (emit! as `(,$i.orr ,x ,$r.g0 ,$r.argreg2))
	  (if inline-assignment
	      (emit! as `(,$i.label ,l)))))))

(define (emit-set-boolean! as)
  (emit-set-boolean-reg! as $r.result))

(define (emit-set-boolean-reg! as dest)
  (let ((l1 (new-label)))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.true ,dest))
    (emit! as `(,$i.bne.a ,l1))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.false ,dest))
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

(define (emit-double-tagcheck-assert! as tag1 tag2 excode)
  (let ((l0 (new-label))
	(l1 (new-label))
	(l2 (new-label)))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.andi ,$r.result 7 ,$r.tmp0))
    (emit! as `(,$i.xoricc ,$r.tmp0 ,tag1 ,$r.g0))
    (emit! as `(,$i.be.a ,l1))
    (emit! as `(,$i.ldbi ,$r.result (+ (- ,tag1) 3) ,$r.tmp0))
    (emit! as `(,$i.label ,l2))
    (emit! as `(,$i.ori ,$r.g0 ,(thefixnum excode) ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.o7))
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

(define (emit-single-tagcheck-assert! as tag1 excode)
  (emit-single-tagcheck-assert-reg! as tag1 $r.result excode))

(define (emit-single-tagcheck-assert-reg! as tag1 reg excode)
  (let ((l0 (new-label))
	(l1 (new-label))
	(l2 (new-label)))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.andi ,reg 7 ,$r.tmp0))
    (emit! as `(,$i.xoricc ,$r.tmp0 ,tag1 ,$r.g0))
    (emit! as `(,$i.be.a ,l1))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.label ,l2))
    (emit! as `(,$i.ori ,$r.g0 ,(thefixnum excode) ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l0 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l1))
    l2))

; Assert that a machine register has a fixnum in it.
; Returns the label of the fault code.

(define (emit-assert-fixnum! as reg excode)
  (let ((l0 (new-label))
	(l1 (new-label))
	(l2 (new-label)))
    (emit! as `(,$i.label ,l2))
    (emit! as `(,$i.andicc ,reg 3 ,$r.g0))
    (emit! as `(,$i.be.a ,l1))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.ori ,$r.g0 ,(thefixnum excode) ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l2 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l1))
    l0))

; Assert that RESULT has a character in it.
; Returns the label of the fault code.

(define (emit-assert-char! as excode)
  (let ((l0 (new-label))
	(l1 (new-label))
	(l2 (new-label)))
    (emit! as `(,$i.label ,l1))
    (emit! as `(,$i.andi ,$r.result #xFF ,$r.tmp0))
    (emit! as `(,$i.subicc ,$r.tmp0 ,$imm.character ,$r.g0))
    (emit! as `(,$i.be.a ,l2))
    (emit! as `(,$i.slot))
    (emit! as `(,$i.label ,l0))
    (emit! as `(,$i.ori ,$r.g0 ,(thefixnum excode) ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l1 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l2))
    l0))

; This is more expensive than what is good for it (5 cycles in the usual case),
; but there does not seem to be a better way.

(define (emit-assert-positive-fixnum! as reg excode)
  (let ((l1 (new-label))
	(l2 (new-label))
	(l3 (new-label))) 
    (emit! as `(,$i.label ,l2))
    (emit! as `(,$i.tsubrcc ,reg ,$r.g0 ,$r.g0))
    (emit! as `(,$i.bvc ,l1))
    (emit! as `(,$i.nop))
    (emit! as `(,$i.label ,l3))
    (emit! as `(,$i.ori ,$r.g0 ,(thefixnum excode) ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.o7))
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
	        (emit-load-reg! as r $r.tmp1)
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

(define (emit-bcmp-primop! as ntest src1 src2 label generic src2isreg)
  (let ((l1 (new-label))
	(l2 (make-asm-label label))
	(r  (if src2isreg
		(if (not (hardware-mapped? src2))
		    (emit-load-reg! as src2 $r.tmp1)
		    src2)
		(thefixnum src2)))
	(op (if src2isreg $i.tsubrcc $i.tsubicc))
	(mov (if src2isreg $i.orr $i.ori)))
    (emit! as `(,op ,src1 ,r ,$r.g0))
    (emit! as `(,$i.bvc.a ,l1))
    (emit! as `(,ntest ,l2))
    (emit! as `(,$i.nop))

    ; generic case
    ; must move src1 -> result if src1 is not result
    ; must move src2 -> argreg2 if src2 is not argreg2

    (let ((n (+ (if (not (= src1 $r.result)) 1 0)
		(if (or (not src2isreg) (not (= r $r.argreg2))) 1 0))))
      (if (= n 2)
	  (emit! as `(,mov ,$r.g0 ,r ,$r.argreg2)))
      (emit! as `(,$i.jmpli ,$r.millicode ,generic ,$r.o7))
      (if (> n 0)
	  (if (> n 1)
	      (emit! as `(,$i.orr ,src1 ,$r.g0 ,$r.result))
	      (emit! as `(,mov ,$r.g0 ,r ,$r.argreg2)))
	  (emit! as `(,$i.nop)))
      ; now test result and branch as required
      (emit! as `(,$i.subicc ,$r.result ,$imm.false ,$r.g0))
      (emit! as `(,$i.be ,l2))
      (emit! as `(,$i.label ,l1))
      (emit! as `(,$i.nop)))))

; Generic arithmetic for + and -.
; Some rules:
;   We have two HW registers src1 and dest.
;   If src2isreg is #t then src2 may be a HW reg or a SW reg
;   If src2isreg is #f then src2 is an immediate fixnum, not shifted.
;   Src1 and dest may be RESULT, but src2 may not.
;   Src2 may be ARGREG2, the others may not.

(define (emit-arith-primop! as op invop generic src1 src2 dest src2isreg)
  (let ((l1  (new-label))
	(op2 (if src2isreg
		 (if (not (hardware-mapped? src2))
		     (emit-load-reg! as src2 $r.tmp1)
		     src2)
		 (thefixnum src2))))
    (if (and src2isreg (= op2 dest))
	(begin (emit! as `(,op ,src1 ,op2 ,$r.tmp0))
	       (emit! as `(,$i.bvc.a ,l1))
	       (emit! as `(,$i.orr ,$r.tmp0 ,$r.g0 ,dest)))
	(begin (emit! as `(,op ,src1 ,op2 ,dest))
	       (emit! as `(,$i.bvc.a ,l1))
	       (emit! as `(,$i.slot))
	       (emit! as `(,invop ,dest ,op2 ,dest))))
    (let ((n    (+ (if (not (= src1 $r.result)) 1 0)
		   (if (or (not src2isreg) (not (= op2 $r.argreg2))) 1 0)))
	  (mov2 (if src2isreg $i.orr $i.ori)))
      (if (= n 2)
	  (emit! as `(,mov2 ,$r.g0 ,op2 ,$r.argreg2)))
      (emit! as `(,$i.jmpli ,$r.millicode ,generic ,$r.o7))
      (if (> n 0)
	  (if (> n 1)
	      (emit! as `(,$i.orr ,src1 ,$r.g0 ,$r.result))
	      (emit! as `(,mov2 ,$r.g0 ,op2 ,$r.argreg2)))
	  (emit! as `(,$i.nop)))
      ; Generic arithmetic leaves stuff in RESULT, must move to dest if
      ; dest is not RESULT.
      (if (not (= dest $r.result))
	  (emit! as `(,$i.orr ,$r.result ,$r.g0 ,dest)))
      (emit! as `(,$i.label ,l1)))))

; Get the length of a vector or bytevector structure, with tag checking
; included.

(define (emit-get-length! as tag1 tag2 excode)
  (if (not unsafe-mode)
      (if tag2
	  (emit-double-tagcheck-assert! as tag1 tag2 excode)
	  (emit-single-tagcheck-assert! as tag1 excode)))
  (emit! as `(,$i.ldi ,$r.result (- ,tag1) ,$r.tmp0))
  (emit! as `(,$i.srli ,$r.tmp0 8 ,$r.result))
  (if (= tag1 $tag.bytevector-tag)
      (emit! as `(,$i.slli ,$r.result 2 ,$r.result))))


; BYTEVECTORS

; The major problem is that $r.result is clobbered by the call to alloc(i),
; and hence, it must be preserved. Since $r.argreg3 is known not to be touched
; by the allocator, it is used for preserving the value.

(define (emit-make-bytevector-like! as hdr ptrtag)
  (let ((fault (emit-assert-positive-fixnum! as $r.result $ex.mkbvl)))

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
  (let* ((fault (emit-double-tagcheck-assert! as
					      $tag.bytevector-tag
					      hdr
					      $ex.bvfill))
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
;
; fault is defined iff unsafe-mode = #f

(define (emit-bytevector-like-ref! as x fault charize?)
  (let ((r (if (hardware-mapped? x)
	       x
	       (emit-load-reg! as x $r.tmp1))))
    (if (not unsafe-mode)
	(begin
	  ; check that index is fixnum
	  (emit! as `(,$i.andicc ,r 3 ,$r.g0))
	  (emit! as `(,$i.bne ,fault))
	  (emit! as `(,$i.ldi ,$r.result (- ,$tag.bytevector-tag) ,$r.tmp0))
	  ; check length
	  (emit! as `(,$i.srli ,$r.tmp0 8 ,$r.tmp0))
	  (emit! as `(,$i.srai ,r 2 ,$r.tmp1))
	  (emit! as `(,$i.subrcc ,$r.tmp1 ,$r.tmp0 ,$r.g0))
	  (emit! as `(,$i.bgeu.a ,fault))
	  (emit! as `(,$i.slot)))
	(begin
	  (emit! as `(,$i.srai ,r 2 ,$r.tmp1))))
    ; doit
    (emit! as `(,$i.subi ,$r.result ,(- $tag.bytevector-tag 4) ,$r.tmp0))
    (emit! as `(,$i.ldbr ,$r.tmp0 ,$r.tmp1 ,$r.tmp0))
    (if (not charize?)
	(emit! as `(,$i.slli ,$r.tmp0 2 ,$r.result))
	(begin (emit! as `(,$i.slli ,$r.tmp0 16 ,$r.result))
	       (emit! as `(,$i.ori ,$r.result ,$imm.character ,$r.result))))))


; fault is valid iff unsafe-mode = #f

(define (emit-bytevector-like-set! as x y fault)
  (let ((r1 (if (hardware-mapped? x)
		x
		(emit-load-reg! as x $r.tmp0)))
	(r2 (if (hardware-mapped? y)
		y
		(emit-load-reg! as y $r.tmp1))))
    (if (not unsafe-mode)
	(begin
	  ; both should be fixnums
	  (emit! as `(,$i.orr ,r1 ,r2 ,$r.tmp2))
	  (emit! as `(,$i.andicc ,$r.tmp2 3 ,$r.g0))
	  (emit! as `(,$i.bne ,fault))
	  (emit! as `(,$i.ldi ,$r.result (- ,$tag.bytevector-tag) ,$r.tmp2))
	  ; index must be valid
	  (emit! as `(,$i.srli ,$r.tmp2 8 ,$r.tmp2))
	  (emit! as `(,$i.srai ,r1 2 ,$r.tmp0))
	  (emit! as `(,$i.subrcc ,$r.tmp0 ,$r.tmp2 ,$r.g0))
	  (emit! as `(,$i.bgeu ,fault)))
	(begin
	  (emit! as `(,$i.srai ,r1 2 ,$r.tmp0))))
    (emit! as `(,$i.srli ,r2 2 ,$r.tmp1))
    (emit! as `(,$i.subi ,$r.result ,(- $tag.bytevector-tag 4) ,$r.tmp2))
    (emit! as `(,$i.stbr ,$r.tmp1 ,$r.tmp2 ,$r.tmp0))))


; VECTORS and PROCEDURES

; emit-make-vector-like! assumes argreg3 is not destroyed by alloci.

(define (emit-make-vector-like! as r hdr ptrtag)
  (let ((fault (emit-assert-positive-fixnum! as $r.result $ex.mkvl)))
    (emit! as `(,$i.orr ,$r.result ,$r.g0 ,$r.argreg3))
    (emit! as `(,$i.addi ,$r.result 4 ,$r.result))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.alloci ,$r.o7))
    (if (null? r)
	(emit! as `(,$i.ori ,$r.g0 ,$imm.null ,$r.argreg2))
	(emit-move2hwreg! as r $r.argreg2))
    (emit! as `(,$i.slli ,$r.argreg3 8 ,$r.tmp0))
    (emit! as `(,$i.addi ,$r.tmp0 ,hdr ,$r.tmp0))
    (emit! as `(,$i.sti ,$r.tmp0 0 ,$r.result))
    (emit! as `(,$i.addi ,$r.result ,ptrtag ,$r.result))))


; `vector-ref' in various guises.
;
; Pointer must be valid; header may be reloaded unnecessarily. Fix later.

; fault is valid iff unsafe-mode = #f

(define (emit-vector-like-ref! as r fault tag)
  (let ((r1 (if (hardware-mapped? r)
		r
		(emit-load-reg! as r $r.argreg2))))
    (if (not unsafe-mode)
	(begin
	  (emit! as `(,$i.andicc ,r1 3 ,$r.g0))
	  (emit! as `(,$i.bne.a ,fault))
	  (emit! as `(,$i.slot))
	  (emit! as `(,$i.ldi ,$r.result (- ,tag) ,$r.tmp2))
	  (emit! as `(,$i.srai ,$r.tmp2 8 ,$r.tmp2))
	  (emit! as `(,$i.subrcc ,$r.tmp2 ,r1 ,$r.g0))
	  (emit! as `(,$i.blu ,fault))))
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

; fault is valid iff unsafe-mode = #f

(define (emit-vector-like-set! as x y fault tag)
  (let ((r1 (if (hardware-mapped? x)
		x
		(emit-load-reg! as x $r.argreg2))))
    (if (not unsafe-mode)
	(begin
	  (emit! as `(,$i.andicc ,r1 3 ,$r.g0))
	  (emit! as `(,$i.bne.a ,fault))
	  (emit! as `(,$i.slot))
	  (emit! as `(,$i.ldi ,$r.result (- ,tag) ,$r.tmp2))
	  (emit! as `(,$i.srai ,$r.tmp2 8 ,$r.tmp2))
	  (emit! as `(,$i.subrcc ,$r.tmp2 ,r1 ,$r.g0))
	  (emit! as `(,$i.bleu.a ,fault))
	  (emit! as `(,$i.slot))))
    (let ((y (if (hardware-mapped? y)
		 y
		 (emit-load-reg! as y $r.argreg3))))
      (emit! as `(,$i.addr ,$r.result ,r1 ,$r.tmp0))
      (emit! as `(,$i.sti ,y ,(- 4 tag) ,$r.tmp0))
      (if register-transactions-for-side-effects
	  (if inline-assignment
	      (let ((l (new-label)))
		(emit! as `(,$i.subrcc ,$r.result ,$r.e-top ,$r.g0))
		(emit! as `(,$i.ble.a ,l))
		(emit! as `(,$i.slot))
		(emit! as `(,$i.jmpli ,$r.millicode ,$m.addtrans ,$r.o7))
		(emit! as `(,$i.orr ,$r.g0 ,y ,$r.argreg2))
		(emit! as `(,$i.label ,l)))
	      (begin
		(emit! as `(,$i.jmpli ,$r.millicode ,$m.addtrans ,$r.o7))
		(emit! as `(,$i.orr ,$r.g0 ,y ,$r.argreg2))))))))


; We check the tags of both by xoring them and seeing if the low byte is 0.
; If so, then we can subtract one from the other (tag and all) and check the
; condition codes.
;
; The branch-on-true instruction must have the annull bit set.

(define (emit-char-cmp as r btrue excode)
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
    (emit! as `(,$i.ori ,$r.g0 ,(thefixnum excode) ,$r.tmp0))
    (emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.o7))
    (emit! as `(,$i.addi ,$r.o7 (- ,l0 (- $ 4) 8) ,$r.o7))
    (emit! as `(,$i.label ,l1))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.false ,$r.result))
    (emit! as `(,btrue ,l2))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.true ,$r.result))
    (emit! as `(,$i.label ,l2))))

; eof
