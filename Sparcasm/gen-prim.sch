;; -*- Scheme -*-
;; Larceny assembler (Sparc) -- emitting code for integrables.
;;
;; $Id: gen-prim.sch,v 1.3 1997/05/31 01:52:16 lth Exp lth $
;;
;; History
;;   November 3, 1995 / lth (v0.25)
;;     - Minor fixes of cosmetic art; introduced fixnum?.
;;
;;   August 2, 1995 / lth (v0.25)
;;     - Fixed some bugs in assertion routines where the value was not
;;       passed to the error handler if it was not already in RESULT.
;;
;;   July 16, 1995 / lth (v0.24)
;;     - Added string-set!
;;
;;   June 27, 1995 / lth (v0.24)
;;     - Added syscall
;;     - Added sys$bvlcmp
;;
;;   May 21-24, 1995 / lth (v0.24)
;;     - Added internal primitives for boolean optimization for control
;;       for characters.
;;     - Added support for non-naive op2imm character predicates.
;;     - Implemented force-hwreg! abstraction.
;;     - Implemented some prototype sparc instruction abstractions.
;;     - Implemented millicode-call abstraction.
;;     - Fixed bug in exception handling code for character predicates.
;;     - Rewrote implementation of primop table to be more abstract;
;;       can now be converted to hash table.
;;     - Unsafe-mode now effective on character comparison predicates.
;;
;;   March 7, 1995 / lth (v0.23)
;;     Support for inlining of generation check for set-car!/set-cdr!/
;;     cell-set!.
;;
;;   July 6, 1994 / lth (v0.20)
;;     Fixed bug which left a vector header behind in argreg3 after making
;;     a vector. This is really evil if the value is propagated into a
;;     GP register and then saved in a stack frame; the collector will have
;;     a fit if implemented in a particular way... Don't know why this was
;;     not a problem in the old collector.
;;
;;   July 5, 1994 / lth (v0.20)
;;     Changed to deal with new list of millicode procs.
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
;; - Break typechecking out into separate procedures (to reuse).
;; - Really want to use a hash table for the opcode table, not a list
;;   like here.
;; - Attempt to make do with tmp0, tmp1, argreg2, and argreg3, so that
;;   tmp2 can be freed up for general use.

;; these are a bit obsolete but too much effort to rename right now...

(define (emit-primop0! as op)
  ((find-primop op) as))

(define (emit-primop1! as op r)
  ((find-primop op) as r))

(define (emit-primop2! as op r1 r2)
  ((find-primop op) as r1 r2))

(define conv-list '((+ . +imm) (- . -imm)))

(define (emit-primop2imm! as op imm)
  ((find-primop (cdr (assq op conv-list))) as imm))

;; These are new and should be used in all cases.

(define (emit-primop.3arg! as a1 a2 a3)
  ((find-primop a1) as a2 a3))

(define (emit-primop.4arg! as a1 a2 a3 a4)
  ((find-primop a1) as a2 a3 a4))


;---------------------------------------------------------------------------
; Assoc list of primops, indexed by name.

(define primop-list '())

(define (define-primop name proc)
  (set! primop-list (cons (cons name proc) primop-list)))

(define (find-primop name)
  (cdr (assq name primop-list)))

(define (setup-primops)
  (set! primop-list (reverse! primop-list)))


;--------------------------------------------------------------------------
; Utility: src is a register, hwreg is a hardware register. If src is a
; hardware register, return src. Otherwise, emit an instruction to load
; src into hwreg and return hwreg.

(define (force-hwreg! as src hwreg)
  (if (hardware-mapped? src)
      src
      (emit-load-reg! as src hwreg)))


;---------------------------------------------------------------------------
; Primops

(define-primop 'unspecified
  (lambda (as)
    (emit-immediate->register! as $imm.unspecified $r.result)))

(define-primop 'undefined
  (lambda (as)
    (emit-immediate->register! as $imm.undefined $r.result)))

(define-primop 'eof-object
  (lambda (as)
    (emit-immediate->register! as $imm.eof $r.result)))

(define-primop 'enable-interrupts
  (lambda (as)
    (millicode-call/0arg as $m.enable-interrupts)))

(define-primop 'disable-interrupts
  (lambda (as)
    (millicode-call/0arg as $m.disable-interrupts)))

; Number predicates

(define-primop 'zero?
  (lambda (as)
    (emit-cmp-primop! as $i.tsubrcc $i.be.a $m.zerop $r.g0)))

(define-primop '=
  (lambda (as r)
    (emit-cmp-primop! as $i.tsubrcc $i.be.a $m.numeq r)))

(define-primop '<
  (lambda (as r)
    (emit-cmp-primop! as $i.tsubrcc $i.bl.a $m.numlt r)))

(define-primop '<=
  (lambda (as r)
    (emit-cmp-primop! as $i.tsubrcc $i.ble.a $m.numle r)))

(define-primop '>
  (lambda (as r)
    (emit-cmp-primop! as $i.tsubrcc $i.bg.a $m.numgt r)))

(define-primop '>=
  (lambda (as r)
    (emit-cmp-primop! as $i.tsubrcc $i.bge.a $m.numge r)))

(define-primop 'complex?
  (lambda (as)
    (millicode-call/0arg as $m.complexp)))

(define-primop 'real?
  (lambda (as)
    (millicode-call/0arg as $m.realp)))

(define-primop 'rational?
  (lambda (as)
    (millicode-call/0arg as $m.rationalp)))

(define-primop 'integer?
  (lambda (as)
    (millicode-call/0arg as $m.integerp)))

(define-primop 'exact?
  (lambda (as)
    (millicode-call/0arg as $m.exactp)))

(define-primop 'inexact?
  (lambda (as)
    (millicode-call/0arg as $m.inexactp)))

(define-primop 'fixnum?
  (lambda (as)
    (emit! as `(,$i.andicc ,$r.result 3 ,$r.g0))
    (emit-set-boolean! as)))

; -----------------------------------------------------------------

; Generic arithmetic and (bit-)logical operations

(define-primop '+
  (lambda (as r)
    (emit-primop.4arg! as 'internal:+2reg $r.result r $r.result)))

(define-primop '-
  (lambda (as r)
    (emit-primop.4arg! as 'internal:-2reg $r.result r $r.result)))

(define-primop '*
  (lambda (as r)
    (millicode-call/1arg as $m.multiply r)))

(define-primop '/
  (lambda (as r)
    (millicode-call/1arg as $m.divide r)))

(define-primop 'quotient
  (lambda (as r)
    (millicode-call/1arg as $m.quotient r)))

(define-primop 'remainder
  (lambda (as r)
    (millicode-call/1arg as $m.remainder r)))

; FIXME: can speed up the common case.
(define-primop '--
  (lambda (as)
    (let ((l1 (new-label)))
      (emit! as `(,$i.tsubrcc ,$r.g0 ,$r.result ,$r.tmp0))
      (emit! as `(,$i.bvc.a ,l1))
      (emit! as `(,$i.orr ,$r.g0 ,$r.tmp0 ,$r.result))
      (millicode-call/0arg as $m.negate)
      (emit! as `(,$i.label ,l1)))))

(define-primop 'round
  (lambda (as)
    (millicode-call/0arg as $m.round)))

(define-primop 'truncate
  (lambda (as)
    (millicode-call/0arg as $m.truncate)))

; fixnums only

(define-primop 'lognot
  (lambda (as)
    (emit-assert-fixnum! as $r.result $ex.lognot)
    (emit! as `(,$i.ornr ,$r.g0 ,$r.result ,$r.result)) ; order matters.
    (emit! as `(,$i.xori ,$r.result 3 ,$r.result))))

; fixnums only

(define-primop 'logand
  (lambda (as x)
    (let ((tmp (force-hwreg! as x $r.tmp1))
	  (L1  (new-label)))
      (emit! as `(,$i.tsubrcc ,$r.result ,tmp ,$r.g0))
      (emit! as `(,$i.bvc.a ,L1))
      (emit! as `(,$i.andr ,$r.result ,tmp ,$r.result))
      (emit! as `(,$i.ori ,$r.g0 ,$ex.logand ,$r.tmp0))
      (millicode-call/1arg as $m.exception tmp)
      (emit! as `(,$i.label ,L1)))))

; fixnums only
; Can do better tagcheck using tsubrcc

(define-primop 'logior
  (lambda (as x)
    (let ((tmp (force-hwreg! as x $r.tmp1)))
      (emit! as `(,$i.orr ,$r.result ,tmp ,$r.tmp0))
      (emit-assert-fixnum! as $r.tmp0 $ex.logior)
      (emit! as `(,$i.orr ,$r.result ,tmp ,$r.result)))))

; fixnums only
; FIXME: can do better tag check using tsubrcc

(define-primop 'logxor
  (lambda (as x)
    (let ((tmp (force-hwreg! as x $r.tmp1)))
      (emit! as `(,$i.orr ,$r.result ,tmp ,$r.tmp0))
      (emit-assert-fixnum! as $r.tmp0 $ex.logxor)
      (emit! as `(,$i.xorr ,$r.result ,tmp ,$r.result)))))

; Fixnums only, and only positive shifts are meaningful.
; FIXME: This is incompatible with MacScheme and MIT Scheme.
; FIXME: Really ought to reuse fault handler (requires rewrite of asserts).

(define-primop 'lsh
  (lambda (as x)
    (let ((tmp (force-hwreg! as x $r.tmp1)))
      (emit-assert-fixnum! as $r.result $ex.lsh)
      (emit-assert-positive-fixnum! as tmp $ex.lsh)
      (emit! as `(,$i.srai ,tmp 2 ,$r.tmp1))
      (emit! as `(,$i.sllr ,$r.result ,$r.tmp1 ,$r.result)))))

; Fixnums only, and only positive shifts are meaningful.
; FIXME: this should be lsh with a negative shift.
; Watch the semantics of the shift instruction! We have to do the
; original shift first and *then* adjust; no combination is possible
; in general.

(define-primop 'rshl
  (lambda (as x)
    (let ((tmp (force-hwreg! as x $r.tmp1)))
      (emit-assert-fixnum! as $r.result $ex.rshl)
      (emit-assert-positive-fixnum! as tmp $ex.rshl)
      (emit! as `(,$i.srai ,tmp 2 ,$r.tmp1))
      (emit! as `(,$i.srlr ,$r.result ,$r.tmp1 ,$r.result))
      (emit! as `(,$i.andni ,$r.result 3 ,$r.result)))))

; fixnums only, and only positive shifts are meaningful.

(define-primop 'rsha
  (lambda (as x)
    (let ((tmp (force-hwreg! as x $r.tmp1)))
      (emit-assert-fixnum! as $r.result $ex.rsha)
      (emit-assert-positive-fixnum! as tmp $ex.rsha)
      (emit! as `(,$i.srai ,tmp 2 ,$r.tmp1))
      (emit! as `(,$i.srar ,$r.result ,$r.tmp1 ,$r.result))
      (emit! as `(,$i.andni ,$r.result 3 ,$r.result)))))

; fixnums only

(define-primop 'rot
  (lambda (as x)
    (error "Sparcasm: ROT primop is not implemented.")))

; Various type predicates

(define-primop 'null?
  (lambda (as)
    (sparc.cmpi as $r.result $imm.null)
    (emit-set-boolean! as)))

(define-primop 'pair?
  (lambda (as)
    (emit-single-tagcheck->bool! as $tag.pair-tag)))

; Tests the specific representation, not 'flonum or compnum with 0i'.

(define-primop 'flonum?
  (lambda (as)
    (emit-double-tagcheck->bool! as $tag.bytevector-tag
				 (+ $imm.bytevector-header
				    $tag.flonum-typetag))))

(define-primop 'symbol?
  (lambda (as)
    (emit-double-tagcheck->bool! as $tag.vector-tag
				 (+ $imm.vector-header
				    $tag.symbol-typetag))))

(define-primop 'char?
  (lambda (as)
    (emit! as `(,$i.andi ,$r.result #xFF ,$r.tmp0))
    (emit! as `(,$i.xoricc ,$r.tmp0 ,$imm.character ,$r.g0))
    (emit-set-boolean! as)))

(define-primop 'string?
  (lambda (as)
    (emit-double-tagcheck->bool! as
				 $tag.bytevector-tag
				 (+ $imm.bytevector-header
				    $tag.string-typetag))))

(define-primop 'bytevector?
  (lambda (as)
    (emit-double-tagcheck->bool! as
				 $tag.bytevector-tag
				 (+ $imm.bytevector-header
				    $tag.bytevector-typetag))))

(define-primop 'bytevector-like?
  (lambda (as)
    (emit-single-tagcheck->bool! as $tag.bytevector-tag)))

(define-primop 'vector?
  (lambda (as)
    (emit-double-tagcheck->bool! as
				 $tag.vector-tag
				 (+ $imm.vector-header
				    $tag.vector-typetag))))

(define-primop 'vector-like?
  (lambda (as)
    (emit-single-tagcheck->bool! as $tag.vector-tag)))

(define-primop 'procedure?
  (lambda (as)
    (emit-single-tagcheck->bool! as $tag.procedure-tag)))

; Pair operations.

; If peephole optimization is turned on, then internal:cons2reg
; will be called directly.

(define-primop 'cons
  (lambda (as r)
    (emit-primop.4arg! as 'internal:cons2reg $r.result r $r.result)))

(define-primop 'car
  (lambda (as)
    (emit-primop.3arg! as 'internal:car2reg $r.result $r.result)))

(define-primop 'cdr
  (lambda (as)
    (emit-primop.3arg! as 'internal:cdr2reg $r.result $r.result)))

(define-primop 'set-car!
  (lambda (as x)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert! as $tag.pair-tag $ex.car))
    (emit-setcar/setcdr! as x 0)))

(define-primop 'set-cdr!
  (lambda (as x)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert! as $tag.pair-tag $ex.cdr))
    (emit-setcar/setcdr! as x 4)))

; Cells are internal data structures, represented using pairs.
; No error checking is done on cell references.

(define-primop 'make-cell
  (lambda (as)
    (emit-primop.4arg! as 'internal:cons2reg $r.result $r.g0 $r.result)))

(define-primop 'cell-ref
  (lambda (as)
    (emit-primop.3arg! as 'internal:cellref2reg $r.result $r.result)))

(define-primop 'cell-set!
  (lambda (as r)
    (emit-setcar/setcdr! as r 0)))

; Hooks to various system services

(define-primop 'syscall
  (lambda (as)
    (millicode-call/0arg as $m.syscall)))

(define-primop 'break
  (lambda (as)
    (millicode-call/0arg as $m.break)))

; Continuations. FIXME: Should implement as syscalls.

(define-primop 'creg
  (lambda (as)
    (millicode-call/0arg as $m.creg)))

(define-primop 'creg-set!
  (lambda (as)
    (millicode-call/0arg as $m.creg-set!)))

; Typetags

(define-primop 'typetag
  (lambda (as)
    (millicode-call/0arg as $m.typetag)))

(define-primop 'typetag-set!
  (lambda (as r)
    (millicode-call/1arg as $m.typetag-set r)))

; Misc.

(define-primop 'exact->inexact
  (lambda (as)
    (millicode-call/0arg as $m.exact->inexact)))

(define-primop 'inexact->exact
  (lambda (as)
    (millicode-call/0arg as $m.inexact->exact)))

(define-primop 'real-part
  (lambda (as)
    (millicode-call/0arg as $m.real-part)))

(define-primop 'imag-part
  (lambda (as)
    (millicode-call/0arg as $m.imag-part)))

(define-primop 'char->integer
  (lambda (as)
    (emit-assert-char! as $ex.char2int #f)
    (emit! as `(,$i.srli ,$r.result 14 ,$r.result))))

(define-primop 'integer->char
  (lambda (as)
    (emit-assert-fixnum! as $r.result $ex.int2char)
    (emit! as `(,$i.andi ,$r.result #x3FF ,$r.result))
    (emit! as `(,$i.slli ,$r.result 14 ,$r.result))
    (emit! as `(,$i.ori  ,$r.result ,$imm.character ,$r.result))))

(define-primop 'make-rectangular
  (lambda (as r)
    (millicode-call/1arg as $m.make-rectangular r)))

(define-primop 'not
  (lambda (as)
    (sparc.cmpi as $r.result $imm.false)
    (emit-set-boolean! as)))

(define-primop 'eq?
  (lambda (as x)
    (emit-primop.4arg! as 'internal:eq2reg $r.result x $r.result)))

(define-primop 'eqv?
  (lambda (as x)
    (let ((tmp (force-hwreg! as x $r.tmp0))
	  (l1  (new-label)))
      (emit! as `(,$i.subrcc ,$r.result ,tmp ,$r.g0))
      (emit! as `(,$i.be.a ,l1))
      (emit! as `(,$i.ori ,$r.g0 ,$imm.true ,$r.result))
      (millicode-call/1arg as $m.eqv tmp)
      (emit! as `(,$i.label ,l1)))))


; String and bytevector operations

(define-primop 'make-bytevector
	 (lambda (as)
	   (emit-make-bytevector-like! as
				       (+ $imm.bytevector-header
					  $tag.bytevector-typetag)
				       $tag.bytevector-tag)))

(define-primop 'bytevector-fill!
	 (lambda (as r)
	   (emit-bytevector-like-fill! as 
				       r
				       $imm.bytevector-header)))

(define-primop 'string-length
	 (lambda (as)
	   (emit-get-length! as
			     $tag.bytevector-tag
			     (+ $imm.bytevector-header $tag.string-typetag)
			     $ex.slen)))

(define-primop 'bytevector-length
	 (lambda (as)
	   (emit-get-length! as 
			     $tag.bytevector-tag
			     (+ $imm.bytevector-header $tag.bytevector-typetag)
			     $ex.bvlen)))

(define-primop 'bytevector-like-length
	 (lambda (as)
	   (emit-get-length! as $tag.bytevector-tag #f $ex.bvllen)))

(define-primop 'string-ref
	 (lambda (as r)
	   (let ((fault (if (not (unsafe-code))
			    (emit-double-tagcheck-assert! 
			     as
			     $tag.bytevector-tag
			     (+ $imm.bytevector-header $tag.string-typetag)
			     $ex.sref)
			    #f)))
	     (emit-bytevector-like-ref! as r fault #t))))

(define-primop 'bytevector-ref
	 (lambda (as r)
	   (let ((fault (if (not (unsafe-code))
			    (emit-double-tagcheck-assert!
			     as
			     $tag.bytevector-tag
			     (+ $imm.bytevector-header $tag.bytevector-typetag)
			     $ex.bvref)
			    #f)))
	     (emit-bytevector-like-ref! as r fault #f))))

(define-primop 'bytevector-like-ref
	 (lambda (as r)
	   (let ((fault (if (not (unsafe-code))
			    (emit-single-tagcheck-assert! as
							  $tag.bytevector-tag
							  $ex.bvlref)
			    #f)))
	     (emit-bytevector-like-ref! as r fault #f))))

(define-primop 'bytevector-like-set!
  (lambda (as x y)
    (let ((fault (if (not (unsafe-code))
		     (emit-single-tagcheck-assert! as
						   $tag.bytevector-tag
						   $ex.bvlset)
		     #f)))
      (emit-bytevector-like-set! as x y fault))))

(define-primop 'bytevector-set!
  (lambda (as x y)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert!
		      as
		      $tag.bytevector-tag
		      (+ $imm.bytevector-header $tag.bytevector-typetag)
		      $ex.bvset)
		     #f)))
      (emit-bytevector-like-set! as x y fault))))

(define-primop 'sys$bvlcmp
  (lambda (as x)
    (millicode-call/1arg as $m.bvlcmp x)))

; This is hard, and very ugly. It must be rewritten, but no point in 
; rewriting until the tag checking machinery is rewritten.
; Checked: 25 static instructions (maybe more). Can be sped up at the cost of
;          higher instruction count.
; Unchecked: 4 static instructions.

(define-primop 'string-set!
  (lambda (as r1 r2)
    (let ((fault  (new-label)))
      ; First check tags of lhs
      (if (not (unsafe-code))
	  (let ((again  (new-label))
		(next1  (new-label))
		(ptrtag $tag.bytevector-tag)
		(hdrtag (+ $imm.bytevector-header $tag.string-typetag))
		(excode $ex.sset))
	    (emit! as `(,$i.label ,again))
	    (emit! as `(,$i.andi ,$r.result 7 ,$r.tmp0))
	    (emit! as `(,$i.xoricc ,$r.tmp0 ,ptrtag ,$r.g0))
	    (emit! as `(,$i.be.a ,next1))
	    (emit! as `(,$i.ldi ,$r.result (- ,ptrtag) ,$r.tmp0))
	    (emit! as `(,$i.label ,fault))
	    (emit! as `(,$i.ori ,$r.g0 ,(thefixnum excode) ,$r.tmp0))
	    (emit-move2hwreg! as r1 $r.argreg2)
	    (emit-move2hwreg! as r2 $r.argreg3)
	    (millicode-call/ret as $m.exception again)
	    (emit! as `(,$i.label ,next1))
	    ; tmp0 now has the header
	    (emit! as `(,$i.andi ,$r.tmp0 #xFF ,$r.tmp1))
	    (emit! as `(,$i.subicc ,$r.tmp1 ,hdrtag ,$r.g0))
	    (emit! as `(,$i.bne ,fault))
	    (emit! as `(,$i.nop))))
      ; Now check index and rhs.
      ; Header is in tmp0; tmp1 and tmp2 are free.
      (let ((r1 (force-hwreg! as r1 $r.tmp2))
	    (r2 (force-hwreg! as r2 $r.tmp1)))
	(if (not (unsafe-code))
	    (begin
	      ; Can we combine these tests using tsubcc?
	      ; r1 should be fixnum
	      (emit! as `(,$i.andicc ,r1 3 ,$r.g0))
	      (emit! as `(,$i.bne ,fault))
	      (emit! as `(,$i.nop))
	      ; index must be valid; header is in tmp0
	      (emit! as `(,$i.srli ,$r.tmp0 8 ,$r.tmp0))
	      (emit! as `(,$i.srai ,r1 2 ,$r.tmp2))     ; should be fixed.
	      (emit! as `(,$i.subrcc ,$r.tmp2 ,$r.tmp0 ,$r.g0))
	      (emit! as `(,$i.bgeu ,fault))
	      (emit! as `(,$i.nop))
	      ; r2 should be a character
	      (emit! as `(,$i.andi ,r2 #xFF ,$r.tmp0))
	      (emit! as `(,$i.subicc ,$r.tmp0 ,$imm.character ,$r.g0))
	      (emit! as `(,$i.bne ,fault))
	      (emit! as `(,$i.nop)))
	    (begin
	      (emit! as `(,$i.srai ,r1 2 ,$r.tmp2))))
	; tmp2 has nativeint index. 
	; r2/tmp1 has character.
	; tmp0 is garbage.
	(emit! as `(,$i.srli ,r2 16 ,$r.tmp1))
	(emit! as `(,$i.subi ,$r.result ,(- $tag.bytevector-tag 4) ,$r.tmp0))
	(emit! as `(,$i.stbr ,$r.tmp1 ,$r.tmp0 ,$r.tmp2))))))

;-----------------------------------------------------------------

; Vector and procedure operations

(define-primop 'sys$partial-list->vector
  (lambda (as r)
    (millicode-call/1arg as $m.partial-list->vector r)))

(define-primop 'make-procedure
	 (lambda (as)
	   (emit-make-vector-like! as
				   '()
				   $imm.procedure-header
				   $tag.procedure-tag)))

(define-primop 'make-vector
	 (lambda (as r)
	   (emit-make-vector-like! as
				   r
				   (+ $imm.vector-header $tag.vector-typetag)
				   $tag.vector-tag)))

(define-primop 'vector-length
	 (lambda (as)
	   (emit-get-length! as
			     $tag.vector-tag
			     (+ $imm.vector-header $tag.vector-typetag)
			     $ex.vlen)))

(define-primop 'vector-like-length
	 (lambda (as)
	   (emit-get-length! as $tag.vector-tag #f $ex.vllen)))

(define-primop 'procedure-length
	 (lambda (as)
	   (emit-get-length! as $tag.procedure-tag #f $ex.plen)))

(define-primop 'vector-ref
	 (lambda (as r)
	   (let ((fault (if (not (unsafe-code))
			    (emit-double-tagcheck-assert!
			     as
			     $tag.vector-tag
			     (+ $imm.vector-header $tag.vector-typetag)
			     $ex.vref)
			    #f)))
	     (emit-vector-like-ref! as r fault $tag.vector-tag))))

(define-primop 'vector-like-ref
	 (lambda (as r)
	   (let ((fault (if (not (unsafe-code))
			    (emit-single-tagcheck-assert! as
							  $tag.vector-tag
							  $ex.vlref)
			    #f)))
	     (emit-vector-like-ref! as r fault $tag.vector-tag))))

(define-primop 'procedure-ref
	 (lambda (as r)
	   (let ((fault (if (not (unsafe-code))
			    (emit-single-tagcheck-assert! as
							  $tag.procedure-tag
							  $ex.pref)
			    #f)))
	     (emit-vector-like-ref! as r fault $tag.procedure-tag))))

(define-primop 'vector-set!
	 (lambda (as r1 r2)
	   (let ((fault (if (not (unsafe-code))
			    (emit-double-tagcheck-assert!
			     as
			     $tag.vector-tag
			     (+ $imm.vector-header $tag.vector-typetag)
			     $ex.vset)
			    #f)))
	     (emit-vector-like-set! as r1 r2 fault $tag.vector-tag))))

(define-primop 'vector-like-set!
	 (lambda (as r1 r2)
	   (let ((fault (if (not (unsafe-code))
			    (emit-single-tagcheck-assert! as
							  $tag.vector-tag
							  $ex.vlset)
			    #f)))
	     (emit-vector-like-set! as r1 r2 fault $tag.vector-tag))))

(define-primop 'procedure-set!
	 (lambda (as r1 r2)
	   (let ((fault (if (not (unsafe-code))
			    (emit-single-tagcheck-assert! as
							  $tag.procedure-tag
							  $ex.pset)
			    #f)))
	     (emit-vector-like-set! as r1 r2 fault $tag.procedure-tag))))

; -----------------------------------------------------------------

; Character predicates

(define-primop 'char<?
  (lambda (as x)
    (emit-char-cmp as x $i.bl.a $ex.char<?)))

(define-primop 'char<=?
  (lambda (as x)
    (emit-char-cmp as x $i.ble.a $ex.char<=?)))

(define-primop 'char=?
  (lambda (as x)
    (emit-char-cmp as x $i.be.a $ex.char=?)))

(define-primop 'char>?
  (lambda (as x)
    (emit-char-cmp as x $i.bg.a $ex.char>?)))

(define-primop 'char>=?
  (lambda (as x)
    (emit-char-cmp as x $i.bge.a $ex.char>=?)))

;-----------------------------------------------------------------
   
; These are introduced by peephole optimization. Old (obsolete but still
; used, by old code) names are not prefixed by internal: whereas new
; procedures are. Old code usually calls new code. Other code (above)
; also uses the internal versions.

(define-primop 'internal:car2reg
  (lambda (as src1 dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(error "Assembler: internal:car2reg: reg is not HW"))
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert-reg! as
					  $tag.pair-tag src1 $ex.car))
    (emit! as `(,$i.ldi ,src1 ,(- 0 $tag.pair-tag) ,dest))))

(define-primop 'internal:cdr2reg
  (lambda (as src1 dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(error "Assembler: internal:cdr2reg: reg is not HW"))
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert-reg! as
					  $tag.pair-tag src1 $ex.cdr))
    (emit! as `(,$i.ldi ,src1 ,(- 4 $tag.pair-tag) ,dest))))

(define-primop 'internal:cellref2reg
  (lambda (as src1 dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(error "Assembler: internal:cellref2reg: reg is not HW"))
    (emit! as `(,$i.ldi ,src1 ,(- $tag.pair-tag) ,dest))))

(define-primop 'internal:cons2reg
  (lambda (as src1 src2 dest)

    (define (emit-common-case cont)
      (if (eqv? src1 $r.result)
	  (emit! as `(,$i.orr ,$r.result ,$r.g0 ,$r.argreg2)))
      (emit! as `(,$i.jmpli ,$r.millicode ,$m.alloc ,$r.o7))
      (emit! as `(,$i.ori ,$r.g0 8 ,$r.result))
      (if (eqv? src1 $r.result)
	  (emit! as `(,$i.sti ,$r.argreg2 0 ,$r.result))
	  (emit! as `(,$i.sti ,src1 0 ,$r.result)))
      (let ((src2 (force-hwreg! as src2 $r.tmp1)))
	(emit! as `(,$i.sti ,src2 4 ,$r.result))
	(if cont
	    (emit! as `(,$i.b ,cont)))
	(emit! as `(,$i.addi ,$r.result 1 ,dest))))

					; lights-camera-action!
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(error "Assembler: internal:cons2reg: reg is not HW"))
    (if (inline-cons)
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
	  (let ((src2 (force-hwreg! as src2 $r.tmp1)))
	    (if (= src2 dest)
		(begin (emit! as `(,$i.sti ,src2 -4 ,$r.e-top))
		       (emit! as `(,$i.subi ,$r.e-top 7 ,dest)))
		(begin (emit! as `(,$i.subi ,$r.e-top 7 ,dest))
		       (emit! as `(,$i.sti ,src2 -4 ,$r.e-top))))
	    (emit! as `(,$i.label ,l2))))
	(begin
	  (emit-common-case #f)))))

(define-primop 'internal:+2reg
  (lambda (as src1 src2 dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(error "Assembler: internal:+2reg: reg is not HW"))
    (emit-arith-primop! as $i.taddrcc $i.subr $m.add src1 src2 dest #t)))

(define-primop 'internal:-2reg
  (lambda (as src1 src2 dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(error "Assembler:internal:-2reg: reg is not HW"))
    (emit-arith-primop! as $i.tsubrcc $i.addr $m.subtract src1 src2 dest #t)))

(define-primop 'internal:+imm2reg
  (lambda (as src1 imm dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(error "Assembler: internal:+imm2reg: reg is not HW"))
    (emit-arith-primop! as $i.taddicc $i.subi $m.add src1 imm dest #f)))

(define-primop 'internal:-imm2reg
  (lambda (as src1 imm dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(error "Assembler: internal:-imm2reg: reg is not HW"))
    (emit-arith-primop! as $i.tsubicc $i.addi $m.subtract src1 imm dest #f)))

(define-primop 'internal:bfnull?
  (lambda (as reg label)
    (if (not (hardware-mapped? reg))
	(error "Assembler: internal:bfnull?: reg is not HW"))
    (emit! as `(,$i.subicc ,reg ,$imm.null ,$r.g0))
    (emit! as `(,$i.bne.a ,(make-asm-label label)))
    (emit! as `(,$i.slot))))

(define-primop 'internal:bfpair?
  (lambda (as reg label)
    (if (not (hardware-mapped? reg))
	(error "Assembler: internal:bfpair?: reg is not HW"))
    (emit! as `(,$i.andi ,reg ,$tag.tagmask ,$r.tmp0))
    (emit! as `(,$i.xoricc ,$r.tmp0 ,$tag.pair-tag ,$r.g0))
    (emit! as `(,$i.bne.a ,(make-asm-label label)))
    (emit! as `(,$i.slot))))

(define-primop 'internal:bfzero?
  (lambda (as reg label)
    (if (not (hardware-mapped? reg))
	(error "Assembler: internal:bfzero?: reg is not HW"))
    (emit-bcmp-primop! as $i.bne.a reg $r.g0 label $m.zerop #t)))

(define-primop 'internal:bf=
  (lambda (as src1 src2 label)
    (emit-bcmp-primop! as $i.bne.a src1 src2 label $m.numeq #t)))

(define-primop 'internal:bf<
  (lambda (as src1 src2 label)
    (emit-bcmp-primop! as $i.bge.a src1 src2 label $m.numlt #t)))

(define-primop 'internal:bf<=
  (lambda (as src1 src2 label)
    (emit-bcmp-primop! as $i.bg.a src1 src2 label $m.numle #t)))

(define-primop 'internal:bf>
  (lambda (as src1 src2 label)
    (emit-bcmp-primop! as $i.ble.a src1 src2 label $m.numgt #t)))

(define-primop 'internal:bf>=
  (lambda (as src1 src2 label)
    (emit-bcmp-primop! as $i.bl.a src1 src2 label $m.numge #t)))

(define-primop 'internal:bf=imm
  (lambda (as src1 imm label)
    (emit-bcmp-primop! as $i.bne.a src1 imm label $m.numeq #f)))

(define-primop 'internal:bf<imm
  (lambda (as src1 imm label)
    (emit-bcmp-primop! as $i.bge.a src1 imm label $m.numlt #f)))

(define-primop 'internal:bf<=imm
  (lambda (as src1 imm label)
    (emit-bcmp-primop! as $i.bg.a src1 imm label $m.numle #f)))

(define-primop 'internal:bf>imm
  (lambda (as src1 imm label)
    (emit-bcmp-primop! as $i.ble.a src1 imm label $m.numgt #f)))

(define-primop 'internal:bf>=imm
  (lambda (as src1 imm label)
    (emit-bcmp-primop! as $i.bl.a src1 imm label $m.numge #f)))

(define-primop 'internal:bfchar=?
  (lambda (as src1 src2 label)
    (emit-char-bcmp-primop! as $i.bne.a src1 src2 label $ex.char=?)))

(define-primop 'internal:bfchar<=?
  (lambda (as src1 src2 label)
    (emit-char-bcmp-primop! as $i.bg.a src1 src2 label $ex.char<=?)))

(define-primop 'internal:bfchar<?
  (lambda (as src1 src2 label)
    (emit-char-bcmp-primop! as $i.bge.a src1 src2 label $ex.char<?)))

(define-primop 'internal:bfchar>=?
  (lambda (as src1 src2 label)
    (emit-char-bcmp-primop! as $i.bl.a src1 src2 label $ex.char>=?)))

(define-primop 'internal:bfchar>?
  (lambda (as src1 src2 label)
    (emit-char-bcmp-primop! as $i.ble.a src1 src2 label $ex.char>?)))

(define-primop 'internal:bfchar=?imm
  (lambda (as src imm label)
    (emit-char-bcmp-primop! as $i.bne.a src imm label $ex.char=?)))

(define-primop 'internal:bfchar>=?imm
  (lambda (as src imm label)
    (emit-char-bcmp-primop! as $i.bl.a src imm label $ex.char>=?)))

(define-primop 'internal:bfchar>?imm
  (lambda (as src imm label)
    (emit-char-bcmp-primop! as $i.ble.a src imm label $ex.char>?)))

(define-primop 'internal:bfchar<=?imm
  (lambda (as src imm label)
    (emit-char-bcmp-primop! as $i.bg.a src imm label $ex.char<=?)))

(define-primop 'internal:bfchar<?imm
  (lambda (as src imm label)
    (emit-char-bcmp-primop! as $i.bge.a src imm label $ex.char<?)))

(define-primop 'internal:eq2reg
  (lambda (as src1 src2 dest)
    (let ((tmp (force-hwreg! as src2 $r.tmp0)))
      (emit! as `(,$i.subrcc ,src1 ,tmp ,$r.g0))
      (emit-set-boolean-reg! as dest))))

(define-primop 'internal:bfeq?
  (lambda (as src1 src2 label)
    (emit! as `(,$i.subrcc ,src1 ,src2 ,$r.g0))
    (emit! as `(,$i.bne.a ,(make-asm-label label)))
    (emit! as `(,$i.slot))))

(define-primop 'internal:bfeq?imm
  (lambda (as src1 imm label)
    (emit! as `(,$i.subicc ,src1 ,(thefixnum imm) ,$r.g0))
    (emit! as `(,$i.bne.a ,(make-asm-label label)))
    (emit! as `(,$i.slot))))


;; old ones; remove when new versions are mature.

(define-primop '+imm
  (lambda (as imm)
    (display "*****Obsolete primitive +imm") (newline)
    (emit-primop.4arg! as 'internal:+imm2reg $r.result imm $r.result)))

(define-primop '-imm
  (lambda (as imm)
    (display "*****Obsolete primitive -imm") (newline)
    (emit-primop.4arg! as 'internal:-imm2reg $r.result imm $r.result)))

(define-primop 'bfnull?
  (lambda (as label)
    (display "*****Obsolete primitive bfnull?") (newline)
    (emit-primop.3arg! as 'internal:bfnull? $r.result label)))

(define-primop 'bfpair?
  (lambda (as label)
    (display "*****Obsolete primitive bfpair?") (newline)
    (emit-primop.3arg! as 'internal:bfpair? $r.result label)))

(define-primop 'bfzero?
  (lambda (as label)
    (display "*****Obsolete primitive bfzero?") (newline)
    (emit-primop.3arg! as 'internal:bfzero? $r.result label)))

(define-primop 'bf=
  (lambda (as r label)
    (display "*****Obsolete primitive bf=") (newline)
    (emit-primop.4arg! as 'internal:bf= $r.result r label)))

(define-primop 'bf<
  (lambda (as r label)
    (display "*****Obsolete primitive bf<") (newline)
    (emit-primop.4arg! as 'internal:bf< $r.result r label)))

(define-primop 'bf<=
  (lambda (as r label)
    (display "*****Obsolete primitive bf<=") (newline)
    (emit-primop.4arg! as 'internal:bf<= $r.result r label)))

(define-primop 'bf>
  (lambda (as r label)
    (display "*****Obsolete primitive bf>") (newline)
    (emit-primop.4arg! as 'internal:bf> $r.result r label)))

(define-primop 'bf>=
  (lambda (as r label)
    (display "*****Obsolete primitive bf>=") (newline)
    (emit-primop.4arg! as 'internal:bf>= $r.result r label)))

;-----------------------------------------------------------------

; These are used by the mal code for the i/o system, and never
; actually generated by the compiler (they should not figure in the
; integrables table!)
;
; Obsolete, supplanted by syscalls.

(define-primop 'open-file
  (lambda (as r1 r2)
    (display "*****Obsolete primitive $m.open-file") (newline)
    (millicode-call/2arg as $m.open-file r1 r2)))

(define-primop 'close-file
  (lambda (as)
    (display "*****Obsolete primitive $m.close-file") (newline)
    (millicode-call/0arg as $m.close-file)))

(define-primop 'unlink-file
  (lambda (as)
    (display "*****Obsolete primitive $m.unlink-file") (newline)
    (millicode-call/0arg as $m.unlink-file)))

(define-primop 'read-file
  (lambda (as r1 r2)
    (display "*****Obsolete primitive $m.read-file") (newline)
    (millicode-call/2arg as $m.read-file r1 r2)))

(define-primop 'write-file
  (lambda (as r1 r2)
    (display "*****Obsolete primitive $m.write-file") (newline)
    (millicode-call/2arg as $m.write-file r1 r2)))

; This one is used to get resource data.
; It takes a vector argument and fills in the slots of the vector with
; the resource data.

(define-primop 'sys$resource-usage!
  (lambda (as)
    (display "*****Obsolete primitive $m.resource-file") (newline)
    (millicode-call/0arg as $m.resource-usage)))

(define-primop 'modulo
  (lambda (as r)
    (display "OBSOLETE MILLICODE CALL $m.modulo") (newline)
    (millicode-call/1arg as $m.modulo r)))

; obsolete
(define-primop 'sys$gc
  (lambda (as)
    (display "*****Obsolete primitive $m.gc") (newline)
    (millicode-call/0arg as $m.gc)))

; obsolete
(define-primop 'sys$exit
  (lambda (as)
    (display "*****Obsolete primitive $m.exit") (newline)
    (millicode-call/0arg as $m.exit)))

; obsolete
(define-primop 'sys$dumpheap 
  (lambda (as r)
    (display "*****Obsolete primitive $m.dumpheap") (newline)
    (millicode-call/1arg as $m.dumpheap r)))


(setup-primops)


; Millicode calling

(define (millicode-call/0arg as mproc)
  (emit! as `(,$i.jmpli ,$r.millicode ,mproc ,$r.o7))
  (emit! as `(,$i.nop)))

(define (millicode-call/1arg as mproc r)
  (emit! as `(,$i.jmpli ,$r.millicode ,mproc ,$r.o7))
  (emit-move2hwreg! as r $r.argreg2))

(define (millicode-call/2arg as mproc r1 r2)
  (emit-move2hwreg! as r1 $r.argreg2)
  (emit! as `(,$i.jmpli ,$r.millicode ,mproc ,$r.o7))
  (emit-move2hwreg! as r2 $r.argreg3))

(define (millicode-call/ret as mproc label)
  (emit! as `(,$i.jmpli ,$r.millicode ,mproc ,$r.o7))
  (emit! as `(,$i.addi ,$r.o7 (- ,label (- $ 4) 8) ,$r.o7)))


; set-car!, set-cdr!, cell-set!
; Tag checks must be done by the caller.
; Pair ptr in RESULT, argument in register X.

(define (emit-setcar/setcdr! as x offs)
  (let ((x (force-hwreg! as x $r.argreg2)))
    (emit! as `(,$i.sti ,x ,(- offs $tag.pair-tag) ,$r.result))
    (if (write-barrier)
	(let ((l (new-label)))
	  (if (inline-assignment)
	      (begin
		(emit! as `(,$i.subrcc ,$r.result ,$r.e-top ,$r.g0))
		(emit! as `(,$i.ble.a ,l))
		(emit! as `(,$i.slot))))
	  (millicode-call/1arg as $m.addtrans x)
;	  (emit! as `(,$i.jmpli ,$r.millicode ,$m.addtrans ,$r.o7))
;	  (emit! as `(,$i.orr ,x ,$r.g0 ,$r.argreg2))
	  (if (inline-assignment)
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
    (millicode-call/ret as $m.exception l0)
;    (emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.o7))
;    (emit! as `(,$i.addi ,$r.o7 (- ,l0 (- $ 4) 8) ,$r.o7))
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
    (if (not (= reg $r.result))
	(emit! as `(,$i.orr ,$r.g0 ,reg ,$r.result)))
    (emit! as `(,$i.ori ,$r.g0 ,(thefixnum excode) ,$r.tmp0))
    (millicode-call/ret as $m.exception l0)
;    (emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.o7))
;    (emit! as `(,$i.addi ,$r.o7 (- ,l0 (- $ 4) 8) ,$r.o7))
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
    (if (not (= reg $r.result))
	(emit! as `(,$i.orr ,$r.g0 ,reg ,$r.result)))
    (emit! as `(,$i.ori ,$r.g0 ,(thefixnum excode) ,$r.tmp0))
    (millicode-call/ret as $m.exception l2)
    (emit! as `(,$i.label ,l1))
    l0))

; Assert that RESULT has a character in it.
; Returns the label of the fault code.

(define (emit-assert-char! as excode fault)
  (let ((l0 (new-label))
	(l1 (new-label))
	(l2 (new-label)))
    (emit! as `(,$i.label ,l1))
    (emit! as `(,$i.andi ,$r.result #xFF ,$r.tmp0))
    (emit! as `(,$i.subicc ,$r.tmp0 ,$imm.character ,$r.g0))
    (if fault
	(begin (emit! as `(,$i.bne ,fault))
	       (emit! as `(,$i.nop))
	       fault)
	(begin (emit! as `(,$i.be.a ,l2))
	       (emit! as `(,$i.slot))
	       (emit! as `(,$i.label ,l0))
	       (emit! as `(,$i.ori ,$r.g0 ,(thefixnum excode) ,$r.tmp0))
	       (millicode-call/ret as $m.exception l1)
;    (emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.o7))
;    (emit! as `(,$i.addi ,$r.o7 (- ,l1 (- $ 4) 8) ,$r.o7))
	       (emit! as `(,$i.label ,l2))
	       l0))))

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
    (if (not (= reg $r.result))
	(emit! as `(,$i.orr ,$r.g0 ,reg ,$r.result)))
    (emit! as `(,$i.ori ,$r.g0 ,(thefixnum excode) ,$r.tmp0))
    (millicode-call/ret as $m.exception l2)
;    (emit! as `(,$i.jmpli ,$r.millicode ,$m.exception ,$r.o7))
;    (emit! as `(,$i.addi ,$r.o7 (- ,l2 (- $ 4) 8) ,$r.o7))
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
	(r  (force-hwreg! as r $r.tmp1)))
    (emit! as `(,cmp ,$r.result ,r ,$r.g0))
    (emit! as `(,$i.bvc.a ,l1))
    (emit! as `(,$i.ori ,$r.g0 ,$imm.false ,$r.result))
    (emit! as `(,$i.orr ,r ,$r.g0 ,$r.argreg2))
    (millicode-call/ret as generic l2)
;    (emit! as `(,$i.jmpli ,$r.millicode ,generic ,$r.o7))
;    (emit! as `(,$i.addi ,$r.o7 (- ,l2 (- $ 4) 8) ,$r.o7))
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
		(force-hwreg! as src2 $r.tmp1)
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
		 (force-hwreg! as src2 $r.tmp1)
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
  (if (not (unsafe-code))
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

    (emit! as `(,$i.jmpli ,$r.millicode ,$m.alloc-bv ,$r.o7))  ; NOT-IN-SYNC
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
	 (r     (force-hwreg! as r $r.tmp2)))

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
; fault is defined iff (unsafe-code) = #f

(define (emit-bytevector-like-ref! as x fault charize?)
  (let ((r (force-hwreg! as x $r.tmp1)))
    (if (not (unsafe-code))
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


; fault is valid iff (unsafe-code) = #f
; FIXME: values passed to error handler appear to be bogus (error message
; is very strange.)

(define (emit-bytevector-like-set! as x y fault)
  (let ((r1 (force-hwreg! as x $r.tmp0))
	(r2 (force-hwreg! as y $r.tmp1)))
    (if (not (unsafe-code))
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

; fault is valid iff (unsafe-code) = #f

(define (emit-vector-like-ref! as r fault tag)
  (let ((r1 (force-hwreg! as r $r.argreg2)))
    (if (not (unsafe-code))
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

; fault is valid iff (unsafe-code) = #f

(define (emit-vector-like-set! as x y fault tag)
  (let ((r1 (force-hwreg! as x $r.argreg2)))
    (if (not (unsafe-code))
	(begin
	  (emit! as `(,$i.andicc ,r1 3 ,$r.g0))
	  (emit! as `(,$i.bne.a ,fault))
	  (emit! as `(,$i.slot))
	  (emit! as `(,$i.ldi ,$r.result (- ,tag) ,$r.tmp2))
	  (emit! as `(,$i.srai ,$r.tmp2 8 ,$r.tmp2))
	  (emit! as `(,$i.subrcc ,$r.tmp2 ,r1 ,$r.g0))
	  (emit! as `(,$i.bleu.a ,fault))
	  (emit! as `(,$i.slot))))
    (let ((y (force-hwreg! as y $r.argreg3)))
      (emit! as `(,$i.addr ,$r.result ,r1 ,$r.tmp0))
      (emit! as `(,$i.sti ,y ,(- 4 tag) ,$r.tmp0))
      (if (write-barrier)
	  (if (inline-assignment)
	      (let ((l (new-label)))
		(emit! as `(,$i.subrcc ,$r.result ,$r.e-top ,$r.g0))
		(emit! as `(,$i.ble.a ,l))
		(emit! as `(,$i.slot))
		(millicode-call/1arg as $m.addtrans y)
;		(emit! as `(,$i.jmpli ,$r.millicode ,$m.addtrans ,$r.o7))
;		(emit! as `(,$i.orr ,$r.g0 ,y ,$r.argreg2))
		(emit! as `(,$i.label ,l)))
	      (begin
		(millicode-call/1arg as $m.addtrans y)
;		(emit! as `(,$i.jmpli ,$r.millicode ,$m.addtrans ,$r.o7))
;		(emit! as `(,$i.orr ,$r.g0 ,y ,$r.argreg2))
		))))))


; Character comparison.

; r is a register or a character constant.

(define (emit-char-cmp as r btrue excode)
  (emit-charcmp! as (lambda ()
		      (let ((l2 (new-label)))
			(sparc.set as $imm.false $r.result)
			(emit! as `(,btrue ,l2))
			(sparc.set as $imm.true $r.result)
			(sparc.label as l2)))
		 $r.result
		 r
		 excode))
 
; op1 is a hw register
; op2 is a register or a character constant

(define (emit-char-bcmp-primop! as bfalse op1 op2 label excode)
  (let ((l0 (make-asm-label label)))
    (emit-charcmp! as (lambda ()
			(emit! as `(,bfalse ,l0))
			(emit! as `(,$i.slot)))
		   op1
		   op2
		   excode)))

; We check the tags of both by xoring them and seeing if the low byte is 0.
; If so, then we can subtract one from the other (tag and all) and check the
; condition codes.  FIXME. That's not right, consider (char=? 1 1).
;
; The branch-on-true instruction must have the annull bit set.
;
; op1 is a hw register
; op2 is a register or a character constant.

(define (emit-charcmp! as tail op1 op2 excode)
  (let ((op2 (if (char? op2)
		 op2
		 (force-hwreg! as op2 $r.argreg2))))
    (cond ((not (unsafe-code))
	   (let ((l0 (new-label))
		 (l1 (new-label)))
	     (sparc.label as l0)
	     (cond ((char? op2)
		    (sparc.xori as op1 $imm.character $r.tmp0)
		    (sparc.test as $r.tmp0 #xFF)
		    (sparc.srli as op1 16 $r.tmp0)
		    (sparc.be.a as l1)
		    (sparc.cmpi as $r.tmp0 (char->integer op2)))
		   (else
		    (sparc.xorr as op1 op2 $r.tmp0)
		    (sparc.test as $r.tmp0 #xFF)
		    (sparc.be.a as l1)
		    (sparc.cmpr as op1 op2)))
	     (if (not (eqv? op1 $r.result))
		 (sparc.move as op1 $r.result))
	     (cond ((char? op2)
		    (sparc.set as (char->immediate op2) $r.argreg2))
		   ((not (eqv? op2 $r.argreg2))
		    (sparc.move as op2 $r.argreg2)))
	     (sparc.set as (thefixnum excode) $r.tmp0)
	     (millicode-call/ret as $m.exception l0)
	     (sparc.label as l1)))
	  ((not (char? op2))
	   (sparc.cmpr as op1 op2))
	  (else
	   (sparc.srli as op1 16 $r.tmp0)
	   (sparc.cmpi as $r.tmp0 (char->integer op2))))
    (tail)))


;---------------------------------------------------------------------------
; Some useful abstractions (more to come). These are instructions as well
; as pseudo-instructions. They don't belong in this file, but OK here for now.
;
; While having a procedure for each instruction slows down assembly,
; it allows us to implement each instruction more efficiently than 
; what free use of emit! allows; overall it should be an eventual win.
; They could even be macros.

(define (sparc.be.a as l)
  (emit! as `(,$i.be.a ,l)))

(define (sparc.cmpr as r1 r2)
  (emit! as `(,$i.subrcc ,r1 ,r2 ,$r.g0)))

(define (sparc.cmpi as r imm)
  (emit! as `(,$i.subicc ,r ,imm ,$r.g0)))

(define (sparc.label as l)
  (emit! as `(,$i.label ,l)))

(define (sparc.move as rs rd)
  (emit! as `(,$i.orr ,rs ,$r.g0 ,rd)))

(define (sparc.set as imm rd)
  (emit-immediate->register! as imm rd))

(define (sparc.srli as rs imm rd)
  (emit! as `(,$i.srli ,rs ,imm ,rd)))

(define (sparc.test as r mask)
  (emit! as `(,$i.andicc ,r ,mask ,$r.g0)))

(define (sparc.xori as rs imm rd)
  (emit! as `(,$i.xori ,rs ,imm ,rd)))

(define (sparc.xorr as rs1 rs2 rd)
  (emit! as `(,$i.xorr ,rs1 ,rs2 ,rd)))

; eof
