; Asm/Sparc/gen-prim.sch
; Larceny -- SPARC assembler code emitters for primops.
;
; $Id: gen-prim.sch,v 1.1 1997/07/07 20:36:24 lth Exp lth $
;
; Temp-register allocation here is completely out of hand. We have to come
; up with a coherent strategy for allocating temporary registers, e.g. a
; set of registers to pick from when one is needed.
;
; Also, too many implicit registers are assumed (this is definitely related
; to the allocation mess) and should rather be passed/returned as appropriate.
; The key problem is to never load a memory-mapped register or a datum more
; than once. Remember that calling a subroutine which returns a register may
; return a temp which cannot subsequently be allocated.
;
; BUGS
; - Like mentioned above, temp register allocation is gross.
; - Some operations do too much work; for example, vector-ref will load the
;   vector header twice.
; - Really want to use a hash table for the opcode table, not a list
;   like here.
; - Attempt to make do with tmp0, tmp1, argreg2, and argreg3, so that
;   tmp2 can be freed up for general use.  This is not hard; only string-set!
;   and bytevector-fill! currently need tmp2.

(define (emit-primop.1arg! as op)
  ((find-primop op) as))

(define (emit-primop.2arg! as op r)
  ((find-primop op) as r))

(define (emit-primop.3arg! as a1 a2 a3)
  ((find-primop a1) as a2 a3))

(define (emit-primop.4arg! as a1 a2 a3 a4)
  ((find-primop a1) as a2 a3 a4))

;(define conv-list '((+ . +imm) (- . -imm)))
;
;(define (emit-primop2imm! as op imm)
;  ((find-primop (cdr (assq op conv-list))) as imm))


;---------------------------------------------------------------------------
; Assoc list of primops, indexed by name.
; Should be hash table?  It's getting long.

(define primop-list '())

(define (define-primop name proc)
  (set! primop-list (cons (cons name proc) primop-list)))

(define (find-primop name)
  (cdr (assq name primop-list)))

(define (setup-primops)
  (set! primop-list (reverse! primop-list)))


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
    (emit-cmp-primop! as sparc.be.a $m.zerop $r.g0)))

(define-primop '=
  (lambda (as r)
    (emit-cmp-primop! as sparc.be.a $m.numeq r)))

(define-primop '<
  (lambda (as r)
    (emit-cmp-primop! as sparc.bl.a $m.numlt r)))

(define-primop '<=
  (lambda (as r)
    (emit-cmp-primop! as sparc.ble.a $m.numle r)))

(define-primop '>
  (lambda (as r)
    (emit-cmp-primop! as sparc.bg.a $m.numgt r)))

(define-primop '>=
  (lambda (as r)
    (emit-cmp-primop! as sparc.bge.a $m.numge r)))

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
    (sparc.btsti as $r.result 3)
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

(define-primop '--
  (lambda (as)
    (let ((L1 (new-label)))
      (sparc.tsubrcc as $r.g0 $r.result $r.result)
      (sparc.bvc.a   as L1)
      (sparc.slot    as)
      (sparc.subrcc  as $r.g0 $r.result $r.result)
      (millicode-call/0arg as $m.negate)
      (sparc.label   as L1))))

(define-primop 'round
  (lambda (as)
    (millicode-call/0arg as $m.round)))

(define-primop 'truncate
  (lambda (as)
    (millicode-call/0arg as $m.truncate)))

(define-primop 'lognot
  (lambda (as)
    (emit-assert-fixnum! as $r.result $ex.lognot)
    (sparc.ornr as $r.g0 $r.result $r.result)  ; argument order matters
    (sparc.xori as $r.result 3 $r.result)))

(define-primop 'logand
  (lambda (as x)
    (logical-op as x sparc.andr $ex.logand)))

(define-primop 'logior
  (lambda (as x)
    (logical-op as x sparc.orr $ex.logior)))

(define-primop 'logxor
  (lambda (as x)
    (logical-op as x sparc.xorr $ex.logxor)))

; Fixnums only, and only positive shifts are meaningful.
; FIXME: This is incompatible with MacScheme and MIT Scheme.
; FIXME: Really ought to reuse fault handler (requires rewrite of asserts).
; FIXME: need to return to start of sequence after fault.

(define-primop 'lsh
  (lambda (as x)
    (let ((tmp (force-hwreg! as x $r.tmp1)))
      (if (not (unsafe-code))
	  (begin (emit-assert-fixnum! as $r.result $ex.lsh)
		 (emit-assert-positive-fixnum! as tmp $ex.lsh)))
      (sparc.srai as tmp 2 $r.tmp1)
      (sparc.sllr as $r.result $r.tmp1 $r.result))))

; Fixnums only, and only positive shifts are meaningful.
; FIXME: this should maybe be lsh with a negative shift.
; FIXME: ought to reuse fault handler.
; FIXME: need to return to start of sequence after fault.
;
; Watch the semantics of the shift instruction! We have to do the
; original shift first and *then* adjust; no combination is possible
; in general.

(define-primop 'rshl
  (lambda (as x)
    (let ((tmp (force-hwreg! as x $r.tmp1)))
      (if (not (unsafe-code))
	  (begin (emit-assert-fixnum! as $r.result $ex.rshl)
		 (emit-assert-positive-fixnum! as tmp $ex.rshl)))
      (sparc.srai  as tmp 2 $r.tmp1)
      (sparc.srlr  as $r.result $r.tmp1 $r.result)
      (sparc.andni as $r.result 3 $r.result))))

; fixnums only, and only positive shifts are meaningful.
; FIXME: ought to reuse fault handler.
; FIXME: need to return to start of sequence after fault.

(define-primop 'rsha
  (lambda (as x)
    (let ((tmp (force-hwreg! as x $r.tmp1)))
      (if (not (unsafe-code))
	  (begin (emit-assert-fixnum! as $r.result $ex.rsha)
		 (emit-assert-positive-fixnum! as tmp $ex.rsha)))
      (sparc.srai  as tmp 2 $r.tmp1)
      (sparc.srar  as $r.result $r.tmp1 $r.result)
      (sparc.andni as $r.result 3 $r.result))))

; fixnums only.
; FIXME: for symmetry with shifts there should be rotl and rotr (?)
;        or perhaps rot should only ever rotate one way.
; FIXME: implement.

(define-primop 'rot
  (lambda (as x)
    (asm-error "Sparcasm: ROT primop is not implemented.")))

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
    (sparc.andi as $r.result #xFF $r.tmp0)
    (sparc.cmpi as $r.tmp0 $imm.character)
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

; Continuations.

(define-primop 'creg
  (lambda (as)
    (millicode-call/0arg as $m.creg)))

(define-primop 'creg-set!
  (lambda (as)
    (millicode-call/0arg as $m.creg-set!)))

; Typetags.

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
    (sparc.srli as $r.result 14 $r.result)))

(define-primop 'integer->char
  (lambda (as)
    (emit-assert-fixnum! as $r.result $ex.int2char)
    (sparc.andi as $r.result #x3FF $r.result)
    (sparc.slli as $r.result 14 $r.result)
    (sparc.ori  as $r.result $imm.character $r.result)))

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
	  (L1  (new-label)))
      (sparc.cmpr as $r.result tmp)
      (sparc.be.a as L1)
      (sparc.set  as $imm.true $r.result)
      (millicode-call/1arg as $m.eqv tmp)
      (sparc.label as L1))))


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

; String-set!.
;
; This is hard, and very ugly. It must be rewritten, but no point in 
; rewriting until the tag checking machinery is rewritten.
; Checked: 25 static instructions (maybe more). Can be sped up at the cost of
;          higher instruction count.
; Unchecked: 4 static instructions.
;
; FIXME: Try to get rid of TMP2.

(define-primop 'string-set!
  (lambda (as r1 r2)
    (let ((FAULT  (new-label)))
      ; First check tags of lhs
      (if (not (unsafe-code))
	  (let ((AGAIN  (new-label))
		(NEXT1  (new-label))
		(ptrtag $tag.bytevector-tag)
		(hdrtag (+ $imm.bytevector-header $tag.string-typetag))
		(excode $ex.sset))
	    (sparc.label as AGAIN)
	    (sparc.andi  as $r.result $tag.tagmask $r.tmp0)
	    (sparc.cmpi  as $r.tmp0 ptrtag)
	    (sparc.be.a  as NEXT1)
	    (sparc.ldi   as $r.result (- ptrtag) $r.tmp0)
	    (sparc.label as FAULT)
	    (sparc.set   as (thefixnum excode) $r.tmp0)
	    (emit-move2hwreg! as r1 $r.argreg2)
	    (emit-move2hwreg! as r2 $r.argreg3)
	    (millicode-call/ret as $m.exception again)
	    (sparc.label as NEXT1)
	    ; TMP0 now has the header.
	    (sparc.andi  as $r.tmp0 #xFF $r.tmp1)
	    (sparc.cmpi  as $r.tmp1 hdrtag)
	    (sparc.bne   as FAULT)
	    (sparc.nop   as)))
      ; Now check index and rhs.
      ; Header is in TMP0; TMP1 and TMP2 are free.
      (let ((r1 (force-hwreg! as r1 $r.tmp2))
	    (r2 (force-hwreg! as r2 $r.tmp1)))
	(if (not (unsafe-code))
	    (begin
	      ; Can we combine these tests using tsubcc?
	      ; r1 should be fixnum
	      (sparc.btsti  as r1 3)
	      (sparc.bne    as FAULT)
	      (sparc.nop    as)
	      ; index must be valid; header is in tmp0
	      (sparc.srli   as $r.tmp0 8 $r.tmp0)      ; limit
	      (sparc.srai   as r1 2 $r.tmp2)           ; index
	      (sparc.cmpr   as $r.tmp2 $r.tmp0)
	      (sparc.bgeu   as FAULT)
	      (sparc.nop    as)
	      ; r2 should be a character
	      (sparc.andi   as r2 #xFF $r.tmp0)
	      (sparc.cmpi   as $r.tmp0 $imm.character)
	      (sparc.bne    as FAULT)
	      (sparc.nop    as))
	    (begin
	      (sparc.srai as r1 2 $r.tmp2)))
	; tmp2 has nativeint index. 
	; r2/tmp1 has character.
	; tmp0 is garbage.
	(sparc.srli as r2 16 $r.tmp1)
	(sparc.subi as $r.result (- $tag.bytevector-tag 4) $r.tmp0)
	(sparc.stbr as $r.tmp1 $r.tmp0 $r.tmp2)))))

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
    (emit-char-cmp as x sparc.bl.a $ex.char<?)))

(define-primop 'char<=?
  (lambda (as x)
    (emit-char-cmp as x sparc.ble.a $ex.char<=?)))

(define-primop 'char=?
  (lambda (as x)
    (emit-char-cmp as x sparc.be.a $ex.char=?)))

(define-primop 'char>?
  (lambda (as x)
    (emit-char-cmp as x sparc.bg.a $ex.char>?)))

(define-primop 'char>=?
  (lambda (as x)
    (emit-char-cmp as x sparc.bge.a $ex.char>=?)))

;-----------------------------------------------------------------
   
; These are introduced by peephole optimization and are called from
; some routines above.

(define-primop 'internal:car2reg
  (lambda (as src1 dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(asm-error "Assembler: internal:car2reg: reg is not HW"))
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert-reg! as
					  $tag.pair-tag src1 $ex.car))
    (sparc.ldi as src1 (- $tag.pair-tag) dest)))

(define-primop 'internal:cdr2reg
  (lambda (as src1 dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(asm-error "Assembler: internal:cdr2reg: reg is not HW"))
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert-reg! as
					  $tag.pair-tag src1 $ex.cdr))
    (sparc.ldi as src1 (- 4 $tag.pair-tag) dest)))

(define-primop 'internal:cellref2reg
  (lambda (as src1 dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(asm-error "Assembler: internal:cellref2reg: reg is not HW"))
    (sparc.ldi as src1 (- $tag.pair-tag) dest)))

(define-primop 'internal:cons2reg
  (lambda (as src1 src2 dest)

    (define (emit-common-case cont)
      (let ((src1 (if (eqv? src1 $r.result)
		      (begin (sparc.move as $r.result $r.argreg2)
			     $r.argreg2)
		      src1)))
	(millicode-call/numarg-in-result as $m.alloc 8)
	(sparc.sti as src1 0 $r.result)
	(sparc.sti as (force-hwreg! as src2 $r.tmp1) 4 $r.result)
	(if cont (sparc.b as cont))
	(sparc.addi as $r.result 1 dest)))

    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(asm-error "Assembler: internal:cons2reg: reg is not HW"))
    (if (inline-cons)
	(let ((L1 (new-label))
	      (L2 (new-label)))
	  ; Fast path.
	  (sparc.addi  as $r.e-top 8 $r.e-top)
	  (sparc.cmpr  as $r.e-top $r.e-limit)
	  (sparc.ble.a as L1)
	  (sparc.sti   as src1 -8 $r.e-top)

	  ; Overflow: just do an 'alloc' to trigger the correct gc.
	  (sparc.subi as $r.e-top 8 $r.e-top)
	  (emit-common-case L2)

	  ; Fast path again.
	  (sparc.label as L1)
	  (let ((src2 (force-hwreg! as src2 $r.tmp1)))
	    (sparc.sti   as src2 -4 $r.e-top)
	    (sparc.subi  as $r.e-top 7 dest)
	    (sparc.label as L2)))
	(emit-common-case #f))))

(define-primop 'internal:+2reg
  (lambda (as src1 src2 dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(asm-error "Assembler: internal:+2reg: reg is not HW"))
    (emit-arith-primop! as sparc.taddrcc sparc.subr $m.add src1 src2 dest #t)))

(define-primop 'internal:-2reg
  (lambda (as src1 src2 dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(asm-error "Assembler:internal:-2reg: reg is not HW"))
    (emit-arith-primop! as sparc.tsubrcc sparc.addr $m.subtract src1 src2 dest #t)))

(define-primop 'internal:+imm2reg
  (lambda (as src1 imm dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(asm-error "Assembler: internal:+imm2reg: reg is not HW"))
    (emit-arith-primop! as sparc.taddicc sparc.subi $m.add src1 imm dest #f)))

(define-primop 'internal:-imm2reg
  (lambda (as src1 imm dest)
    (if (not (and (hardware-mapped? src1) (hardware-mapped? dest)))
	(asm-error "Assembler: internal:-imm2reg: reg is not HW"))
    (emit-arith-primop! as sparc.tsubicc sparc.addi $m.subtract src1 imm dest #f)))

(define-primop 'internal:bfnull?
  (lambda (as reg label)
    (if (not (hardware-mapped? reg))
	(asm-error "Assembler: internal:bfnull?: reg is not HW"))
    (sparc.cmpi  as reg $imm.null)
    (sparc.bne.a as (make-asm-label label))
    (sparc.slot  as)))

(define-primop 'internal:bfpair?
  (lambda (as reg label)
    (if (not (hardware-mapped? reg))
	(asm-error "Assembler: internal:bfpair?: reg is not HW"))
    (sparc.andi  as reg $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 $tag.pair-tag)
    (sparc.bne.a as (make-asm-label label))
    (sparc.slot  as)))

(define-primop 'internal:bfzero?
  (lambda (as reg label)
    (if (not (hardware-mapped? reg))
	(asm-error "Assembler: internal:bfzero?: reg is not HW"))
    (emit-bcmp-primop! as sparc.bne.a reg $r.g0 label $m.zerop #t)))

(define-primop 'internal:bf=
  (lambda (as src1 src2 label)
    (emit-bcmp-primop! as sparc.bne.a src1 src2 label $m.numeq #t)))

(define-primop 'internal:bf<
  (lambda (as src1 src2 label)
    (emit-bcmp-primop! as sparc.bge.a src1 src2 label $m.numlt #t)))

(define-primop 'internal:bf<=
  (lambda (as src1 src2 label)
    (emit-bcmp-primop! as sparc.bg.a src1 src2 label $m.numle #t)))

(define-primop 'internal:bf>
  (lambda (as src1 src2 label)
    (emit-bcmp-primop! as sparc.ble.a src1 src2 label $m.numgt #t)))

(define-primop 'internal:bf>=
  (lambda (as src1 src2 label)
    (emit-bcmp-primop! as sparc.bl.a src1 src2 label $m.numge #t)))

(define-primop 'internal:bf=imm
  (lambda (as src1 imm label)
    (emit-bcmp-primop! as sparc.bne.a src1 imm label $m.numeq #f)))

(define-primop 'internal:bf<imm
  (lambda (as src1 imm label)
    (emit-bcmp-primop! as sparc.bge.a src1 imm label $m.numlt #f)))

(define-primop 'internal:bf<=imm
  (lambda (as src1 imm label)
    (emit-bcmp-primop! as sparc.bg.a src1 imm label $m.numle #f)))

(define-primop 'internal:bf>imm
  (lambda (as src1 imm label)
    (emit-bcmp-primop! as sparc.ble.a src1 imm label $m.numgt #f)))

(define-primop 'internal:bf>=imm
  (lambda (as src1 imm label)
    (emit-bcmp-primop! as sparc.bl.a src1 imm label $m.numge #f)))

(define-primop 'internal:bfchar=?
  (lambda (as src1 src2 label)
    (emit-char-bcmp-primop! as sparc.bne.a src1 src2 label $ex.char=?)))

(define-primop 'internal:bfchar<=?
  (lambda (as src1 src2 label)
    (emit-char-bcmp-primop! as sparc.bg.a src1 src2 label $ex.char<=?)))

(define-primop 'internal:bfchar<?
  (lambda (as src1 src2 label)
    (emit-char-bcmp-primop! as sparc.bge.a src1 src2 label $ex.char<?)))

(define-primop 'internal:bfchar>=?
  (lambda (as src1 src2 label)
    (emit-char-bcmp-primop! as sparc.bl.a src1 src2 label $ex.char>=?)))

(define-primop 'internal:bfchar>?
  (lambda (as src1 src2 label)
    (emit-char-bcmp-primop! as sparc.ble.a src1 src2 label $ex.char>?)))

(define-primop 'internal:bfchar=?imm
  (lambda (as src imm label)
    (emit-char-bcmp-primop! as sparc.bne.a src imm label $ex.char=?)))

(define-primop 'internal:bfchar>=?imm
  (lambda (as src imm label)
    (emit-char-bcmp-primop! as sparc.bl.a src imm label $ex.char>=?)))

(define-primop 'internal:bfchar>?imm
  (lambda (as src imm label)
    (emit-char-bcmp-primop! as sparc.ble.a src imm label $ex.char>?)))

(define-primop 'internal:bfchar<=?imm
  (lambda (as src imm label)
    (emit-char-bcmp-primop! as sparc.bg.a src imm label $ex.char<=?)))

(define-primop 'internal:bfchar<?imm
  (lambda (as src imm label)
    (emit-char-bcmp-primop! as sparc.bge.a src imm label $ex.char<?)))

(define-primop 'internal:eq2reg
  (lambda (as src1 src2 dest)
    (let ((tmp (force-hwreg! as src2 $r.tmp0)))
      (sparc.cmpr as src1 tmp)
      (emit-set-boolean-reg! as dest))))

(define-primop 'internal:bfeq?
  (lambda (as src1 src2 label)
    (sparc.cmpr  as src1 src2)
    (sparc.bne.a as (make-asm-label label))
    (sparc.slot  as)))

(define-primop 'internal:bfeq?imm
  (lambda (as src1 imm label)
    (sparc.cmpi  as src1 (thefixnum imm))
    (sparc.bne.a as (make-asm-label label))
    (sparc.slot  as)))

(setup-primops)

; Logical operations: LOGAND, LOGIOR, LOGXOR.
; Fixnum arguments in RESULT and rs.

(define (logical-op as rs op excode)
  (let ((L0  (new-label))
	(L1  (new-label)))
    (sparc.label     as L0)
    (let ((tmp (force-hwreg! as rs $r.argreg2)))
      (sparc.tsubrcc as $r.result tmp $r.g0)
      (sparc.bvc.a   as L1)
      (op            as $r.result tmp $r.result)
      (if (not (= tmp $r.argreg2))
	  (sparc.move as tmp $r.argreg2))
      (sparc.set     as (thefixnum excode) $r.tmp0)
      (millicode-call/ret as $m.exception L0)
      (sparc.label   as L1))))

; set-car!, set-cdr!, cell-set!
;
; Tag checks must be done by the caller.
; Pair ptr in RESULT, argument in register X.

(define (emit-setcar/setcdr! as x offs)
  (let ((x (force-hwreg! as x $r.argreg2)))
    (sparc.sti as x (- offs $tag.pair-tag) $r.result)
    (if (write-barrier)
	(let ((L0 (new-label)))
	  (if (inline-assignment)
	      (begin (sparc.cmpr  as $r.result $r.e-top)
		     (sparc.ble.a as L0)
		     (sparc.slot  as)))
	  (millicode-call/1arg as $m.addtrans x)
	  (if (inline-assignment) (sparc.label as L0))))))

(define (emit-set-boolean! as)
  (emit-set-boolean-reg! as $r.result))

(define (emit-set-boolean-reg! as dest)
  (let ((L1 (new-label)))
    (sparc.set   as $imm.true dest)
    (sparc.bne.a as L1)
    (sparc.set   as $imm.false dest)
    (sparc.label as L1)))

(define (emit-double-tagcheck->bool! as tag1 tag2)
  (let ((L1 (new-label)))
    (sparc.andi  as $r.result $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 tag1)
    (sparc.bne.a as L1)
    (sparc.set   as $imm.false $r.result)
    (sparc.ldbi  as $r.result (+ (- tag1) 3) $r.tmp0)
    (sparc.set   as $imm.true $r.result)
    (sparc.cmpi  as $r.tmp0 tag2)
    (sparc.bne.a as L1)
    (sparc.set   as $imm.false $r.result)
    (sparc.label as L1)))

(define (emit-double-tagcheck-assert! as tag1 tag2 excode)
  (let ((L0    (new-label))
	(L1    (new-label))
	(FAULT (new-label)))
    (sparc.label as L0)
    (sparc.andi  as $r.result $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 tag1)
    (sparc.be.a  as L1)
    (sparc.ldbi  as $r.result (+ (- tag1) 3) $r.tmp0)
    (sparc.label as FAULT)
    (sparc.set   as (thefixnum excode) $r.tmp0)
    (millicode-call/ret as $m.exception l0)
    (sparc.label as L1)
    (sparc.cmpi  as $r.tmp0 tag2)
    (sparc.bne.a as FAULT)
    (sparc.slot  as)
    FAULT))

(define (emit-single-tagcheck->bool! as tag)
  (sparc.andi as $r.result $tag.tagmask $r.tmp0)
  (sparc.cmpi as $r.tmp0 tag)
  (emit-set-boolean! as))

(define (emit-single-tagcheck-assert! as tag1 excode)
  (emit-single-tagcheck-assert-reg! as tag1 $r.result excode))

(define (emit-single-tagcheck-assert-reg! as tag1 reg excode)
  (let ((L0    (new-label))
	(L1    (new-label))
	(FAULT (new-label)))
    (sparc.label as L0)
    (sparc.andi  as reg $tag.tagmask $r.tmp0)
    (sparc.cmpi  as $r.tmp0 tag1)
    (fault-if-ne as excode #f #f reg L0)))

; Assert that a machine register has a fixnum in it.
; Returns the label of the fault code.

(define (emit-assert-fixnum! as reg excode)
  (let ((L0    (new-label))
	(L1    (new-label))
	(FAULT (new-label)))
    (sparc.label  as L0)
    (sparc.btsti  as reg 3)
    (fault-if-ne as excode #f #f reg L0)))

; Assert that RESULT has a character in it.
; Returns the label of the fault code.

(define (emit-assert-char! as excode fault-label)
  (let ((L0    (new-label))
	(L1    (new-label))
	(FAULT (new-label)))
    (sparc.label as L0)
    (sparc.andi  as $r.result #xFF $r.tmp0)
    (sparc.cmpi  as $r.tmp0 $imm.character)
    (fault-if-ne as excode #f fault-label #f L0)))

; Generate code for fault handling if the zero flag is not set.
; - excode is the nativeint exception code.
; - cont-label, if not #f, is the label to go to if there is no fault.
; - fault-label, if not #f, is the label of an existing fault handler.
; - must-move-reg, if not #f, is the number of a register which must be
;   moved into RESULT before the fault handler is called.
; - ret-label, if not #f, is the return address to be set up before calling
;   the fault handler.
;
; Ret-label and fault-label cannot simultaneously be non-#f; in this case
; the ret-label is ignored (since the existing fault handler most likely
; sets up the return in the desired manner).

(define (fault-if-ne as excode cont-label fault-label must-move-reg ret-label)
  (if fault-label
      (begin (sparc.bne as fault-label)
	     (if (and must-move-reg (not (= must-move-reg $r.result)))
		 (sparc.move as must-move-reg $r.result)
		 (sparc.nop as))
	     fault-label)
      (let ((FAULT (new-label))
	    (L1    (new-label)))
	(sparc.be.a  as (or cont-label L1))
	(sparc.slot  as)
	(sparc.label as FAULT)
	(if (and must-move-reg (not (= must-move-reg $r.result)))
	    (sparc.move as must-move-reg $r.result))
	(sparc.set   as (thefixnum excode) $r.tmp0)
	(millicode-call/ret as $m.exception (or ret-label L1))
	(if (or (not cont-label) (not ret-label))
	    (sparc.label as L1))
	FAULT)))

; This is more expensive than what is good for it (5 cycles in the usual case),
; but there does not seem to be a better way.

(define (emit-assert-positive-fixnum! as reg excode)
  (let ((L1 (new-label))
	(L2 (new-label))
	(L3 (new-label))) 
    (sparc.label   as L2)
    (sparc.tsubrcc as reg $r.g0 $r.g0)
    (sparc.bvc     as L1)
    (sparc.nop     as)
    (sparc.label   as L3)
    (if (not (= reg $r.result))
	(sparc.move as reg $r.result))
    (sparc.set     as (thefixnum excode) $r.tmp0)
    (millicode-call/ret as $m.exception l2)
    (sparc.label   as L1)
    (sparc.bl      as L3)
    (sparc.nop     as)
    L3))

; Emit code which performs a comparison operation on %result and another
; register and which returns a boolean in %result.
;
; The extra cost of computing the boolean (rather than leaving it implicitly
; in the status register) is 3 cycles.

(define (emit-cmp-primop! as bcc.a generic r)
  (let ((L1 (new-label))
	(L2 (new-label))
	(r  (force-hwreg! as r $r.tmp1)))
    (sparc.tsubrcc as $r.result r $r.g0)
    (sparc.bvc.a   as L1)
    (sparc.set     as $imm.false $r.result)
    (sparc.move    as r $r.argreg2)
    (millicode-call/ret as generic L2)
    (sparc.label   as L1)
    (bcc.a         as L2)
    (sparc.set     as $imm.true $r.result)
    (sparc.label   as L2)))

; Possibly it would be better to unchain the branches and let slots be filled?

(define (emit-bcmp-primop! as bcc.a src1 src2 label generic src2isreg)
  (let ((L1  (new-label))
	(L2  (make-asm-label label))
	(op2 (if src2isreg
		(force-hwreg! as src2 $r.tmp1)
		(thefixnum src2)))
	(sub (if src2isreg sparc.tsubrcc sparc.tsubicc))
	(mov (if src2isreg sparc.move sparc.set)))
    (sub         as src1 op2 $r.g0)
    (sparc.bvc.a as L1)
    (bcc.a       as L2)
    (sparc.nop   as)

    ; generic case
    ; must move src1 -> result if src1 is not result
    ; must move src2 -> argreg2 if src2 is not argreg2

    (let ((n (+ (if (not (= src1 $r.result)) 1 0)
		(if (or (not src2isreg) (not (= op2 $r.argreg2))) 1 0))))
      (if (= n 2)
	  (mov as op2 $r.argreg2))
      (sparc.jmpli as $r.millicode generic $r.o7)
      (cond ((= n 0) (sparc.nop  as))
	    ((= n 1) (mov        as op2 $r.argreg2))
	    (else    (sparc.move as src1 $r.result)))
      ; now test result and branch as required
      (sparc.cmpi  as $r.result $imm.false)
      (sparc.be    as L2)
      (sparc.label as L1)
      (sparc.nop   as))))

; Generic arithmetic for + and -.
; Some rules:
;   We have two HW registers src1 and dest.
;   If src2isreg is #t then src2 may be a HW reg or a SW reg
;   If src2isreg is #f then src2 is an immediate fixnum, not shifted.
;   Src1 and dest may be RESULT, but src2 may not.
;   Src2 may be ARGREG2, the others may not.

(define (emit-arith-primop! as op invop generic src1 src2 dest src2isreg)
  (let ((L1  (new-label))
	(op2 (if src2isreg
		 (force-hwreg! as src2 $r.tmp1)
		 (thefixnum src2))))
    (if (and src2isreg (= op2 dest))
	(begin (op          as src1 op2 $r.tmp0)
	       (sparc.bvc.a as L1)
	       (sparc.move  as $r.tmp0 dest))
	(begin (op          as src1 op2 dest)
	       (sparc.bvc.a as L1)
	       (sparc.slot  as)
	       (invop       as dest op2 dest)))
    (let ((n    (+ (if (not (= src1 $r.result)) 1 0)
		   (if (or (not src2isreg) (not (= op2 $r.argreg2))) 1 0)))
	  (mov2 (if src2isreg sparc.move sparc.set)))
      (if (= n 2)
	  (mov2 as op2 $r.argreg2))
      (sparc.jmpli as $r.millicode generic $r.o7)
      (cond ((= n 0) (sparc.nop  as))
	    ((= n 1) (mov2       as op2 $r.argreg2))
	    (else    (sparc.move as src1 $r.result)))
      ; Generic arithmetic leaves stuff in RESULT, must move to dest if
      ; dest is not RESULT.
      (if (not (= dest $r.result))
	  (sparc.move as $r.result dest))
      (sparc.label as L1))))


; Get the length of a vector or bytevector structure, with tag checking
; included.

(define (emit-get-length! as tag1 tag2 excode)
  (if (not (unsafe-code))
      (if tag2
	  (emit-double-tagcheck-assert! as tag1 tag2 excode)
	  (emit-single-tagcheck-assert! as tag1 excode)))
  (sparc.ldi  as $r.result (- tag1) $r.tmp0)
  (sparc.srli as $r.tmp0 8 $r.result)
  (if (= tag1 $tag.bytevector-tag)
      (sparc.slli as $r.result 2 $r.result)))


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

    (sparc.move as $r.result $r.argreg3)
    (sparc.addi as $r.result 28 $r.result)
    (sparc.andi as $r.result (asm:signed #xFFFFFFF0) $r.result)

    ; Allocate space

    (sparc.jmpli as $r.millicode $m.alloc $r.o7)
    (sparc.srai  as $r.result 2 $r.result)

    ; Setup header, tag pointer.

    (sparc.slli as $r.argreg3 6 $r.tmp0)
    (sparc.addi as $r.tmp0 hdr $r.tmp0)
    (sparc.sti  as $r.tmp0 0 $r.result)
    (sparc.addi as $r.result ptrtag $r.result)))

; Given a bytevector-like structure and a fixnum, fill the bytevector 
; with the fixnum.
;
; FIXME: This should be in millicode?
; FIXME: Word fill is better, but the word has to be created; that
;        takes several instructions. Add this when/if we move to millicode
;        or have optimization modes.
; FIXME: Does not check that the fixnum actually fits in a byte.
; FIXME: Try to get rid of tmp2.

(define (emit-bytevector-like-fill! as r hdr)
  (let* ((fault (emit-double-tagcheck-assert! as
					      $tag.bytevector-tag
					      hdr
					      $ex.bvfill))
	 (L1    (new-label))
	 (L2    (new-label))
	 (r     (force-hwreg! as r $r.tmp2)))

    ; tmp0 is the count/index
    ; tmp1 is the base pointer
    ; r/tmp2 has the byte to fill with

    (sparc.btsti  as r 3)
    (sparc.bne    as fault)
    (sparc.srai   as r 2 $r.tmp2)
    (sparc.ldi    as $r.result (- $tag.bytevector-tag) $r.tmp0)
    (sparc.addi   as $r.result (- 4 $tag.bytevector-tag) $r.tmp1)
    (sparc.srai   as $r.tmp0 8 $r.tmp0)
    (sparc.label  as L2)
    (sparc.deccc  as $r.tmp0)
    (sparc.bge.a  as L2)
    (sparc.stbr   as $r.tmp2 $r.tmp0 $r.tmp1)
    (sparc.label  as L1)))

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
	  (sparc.btsti  as r 3)
	  (sparc.bne    as fault)
	  (sparc.ldi    as $r.result (- $tag.bytevector-tag) $r.tmp0)
	  ; check length
	  (sparc.srli   as $r.tmp0 8 $r.tmp0)
	  (sparc.srai   as r 2 $r.tmp1)
	  (sparc.cmpr   as $r.tmp1 $r.tmp0)
	  (sparc.bgeu.a as fault)
	  (sparc.slot   as))
	(begin
	  (sparc.srai   as r 2 $r.tmp1)))
    ; doit
    (sparc.subi as $r.result (- $tag.bytevector-tag 4) $r.tmp0)
    (sparc.ldbr as $r.tmp0 $r.tmp1 $r.tmp0)
    (if (not charize?)
	(sparc.slli as $r.tmp0 2 $r.result)
	(begin (sparc.slli as $r.tmp0 16 $r.result)
	       (sparc.ori  as $r.result $imm.character $r.result)))))


; fault is valid iff (unsafe-code) = #f
;
; FIXME: argument values passed to error handler appear to be 
;        bogus (error message is very strange).
; FIXME: There's no check that the value actually fits in a byte.

(define (emit-bytevector-like-set! as idx byte fault)
  (let ((r1 (force-hwreg! as idx $r.tmp0))
	(r2 (force-hwreg! as byte $r.argreg3)))
    (if (not (unsafe-code))
	(begin
	  ; both should be fixnums
	  (sparc.orr     as r1 r2 $r.tmp1)
	  (sparc.andicc  as $r.tmp1 3 $r.g0)
	  ; always OK to load header in delay slot
	  (sparc.bne     as fault)
	  (sparc.ldi     as $r.result (- $tag.bytevector-tag) $r.tmp1)
	  ; index must be valid
	  (sparc.srli    as $r.tmp1 8 $r.tmp1)    ; limit
	  (sparc.srai    as r1 2 $r.tmp0)         ; index
	  (sparc.cmpr    as $r.tmp0 $r.tmp1)
	  (sparc.bgeu    as fault))
	(begin
	  (sparc.srai   as r1 2 $r.tmp0)))
    (sparc.srli as r2 2 $r.tmp1)
    ; Using ARGREG2 as the destination is OK because the resulting pointer
    ; value always looks like a fixnum.  By doing so, we avoid needing TMP2.
    (sparc.subi as $r.result (- $tag.bytevector-tag 4) $r.argreg2)
    (sparc.stbr as $r.tmp1 $r.tmp0 $r.argreg2)))


; VECTORS and PROCEDURES

; emit-make-vector-like! assumes argreg3 is not destroyed by alloci.

(define (emit-make-vector-like! as r hdr ptrtag)
  (let ((FAULT (emit-assert-positive-fixnum! as $r.result $ex.mkvl)))
    (sparc.move  as $r.result $r.argreg3)
    (sparc.addi  as $r.result 4 $r.result)
    (sparc.jmpli as $r.millicode $m.alloci $r.o7)
    (if (null? r)
	(sparc.set as $imm.null $r.argreg2)
	(emit-move2hwreg! as r $r.argreg2))
    (sparc.slli  as $r.argreg3 8 $r.tmp0)
    (sparc.addi  as $r.tmp0 hdr $r.tmp0)
    (sparc.sti   as $r.tmp0 0 $r.result)
    (sparc.addi  as $r.result ptrtag $r.result)))


; `vector-ref' in various guises.
;
; Pointer must be valid; header may be reloaded unnecessarily. Fix later.
; FAULT is valid iff (unsafe-code) = #f

(define (emit-vector-like-ref! as r1 FAULT tag)
  (let ((index (force-hwreg! as r1 $r.argreg2)))
    (if (not (unsafe-code))
	(begin
	  ; Index must be fixnum.
	  (sparc.btsti as index 3)
	  (sparc.bne   as FAULT)
	  (sparc.ldi   as $r.result (- tag) $r.tmp0)
	  ; Index must be within bounds.
	  (sparc.srai  as $r.tmp0 8 $r.tmp0)
	  (sparc.cmpr  as $r.tmp0 index)
	  (sparc.bleu  as FAULT)
	  ; No NOP; the following instruction is valid in the slot.
	  ))
    (sparc.addi as $r.result (- 4 tag) $r.tmp0)
    (sparc.ldr  as $r.tmp0 index $r.result)))


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

(define (emit-vector-like-set! as r1 r2 fault tag)
  (let ((index (force-hwreg! as r1 $r.argreg2)))
    (if (not (unsafe-code))
	(begin
	  (sparc.btsti as index 3)
	  (sparc.bne   as fault)
	  (sparc.ldi   as $r.result (- tag) $r.tmp0)
	  (sparc.srai  as $r.tmp0 8 $r.tmp0)
	  (sparc.cmpr  as $r.tmp0 index)
	  (sparc.bleu  as fault)
	  ; no NOP; either of the following instructions are valid in slot.
	  ))
    (let ((value (force-hwreg! as r2 $r.argreg3)))
      (sparc.addr as $r.result index $r.tmp0)
      (sparc.sti  as value (- 4 tag) $r.tmp0)
      (if (write-barrier)
	  (if (inline-assignment)
	      (let ((L0 (new-label)))
		(sparc.cmpr  as $r.result $r.e-top)
		(sparc.ble.a as L0)
		(sparc.slot  as)
		(millicode-call/1arg as $m.addtrans value)
		(sparc.label as L0))
	      (begin
		(millicode-call/1arg as $m.addtrans value)
		))))))


; Character comparison.

; r is a register or a character constant.

(define (emit-char-cmp as r btrue.a excode)
  (emit-charcmp! as (lambda ()
		      (let ((l2 (new-label)))
			(sparc.set   as $imm.false $r.result)
			(btrue.a     as L2)
			(sparc.set   as $imm.true $r.result)
			(sparc.label as L2)))
		 $r.result
		 r
		 excode))
 
; op1 is a hw register
; op2 is a register or a character constant

(define (emit-char-bcmp-primop! as bfalse.a op1 op2 label excode)
  (let ((L0 (make-asm-label label)))
    (emit-charcmp! as (lambda ()
			(bfalse.a   as L0)
			(sparc.slot as))
		   op1
		   op2
		   excode)))

; We check the tags of both by xoring them and seeing if the low byte is 0.
; If so, then we can subtract one from the other (tag and all) and check the
; condition codes.  
; FIXME: BUG: That check is incorrect, consider (char=? 1 1).
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
	   (let ((L0 (new-label))
		 (L1 (new-label)))
	     (sparc.label as L0)
	     (cond ((char? op2)
		    (sparc.xori  as op1 $imm.character $r.tmp0)
		    (sparc.btsti as $r.tmp0 #xFF)
		    (sparc.srli  as op1 16 $r.tmp0)
		    (sparc.be.a  as L1)
		    (sparc.cmpi  as $r.tmp0 (char->integer op2)))
		   (else
		    (sparc.xorr  as op1 op2 $r.tmp0)
		    (sparc.btsti as $r.tmp0 #xFF)
		    (sparc.be.a  as L1)
		    (sparc.cmpr  as op1 op2)))
	     (if (not (eqv? op1 $r.result))
		 (sparc.move as op1 $r.result))
	     (cond ((char? op2) 
		    (sparc.set  as (char->immediate op2) $r.argreg2))
		   ((not (eqv? op2 $r.argreg2))
		    (sparc.move as op2 $r.argreg2)))
	     (sparc.set   as (thefixnum excode) $r.tmp0)
	     (millicode-call/ret as $m.exception L0)
	     (sparc.label as L1)))
	  ((not (char? op2))
	   (sparc.cmpr as op1 op2))
	  (else
	   (sparc.srli as op1 16 $r.tmp0)
	   (sparc.cmpi as $r.tmp0 (char->integer op2))))
    (tail)))

; eof
