; Copyright 1998 Lars T Hansen and William D Clinger.
; 
; $Id: sparcprim-part1.sch,v 1.1.1.1 1998/11/19 21:52:00 lth Exp $
;
; Asm/Sparc/sparcprim-part1.sch -- SPARC primitives, part 1.
; Primitives defined in Compiler/sparc.imp.sch.

; Primop emitters.

(define (emit-primop.1arg! as op)
  ((find-primop op) as))

(define (emit-primop.2arg! as op r)
  ((find-primop op) as r))

(define (emit-primop.3arg! as a1 a2 a3)
  ((find-primop a1) as a2 a3))

(define (emit-primop.4arg! as a1 a2 a3 a4)
  ((find-primop a1) as a2 a3 a4))


; Hash table of primops

(define primop-vector (make-vector 256 '()))

(define (define-primop name proc)
  (let ((h (logand (symbol-hash name) 255)))
    (vector-set! primop-vector h (cons (cons name proc)
				       (vector-ref primop-vector h)))
    name))

(define (find-primop name)
  (let ((h (logand (symbol-hash name) 255)))
    (cdr (assq name (vector-ref primop-vector h)))))


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

(define-primop 'gc-counter
  (lambda (as)
    (sparc.ldi as $r.globals $g.gccnt $r.result)))

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

(define-primop '+
  (lambda (as r)
    (emit-primop.4arg! as 'internal:+ $r.result r $r.result)))

(define-primop '-
  (lambda (as r)
    (emit-primop.4arg! as 'internal:- $r.result r $r.result)))

(define-primop '*
  (lambda (as r)
    (let ((rs2    (force-hwreg! as r $r.argreg2))
	  (Ltagok (new-label))
	  (Loflo  (new-label))
	  (Ldone  (new-label)))
      (sparc.orr     as $r.result rs2 $r.tmp0)
      (sparc.btsti   as $r.tmp0 3)
      (sparc.be.a    as Ltagok)
      (sparc.srai    as $r.result 2 $r.tmp0)
      (sparc.label   as Loflo)
      (if (not (= rs2 $r.argreg2))
	  (sparc.move as rs2 $r.argreg2))
      (millicode-call/ret as $m.multiply Ldone)
      (sparc.label   as Ltagok)
      (sparc.smulr   as $r.tmp0 rs2 $r.tmp0)
      (sparc.rdy     as $r.tmp1)
      (sparc.srai    as $r.tmp0 31 $r.tmp2)
      (sparc.cmpr    as $r.tmp1 $r.tmp2)
      (sparc.bne.a   as Loflo)
      (sparc.slot    as)
      (sparc.move    as $r.tmp0 $r.result)
      (sparc.label   as Ldone))))

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
    (emit-negate as $r.result $r.result)))

(define-primop 'round
  (lambda (as)
    (millicode-call/0arg as $m.round)))

(define-primop 'truncate
  (lambda (as)
    (millicode-call/0arg as $m.truncate)))

(define-primop 'lognot
  (lambda (as)
    (if (not (unsafe-code))
	(emit-assert-fixnum! as $r.result $ex.lognot))
    (sparc.ornr as $r.g0 $r.result $r.result)  ; argument order matters
    (sparc.xori as $r.result 3 $r.result)))

(define-primop 'logand
  (lambda (as x)
    (logical-op as $r.result x $r.result sparc.andr $ex.logand)))

(define-primop 'logior
  (lambda (as x)
    (logical-op as $r.result x $r.result sparc.orr $ex.logior)))

(define-primop 'logxor
  (lambda (as x)
    (logical-op as $r.result x $r.result sparc.xorr $ex.logxor)))

; Fixnum shifts.
;
; Only positive shifts are meaningful.
; FIXME: These are incompatible with MacScheme and MIT Scheme.
; FIXME: need to return to start of sequence after fault.

(define-primop 'lsh
  (lambda (as x)
    (emit-shift-operation as $ex.lsh $r.result x $r.result)))

(define-primop 'rshl
  (lambda (as x)
    (emit-shift-operation as $ex.rshl $r.result x $r.result)))

(define-primop 'rsha
  (lambda (as x)
    (emit-shift-operation as $ex.rsha $r.result x $r.result)))


; fixnums only.
; FIXME: for symmetry with shifts there should be rotl and rotr (?)
;        or perhaps rot should only ever rotate one way.
; FIXME: implement.

(define-primop 'rot
  (lambda (as x)
    (asm-error "Sparcasm: ROT primop is not implemented.")))

(define-primop 'null?
  (lambda (as)
    (sparc.cmpi as $r.result $imm.null)
    (emit-set-boolean! as)))

(define-primop 'pair?
  (lambda (as)
    (emit-single-tagcheck->bool! as $tag.pair-tag)))

(define-primop 'eof-object?
  (lambda (as)
    (sparc.cmpi as $r.result $imm.eof)
    (emit-set-boolean! as)))

; Tests the specific representation, not 'flonum or compnum with 0i'.

(define-primop 'flonum?
  (lambda (as)
    (emit-double-tagcheck->bool! as $tag.bytevector-tag
				 (+ $imm.bytevector-header
				    $tag.flonum-typetag))))

(define-primop 'compnum?
  (lambda (as)
    (emit-double-tagcheck->bool! as $tag.bytevector-tag
				 (+ $imm.bytevector-header
				    $tag.compnum-typetag))))

(define-primop 'symbol?
  (lambda (as)
    (emit-double-tagcheck->bool! as $tag.vector-tag
				 (+ $imm.vector-header
				    $tag.symbol-typetag))))

(define-primop 'port?
  (lambda (as)
    (emit-double-tagcheck->bool! as $tag.vector-tag
				 (+ $imm.vector-header
				    $tag.port-typetag))))

(define-primop 'structure?
  (lambda (as)
    (emit-double-tagcheck->bool! as $tag.vector-tag
				 (+ $imm.vector-header
				    $tag.structure-typetag))))

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

(define-primop 'cons
  (lambda (as r)
    (emit-primop.4arg! as 'internal:cons $r.result r $r.result)))

(define-primop 'car
  (lambda (as)
    (emit-primop.3arg! as 'internal:car $r.result $r.result)))

(define-primop 'cdr
  (lambda (as)
    (emit-primop.3arg! as 'internal:cdr $r.result $r.result)))

(define-primop 'set-car!
  (lambda (as x)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert! as $tag.pair-tag $ex.car #f))
    (emit-setcar/setcdr! as $r.result x 0)))

(define-primop 'set-cdr!
  (lambda (as x)
    (if (not (unsafe-code))
	(emit-single-tagcheck-assert! as $tag.pair-tag $ex.cdr #f))
    (emit-setcar/setcdr! as $r.result x 4)))

; Cells are internal data structures, represented using pairs.
; No error checking is done on cell references.

(define-primop 'make-cell
  (lambda (as)
    (emit-primop.4arg! as 'internal:cons $r.result $r.g0 $r.result)))

(define-primop 'cell-ref
  (lambda (as)
    (emit-primop.3arg! as 'internal:cell-ref $r.result $r.result)))

(define-primop 'cell-set!
  (lambda (as r)
    (emit-setcar/setcdr! as $r.result r 0)))

(define-primop 'syscall
  (lambda (as)
    (millicode-call/0arg as $m.syscall)))

(define-primop 'break
  (lambda (as)
    (millicode-call/0arg as $m.break)))

(define-primop 'creg
  (lambda (as)
    (millicode-call/0arg as $m.creg)))

(define-primop 'creg-set!
  (lambda (as)
    (millicode-call/0arg as $m.creg-set!)))

(define-primop 'typetag
  (lambda (as)
    (millicode-call/0arg as $m.typetag)))

(define-primop 'typetag-set!
  (lambda (as r)
    (millicode-call/1arg as $m.typetag-set r)))

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
    (if (not (unsafe-code))
	(emit-assert-char! as $ex.char2int #f))
    (sparc.srli as $r.result 14 $r.result)))

(define-primop 'integer->char
  (lambda (as)
    (if (not (unsafe-code))
	(emit-assert-fixnum! as $r.result $ex.int2char))
    (sparc.andi as $r.result #x3FF $r.result)
    (sparc.slli as $r.result 14 $r.result)
    (sparc.ori  as $r.result $imm.character $r.result)))

(define-primop 'not
  (lambda (as)
    (sparc.cmpi as $r.result $imm.false)
    (emit-set-boolean! as)))

(define-primop 'eq?
  (lambda (as x)
    (emit-primop.4arg! as 'internal:eq? $r.result x $r.result)))

(define-primop 'eqv?
  (lambda (as x)
    (let ((tmp (force-hwreg! as x $r.tmp0))
	  (L1  (new-label)))
      (sparc.cmpr as $r.result tmp)
      (sparc.be.a as L1)
      (sparc.set  as $imm.true $r.result)
      (millicode-call/1arg as $m.eqv tmp)
      (sparc.label as L1))))

(define-primop 'make-bytevector
  (lambda (as)
    (if (not (unsafe-code))
	(emit-assert-positive-fixnum! as $r.result $ex.mkbvl))
    (emit-allocate-bytevector as
			      (+ $imm.bytevector-header
				 $tag.bytevector-typetag)
			      #f)
    (sparc.addi as $r.result $tag.bytevector-tag $r.result)))

(define-primop 'bytevector-fill!
  (lambda (as rs2)
    (let* ((fault (emit-double-tagcheck-assert! as
						$tag.bytevector-tag
						(+ $imm.bytevector-header
						   $tag.bytevector-typetag)
						$ex.bvfill
						rs2))
	   (rs2 (force-hwreg! as rs2 $r.argreg2)))
      (sparc.btsti  as rs2 3)
      (sparc.bne    as fault)
      (sparc.srai   as rs2 2 $r.tmp2)
      (sparc.ldi    as $r.result (- $tag.bytevector-tag) $r.tmp0)
      (sparc.addi   as $r.result (- 4 $tag.bytevector-tag) $r.tmp1)
      (sparc.srai   as $r.tmp0 8 $r.tmp0)
      (emit-bytevector-fill as $r.tmp0 $r.tmp1 $r.tmp2))))

(define-primop 'bytevector-length
  (lambda (as)
    (emit-get-length! as 
		      $tag.bytevector-tag
		      (+ $imm.bytevector-header $tag.bytevector-typetag)
		      $ex.bvlen
		      $r.result
		      $r.result)))

(define-primop 'bytevector-like-length
  (lambda (as)
    (emit-get-length! as
		      $tag.bytevector-tag
		      #f
		      $ex.bvllen
		      $r.result
		      $r.result)))

(define-primop 'bytevector-ref
  (lambda (as r)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert!
		      as
		      $tag.bytevector-tag
		      (+ $imm.bytevector-header $tag.bytevector-typetag)
		      $ex.bvref
		      r)
		     #f)))
      (emit-bytevector-like-ref! as $r.result r $r.result fault #f #t))))

(define-primop 'bytevector-like-ref
  (lambda (as r)
    (let ((fault (if (not (unsafe-code))
		     (emit-single-tagcheck-assert! as
						   $tag.bytevector-tag
						   $ex.bvlref
						   r)
		     #f)))
      (emit-bytevector-like-ref! as $r.result r $r.result fault #f #f))))

(define-primop 'bytevector-set!
  (lambda (as r1 r2)
    (let ((fault (if (not (unsafe-code))
		     (emit-double-tagcheck-assert!
		      as
		      $tag.bytevector-tag
		      (+ $imm.bytevector-header $tag.bytevector-typetag)
		      $ex.bvset
		      r1)
		     #f)))
      (emit-bytevector-like-set! as r1 r2 fault #t))))

(define-primop 'bytevector-like-set!
  (lambda (as r1 r2)
    (let ((fault (if (not (unsafe-code))
		     (emit-single-tagcheck-assert! as
						   $tag.bytevector-tag
						   $ex.bvlset
						   r1)
		     #f)))
      (emit-bytevector-like-set! as r1 r2 fault #f))))

(define-primop 'sys$bvlcmp
  (lambda (as x)
    (millicode-call/1arg as $m.bvlcmp x)))

; Strings

; RESULT must have nonnegative fixnum.
; RS2 must have character.

(define-primop 'make-string
  (lambda (as rs2)
    (let ((FAULT (new-label))
	  (START (new-label)))
      (sparc.label as START)
      (let ((rs2 (force-hwreg! as rs2 $r.argreg2)))
	(if (not (unsafe-code))
	    (let ((L1 (new-label))
		  (L2 (new-label)))
	      (sparc.tsubrcc as $r.result $r.g0 $r.g0)
	      (sparc.bvc.a   as L1)
	      (sparc.andi    as rs2 255 $r.tmp0)
	      (sparc.label   as FAULT)
	      (if (not (= rs2 $r.argreg2))
		  (sparc.move as rs2 $r.argreg2))
	      (sparc.set     as (thefixnum $ex.mkbvl) $r.tmp0) ; Wrong code.
	      (millicode-call/ret as $m.exception START)
	      (sparc.label   as L1)
	      (sparc.bl      as FAULT)
	      (sparc.cmpi    as $r.tmp0 $imm.character)
	      (sparc.bne     as FAULT)
	      (sparc.move as $r.result $r.argreg3))
	    (begin
	      (sparc.move as $r.result $r.argreg3)))
	(emit-allocate-bytevector as
				  (+ $imm.bytevector-header
				     $tag.string-typetag)
				  $r.argreg3)
	(sparc.srai   as rs2 16 $r.tmp1)
	(sparc.addi   as $r.result 4 $r.result)
	(sparc.srai   as $r.argreg3 2 $r.tmp0)
	(emit-bytevector-fill as $r.tmp0 $r.result $r.tmp1)
	(sparc.addi as $r.result (- $tag.bytevector-tag 4) $r.result)))))

(define-primop 'string-length
  (lambda (as)
    (emit-primop.3arg! as 'internal:string-length $r.result $r.result)))

(define-primop 'string-ref
  (lambda (as r)
    (emit-primop.4arg! as 'internal:string-ref $r.result r $r.result)))

(define-primop 'string-set!
  (lambda (as r1 r2)
    (emit-string-set! as $r.result r1 r2)))

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

(define-primop 'make-vector:0
  (lambda (as r) (make-vector-n as 0 r)))

(define-primop 'make-vector:1
  (lambda (as r) (make-vector-n as 1 r)))

(define-primop 'make-vector:2
  (lambda (as r) (make-vector-n as 2 r)))

(define-primop 'make-vector:3
  (lambda (as r) (make-vector-n as 3 r)))

(define-primop 'make-vector:4
  (lambda (as r) (make-vector-n as 4 r)))

(define-primop 'make-vector:5
  (lambda (as r) (make-vector-n as 5 r)))

(define-primop 'make-vector:6
  (lambda (as r) (make-vector-n as 6 r)))

(define-primop 'make-vector:7
  (lambda (as r) (make-vector-n as 7 r)))

(define-primop 'make-vector:8
  (lambda (as r) (make-vector-n as 8 r)))

(define-primop 'make-vector:9
  (lambda (as r) (make-vector-n as 9 r)))

(define-primop 'vector-length
  (lambda (as)
    (emit-primop.3arg! as 'internal:vector-length $r.result $r.result)))

(define-primop 'vector-like-length
  (lambda (as)
    (emit-get-length! as $tag.vector-tag #f $ex.vllen $r.result $r.result)))

(define-primop 'procedure-length
  (lambda (as)
    (emit-get-length! as $tag.procedure-tag #f $ex.plen $r.result $r.result)))

(define-primop 'vector-ref
  (lambda (as r)
    (emit-primop.4arg! as 'internal:vector-ref $r.result r $r.result)))

(define-primop 'vector-like-ref
  (lambda (as r)
    (let ((fault (if (not (unsafe-code))
		     (emit-single-tagcheck-assert! as
						   $tag.vector-tag
						   $ex.vlref
						   r)
		     #f)))
      (emit-vector-like-ref!
       as $r.result r $r.result fault $tag.vector-tag #f))))

(define-primop 'procedure-ref
  (lambda (as r)
    (let ((fault (if (not (unsafe-code))
		     (emit-single-tagcheck-assert! as
						   $tag.procedure-tag
						   $ex.pref
						   r)
		     #f)))
      (emit-vector-like-ref!
       as $r.result r $r.result fault $tag.procedure-tag #f))))

(define-primop 'vector-set!
  (lambda (as r1 r2)
    (emit-primop.4arg! as 'internal:vector-set! $r.result r1 r2)))

(define-primop 'vector-like-set!
  (lambda (as r1 r2)
    (let ((fault (if (not (unsafe-code))
		     (emit-single-tagcheck-assert! as
						   $tag.vector-tag
						   $ex.vlset
						   r1)
		     #f)))
      (emit-vector-like-set! as $r.result r1 r2 fault $tag.vector-tag #f))))

(define-primop 'procedure-set!
  (lambda (as r1 r2)
    (let ((fault (if (not (unsafe-code))
		     (emit-single-tagcheck-assert! as
						   $tag.procedure-tag
						   $ex.pset
						   r1)
		     #f)))
      (emit-vector-like-set! as $r.result r1 r2 fault $tag.procedure-tag #f))))

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

; Experimental (for performance).
; This makes massive assumptions about the layout of the port structure:
; A port is a vector-like where
;   #0 = port.input?
;   #4 = port.buffer
;   #7 = port.rd-lim
;   #8 = port.rd-ptr
; See Lib/iosys.sch for more information.

(define-primop 'sys$read-char
  (lambda (as)
    (let ((Lfinish (new-label))
	  (Lend    (new-label)))
      (if (not (unsafe-code))
	  (begin
	    (sparc.andi as $r.result $tag.tagmask $r.tmp0) ; mask argument tag
	    (sparc.cmpi as $r.tmp0 $tag.vector-tag); vector-like? 
	    (sparc.bne as Lfinish)		   ; skip if not vector-like
	    (sparc.nop as)
	    (sparc.ldbi as $r.RESULT 0 $r.tmp1)))   ; header byte
      (sparc.ldi  as $r.RESULT 1 $r.tmp2)	    ; port.input? or garbage
      (if (not (unsafe-code))
	  (begin
	    (sparc.cmpi as $r.tmp1 $hdr.port)       ; port?
	    (sparc.bne as Lfinish)))		    ; skip if not port
      (sparc.cmpi as $r.tmp2 $imm.false)  	    ; [slot] input port?
      (sparc.be as Lfinish)			    ; skip if not active port
      (sparc.ldi as $r.RESULT (+ 1 32) $r.tmp1)	    ; [slot] port.rd-ptr 
      (sparc.ldi as $r.RESULT (+ 1 28) $r.tmp2)	    ; port.rd-lim
      (sparc.ldi as $r.RESULT (+ 1 16) $r.tmp0)	    ; port.buffer
      (sparc.cmpr as $r.tmp1 $r.tmp2)		    ; rd-ptr < rd-lim?
      (sparc.bge as Lfinish)			    ; skip if rd-ptr >= rd-lim
      (sparc.subi as $r.tmp0 1 $r.tmp0)		    ; [slot] addr of string@0
      (sparc.srai as $r.tmp1 2 $r.tmp2)		    ; rd-ptr as native int
      (sparc.ldbr as $r.tmp0 $r.tmp2 $r.tmp2)	    ; get byte from string
      (sparc.addi as $r.tmp1 4 $r.tmp1)		    ; bump rd-ptr
      (sparc.sti as $r.tmp1 (+ 1 32) $r.RESULT)	    ; store rd-ptr in port
      (sparc.slli as $r.tmp2 16 $r.tmp2)	    ; convert to char #1
      (sparc.b as Lend)
      (sparc.ori as $r.tmp2 $imm.character $r.RESULT) ; [slot] convert to char
      (sparc.label as Lfinish)
      (sparc.set as $imm.false $r.RESULT)	    ; failed
      (sparc.label as Lend))))


; eof
