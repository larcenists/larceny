; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; 14 March 2002.
;
; Asm/Standard-C/peepopt.sch -- MAL peephole optimizer, for the Standard-C assembler.
;
; The procedure `peep' is called on the as structure before every
; instruction is assembled.  It may replace the prefix of the instruction
; stream by some other instruction sequence.
;
; Invariant: if the peephole optimizer doesn't change anything, then 
;
;  (let ((x (as-source as)))
;    (peep as)
;    (eq? x (as-source as)))     => #t
;
; Note this still isn't right -- it should be integrated with pass5p2 --
; but it's a step in the right direction.

(define *peephole-table* (make-vector *number-of-mnemonics* #f))

(define (define-peephole n p)
  (vector-set! *peephole-table* n p)
  (unspecified))

(define (peep as)
  (let ((t0 (as-source as)))
    (if (not (null? t0))
        (let ((i1 (car t0)))
          (let ((p (vector-ref *peephole-table* (car i1))))
            (if p
                (let* ((t1 (if (null? t0) t0 (cdr t0)))
                       (i2 (if (null? t1) '(-1 0 0 0) (car t1)))
                       (t2 (if (null? t1) t1 (cdr t1)))
                       (i3 (if (null? t2) '(-1 0 0 0) (car t2)))
                       (t3 (if (null? t2) t2 (cdr t2))))
                  (p as i1 i2 i3 t1 t2 t3))))))))

(define-peephole $reg
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $op1)
           (cond ((= (car i3) $check)
                  (reg-op1-check as i1 i2 i3 t3))))
          ((= (car i2) $op2)
           (cond ((= (car i3) $check)
                  (reg-op2-check as i1 i2 i3 t3))))
          ((= (car i2) $op2imm)
           (cond ((= (car i3) $check)
                  (reg-op2imm-check as i1 i2 i3 t3))))
          ((= (car i2) $setreg)
           (reg-setreg as i1 i2 t2)))))

(define-peephole $op1
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $branchf)
           (op1-branchf as i1 i2 t2))
          ((= (car i2) $check)
           (op1-check as i1 i2 t2)))))

(define-peephole $op2
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $branchf)
           (op2-branchf as i1 i2 t2))
          ((= (car i2) $check)
           (op2-check as i1 i2 t2)))))

(define-peephole $op2imm
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $branchf)
           (op2imm-branchf as i1 i2 t2))
          ((= (car i2) $check)
           (op2imm-check as i1 i2 t2)))))

(define-peephole $const
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $op2)
           (const-op2 as i1 i2 t2)))))

(define-peephole $branch
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $.align)
           (cond ((= (car i3) $.label)
                  (branch-and-label as i1 i2 i3 t3)))))))

(define-peephole $reg/op1/check
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $reg)
           (cond ((= (car i3) $op1)
                  (if (not (null? t3))
                      (let ((i4 (car t3))
                            (t4 (cdr t3)))
                        (cond ((= (car i4) $setreg)
                               (reg/op1/check-reg-op1-setreg
                                as i1 i2 i3 i4 t4)))))))))))

(define-peephole $reg/op2/check
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $reg)
           (cond ((= (car i3) $op2imm)
                  (if (not (null? t3))
                      (let ((i4 (car t3))
                            (t4 (cdr t3)))
                        (cond ((= (car i4) $check)
                               (reg/op2/check-reg-op2imm-check
                                as i1 i2 i3 i4 t4)))))))))))

; Worker procedures.

(define (op1-branchf as i:op1 i:branchf tail)
  (let* ((op (operand1 i:op1))
	 (L  (operand1 i:branchf))
	 (op (case op
	       ((null?)       'internal:branchf-null?)
	       ((pair?)       'internal:branchf-pair?)
;	       ((zero?)       'internal:branchf-zero?)
	       ((eof-object?) 'internal:branchf-eof-object?)
	       ((fixnum?)     'internal:branchf-fixnum?)
	       ((char?)       'internal:branchf-char?)
	       ((fxzero?)     'internal:branchf-fxzero?)
;	       ((fxnegative?) 'internal:branchf-fxnegative?)
;	       ((fxpositive?) 'internal:branchf-fxpositive?)
	       (else #f))))
    (if op
        (as-source! as (cons (list $op1/branchf op L) tail)))))

(define (op2-branchf as i:op2 i:branchf tail)
  (let* ((op  (operand1 i:op2))
	 (rs2 (operand2 i:op2))
	 (L   (operand1 i:branchf))
	 (op  (case op
;		((<)       'internal:branchf-<)
;		((>)       'internal:branchf->)
;		((>=)      'internal:branchf->=)
;		((<=)      'internal:branchf-<=)
;		((=)       'internal:branchf-=)
;		((eq?)     'internal:branchf-eq?)
;		((char=?)  'internal:branchf-char=?)
;		((char>=?) 'internal:branchf-char>=?)
;		((char>?)  'internal:branchf-char>?)
;		((char<=?) 'internal:branchf-char<=?)
;		((char<?)  'internal:branchf-char<?)
;		((fx=)     'internal:branchf-fx=)
;		((fx>)     'internal:branchf-fx>)
;		((fx>=)    'internal:branchf-fx>=)
;		((fx<)     'internal:branchf-fx<)
;		((fx<=)    'internal:branchf-fx<=)
		(else #f))))
    (if op
        (as-source! as
                    (cons (list $op2/branchf op rs2 L)
                          tail)))))

(define (op2imm-branchf as i:op2imm i:branchf tail)
  (let* ((op  (operand1 i:op2imm))
	 (imm (operand2 i:op2imm))
	 (L   (operand1 i:branchf))
	 (op  (case op
;		((<)       'internal:branchf-</imm)
;		((>)       'internal:branchf->/imm)
;		((>=)      'internal:branchf->=/imm)
;		((<=)      'internal:branchf-<=/imm)
;		((=)       'internal:branchf-=/imm)
;		((eq?)     'internal:branchf-eq?/imm)
;		((char=?)  'internal:branchf-char=?/imm)
;		((char>=?) 'internal:branchf-char>=?/imm)
;		((char>?)  'internal:branchf-char>?/imm)
;		((char<=?) 'internal:branchf-char<=?/imm)
;		((char<?)  'internal:branchf-char<?/imm)
;		((fx=)     'internal:branchf-fx=/imm)
;		((fx>)     'internal:branchf-fx>/imm)
;		((fx>=)    'internal:branchf-fx>=/imm)
;		((fx<)     'internal:branchf-fx</imm)
;		((fx<=)    'internal:branchf-fx<=/imm)
		(else #f))))
    (if op
        (as-source! as
                    (cons (list $op2imm/branchf op imm L)
                          tail)))))

; Check optimization.

(define (reg-op1-check as i:reg i:op1 i:check tail)
  (let ((rs (operand1 i:reg))
        (op (operand1 i:op1)))
    (peep-reg/op1/check as
			op
			rs
			(operand4 i:check)
			(list (operand1 i:check)
			      (operand2 i:check)
			      (operand3 i:check))
			tail)))

(define (op1-check as i:op1 i:check tail)
  (let ((op (operand1 i:op1)))
    (peep-reg/op1/check as
                        op
                        'RESULT
                        (operand4 i:check)
                        (list (operand1 i:check)
                              (operand2 i:check)
                              (operand3 i:check))
                        tail)))

(define (peep-reg/op1/check as op rs L1 liveregs tail)
  (let ((op (case op
              ((fixnum?)      'internal:check-fixnum?)
              ((pair?)        'internal:check-pair?)
              ((vector?)      'internal:check-vector?)
              ((string?)      'internal:check-string?)
              (else #f))))
    (if op
        (as-source! as
                    (cons (list $reg/op1/check op rs L1 liveregs)
                          tail)))))

(define (reg-op2-check as i:reg i:op2 i:check tail)
  (let ((rs1 (operand1 i:reg))
        (rs2 (operand2 i:op2))
        (op (operand1 i:op2)))
    (peep-reg/op2/check as
			op
			rs1
			rs2
			(operand4 i:check)
			(list (operand1 i:check)
			      (operand2 i:check)
			      (operand3 i:check))
			tail)))

(define (op2-check as i:op2 i:check tail)
  (let ((rs2 (operand2 i:op2))
        (op (operand1 i:op2)))
    (peep-reg/op2/check as
                        op
                        'RESULT
                        rs2
                        (operand4 i:check)
                        (list (operand1 i:check)
                              (operand2 i:check)
                              (operand3 i:check))
                        tail)))

(define (peep-reg/op2/check as op rs1 rs2 L1 liveregs tail)
  (let ((op (case op
;              ((<:fix:fix)   'internal:check-<:fix:fix)
;              ((<=:fix:fix)  'internal:check-<=:fix:fix)
;              ((>=:fix:fix)  'internal:check->=:fix:fix)
              (else #f))))
    (if op
        (as-source! as
                    (cons (list $reg/op2/check op rs1 rs2 L1 liveregs)
                          tail)))))

(define (reg-op2imm-check as i:reg i:op2imm i:check tail)
  (let ((rs1 (operand1 i:reg))
        (op (operand1 i:op2imm))
        (imm (operand2 i:op2imm)))
    (peep-reg/op2imm/check as
			   op
			   rs1
			   imm
			   (operand4 i:check)
			   (list (operand1 i:check)
				 (operand2 i:check)
				 (operand3 i:check))
			   tail)))

(define (op2imm-check as i:op2imm i:check tail)
  (let ((op (operand1 i:op2imm))
        (imm (operand2 i:op2imm)))
    (peep-reg/op2imm/check as
                           op
                           'RESULT
                           imm
                           (operand4 i:check)
                           (list (operand1 i:check)
                                 (operand2 i:check)
                                 (operand3 i:check))
                           tail)))

(define (peep-reg/op2imm/check as op rs1 imm L1 liveregs tail)
  (let ((op (case op
;              ((<:fix:fix)   'internal:check-<:fix:fix/imm)
;              ((<=:fix:fix)  'internal:check-<=:fix:fix/imm)
;              ((>=:fix:fix)  'internal:check->=:fix:fix/imm)
              (else #f))))
    (if op
        (as-source! as
                    (cons (list $reg/op2imm/check op rs1 imm L1 liveregs)
                          tail)))))

(define (reg/op1/check-reg-op1-setreg as i:ro1check i:reg i:op1 i:setreg tail)
  (let ((o1 (operand1 i:ro1check))
        (r1 (operand2 i:ro1check))
        (r2 (operand1 i:reg))
        (o2 (operand1 i:op1))
        (r3 (operand1 i:setreg)))
;    (if (and (eq? o1 'internal:check-vector?)
;             (eq? r1 r2)
;             (eq? o2 'vector-length:vec))
;        (as-source! as
;                    (cons (list $reg/op2/check
;                                'internal:check-vector?/vector-length:vec
;                                r1
;                                r3
;                                (operand3 i:ro1check)
;                                (operand4 i:ro1check))
;                          tail)))
;    (if (and (eq? o1 'internal:check-string?)
;             (eq? r1 r2)
;             (eq? o2 'string-length:str))
;        (as-source! as
;                    (cons (list $reg/op2/check
;                                'internal:check-string?/string-length:str
;                                r1
;                                r3
;                                (operand3 i:ro1check)
;                                (operand4 i:ro1check))
;                          tail)))
    (unspecified) ))

; Range checks of the form 0 <= i < n can be performed by a single check.
; This peephole optimization recognizes
;         reg     rs1
;         op2     <:fix:fix,rs2
;         check   r1,r2,r3,L
;         reg     rs1                     ; must match earlier reg
;         op2imm  >=:fix:fix,0
;         check   r1,r2,r3,L              ; label must match earlier check

(define (reg/op2/check-reg-op2imm-check
         as i:ro2check i:reg i:op2imm i:check tail)
  (let ((o1   (operand1 i:ro2check))
        (rs1  (operand2 i:ro2check))
        (rs2  (operand3 i:ro2check))
        (L1   (operand4 i:ro2check))
        (live (operand5 i:ro2check))
        (rs3  (operand1 i:reg))
        (o2   (operand1 i:op2imm))
        (x    (operand2 i:op2imm))
        (L2   (operand4 i:check)))
;    (if (and (eq? o1 'internal:check-<:fix:fix)
;             (eq? o2 '>=:fix:fix)
;             (eq? rs1 rs3)
;             (eq? x 0)
;             (eq? L1 L2))
;        (as-source! as
;                    (cons (list $reg/op2/check 'internal:check-range
;                                                rs1 rs2 L1 live)
;                          tail)))
    (unspecified)))

; End of check optimization.

; Reg-setreg is not restricted to hardware registers, as $movereg is 
; a standard instruction.

(define (reg-setreg as i:reg i:setreg tail)
  (let ((rs (operand1 i:reg))
        (rd (operand1 i:setreg)))
    (if (= rs rd)
        (as-source! as tail)
        (as-source! as (cons (list $movereg rs rd) tail)))))

; Make-vector on vectors of known short length.

(define (const-op2 as i:const i:op2 tail)
  (let ((vn '#(make-vector:0 make-vector:1 make-vector:2 make-vector:3
               make-vector:4 make-vector:5 make-vector:6 make-vector:7
               make-vector:8 make-vector:9))
        (c  (operand1 i:const))
        (op (operand1 i:op2))
        (r  (operand2 i:op2)))
;    (if (and (eq? op 'make-vector)
;             (fixnum? c)
;             (<= 0 c 9))
;        (as-source! as (cons (list $op2 (vector-ref vn c) r) tail)))
    (unspecified) ))

; Gets rid of spurious branch-to-next-instruction
;    (branch Lx k)
;    (.align y)
;    (.label Lx)
; => (.align y)
;    (.label Lx)

(define (branch-and-label as i:branch i:align i:label tail)
  (let ((branch-label (operand1 i:branch))
        (label        (operand1 i:label)))
    (if (= branch-label label)
        (as-source! as (cons i:align (cons i:label tail))))))

; Test code

(define (peeptest istream)
  (let ((as (make-assembly-structure istream)))
    (let loop ((l '()))
      (if (null? (as-source as))
          (reverse l)
          (begin (peep as)
                 (let ((a (car (as-source as))))
                   (as-source! as (cdr (as-source as)))
                   (loop (cons a l))))))))


; eof
