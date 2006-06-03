; Copyright 1998 Lars T Hansen.
;
; $Id: peepopt.sch 2543 2005-07-20 21:54:03Z pnkfelix $
;
; 12 December 2003.
;
; Asm/Intel/peepopt.sch -- MAL peephole optimizer, for the Intel assembler.
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
    (cond ((= (car i2) $setreg)
           (reg-setreg as i1 i2 t2)))))

(define-peephole $setreg
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $reg)
	   (setreg-reg as i1 i2 t2))
	  ((= (car i2) $store)
	   (setreg-store as i1 i2 t2)))))

(define-peephole $global
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $invoke)
	   (as-source! as (cons (list $global/invoke (cadr i1) (cadr i2))
				t2))))))

(define-peephole $branch
  (lambda (as i1 i2 i3 t1 t2 t3)
    (cond ((= (car i2) $.align)
           (cond ((= (car i3) $.label)
                  (branch-and-label as i1 i2 i3 t3)))))))

; Reg-setreg is not restricted to hardware registers, as $movereg is 
; a standard instruction.

(define (reg-setreg as i:reg i:setreg tail)
  (let ((rs (operand1 i:reg))
        (rd (operand1 i:setreg)))
    (if (= rs rd)
        (as-source! as tail)
        (as-source! as (cons (list $movereg rs rd) tail)))))

; Optimize
;   setreg n
;   reg n
; on the assumption that setreg does not destroy RESULT

(define (setreg-reg as i:setreg i:reg tail)
  (if (= (operand1 i:setreg) (operand1 i:reg))
      (as-source! as (cons i:setreg tail))))

; Ditto for
;   setreg n
;   store n, k

(define (setreg-store as i:setreg i:store tail)
  (if (= (operand1 i:setreg) (operand1 i:store))
      (as-source! as (cons i:setreg
			   (cons (list $setstk (operand2 i:store))
				 tail)))))

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

