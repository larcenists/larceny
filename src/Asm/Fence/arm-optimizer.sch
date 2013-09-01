; Copyright 2012 Lars T Hansen
;
; ARM optimizer.
;
; This file overrides the designated emitter functions in the ARM
; machine assembler.
;
; The optimizer removes conditional forward branches by adding
; conditions to subsequent unconditional instructions.  If a branch
; meets its target location it can be removed.  Experiments suggest a
; 5.3% reduction in code size.  No performance data available.
;
; TODO: The optimization should really be restricted to short windows
; of instructions, otherwise a branch would be faster even if it is
; larger.  Given how branchy the generated code is that should not be
; a problem but there are no data to back that assumption.

; Overrides the switch in Asm/Fence/asm-switches.sch

(define (cant-optimization . rest)
  (cond ((null? rest)
         (not (eq? *peep-state* 'disabled)))
        ((not (car rest))
         (set! *peep-state* 'disabled)
         #f)
        ((arm-peephole-optimization) #t)
        (else
         (set! *peep-state* 'empty)
         #t)))

; TODO: the globals *peep-state* and *peep-window* belong in 'as'.

; Optimizer states:
;  disabled: optimization permanently off
;  blocked:  optimization temporarily off (critical region)
;  empty:    window empty
;  bcc:      window has a conditional branch in slot 0
;  bcc/blx:  window has a conditional branch in slot 0 
;            and an unconditional blx (or similar) in slot 1

(define *peep-state* 'empty)

(define *peep-window* 
  (vector (vector 0 #f #f 0 0 0 0) ; #(not-cond fixup L  b3 b2 b1 b0)
          (vector 0 #f #f 0 0 0 0) ; #(0        #f    #f b3 b2 b1 b0)
  ))

(define (arm.emit-alu as b3 b2 b1 b0)
  (assert (= arm-cond.AL (fxlogand b3 #xF0)))
  (case *peep-state*
    ((empty disabled blocked)
     (arm.emit-bytes as b3 b2 b1 b0))
    ((bcc)
     (let* ((w     (vector-ref *peep-window* 0))
            (not-c (vector-ref w 0)))
       (arm.emit-bytes as (+ (- b3 arm-cond.AL) not-c) b2 b1 b0)))
    ((bcc/blx)
     (armopt.flush as)
     (arm.emit-bytes as b3 b2 b1 b0))
    (else ???)))

(define (arm.emit-mem as b3 b2 b1 b0)
  (arm.emit-alu as b3 b2 b1 b0))

(define (arm.emit-trap as b3 b2 b1 b0)
  (case *peep-state*
    ((empty disabled blocked)
     (arm.emit-bytes as b3 b2 b1 b0))
    ((bcc)
     (armopt.set-window as 1 0 #f #f b3 b2 b1 b0)
     (set! *peep-state* 'bcc/blx))
    ((bcc/blx)
     (armopt.flush as)
     (arm.emit-trap as b3 b2 b1 b0))
    (else ???)))

(define (arm.emit-branch as fixup L b3 b2 b1 b0)
  (case *peep-state*
    ((empty disabled blocked)
     (armopt.emit-branch-bytes as fixup b3 b2 b1 b0))
    ((bcc)
     (let* ((w     (vector-ref *peep-window* 0))
            (not-c (vector-ref w 0)))
       (armopt.emit-branch-bytes as fixup (+ (- b3 arm-cond.AL) not-c) b2 b1 b0)))
    ((bcc/blx)
     (armopt.flush as)
     (armopt.emit-branch-bytes as fixup b3 b2 b1 b0))
    (else ???)))

(define (arm.emit-bcc-fwd as fixup L b3 b2 b1 b0)
  (assert (not (= arm-cond.AL (fxlogand b3 #xF0))))
  (case *peep-state*
    ((empty)
     (let ((c (vector-ref armopt.opposite (fxrshl b3 4))))
       (if (not c)
           (armopt.emit-branch-bytes as fixup b3 b2 b1 b0)
           (begin
             (armopt.set-window as 0 c fixup L b3 b2 b1 b0)
             (set! *peep-state* 'bcc)))))
    ((bcc)
     (armopt.flush as)
     (arm.emit-bcc-fwd as fixup L b3 b2 b1 b0))
    ((disabled blocked)
     (armopt.emit-branch-bytes as fixup b3 b2 b1 b0))
    (else ???)))

(define (arm.emit-other as b3 b2 b1 b0)
  (armopt.flush as)
  (arm.emit-bytes as b3 b2 b1 b0))

(define (arm.emit-label as L)
  (let ((w0 (vector-ref *peep-window* 0))
        (w1 (vector-ref *peep-window* 1)))
    (case *peep-state*
      ((bcc)
       (if (eq? (vector-ref w0 2) L)
           (armopt.quash as)
           (armopt.flush as)))
      ((bcc/blx)
       (if (eq? (vector-ref w0 2) L)
           (let ((b3 (vector-ref w1 3))
                 (b2 (vector-ref w1 4))
                 (b1 (vector-ref w1 5))
                 (b0 (vector-ref w1 6))
                 (not-c (vector-ref w0 0)))
             (armopt.quash as)
             (arm.emit-bytes as (+ (- b3 arm-cond.AL) not-c) b2 b1 b0))
           (armopt.flush as)))
      (else
       (armopt.flush as)))))

(define (arm.begin-no-optimize as)
  (case *peep-state*
    ((disabled) #t)
    ((empty)
     (set! *peep-state* 'blocked))
    ((blocked)
     (error "No-optimize regions can't be nested"))
    ((bcc bcc/blx)
     (armopt.flush as)
     (set! *peep-state* 'blocked))
    (else ???)))

(define (arm.end-no-optimize as)
  (case *peep-state*
    ((blocked)
     (set! *peep-state* 'empty))
    ((disabled) #t)
    ((empty bcc bcc/blx)
     (error "No-optimize region is not in effect"))
    (else ???)))

;;; Optimizer internal below this point

(define armopt.opposite
  (vector arm-cond.NE arm-cond.EQ arm-cond.CC arm-cond.CS
          arm-cond.PL arm-cond.MI arm-cond.VC arm-cond.VS
          #f          #f          arm-cond.LT arm-cond.GE
          arm-cond.LE arm-cond.GT #f          #f))

(define (armopt.set-window as n not-c fixup L b3 b2 b1 b0)
  (let ((w (vector-ref *peep-window* n)))
    (vector-set! w 0 not-c)
    (vector-set! w 1 fixup)
    (vector-set! w 2 L)
    (vector-set! w 3 b3)
    (vector-set! w 4 b2)
    (vector-set! w 5 b1)
    (vector-set! w 6 b0)))

(define (armopt.flush as)

  (define (flush w)
    (let ((fixup (vector-ref w 1))
          (b3    (vector-ref w 3))
          (b2    (vector-ref w 4))
          (b1    (vector-ref w 5))
          (b0    (vector-ref w 6)))
      (if fixup
          (armopt.emit-branch-bytes as fixup b3 b2 b1 b0)
          (arm.emit-bytes as b3 b2 b1 b0))))

  (case *peep-state*
    ((empty disabled) #t)
    ((blocked) #t)
    ((bcc) 
     (flush (vector-ref *peep-window* 0))
     (set! *peep-state* 'empty))
    ((bcc/blx)
     (flush (vector-ref *peep-window* 0))
     (flush (vector-ref *peep-window* 1))
     (set! *peep-state* 'empty))
    (else ???)))

(define (armopt.quash as)
  (set! *peep-state* 'empty))

(define (armopt.emit-branch-bytes as fixup b3 b2 b1 b0)
  (emit-fixup-proc! as fixup)
  (arm.emit-bytes as b3 b2 b1 b0))
