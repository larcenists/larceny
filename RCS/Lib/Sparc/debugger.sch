; -*- Scheme -*-
;
; Lowlevel debugging support for Larceny (Sparc Version)
;
; $Id$

(define break-entry)
(define unbreak-entry)
(define sparc-exception-handler)

(let ()

  ; List of breakpoints. Each entry is a list of three elements.
  ; The first is the procedure in which the breakpoint is set;
  ; the second is the offset in the procedure at which the breakpoint
  ; is set; and the third is the original instruction at the site of the
  ; breakpoint, as a bytevector of four bytes.

  (define bp-list '())

  ; The trap instruction which is inserted at the point of the break.

  (define break-instruction (bytevector ...))

  ; Grab an instruction out of the code vector.

  (define (get-instr cv offset)
    (bytevector (bytevector-like-ref cv offset)
		(bytevector-like-ref cv (+ offset 1))
		(bytevector-like-ref cv (+ offset 2))
		(bytevector-like-ref cv (+ offset 3))))

  ; Put an instruction back into the code vector.

  (define (set-instr! cv offset instr)
    (bytevector-like-set! cv offset (bytevector-like-ref instr 0))
    (bytevector-like-set! cv (+ offset 1) (bytevector-like-ref instr 1))
    (bytevector-like-set! cv (+ offset 2) (bytevector-like-ref instr 2))
    (bytevector-like-set! cv (+ offset 3) (bytevector-like-ref instr 3)))

  ; Set a breakpoint at the beginning of the given procedure.

  (define (%break-entry proc)
    (call-without-interrupts
     (lambda ()
       (if (not (procedure? proc))
	   (error "Break-entry: Not a procedure:" proc)
	   (let ((probe (assv proc bp-list)))
	     (if probe
		 (error "Break-entry: already set for this procedure.")
		 (let* ((cv (procedure-ref proc 0))
			(i  (get-instr cv 0)))
		   (set-instr! cv 0 break-instruction)
		   (set! bp-list (cons (list proc 0 i) bp-list))
		   #t)))))))

  (define (%unbreak-entry proc)
    (call-without-interrupts
     (lambda ()
       (let ((probe (assv proc bp-list)))
	 (if (not probe)
	     (error "Unbreak-entry: not set for this procedure.")
	     (let ((cv (procedure-ref proc 0)))
	       (set-instr! cv 0 (cadr probe))
	       (remq probe bp-list)
	       #t))))))

  (define (%exception-handler k code)
    (display "Larceny exception ")
    (display (number->string code))
    (display ": ")
    (display (exception-string code))
    (newline)
    (exit))

  (set! break-entry %break-entry)
  (set! unbreak-entry %unbreak-entry)
  (set! sparc-exception-handler %exception-handler)
  "Lowlevel debugger")
