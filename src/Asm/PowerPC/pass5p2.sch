; Copyright 1991 Lightship Software, Incorporated.
;
; $Id$
;
; PowerPC machine assembler.
; Lars T Hansen

; Overrides the procedures of the same name in Asm/Common/pass5p1.sch.

(define (assembly-table) $ppc-assembly-table$)

(define (assembly-start as)
  (let ((u (as-user as)))
    (user-data.proc-counter! u 0)
    (user-data.toplevel-counter! u (+ 1 (user-data.toplevel-counter u))))
  (let ((e (new-proc-id as)))
    (as-source! as (cons (list $.entry e #t) (as-source as)))))

(define (assembly-end as segment)
  (list (car segment) (cdr segment) (lookup-functions as)))

(define (assembly-user-data)
  (make-user-data))

(define (assembly-declarations user-data)
  '())

; User-data structure has three fields:
;  toplevel-counter     Different for each compiled segment
;  proc-counter         A serial number for labels
;  seen-labels          A list of labels at lower addresses

(define (make-user-data) (list 0 0 '()))

(define (user-data.toplevel-counter u) (car u))
(define (user-data.proc-counter u) (cadr u))
(define (user-data.labels u) (caddr u))

(define (user-data.toplevel-counter! u x) (set-car! u x))
(define (user-data.proc-counter! u x) (set-car! (cdr u) x))
(define (user-data.labels! u x) (set-car! (cddr u) x))


; Assembly listing.

(define listify? #f)

(define $ppc-assembly-table$
  (make-vector
   *number-of-mnemonics*
   (lambda (instruction as)
     (asm-error "Unrecognized mnemonic " instruction))))

(define (define-instruction i proc)
  (vector-set! $ppc-assembly-table$ i proc)
  #t)

(define (list-instruction name instruction)
  (if listify?
      (begin (display list-indentation)
             (display "        ")
             (display name)
             (display (make-string (max (- 12 (string-length name)) 1)
                                   #\space))
             (if (not (null? (cdr instruction)))
                 (begin (write (cadr instruction))
                        (do ((operands (cddr instruction)
                                       (cdr operands)))
                            ((null? operands))
                            (write-char #\,)
                            (write (car operands)))))
             (newline)
	     (flush-output-port))))

(define (list-label instruction)
  (if listify?
      (begin (display list-indentation)
             (write-char #\L)
             (write (cadr instruction))
             (newline))))

(define (list-lambda-start instruction)
  (list-instruction "lambda" (list $lambda '* (operand2 instruction)))
  (set! list-indentation (string-append list-indentation "|   ")))

(define (list-lambda-end)
  (set! list-indentation
        (substring list-indentation
                   0
                   (- (string-length list-indentation) 4))))

(define list-indentation "")


; The BITS procedure takes a string template and returns a procedure
; that takes an output port and some values to fill into that
; template.  When the procedure is called, text is emitted to the
; output port with the values in the correct place.
;
; The template can contain anything, but some characters control
; insertion of values at preprocessing time or when the resulting
; text is called:
;    @( ... ) is an expression to be evaluated
;    @ident   is an identifier to be evaluated; ident is a-z A-Z 0-9 _
;    ~a       denotes an argument to be inserted during code generation
;    .        following newline is ignored, as are all characters preceding it
;
; FIXME: would be better to use ~0 through ~9 so that we do not have
; to repeat arguments!  Or bits could even take a list of named arguments
; as the first parameter, and the macro would reference these.
;
; IDEA: allow &Ln to reference a local label n and &Ln: to define it.

(define (bits s)

  (define (read-symbol in)
    (let loop ((cs '()))
      (let ((c (peek-char in)))
	(cond ((and (char? c)
		    (or (char-alphabetic? c)
			(char-numeric? c)
			(char=? c #\_)))
	       (loop (cons (char-downcase (read-char in)) cs)))
	      (else
	       (string->symbol (list->string (reverse cs))))))))

  (let ((in (open-input-string s))
	(out (open-output-string)))
    (let loop ((slots 0) (templates '()))
      (let ((c (read-char in)))
	(cond ((eof-object? c)
	       (bits-template (reverse (cons (get-output-string out) 
					     templates))
			      slots))
	      ((char=? c #\@)
	       (display (eval (if (eqv? (peek-char in) #\() 
				  (read in) 
				  (read-symbol in))) 
			out)
	       (loop slots templates))
	      ((char=? c #\~)
	       (let ((c (read-char in)))
		 (if (not (eqv? c #\a))
		     (error "malformed format specifier in bits: " c)
		     (let ((t (get-output-string out)))
		       (reset-output-string out)
		       (loop (+ slots 1) (cons t templates))))))
	      ((char=? c #\newline)
	       (display c out)
	       (let loop2 ()
		 (let ((c (read-char in)))
		   (cond ((or (eof-object? c) (char=? c #\.))
			  (loop slots templates))
			 ((char=? c #\newline)
			  (newline out)
			  (loop2))
			 (else
			  (loop2))))))
	      (else
	       (display c out)
	       (loop slots templates)))))))

(define (bits-template templates slots)
  (case slots
    ((0) (lambda (as) 
	   (let ((out (as-output as)))
	     (display (car templates) out)
	     (newline out))))
    ((1) (lambda (as x1)
	   (let ((out (as-output as)))
	     (display (car templates) out)
	     (display x1 out)
	     (display (cadr templates) out)
	     (newline out))))
    ((2) (lambda (as x1 x2)
	   (let ((out (as-output as)))
	     (display (car templates) out)
	     (display x1 out)
	     (display (cadr templates) out)
	     (display x2 out)
	     (display (caddr templates) out)
	     (newline out))))
    ((3) (lambda (as x1 x2 x3)
	   (let ((out (as-output as)))
	     (display (car templates) out)
	     (display x1 out)
	     (display (cadr templates) out)
	     (display x2 out)
	     (display (caddr templates) out)
	     (display x3 out)
	     (display (cadddr templates) out)
	     (newline out))))
    ((4) (lambda (as x1 x2 x3 x4)
	   (let ((out (as-output as)))
	     (display (car templates) out)
	     (display x1 out)
	     (display (cadr templates) out)
	     (display x2 out)
	     (display (caddr templates) out)
	     (display x3 out)
	     (display (cadddr templates) out)
	     (display x4 out)
	     (display (car (cddddr templates)) out)
	     (newline out))))
    (else
     (lambda (as . xs)
       (let ((out (as-output as)))
	 (display (car templates) out)
	 (let loop ((xs xs) (ts (cdr templates)))
	   (if (not (null? ts))
	       (begin
		 (display (car xs) out)
		 (display (car ts) out)
		 (loop (cdr xs) (cdr ts)))
	       (newline out))))))))


; PowerPC machine description

; Registers

(define TEMPX "r0")
(define TEMP "r2")
(define SECOND "r3")
(define TIMER "r4")
(define RESULT "r5")
(define GLOBALS "r6")
(define CONT "r7")
(define HEAP "r8")
(define REG0 "r9")
(define REG1 "r10")

; Tags and typetags

(define PAIR_TAG 1)
(define PROC_TAG 7)
(define VEC_TAG 3)

; immediates

(define UNDEFINED_CONST 329)

; 
(define PROC_CONSTVECTOR 8)


(define M_GLOBAL_EX 64)

(define-instruction $.align
  (lambda (instruction as)
    (list-instruction ".align" instruction)))

(define-instruction $.cont
  (lambda (instruction as)
    (list-instruction ".cont" instruction)))

(define-instruction $.end
  (lambda (instruction as)
    (list-instruction ".end" instruction)))

(define-instruction $.entry
  (lambda (instruction as)
    (list-instruction ".entry" instruction)
    (begin-compiled-scheme-function as (operand1 instruction)
				    (operand2 instruction)
				    #t)))

; Given a constant vector index, return a value s.t. the value at
; that index is loaded if a tagged vector pointer is dereferenced 
; with the value.

(define (constant-vector-offset slot)
  (+ (- VEC_TAG) (* 4 (+ slot 1))))

(define (rib-offset slot)
  (+ (- PROC_TAG) PROC_REG0 (* 4 slot)))

(define-instruction $const
  (let ((imm 
	 (bits "    addi   @RESULT, r0, ~a"))
	(cv
	 (bits "    lwz    @RESULT, @(- PROC_CONSTVECTOR PROC_TAG)(@REG0)
	       .    lwz    @RESULT, ~a(@RESULT)")))
    (lambda (instruction as)
      (list-instruction "const" instruction)
      (if (immediate-constant? (operand1 instruction))
	  (imm as (constant-value (operand1 instruction)))
	  (cv  as (constant-vector-offset 
		   (emit-datum as (operand1 instruction))))))))

(define-instruction $global
  (let ((bits1 
	 (bits "    lwz    @TEMP, @(- PROC_CONSTVECTOR PROC_TAG)(@REG0)
	       .    lwz    @TEMP,  ~a(@TEMP)
	       .    lwz    @RESULT, @(- PAIR_TAG)(@TEMP)"))
	(bits2 
	 (bits "    cmpi   0, 0, @RESULT, @UNDEFINED_CONST
	       .    bc     5, 2, ~a
	       .    or     @RESULT, @TEMP, @TEMP
	       .~a: addi   @TEMPX, @GLOBALS, @M_GLOBAL_EX
	       .    bl     ~a")))
    (lambda (instruction as)
      (bits1 as (constant-vector-offset (operand1 instruction)))
      (if (check-undefined-globals)
	  (let ((L0 (make-label as)))
	    (bits2 as L0 L0 (as-millicode-label as)))))))

(define-instruction $setglbl
  (let ((bits1
	 (bits "    or     @SECOND, @RESULT, @RESULT
	       .    lwz    @RESULT, @(- PROC_CONSTVECTOR PROC_TAG)(@REG0)
	       .    lwz    @RESULT, ~a(@RESULT)
               .    stw    @SECOND, @(- PAIR_TAG)(@RESULT)
               .    addi   @TEMPX, @GLOBALS, @M_FULL_BARRIER
               .    bl     ~a")))
    (lambda (instruction as)
      (list-instruction "setglbl" instruction)
      (bits1 as 
	     (constant-vector-offset (operand1 instruction)) 
	     (as-millicode-label as)))))

(define-instruction $lexical
  (let ((rib0
	 (bits "    lwz    @RESULT, ~a(@REG0)"))
	(firstrib
	 (bits "    lwz    @TEMP, @(- PROC_REG0 PROC_TAG)(@REG0)"))
	(nextrib
	 (bits "    lwz    @TEMP, @(- PROC_REG0 PROC_TAG)(@TEMP)"))
	(lastrib
	 (bits "    lwz    @RESULT, ~a(@TEMP)")))
    (lambda (instruction as)
      (list-instruction "lexical" instruction)
      (cond ((zero? (operand1 instruction))
	     (rib0 as (rib-offset (operand2 instruction))))
	    (else
	     (firstrib as)
	     (do ((i (operand1 instruction) (- i 1)))
		 ((= i 0)
		  (lastrib as (rib-offset (operand2 instruction))))
	       (nextrib as (operand1 instruction))))))))

(define-instruction $reg
  (let ((hwreg
	 (bits "    or      @RESULT, ~a, ~a"))
	(swreg
	 (bits "    lwz     @RESULT, ~a(@GLOBALS)")))
    (lambda (instruction as)
      (list-instruction "reg" instruction)
      (if (hardware-register? (operand1 instruction))
	  (let ((r (register-name (operand1 instruction))))
	    (hwreg as r r))
	  (swreg as (register-offset (operand1 instruction)))))))

(define-instruction $movereg
  (let ((hwreg->hwreg
	 (bits "    or      ~a, ~a, ~a"))
	(swreg->hwreg
	 (bits "    lwz     ~a, ~a(@GLOBALS)"))
	(hwreg->swreg
	 (bits "    stw     ~a, ~a(@GLOBALS)"))
	(swreg->swreg
	 (bits "    lwz     @TEMPX, ~a(@GLOBALS)
	       .    stw     @TEMPX, ~a(@GLOBALS)")))
    (lambda (instruction as)
      (list-instruction "reg" instruction)
      (if (hardware-register? (operand1 instruction))
	  (let ((rS (register-name (operand1 instruction))))
	    (if (hardware-register? (operand2 instruction))
		(let ((rD (register-name (operand2 instruction))))
		  (hwreg->hwreg as rD rS rS))
		(let ((rD (register-offset (operand2 instruction))))
		  (hwreg->swreg as rS rD))))
	  (let ((rS (register-offset (operand1 instruction))))
	    (if (hardware-register? (operand2 instruction))
		(let ((rD (register-name (operand2 instruction))))
		  (swreg->hwreg as rD rS))
		(let ((rD (register-offset (operand2 instruction))))
		  (swreg->swreg as rS rD))))))))

(define-primitive 61  ; op2 +
  (let ((bits1
	 (bits "    or      @TEMP, @RESULT, ~a
	       .    andi.   @TEMPX, @TEMP, 3
	       .    addo    @RESULT, @RESULT, ~a
	       .    mcrxr   1
	       .    crandc  2, 2, 5
	       .    bc      13, 2, ~a
	       .    FIXME
               .~a:")))
    (lambda (instruction as)
      (let ((r (register-name (operand2 instruction)))
	    (L (make-label as)))
	(bits1 as r r L L)))))

(define-primitive 131 ; op2imm -
  (let ((bits1
	 (bits "    andi.   @TEMPX, @RESULT, 3
	       .    addo    @RESULT, @RESULT, ~a
	       .    mcrxr   1
	       .    crandc  2, 2, 5
	       .    bc      13, 2, ~a
	       .    FIXME
               .~a:")))
    (lambda (instruction as)
      (let ((L (make-label as)))
	(bits1 as (- (native->fixnum (operand2 instruction))) L L)))))

(define-primitive 136 ; op2imm <
  (let ((bits1
	 (bits "    andi.   @TEMPX, @RESULT, 3
               .    bc      5, 2, ~a
               .    FIXME
               .~a: cmpi    @RESULT, @RESULT, ~a
	       .    addi    @RESULT, r0, @TRUE_CONST
	       .    bc      4, FIXME, ~a
	       .    addi    @RESULT, r0, @FALSE_CONST
               .~a:")))
    (lambda (instruction as)
      (let ((L1 (make-label as))
	    (L2 (make-label as)))
	(bits1 as L1 L1 (native->fixnum (operand2 instruction)) L2 L2)))))
