; Copyright 1991 Lightship Software, Incorporated.
; 
; $Id$
;
; Target-independent part of the assembler.
;
; This is a simple, table-driven, one-pass assembler.
; Part of it assumes a big-endian target machine.
;
; The input to this pass is a list of symbolic MacScheme machine
; instructions and pseudo-instructions.  Each symbolic MacScheme 
; machine instruction or pseudo-instruction is a list whose car
; is a small non-negative fixnum that acts as the mnemonic for the
; instruction.  The rest of the list is interpreted as indicated
; by the mnemonic.
;
; The output is a pair consisting of machine code (a bytevector or 
; string) and a constant vector.
;
; This assembler is table-driven, and may be customized to emit
; machine code for different target machines.  The table consists
; of a vector of procedures indexed by mnemonics.  Each procedure
; in the table should take two arguments: an assembly structure
; and a source instruction.  The procedure should just assemble
; the instruction using the operations defined below.
;
; The table and target can be changed by redefining the following 
; five procedures.

(define (assembly-table) (error "No assembly table defined."))
(define (assembly-start as) #t)
(define (assembly-end as segment) segment)
(define (assembly-user-data) #f)

; The main entry point.

(define (assemble source . rest)
  (let* ((user (if (null? rest) (assembly-user-data) (car rest)))
	 (as   (make-assembly-structure source (assembly-table) user)))
    (assembly-start as)
    (assemble1 as
	       (lambda (as)
		 (let ((segment (assemble-pasteup as)))
		   (assemble-finalize! as)
		   (assembly-end as segment)))
	       #f)))

; The following procedures are to be called by table routines.
;
; The assembly source for nested lambda expressions should be
; assembled by calling this procedure.  This allows an inner
; lambda to refer to labels defined by outer lambdas.
;
; We delay the assembly of the nested lambda until after the outer lambda
; has been finalized so that all labels in the outer lambda are known
; to the inner lambda.
;
; The continuation procedure k is called to backpatch the constant
; vector of the outer lambda after the inner lambda has been
; finalized.  This is necessary because of the delayed evaluation: the
; outer lambda holds code and constants for the inner lambda in its
; constant vector.

(define (assemble-nested-lambda as source doc k . rest)
  (let* ((user (if (null? rest) #f (car rest)))
	 (nested-as (make-assembly-structure source (as-table as) user)))
    (as-parent! nested-as as)
    (as-nested! as (cons (lambda ()
			   (assemble1 nested-as 
				      (lambda (nested-as)
					(let ((segment
					       (assemble-pasteup nested-as)))
					  (assemble-finalize! nested-as)
					  (k nested-as segment)))
				      doc))
			 (as-nested as)))))

(define operand0 car)      ; the mnemonic
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)
(define (operand4 i) (car (cddddr i)))
(define (operand5 i) (cadr (cddddr i)))

; Emits the bits contained in the bytevector bv.

(define (emit! as bv)
  (as-code! as (cons bv (as-code as)))
  (as-lc! as (+ (as-lc as) (bytevector-length bv))))

; Emits the characters contained in the string s as code (for C generation).

(define (emit-string! as s)
  (as-code! as (cons s (as-code as)))
  (as-lc! as (+ (as-lc as) (string-length s))))

; Given any Scheme object that may legally be quoted, returns an
; index into the constant vector for that constant.

(define (emit-constant as x)
  (do ((i 0 (+ i 1))
       (y (as-constants as) (cdr y)))
      ((or (null? y) (equal? x (car y)))
       (if (null? y)
	   (as-constants! as (append! (as-constants as) (list x))))
       i)))

(define (emit-datum as x)
  (emit-constant as (list 'data x)))

(define (emit-global as x)
  (emit-constant as (list 'global x)))

(define (emit-codevector as x)
  (emit-constants as (list 'codevector x)))

(define (emit-constantvector as x)
  (emit-constants as (list 'constantvector x)))

; Set-constant changes the datum stored, without affecting the tag.
; It can operate on the list form because the pair stored in the list
; is shared between the list and any vector created from the list.

(define (set-constant! as n datum)
  (let ((pair (list-ref (as-constants as) n)))
    (set-car! (cdr pair) datum)))

; Guarantees that the constants will not share structure
; with any others, and will occupy consecutive positions
; in the constant vector.  Returns the index of the first
; constant.

(define (emit-constants as x . rest)
  (let* ((constants (as-constants as))
         (i         (length constants)))
    (as-constants! as (append! constants (cons x rest)))
    i))

; Defines the given label using the current location counter.

(define (emit-label! as L)
  (set-cdr! L (as-lc as)))

; Adds the integer n to the size code bytes beginning at the
; given byte offset from the current value of the location counter.

(define (emit-fixup! as offset size n)
  (as-fixups! as (cons (list (+ offset (as-lc as)) size n)
		       (as-fixups as))))

; Adds the value of the label L to the size code bytes beginning
; at the given byte offset from the current location counter.

(define (emit-fixup-label! as offset size L)
  (as-fixups! as (cons (list (+ offset (as-lc as)) size (list L))
		       (as-fixups as))))

; Allows the procedure proc of two arguments (code vector and current
; location counter) to modify the code vector at will, at fixup time.

(define (emit-fixup-proc! as proc)
  (as-fixups! as (cons (list (as-lc as) 0 proc)
		       (as-fixups as))))

; Labels.

; The current value of the location counter.

(define (here as) (as-lc as))

; Given a MAL label (a number), create an assembler label.

(define (make-asm-label as label)
  (let ((probe (find-label as label)))
    (if probe
	probe
	(let ((l (cons label #f)))
	  (as-labels! as (cons l (as-labels as)))
	  l))))

; This can use hashed lookup.

(define (find-label as L)

  (define (lookup-label-loop x labels parent)
    (let ((entry (assq x labels)))
      (cond (entry)
	    ((not parent) #f)
	    (else 
	     (lookup-label-loop x (as-labels parent) (as-parent parent))))))
    
  (lookup-label-loop L (as-labels as) (as-parent as)))

; Create a new assembler label, distinguishable from a MAL label.

(define new-label
  (let ((n 0))
    (lambda ()
      (set! n (- n 1))
      (cons n #f))))

; Given a value name (a number), return the label value or #f.

(define (label-value as L) (cdr L))

; For peephole optimization.

(define (next-instruction as)
  (let ((source (as-source as)))
    (if (null? source)
        '(-1)
        (car source))))

(define (consume-next-instruction! as)
  (as-source! as (cdr (as-source as))))

(define (push-instruction as instruction)
  (as-source! as (cons instruction (as-source as))))

; For use by the machine assembler: assoc lists connected to as structure.

(define (assembler-value as key)
  (let ((probe (assq key (as-values as))))
    (if probe
	(cdr probe)
	#f)))

(define (assembler-value! as key value)
  (let ((probe (assq key (as-values as))))
    (if probe
	(set-cdr! probe value)
	(as-values! as (cons (cons key value) (as-values as))))))

; For documentation.
;
; The value must be a documentation structure (a vector).

(define (add-documentation as doc)
  (let* ((existing-constants (cadr (car (as-constants as))))
	 (new-constants 
	  (twobit-sort (lambda (a b)
			 (< (car a) (car b)))
		       (cond ((not existing-constants)
			      (list (cons (here as) doc)))
			     ((pair? existing-constants)
			      (cons (cons (here as) doc)
				    existing-constants))
			     (else
			      (list (cons (here as) doc)
				    (cons 0 existing-constants)))))))
    (set-car! (cdar (as-constants as)) new-constants)))

; This is called when a value is too large to be handled by the assembler.
; Info is a string, expr an assembler expression, and val the resulting
; value.  The default behavior is to signal an error.

(define (asm-value-too-large as info expr val)
  (if (as-retry as)
      ((as-retry as))
      (asm-error info ": Value too large: " expr " = " val)))

; The implementations of asm-error and disasm-error depend on the host
; system. Sigh.

(define (asm-error msg . rest)
  (cond ((eq? host-system 'chez)
	 (error 'assembler "~a" (list msg rest)))
	(else
	 (apply error msg rest))))

(define (disasm-error msg . rest)
  (cond ((eq? host-system 'chez)
	 (error 'disassembler "~a" (list msg rest)))
	(else
	 (apply error msg rest))))

; The remaining procedures in this file are local to the assembler.

; An assembly structure is a vector consisting of
;
;    table          (a table of assembly routines)
;    source         (a list of symbolic instructions)
;    lc             (location counter; an integer)
;    code           (a list of bytevectors)
;    constants      (a list)
;    labels         (an alist of labels and values)
;    fixups         (an alist of locations, sizes, and labels or fixnums)
;    nested         (a list of assembly procedures for nested lambdas)
;    values         (an assoc list)
;    parent         (an assembly structure or #f)
;    retry          (a thunk or #f)
;    user-data      (anything)
;
; In fixups, labels are of the form (<L>) to distinguish them from fixnums.

(define (label? x) (and (pair? x) (fixnum? (car x))))
(define label.ident car)

(define (make-assembly-structure source table user-data)
  (vector table
          source
          0
          '()
          '()
          '()
          '()
          '()
	  '()
	  #f
	  #f
	  user-data))

(define (as-reset! as source)
  (as-source! as source)
  (as-lc! as 0)
  (as-code! as '())
  (as-constants! as '())
  (as-labels! as '())
  (as-fixups! as '())
  (as-nested! as '())
  (as-values! as '())
  (as-retry! as #f))

(define (as-table as)     (vector-ref as 0))
(define (as-source as)    (vector-ref as 1))
(define (as-lc as)        (vector-ref as 2))
(define (as-code as)      (vector-ref as 3))
(define (as-constants as) (vector-ref as 4))
(define (as-labels as)    (vector-ref as 5))
(define (as-fixups as)    (vector-ref as 6))
(define (as-nested as)    (vector-ref as 7))
(define (as-values as)    (vector-ref as 8))
(define (as-parent as)    (vector-ref as 9))
(define (as-retry as)     (vector-ref as 10))
(define (as-user as)      (vector-ref as 11))

(define (as-source! as x)    (vector-set! as 1 x))
(define (as-lc! as x)        (vector-set! as 2 x))
(define (as-code! as x)      (vector-set! as 3 x))
(define (as-constants! as x) (vector-set! as 4 x))
(define (as-labels! as x)    (vector-set! as 5 x))
(define (as-fixups! as x)    (vector-set! as 6 x))
(define (as-nested! as x)    (vector-set! as 7 x))
(define (as-values! as x)    (vector-set! as 8 x))
(define (as-parent! as x)    (vector-set! as 9 x))
(define (as-retry! as x)     (vector-set! as 10 x))
(define (as-user! as x)      (vector-set! as 11 x))

; The guts of the assembler.

(define (assemble1 as finalize doc)
  (let ((assembly-table (as-table as))
	(peep? (peephole-optimization))
	(step? (single-stepping))
	(step-instr (list $.singlestep))
	(end-instr (list $.end)))

    (define (loop)
      (let ((source (as-source as)))
        (if (null? source)
	    (begin ((vector-ref assembly-table $.end) end-instr as)
		   (finalize as))
            (begin (if step?
		       ((vector-ref assembly-table $.singlestep)
			step-instr
			as))
		   (if peep?
		       (let peeploop ((src1 source))
			 (peep as)
			 (let ((src2 (as-source as)))
			   (if (not (eq? src1 src2))
			       (peeploop src2)))))
		   (let ((source (as-source as)))
		     (as-source! as (cdr source))
		     ((vector-ref assembly-table (caar source))
		      (car source)
		      as)
		     (loop))))))

    (define (doit)
      (emit-datum as doc)
      (loop))

    (let* ((source (as-source as))
	   (r (call-with-current-continuation
	       (lambda (k)
		 (as-retry! as (lambda () (k 'retry)))
		 (doit)))))
      (if (eq? r 'retry)
	  (let ((old (short-effective-addresses)))
	    (as-reset! as source)
	    (dynamic-wind
	     (lambda ()
	       (short-effective-addresses #f))
	     doit
	     (lambda ()
	       (short-effective-addresses old))))
	  r))))

(define (assemble-pasteup as)

  (define (pasteup-code)
    (let ((code      (make-bytevector (as-lc as)))
	  (constants (list->vector (as-constants as))))
    
      ; The bytevectors: byte 0 is most significant.

      (define (paste-code! bvs i)
	(if (not (null? bvs))
	    (let* ((bv (car bvs))
		   (n  (bytevector-length bv)))
	      (do ((i i (- i 1))
		   (j (- n 1) (- j 1)))	; (j 0 (+ j 1))
		  ((< j 0)		; (= j n)
		   (paste-code! (cdr bvs) i))
                (bytevector-set! code i (bytevector-ref bv j))))))
    
      (paste-code! (as-code as) (- (as-lc as) 1))
      (as-code! as (list code))
      (cons code constants)))

  (define (pasteup-strings)
    (let ((code      (make-string (as-lc as)))
	  (constants (list->vector (as-constants as))))

      (define (paste-code! strs i)
	(if (not (null? strs))
	    (let* ((s (car strs))
		   (n (string-length s)))
	      (do ((i i (- i 1))
		   (j (- n 1) (- j 1)))	; (j 0 (+ j 1))
		  ((< j 0)		; (= j n)
		   (paste-code! (cdr strs) i))
                (string-set! code i (string-ref s j))))))

      (paste-code! (as-code as) (- (as-lc as) 1))
      (as-code! as (list code))
      (cons code constants)))

  (if (bytevector? (car (as-code as)))
      (pasteup-code)
      (pasteup-strings)))

(define (assemble-finalize! as)
  (let ((code (car (as-code as))))

    (define (apply-fixups! fixups)
      (if (not (null? fixups))
          (let* ((fixup      (car fixups))
                 (i          (car fixup))
                 (size       (cadr fixup))
                 (adjustment (caddr fixup))  ; may be procedure
                 (n          (if (label? adjustment)
				 (lookup-label adjustment)
				 adjustment)))
            (case size
	      ((0) (fixup-proc code i n))
              ((1) (fixup1 code i n))
              ((2) (fixup2 code i n))
              ((3) (fixup3 code i n))
              ((4) (fixup4 code i n))
              (else ???))
            (apply-fixups! (cdr fixups)))))

    (define (lookup-label L)
      (or (label-value as (label.ident L))
	  (asm-error "Assembler error -- undefined label " L)))

    (apply-fixups! (reverse! (as-fixups as)))

    (for-each (lambda (nested-as-proc)
		(nested-as-proc))
	      (as-nested as))))


; These fixup routines assume a big-endian target machine.

(define (fixup1 code i n)
  (bytevector-set! code i (+ n (bytevector-ref code i))))

(define (fixup2 code i n)
  (let* ((x  (+ (* 256 (bytevector-ref code i))
                (bytevector-ref code (+ i 1))))
         (y  (+ x n))
         (y0 (modulo y 256))
         (y1 (modulo (quotient (- y y0) 256) 256)))
    (bytevector-set! code i y1)
    (bytevector-set! code (+ i 1) y0)))

(define (fixup3 code i n)
  (let* ((x  (+ (* 65536 (bytevector-ref code i))
		(* 256 (bytevector-ref code (+ i 1)))
                (bytevector-ref code (+ i 2))))
         (y  (+ x n))
         (y0 (modulo y 256))
         (y1 (modulo (quotient (- y y0) 256) 256))
         (y2 (modulo (quotient (- y (* 256 y1) y0) 256) 256)))
    (bytevector-set! code i y2)
    (bytevector-set! code (+ i 1) y1)
    (bytevector-set! code (+ i 2) y0)))

(define (fixup4 code i n)
  (let* ((x  (+ (* 16777216 (bytevector-ref code i))
		(* 65536 (bytevector-ref code (+ i 1)))
		(* 256 (bytevector-ref code (+ i 2)))
		(bytevector-ref code (+ i 3))))
         (y  (+ x n))
         (y0 (modulo y 256))
         (y1 (modulo (quotient (- y y0) 256) 256))
         (y2 (modulo (quotient (- y (* 256 y1) y0) 256) 256))
         (y3 (modulo (quotient (- y (* 65536 y2)
                                    (* 256 y1)
                                    y0)
                               256)
                     256)))
    (bytevector-set! code i y3)
    (bytevector-set! code (+ i 1) y2)
    (bytevector-set! code (+ i 2) y1)
    (bytevector-set! code (+ i 3) y0)))

(define (fixup-proc code i p)
  (p code i))

; For testing.

(define (view-segment segment)
  (define (display-bytevector bv)
    (let ((n (bytevector-length bv)))
      (do ((i 0 (+ i 1)))
          ((= i n))
          (if (zero? (remainder i 4))
              (write-char #\space))
          (if (zero? (remainder i 8))
              (write-char #\space))
          (if (zero? (remainder i 32))
              (newline))
          (let ((byte (bytevector-ref bv i)))
            (write-char
	     (string-ref (number->string (quotient byte 16) 16) 0))
            (write-char
	     (string-ref (number->string (remainder byte 16) 16) 0))))))
  (if (and (pair? segment)
           (bytevector? (car segment))
           (vector? (cdr segment)))
      (begin (display-bytevector (car segment))
             (newline)
             (write (cdr segment))
             (newline)
             (do ((constants (vector->list (cdr segment))
                             (cdr constants)))
                 ((or (null? constants)
                      (null? (cdr constants))))
                 (if (and (bytevector? (car constants))
                          (vector? (cadr constants)))
                     (view-segment (cons (car constants)
                                         (cadr constants))))))))

; emit is a procedure that takes an as and emits instructions into it.

(define (test-asm emit)
  (let ((as (make-assembly-structure #f #f #f)))
    (emit as)
    (let ((segment (assemble-pasteup as)))
      (assemble-finalize! as)
      (disassemble segment))))

(define (compile&assemble x)
  (view-segment (assemble (compile x))))

; eof
