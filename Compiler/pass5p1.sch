; Copyright 1991 Lightship Software, Incorporated.
;
; Fifth pass of the Twobit compiler:
;   assembly.
;
; April 28, 1996
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
; The output is a pair consisting of machine code (a bytevector)
; and a constant vector.
;
; This assembler is table-driven, and may be customized to emit
; machine code for different target machines.  The table consists
; of a vector of procedures indexed by mnemonics.  Each procedure
; in the table should take two arguments: an assembly structure
; and a source instruction.  The procedure should just assemble
; the instruction using the operations defined below.
;
; The table can be changed by redefining the following procedure.

;(define (assembly-table) $bytecode-assembly-table$)
(define (assembly-table) $sparc-assembly-table$)

; The main entry point.
; The assembly table could be a parameter to this procedure.

(define (assemble source)
  (assemble1 (make-assembly-structure source (assembly-table))
             (lambda (as)
               (let ((segment (assemble-pasteup as)))
                 (assemble-finalize! as)
                 segment))))

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

(define (assemble-nested-lambda as source k)
  (let ((nested-as (make-assembly-structure source (as-table as))))
    (as-parent! nested-as as)
    (as-nested! as (cons (lambda ()
			   (assemble1 nested-as 
				      (lambda (nested-as)
					(let ((segment
					       (assemble-pasteup nested-as)))
					  (assemble-finalize! nested-as)
					  (k segment)))))
			 (as-nested as)))))

(define operand0 car)      ; the mnemonic
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

; Emits the bits contained in the bytevector bv.

(define (emit-instr! as bv)
  (as-code! as (cons bv (as-code as)))
  (as-lc! as (+ (as-lc as) (bytevector-length bv))))

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
  (as-labels! as
              (cons (cons L (as-lc as))
                    (as-labels as))))

; Adds the integer n to the size code bytes beginning at the
; given byte offset from the current value of the location counter.

(define (emit-fixup! as offset size n)
  (as-fixups! as
              (cons (list (+ offset (as-lc as))
                          size
                          n)
                    (as-fixups as))))

; Adds the value of the label L to the size code bytes beginning
; at the given byte offset from the current location counter.

(define (emit-fixup-label! as offset size L)
  (as-fixups! as
              (cons (list (+ offset (as-lc as))
                          size
                          (list L))
                    (as-fixups as))))

; Allows the procedure proc of two arguments (code vector and current
; location counter) to modify the code vector at will, at fixup time.

(define (emit-fixup-proc! as proc)
  (as-fixups! as
	      (cons (list (as-lc as)
			  0
			  proc)
		    (as-fixups as))))

; The current value of the location counter.

(define (here as) (as-lc as))

; Label lookup

(define (label-value as L)

  (define (lookup-label-loop x labels parent)
    (let ((entry (assq x labels)))
      (cond (entry (cdr entry))
	    ((not parent) #f)
	    (else 
	     (lookup-label-loop x (as-labels parent) (as-parent parent))))))
    
  (lookup-label-loop L (as-labels as) (as-parent as)))
    

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

; This is called when a value is too large to be handled by the assembler.
; Info is a string, expr an assembler expression, and val the resulting
; value.  The default behaviour is to signal an error.

(define (value-too-large as info expr val)
  (asm-error info ": Value too large: " expr " = " val))

; The implementation of asm-error depends on the host system. Sigh.

(define (asm-error msg . rest)
  (cond ((eq? host-system 'chez)
	 (error 'assembler "~a" (list msg rest)))
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
;
; In fixups, labels are of the form (<L>) to distinguish them from fixnums.

(define label? pair?)
(define label.value car)

(define (make-assembly-structure source table)
  (vector table
          source
          0
          '()
          '()
          '()
          '()
          '()
	  '()
	  #f))

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

(define (as-source! as x)    (vector-set! as 1 x))
(define (as-lc! as x)        (vector-set! as 2 x))
(define (as-code! as x)      (vector-set! as 3 x))
(define (as-constants! as x) (vector-set! as 4 x))
(define (as-labels! as x)    (vector-set! as 5 x))
(define (as-fixups! as x)    (vector-set! as 6 x))
(define (as-nested! as x)    (vector-set! as 7 x))
(define (as-values! as x)    (vector-set! as 8 x))
(define (as-parent! as x)    (vector-set! as 9 x))

; The guts of the assembler.

; FIXME: insert calls to peephole optimizer.

(define (assemble1 as finalize)
  (let ((assembly-table (as-table as)))
    (define (loop)
      (let ((source (as-source as)))
        (if (null? source)
            (finalize as)
            (begin (as-source! as (cdr source))
                   ((vector-ref assembly-table (caar source))
                    (car source)
                    as)
                   (loop)))))
    (emit-datum as #f)  ; documentation slot is always first; #f for now.
    (loop)))

(define (assemble-pasteup as)
  (let ((code      (make-bytevector (as-lc as)))
        (constants (list->vector (as-constants as))))
    
    ; The bytevectors: byte 0 is least significant.

    (define (paste-code! bvs i)
      (if (not (null? bvs))
          (let* ((bv (car bvs))
                 (n  (bytevector-length bv)))
            (do ((i i (- i 1))
                 (j 0 (+ j 1)))
                ((= j n)
                 (paste-code! (cdr bvs) i))
                (bytevector-set! code i (bytevector-ref bv j))))))
    
    (paste-code! (as-code as) (- (as-lc as) 1))
    (as-code! as (list code))
    (cons code constants)))

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
      (or (label-value as (label.value L))
	  (asm-error "Assembler error -- undefined label " L)))

    (apply-fixups! (as-fixups as))
    
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
            (write-char (string-ref (number->string (quotient byte 16) 16) 0))
            (write-char (string-ref (number->string (remainder byte 16) 16) 0))))))
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

(define (compile&assemble x)
  (view-segment (assemble (compile x))))

; eof