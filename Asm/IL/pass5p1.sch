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

; The main entry point.

;; assemble : (listof mal-instr) [user-data] -> ??
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

;; assemble-nested-lambda : 
;;     assembler (listof mal-instr) 'Doc
;;     (assembler Segment -> 'a) [user-data] -> void
(define (assemble-nested-lambda as source doc end . rest)
  (let* ((user (if (null? rest) #f (car rest)))
	 (nested-as (make-assembly-structure source (as-table as) user)))
    (as-parent! nested-as as)
    (as-nested! as (cons (lambda ()
                           (assemble1 nested-as 
                                      (lambda (nested-as)
                                        (let ((segment
                                               (assemble-pasteup nested-as)))
                                          (assemble-finalize! nested-as)
                                          (end nested-as segment)))
                                      doc))
			 (as-nested as)))))

(define operand0 car)      ; the mnemonic
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)
(define (operand4 i) (car (cddddr i)))
(define (operand5 i) (cadr (cddddr i)))

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

(define (emit-doc-datum as x)
  (emit-constant as (list 'doc x)))

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

; Labels.

; The current value of the location counter.

(define (here as) (as-lc as))

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

(define (asm-error msg . rest)
  (error 'assembler "~a" (list msg rest)))
(define (disasm-error msg . rest)
  (error 'disassembler "~a" (list msg rest)))

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

(vector-struct $$assembler
               make-assembler assembler?
               (as-table as-table!)
               (as-source as-source!)
               (as-lc as-lc!)
               (as-code as-code!)
               (as-constants as-constants!)
               (as-labels as-labels!)
               (as-fixups as-fixups!)
               (as-nested as-nested!)
               (as-values as-values!)
               (as-parent as-parent!)
               (as-retry as-retry!)
               (as-user as-user!))

(define (make-assembly-structure source table user-data)
  (make-assembler table source 0 '() '() '() '() '() '() #f #f user-data))

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

; The guts of the assembler.

;; assemble1 : assembler (assembler -> 'answer) 'doc -> 'answer
;; Peephole optimization, single-stepping, and retry have been deleted
;; from this version. See Asm/Common/pass5p1 for old/full version.
(define (assemble1 as finalize doc)
  (let ((assembly-table (as-table as))
        (peep? (peephole-optimization))
        (end-instr (list $.end)))

    (define (assemble-source-item source-item)
      ((vector-ref assembly-table (car source-item))
       source-item
       as))

    (emit-datum as doc)
    (let loop ()
      (let ((source (as-source as)))
        (if (null? source)
            (begin (assemble-source-item end-instr)
                   (finalize as))
            (begin
              (if peep?
                  (let peeploop ((src1 source))
                    (peep as)
                    (let ((src2 (as-source as)))
                      (if (not (eq? src1 src2))
                          (peeploop src2)))))
              (let ((source (as-source as)))
                (as-source! as (cdr source))
                (assemble-source-item (car source))
                (loop))))))))

;; assemble-pasteup : assembler -> (cons code constants)
;; Forces all il:delay ilpackages in the instruction stream
(define (assemble-pasteup as)
  (let* ((code (as-code as))
         (instrs (cvclass-instrs code)))
    (let loop ((instrs instrs) (processed-instrs '()))
      (cond ((null? instrs)
             (cons (make-cvclass (cvclass-il-namespace code)
                                 (cvclass-id code) 
                                 (reverse processed-instrs)
                                 (cvclass-constants code))
                   (list->vector (as-constants as))))
            ((string? (car instrs))
             (loop (cdr instrs) (cons (car instrs) processed-instrs)))
            ((il? (car instrs)) ;; Is it a normal IL instr?
             (loop (cdr instrs) (cons (car instrs) processed-instrs)))
            ((il-delay? (car instrs))
             (let ((forced (il-delay-force (car instrs))))
               (loop (cdr instrs) (cons forced processed-instrs))))
            (else (error 'something-else-in-assembler-code))))))

;; assemble-finalize! : assembler -> void
(define (assemble-finalize! as)
  (for-each (lambda (p) (p)) (as-nested as))
  (as-nested! as '()))

;; assembly-start : assembler -> ??
;; Initializes the assembler?
(define (assembly-start as)
  (let ((u (as-user as)))
    (user-data.proc-counter! u 0)
    (user-data.toplevel-counter! u (+ 1 (user-data.toplevel-counter u))))
  (let ((e (new-proc-id as)))
    (as-source! as (cons (list $.entry e #t) (as-source as)))))

;; assembly-end : assembler ?? -> (list code constants codevector-ids???)
(define (assembly-end as segment)
  (list (car segment) (cdr segment) (lookup-functions as)))

;; assembly-user-data : -> user-data
(define (assembly-user-data)
  (make-user-data))
