; $Id$

;; ------------------------------
;; Listify, .list file generation
;; ------------------------------

(define (list-instruction/line name instr as)
  (let ((line listify-counter))
    (emit as
          (if (codegen-option 'listify-line-directive)
              (il:directive 'line line listify-filename))
          (il:comment/info "instruction" (cons (string->symbol name) (cdr instr)))
          (if (and (codegen-option 'listify-debug-location)
                   (not (member name '(".end"))))
              (il:set-debug-info line listify-filename 
                                 (member name '(".cont" ".proc")))
              '()))
    (when (codegen-option 'listify-write-list-file)
      (write-listify-line line name instr))))

(define (list-label/line instr as)
  (let ((line listify-counter))
    (emit as 
          (il:comment/info "Instruction" (cons '.label (cdr instr))))
    (when (codegen-option 'listify-write-list-file)
      (write-listify-label line (cadr instr)))))

;; list-entry/line : symbol instruction assembler -> number
(define (list-entry/line name instruction as)
  (when (codegen-option 'listify-write-list-file)
    (listify-newline)
    (twobit-format listify-oport "Procedure ~s"
                   (assembler-value as 'current-codevector))
    (listify-newline))
  (list-instruction/line name instruction as))

(define (il:set-debug-info line filename set-filename?)
  (il:comment/wrap "set-debug-info"
                   (il 'ldc.i4 line)
                   (il:stsfld iltype-int32 il-reg "debugLocation")
                   (if set-filename?
                       (list (il:ldstr listify-filename)
                             (il:stsfld iltype-string il-reg "debugFile"))
                       '())))

(define (write-listify-line line name instruction)
  (display list-indentation listify-oport)
  (display "        " listify-oport)
  (display name listify-oport)
  (display (make-string (max (- 12 (string-length name)) 1)
                        #\space)
           listify-oport)
  (if (not (null? (cdr instruction)))
      (begin (write (cadr instruction) listify-oport)
             (do ((operands (cddr instruction)
                            (cdr operands)))
               ((null? operands))
               (write-char #\, listify-oport)
               (write (car operands) listify-oport))))
  (listify-newline))

(define (write-listify-label line label)
  (display list-indentation listify-oport)
  (write-char #\L listify-oport)
  (write label listify-oport)
  (listify-newline))

(define list-indentation "")
(define listify-oport #f)
(define listify-filename #f)
(define listify-counter 0)

(define (listify-reset)
  (set! list-indentation "")
  (set! listify-oport #f)
  (set! listify-filename #f)
  (set! listify-counter 1))

(define (listify-newline)
  (newline listify-oport)
  (set! listify-counter (+ 1 listify-counter)))

