; Heap dumper overriding code for standard-C back end.

; Segments are lists: (code-vector constant-vector function-info).

(define segment.code car)
(define segment.constants cadr)
(define segment.function-info caddr)

(define fun.name car)
(define fun.definite? cadr)
(define fun.entry? caddr)

(define *init-function-name* "twobit_start")
(define *init-thunk-array-name* "twobit_start_procedures")
(define *temp-file* "HEAPDATA.c")
(define *c-output* #f)
(define *segment-number* 0)
(define *entrypoints* '())

(define (before-all-files heap output-file-name input-file-names)
  (set! *segment-number* 0)
  (set! *entrypoints* '()))

(define (after-all-files heap output-file-name input-file-names)
  (c-link-executable "petit-larceny"
		     (append (map (lambda (x)
				    (rewrite-file-type x ".lop" ".o"))
				  input-file-names)
			     (list (rewrite-file-type *temp-file* ".c" ".o")))
		     '("Rts/libpetit.so")))

(define (before-dump-file h filename)
  (let ((out (open-output-file
	      (rewrite-file-type filename ".lop" ".c"))))
    (set! *c-output* out)
    (format *c-output* "/* Generated from ~a */~%" filename)
    (format *c-output* "#include \"twobit.h\"~%~%")))

(define (after-dump-file h filename)
  (close-output-port *c-output*)
  (let ((c-name (rewrite-file-type filename ".lop" ".c"))
	(o-name (rewrite-file-type filename ".lop" ".o")))
    (c-compile-file c-name o-name)
; Keep around for debugging, for now.
;    (delete-file c-name)
    (set! *c-output* #f)))

; FIXME: The treatment of return values is insufficiently general but
; OK for now.  Can abstract it out later.

(define (dump-segment! h segment . rest)
  (let ((entrypoint
	 (dump-function-prototypes (segment.function-info segment))))
    (dump-codevector! h (segment.code segment))
    (let ((the-consts (dump-constantvector! h (segment.constants segment))))
      (set! *segment-number* (+ *segment-number* 1))
      (if (null? rest)
	  (let ((name (string-append "twobit_thunk_"
				     (number->string *segment-number*))))
	    (format *c-output* 
		    "RTYPE ~a(CONT_PARAMS) {~%  return ~a(CONT_ACTUALS);~%}~%"
		    name entrypoint)
	    (set! *entrypoints* (cons name *entrypoints*))
	    (dump-thunk! h $imm.false the-consts))
	  (begin
	    (format *c-output* 
		    "RTYPE ~a(CONT_PARAMS) {~%  return ~a(CONT_ACTUALS);~%}~%" 
		    *init-function-name* entrypoint)
	    (dump-thunk! h $imm.false the-consts))))))

; Print all the function prototypes and return the name of the unique
; entry point.

(define (dump-function-prototypes funs)
  (do ((funs  funs (cdr funs))
       (entry #f))
      ((null? funs)
       (newline *c-output*)
       entry)
    (let* ((fun (car funs))
	   (name (fun.name fun))
	   (definite? (fun.definite? fun))
	   (entry? (fun.entry? fun)))
      (format *c-output* "static RTYPE ~a( CONT_PARAMS );~%" name)
      (if entry?
	  (set! entry name)))))

(define (dump-codevector! heap cv)
  (format *c-output* "~a~%" cv)
  $imm.false)

(define (dump-startup-procedure! h)
  (let ((r #f))
    (call-with-output-file *temp-file*
      (lambda (out)
	(set! *c-output* out)
	(format *c-output* "/* Generated heap bootstrap code */~%")
	(format *c-output* "#include \"twobit.h\"~%~%")
	(let ((thunks  (dump-list-spine! h (heap.thunks h)))
	      (symbols (dump-list-spine! h (symbol-locations h))))
	  (set! r (dump-segment! h
				 (construct-startup-procedure symbols thunks)
				  #t))
	  (format *c-output* "~%~%/* File init procedures */~%~%")
	  (for-each (lambda (e)
		      (format *c-output* "extern void ~a( CONT_PARAMS );~%" e))
		    (reverse *entrypoints*))
	  (format *c-output* "~%cont_t ~a[] = { ~%" *init-thunk-array-name*)
	  (for-each (lambda (e)
		      (format *c-output* "~a,~%" e))
		    (reverse *entrypoints*))
	  (format *c-output* "};~%~%"))))
    (c-compile-file *temp-file* (rewrite-file-type *temp-file* ".c" ".o"))
; Keep around for debugging, for now.
;  (delete-file *temp-file*)
    r))

; Startup procedure is same as standard except for the patch instruction.

(define init-proc
  `((,$.proc)
    (,$args= 1)
    (,$reg 1)
    (,$setreg 2)
    (,$const (thunks))
    (,$op1 petit-patch-boot-code)	; Petit larceny
    (,$setreg 1)
    (,$.label 0)
    (,$reg 1)
    (,$op1 null?)			; (null? l)
    (,$branchf 2)
    (,$const (symbols))			; dummy list of symbols
    (,$setreg 1)
    (,$global go)
    ;(,$op1 break)
    (,$invoke 2)			; (go <list of symbols> argv)
    (,$.label 2)
    (,$save 2)
    (,$store 0 0)
    (,$store 1 1)
    (,$store 2 2)
    (,$setrtn 3)
    (,$reg 1)
    (,$op1 car)
    (,$invoke 0)			; ((car l))
    (,$.label 3)
    (,$.cont)
    (,$restore 2)
    (,$pop 2)
    (,$reg 1)
    (,$op1 cdr)
    (,$setreg 1)
    (,$branch 0)))			; (loop (cdr l))

;;; C compiler interface
;;;
;;; You can't always win.

(define c-optimize
  (let ((x "-O3 -DNDEBUG"))
    (lambda rest
      (if (not (null? rest))
	  (set! x (car rest)))
      x)))

(begin (display "  ")
       (display "C Optimization flags: ")
       (display (c-optimize))
       (newline))

(define (c-compile-file c-name o-name)
  (let ((c-compiler "gcc")
	(optimize   (c-optimize))
	(c-flags    "-c -g -IRts/Sys -IRts/Standard-C -IRts/Build")
	(gcc-flags  "-D__USE_FIXED_PROTOTYPES__ -Wpointer-arith -Wimplicit"))
    (let ((cmd (string-append c-compiler
			      " "
			      c-flags
			      " "
			      optimize
			      " "
			      gcc-flags
			      " "
			      c-name
			      " -o "
			      o-name)))
      (display cmd)
      (newline)
      (system cmd))))

(define (c-link-executable output-name object-files libraries)

  (define (insert-space l)
    (cond ((null? l) l)
	  ((null? (cdr l)) l)
	  (else (cons (car l)
		      (cons " "
			    (insert-space (cdr l)))))))

  (let ((cmd (string-append "gcc -g -o " output-name
			    " "
			    (apply string-append (insert-space object-files))
			    " "
			    (apply string-append (insert-space libraries)))))
    (display cmd)
    (newline)
    (system cmd)))

