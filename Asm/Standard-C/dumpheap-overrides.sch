; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Heap dumper overriding code for Standard-C back end.
; Load this file after loading Asm/Common/dumpheap.sch.
; It's OK to load this file repeatedly.

; Segments are lists: (code-vector constant-vector function-info).

(define segment.code car)
(define segment.constants cadr)
(define segment.function-info caddr)

(define fun.name car)
(define fun.definite? cadr)
(define fun.entry? caddr)

(define (compute-unique-id c-name)
  (md5 c-name))

; Constants
(define *init-function-name* "twobit_start")
(define *init-thunk-array-name* "twobit_start_procedures")
(define *temp-file* "HEAPDATA.c")
(define *delete-temp-files* #f)

; Variables
(define *c-output* #f)
(define *segment-number* 0)
(define *already-compiled* '())
(define *unique-id* #f)
(define *entrypoints* '())
(define *loadables* '())

(define (init-variables)
  (set! *segment-number* 0)
  (set! *unique-id* #f)
  (set! *entrypoints* '())
  (set! *loadables* '())
  (set! *already-compiled* '()))

; Given a list of the LOP files (with directory names) that have been dumped into
; the heap given by output-filename, this procedure must create an executable for
; Petit Larceny.  See dumpheap-unix.sch, dumpheap-win32.sch, etc.

(define (build-petit-larceny heap output-file-name input-file-names)
  (error "You must load a target-dependent file that redefines BUILD-PETIT-LARCENY"))

(define (before-all-files heap output-file-name input-file-names)
  (init-variables))

(define (create-loadable-file filename . rest)

  (define (dump-constants cv)
    (for-each (lambda (x)
                (case (car x)
                  ((codevector)
                   (dump-codevector! #f (cadr x)))
                  ((constantvector)
                   (dump-constants (cadr x)))
                  (else #f)))           ; masks a bug in CASE in 1.0a1
              (vector->list cv)))

  (define (dump-code segment)
    (let ((entrypoint (dump-function-prototypes
                       (segment.function-info segment))))
      (dump-codevector! #f (segment.code segment))
      (dump-constants (segment.constants segment))
      (let ((name (string-append "twobit_thunk_"
                                 *unique-id*
                                 "_"
                                 (number->string *segment-number*))))
        (emit-c-code 
         "RTYPE ~a(CONT_PARAMS) {~%  RETURN_RTYPE(~a(CONT_ACTUALS));~%}~%"
         name
         entrypoint)
        name)))

  (define (dump-fasl bootstrap-id segment out)
    (display "((.petit-patch-procedure " out)
    (display bootstrap-id out)
    (display " " out)
    (display *segment-number* out)
    (newline out)
    (display "'" out)
    (dump-fasl-segment-to-port segment out 'no-code)
    (display "))" out)
    (newline out))

  (define (dump-one-file lop-file)
    (display "Loading code for ") (display lop-file) (newline)
    (let ((fasl-file (rewrite-file-type lop-file ".lop" ".fasl")))
      (let ((entrypoints 
	     (call-with-input-file lop-file
	       (lambda (in)
		 (let loop ((decls '()))
		   (let ((item (read in)))
		     (if (string? item)
			 (loop (cons item decls))
			 (do ((segment (read in) (read in))
			      (segments '() (cons segment segments)))
			     ((eof-object? segment)
			      (dump-segments (length *loadables*)
					     fasl-file
					     (reverse segments)
					     (reverse decls)
					     #f))))))))))
	(set! *loadables* (cons (cons *unique-id* (reverse entrypoints))
				*loadables*)))))

  (define (dump-segments bootstrap-id fasl-file segments decls so-name)
    (before-dump-file #f filename)
    (delete-file fasl-file)
    (let ((entrypoints '())
	  (bootstrap-id (or bootstrap-id
			    (string-append ".petit-bootstrap-id-" *unique-id*))))
      (call-with-output-file fasl-file
	(lambda (out)
	  (if so-name
	      (begin (display "(define " out)
		     (display bootstrap-id out)
		     (display " (.petit-shared-object " out)
		     (write so-name out)
		     (display "))" out)
		     (newline out)
		     (newline out)))
	  (do ((segments segments (cdr segments)))
	      ((null? segments))
	    (let ((segment (car segments)))
	      (set! entrypoints (cons (dump-code segment) entrypoints))
	      (dump-fasl bootstrap-id
			 (cons (segment.code segment)
			       (segment.constants segment))
			 out)
	      (set! *segment-number* (+ *segment-number* 1))))))
      (if so-name
	  (begin
	    (emit-c-code "codeptr_t CDECL twobit_load_table[] = { ~%")
	    (do ((l (reverse entrypoints) (cdr l))
		 (i 0 (+ i 1)))
		((null? l))
	      (emit-c-code "  ~a,~%" (car l)))
	    (emit-c-code 
	     "  0  /* The table may be empty; some compilers complain */~%};~%")))
      (after-dump-file #f filename)
      entrypoints))

  (if (null? rest)
      (dump-one-file filename)
      (dump-segments #f filename (car rest) '() (cadr rest))))


; Specialized and more rational version of create-loadable-file, in three parts.

(define *shared-object-so-expression* #f)
(define *shared-object-so-name* #f)
(define *shared-object-c-name* #f)
(define *shared-object-o-name* #f)
(define *shared-object-entrypoints* #f)

(define (begin-shared-object so-name so-expression)
  (error "begin-shared-object needs to receive a list of assembly-declarations")
  (set! *shared-object-so-expression* so-expression)
  (set! *shared-object-so-name* so-name)
  (set! *shared-object-c-name* (rewrite-file-type so-name '(".dll" ".so") ".c"))
  (set! *shared-object-o-name* (rewrite-file-type so-name '(".dll" ".so") (obj-suffix)))
  (set! *segment-number* 0)
  (set! *shared-object-entrypoints* '())
  (let ((id (compute-unique-id *shared-object-c-name*)))
    (set! *unique-id* id)
    (delete-file *shared-object-c-name*)
    (delete-file *shared-object-o-name*)
    (delete-file *shared-object-so-name*)
    (let ((c-file (open-output-file *shared-object-c-name*)))
      (set! *c-output* c-file)
      (emit-c-code "#include \"petit-instr.h\"~%~%")
      #t)))

(define (add-to-shared-object fasl-name segments)

  (define (dump-prologue file-unique-id out)
    (display `(define ,file-unique-id 
		(.petit-shared-object ,*shared-object-so-expression*)) 
	     out)
    (newline out)
    (newline out))

  (let ((file-unique-id (string-append ".petit-bootstrap-id-" *unique-id*)))
    (delete-file fasl-name)
    (call-with-output-file fasl-name
      (lambda (out)
	(dump-prologue file-unique-id out)
	(do ((segments segments (cdr segments)))
	    ((null? segments))
	  (let ((segment (car segments))
		(segment-unique-id (string-append "twobit_thunk_"
						  *unique-id*
						  "_"
						  (number->string *segment-number*))))
	    (petit-dump-fasl-segment segment
				     file-unique-id
				     segment-unique-id
				     *segment-number*
				     out)
	    (set! *shared-object-entrypoints* 
		  (cons segment-unique-id *shared-object-entrypoints*))
	    (set! *segment-number* (+ *segment-number* 1))))))))

(define (end-shared-object)
  (emit-c-code "codeptr_t CDECL twobit_load_table[] = { ~%")
  (do ((l (reverse *shared-object-entrypoints*) (cdr l)))
      ((null? l))
    (emit-c-code "  ~a,~%" (car l)))
  (emit-c-code "  0  /* The table may be empty; some compilers complain */~%};~%")
  (close-output-port *c-output*)
  (c-compile-file *shared-object-c-name* *shared-object-o-name*)
  (set! *c-output* #f)
  #t)

(define (petit-dump-fasl-segment segment file-unique-id segment-unique-id segment-number 
				 fasl-port)

  (define (dump-constants cv)
    (for-each (lambda (x)
		(case (car x)
		  ((codevector)     (dump-codevector! #f (cadr x)))
		  ((constantvector) (dump-constants (cadr x)))))
	      (vector->list cv)))

  (let ((entrypoint (dump-function-prototypes (segment.function-info segment))))
    (dump-codevector! #f (segment.code segment))
    (dump-constants (segment.constants segment))
    (emit-c-code 
     "RTYPE ~a(CONT_PARAMS) {~%  RETURN_RTYPE(~a(CONT_ACTUALS));~%}~%"
     segment-unique-id
     entrypoint))

  (display "((.petit-patch-procedure " fasl-port)
  (display file-unique-id fasl-port)
  (display " " fasl-port)
  (display segment-number fasl-port)
  (newline fasl-port)
  (display "'" fasl-port)
  (dump-fasl-segment-to-port (cons (segment.code segment)
				   (segment.constants segment))
			     fasl-port
			     'no-code)
  (display "))" fasl-port)
  (newline fasl-port))


; This is probably not the right thing, because the library may need
; to be moved to its destination location before the executable is
; built.

(define (after-all-files heap output-file-name input-file-names)
  (build-petit-larceny heap output-file-name input-file-names))

(define (before-dump-file h filename decls)
  (set! *segment-number* 0)
  (let* ((c-name (rewrite-file-type filename '(".fasl" ".lop") ".c"))
	 (id (compute-unique-id c-name)))
    (set! *unique-id* id)
    (set! *already-compiled* (cons c-name *already-compiled*))
    (if (and (file-exists? c-name)
	     (compat:file-newer? c-name filename))
	(set! *c-output* #f)
	(let ((c-file (open-output-file c-name)))
	  (set! *c-output* c-file)
	  (emit-c-code "/* Generated from ~a */~%" filename)
	  (for-each (lambda (d) (emit-c-code "~a~%" d))
		    decls)
	  (emit-c-code "#include \"petit-instr.h\"~%~%")))))

(define (after-dump-file h filename)
  (if *c-output*
      (close-output-port *c-output*))
  (let ((c-name (rewrite-file-type filename '(".fasl" ".lop") ".c"))
        (o-name (rewrite-file-type filename '(".fasl" ".lop") (obj-suffix))))
    (if (not (and (file-exists? o-name)
                  (compat:file-newer? o-name c-name)))
        (c-compile-file c-name o-name))
    (set! *c-output* #f)))


; It's important that segment-number is updated afterwards to correspond 
; to segment number update in dumping of loadables (because some code may
; be shared).

(define (dump-segment! h segment . rest)
  (let ((entrypoint
         (dump-function-prototypes (segment.function-info segment)))
        (startup?
         (not (null? rest)))
        (template
         "RTYPE ~a(CONT_PARAMS) {~%  RETURN_RTYPE(~a(CONT_ACTUALS));~%}~%"))
    (dump-codevector! h (segment.code segment))
    (let* ((the-consts
            (dump-constantvector! h (segment.constants segment)))
           (t
            (if (not startup?)
                (let ((name
                       (string-append "twobit_thunk_"
                                      *unique-id*
                                      "_"
                                      (number->string *segment-number*))))
                  (emit-c-code template name entrypoint)
                  (set! *entrypoints* (cons name *entrypoints*))
                  (dump-thunk! h $imm.false the-consts))
                (begin
                  (emit-c-code template *init-function-name* entrypoint)
                  (dump-thunk! h $imm.false the-consts)))))
      (set! *segment-number* (+ *segment-number* 1))
      t)))

; Print all the function prototypes and return the name of the unique
; entry point.

(define (dump-function-prototypes funs)
  (do ((funs  funs (cdr funs))
       (entry #f))
      ((null? funs)
       (emit-c-code "~%")
       entry)
    (let* ((fun (car funs))
           (name (fun.name fun))
           (definite? (fun.definite? fun))
           (entry? (fun.entry? fun)))
      (emit-c-code "static RTYPE ~a( CONT_PARAMS );~%" name)
      (if entry?
          (set! entry name)))))

(define (dump-codevector! heap cv)
  (emit-c-code "~a~%" cv)
  $imm.false)

(define (dump-startup-procedure! h)

  (define (dump-init-thunks)
    (emit-c-code "~%~%/* File init procedures */~%~%")
    (for-each (lambda (e)
                (emit-c-code "extern RTYPE ~a( CONT_PARAMS );~%" e))
              (reverse *entrypoints*))
    (emit-c-code "~%codeptr_t ~a[] = { ~%" *init-thunk-array-name*)
    (for-each (lambda (e)
                (emit-c-code "  ~a,~%" e))
              (reverse *entrypoints*))
    (emit-c-code "};~%~%"))

  ; The twobit_load_table is defined by the program that uses the
  ; library that contains the startup heap.

  (define (dump-loadable-thunks)
    (emit-c-code "extern codeptr_t *twobit_load_table[];~%"))

  (let ((r #f))
    (delete-file *temp-file*)
    (call-with-output-file *temp-file*
      (lambda (out)
        (set! *c-output* out)
        (emit-c-code "/* Generated heap bootstrap code */~%")
        (emit-c-code "#include \"petit-instr.h\"~%~%")
        (let ((thunks  (dump-list-spine! h (heap.thunks h)))
              (symbols (dump-list-spine! h (symbol-locations h))))
          (set! r (dump-segment! h
                                 (construct-startup-procedure symbols thunks)
                                 #t))
          (dump-init-thunks)
          (dump-loadable-thunks))))
    (set! *c-output* #f)
    (c-compile-file *temp-file* (rewrite-file-type *temp-file* ".c" (obj-suffix)))
    (if *delete-temp-files*
        (delete-file *temp-file*))
    r))

(define (dump-loadable-thunks filename)
  (delete-file filename)
  (call-with-output-file filename
    (lambda (f)
      (set! *c-output* f)
      (emit-c-code "#include \"petit-instr.h\"~%~%")
      (emit-c-code "int main( int argc, char **argv )~%")
      (emit-c-code "{ return larceny_main( argc, argv ); }~%~%")
      (emit-c-code "~%/* Loadable segments' code */~%~%")
      (let ((l (reverse *loadables*)))
        ; Print prototypes
        (do ((l l (cdr l)))
            ((null? l))
          (do ((f (cdar l) (cdr f)))
              ((null? f))
            (emit-c-code "extern RTYPE ~a( CONT_PARAMS );~%" (car f))))
        ; Print inner tables
        (do ((l l (cdr l))
             (i 0 (+ i 1)))
            ((null? l))
          (emit-c-code "codeptr_t twobit_load_table~a[] = { ~%" i)
          (do ((x (cdar l) (cdr x)))
              ((null? x))
            (emit-c-code "  ~a,~%" (car x)))
          (emit-c-code "};~%~%"))
        ; Print outer table
        (emit-c-code "codeptr_t *twobit_load_table[] = { ~%")
        (do ((l l (cdr l))
             (i 0 (+ i 1)))
            ((null? l))
          (emit-c-code "  twobit_load_table~a,~%" i))
        (emit-c-code 
         "  0  /* The table may be empty; some compilers complain */~%};~%"))))
  (set! *c-output* #f))

(define (emit-c-code fmt . args)
  (if *c-output*
      (apply twobit-format *c-output* fmt args)))

; eof
