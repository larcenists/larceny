; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Heap dumper overriding code for x86-nasm back end.
; Load this file after loading Asm/Common/dumpheap.sch.
; It's OK to load this file repeatedly.

; Segments are lists: (code-vector constant-vector function-info).

(define segment.code car)
(define segment.constants cadr)
(define segment.function-info caddr)

(define fun.name car)
(define fun.definite? cadr)
(define fun.entry? caddr)

; Constants
(define *init-function-name* "twobit_start")
(define *init-thunk-array-name* "twobit_start_procedures")
(define *temp-file* "HEAPDATA.asm")
(define *delete-temp-files* #f)

; Variables
(define *asm-output* #f)
(define *segment-number* 0)
(define *already-compiled* '())
(define *unique-id* #f)
(define *entrypoints* '())
(define *loadables* '())

; Overrides the one in Common/dumpheap.sch

(define heap.root-names
  '(result second third fourth
    reg0 reg1 reg2 reg3 reg3 reg5 reg6 reg7 reg8 reg9 reg10 reg11 reg12
    reg13 reg14 reg15 reg16 reg17 reg18 reg19 reg20 reg21 reg22 reg23
    reg24 reg25 reg26 reg27 reg28 reg29 reg30 reg31 
    startup callouts cont alloc-tmp))
    
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
        (emit-c-code "global ~a~%" name)
	(emit-c-code "align 4~%" name)
	(emit-c-code "~a: jmp ~a~%~%" name entrypoint)
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
	  (bootstrap-id (or bootstrap-id (string-append ".petit-bootstrap-id-" *unique-id*))))
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
	    (emit-c-code "global twobit_load_table~%")
	    (emit-c-code "twobit_load_table:~%")
	    (do ((l (reverse entrypoints) (cdr l))
		 (i 0 (+ i 1)))
		((null? l))
	      (emit-c-code "  dd ~a~%" (car l)))))
      (after-dump-file #f filename)
      entrypoints))

  (if (null? rest)
      (dump-one-file filename)
      (dump-segments #f filename (car rest) '() (cadr rest))))


; Specialized and more rational version of create-loadable-file, in three parts.

(define *shared-object-so-expression* #f)
(define *shared-object-so-name* #f)
(define *shared-object-asm-name* #f)
(define *shared-object-o-name* #f)
(define *shared-object-entrypoints* #f)

(define (begin-shared-object so-name so-expression)
  (error "begin-shared-object needs to receive a list of assembly-declarations")
  (set! *shared-object-so-expression* so-expression)
  (set! *shared-object-so-name* so-name)
  (set! *shared-object-asm-name* (rewrite-file-type so-name '(".dll" ".so") ".asm"))
  (set! *shared-object-o-name* (rewrite-file-type so-name '(".dll" ".so") (obj-suffix)))
  (set! *segment-number* 0)
  (set! *shared-object-entrypoints* '())
  (let ((id (compute-unique-id *shared-object-asm-name*)))
    (set! *unique-id* id)
    (delete-file *shared-object-asm-name*)
    (delete-file *shared-object-o-name*)
    (delete-file *shared-object-so-name*)
    (let ((c-file (open-output-file *shared-object-asm-name*)))
      (set! *asm-output* c-file)
      (emit-c-code "%include \"i386-machine.ah\"~%")
      (emit-c-code "%include \"i386-instr.asm\"~%~%")
      #t)))

(define (add-to-shared-object fasl-name segments)

  (define (dump-prologue file-unique-id out)
    (display `(define ,file-unique-id (.petit-shared-object ,*shared-object-so-expression*)) 
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
		(segment-unique-id (string-append "align 4~%twobit_thunk_"
						  *unique-id*
						  "_"
						  (number->string *segment-number*))))
	    (petit-dump-fasl-segment segment
				     file-unique-id
				     segment-unique-id
				     *segment-number*
				     out)
	    (set! *shared-object-entrypoints* (cons segment-unique-id *shared-object-entrypoints*))
	    (set! *segment-number* (+ *segment-number* 1))))))))

(define (end-shared-object)
  (emit-c-code "global twobit_load_table~%")
  (emit-c-code "twobit_load_table:~%")
  (do ((l (reverse *shared-object-entrypoints*) (cdr l)))
      ((null? l))
    (emit-c-code "  dd ~a~%" (car l)))
  (close-output-port *asm-output*)
  (c-compile-file *shared-object-asm-name* *shared-object-o-name*)
  (set! *asm-output* #f)
  #t)

(define (petit-dump-fasl-segment segment file-unique-id segment-unique-id segment-number fasl-port)

  (define (dump-constants cv)
    (for-each (lambda (x)
		(case (car x)
		  ((codevector)     (dump-codevector! #f (cadr x)))
		  ((constantvector) (dump-constants (cadr x)))))
	      (vector->list cv)))

  (let ((entrypoint (dump-function-prototypes (segment.function-info segment))))
    (dump-codevector! #f (segment.code segment))
    (dump-constants (segment.constants segment))
    (emit-c-code "global ~a~%" segment-unique-id)
    (emit-c-code "~a: jmp ~a~%~%" segment-unique-id entrypoint))

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

(define (compute-unique-id c-name)
  (md5 c-name))

(define (before-dump-file h filename decls)
  (set! *segment-number* 0)
  (let* ((c-name (rewrite-file-type filename '(".fasl" ".lop") ".asm"))
	 (id     (compute-unique-id c-name)))
    (set! *unique-id* id)
    (set! *already-compiled* (cons c-name *already-compiled*))
    (if (and (file-exists? c-name)
	     (compat:file-newer? c-name filename))
	(set! *asm-output* #f)
	(let ((c-file (open-output-file c-name)))
	  (set! *asm-output* c-file)
	  (for-each (lambda (d) (emit-c-code "~a~%" d))
		    decls)
	  (emit-c-code "%include \"i386-machine.ah\"~%")
	  (emit-c-code "%include \"i386-instr.asm\"~%")))))

(define (after-dump-file h filename)
  (if *asm-output*
      (close-output-port *asm-output*))
  (let ((c-name (rewrite-file-type filename '(".fasl" ".lop") ".asm"))
        (o-name (rewrite-file-type filename '(".fasl" ".lop") (obj-suffix))))
    (if (not (and (file-exists? o-name)
                  (compat:file-newer? o-name c-name)))
        (c-compile-file c-name o-name))
    (set! *asm-output* #f)))


; It's important that segment-number is updated afterwards to correspond 
; to segment number update in dumping of loadables (because some code may
; be shared).

(define (dump-segment! h segment . rest)
  (let ((entrypoint
         (dump-function-prototypes (segment.function-info segment)))
        (startup?
         (not (null? rest)))
        (template
	 "global ~a~%align 4~%~a: jmp ~a~%~%"))
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
                  (emit-c-code template name name entrypoint)
                  (set! *entrypoints* (cons name *entrypoints*))
                  (dump-thunk! h $imm.false the-consts))
                (begin
                  (emit-c-code template 
			       *init-function-name* *init-function-name* entrypoint)
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
      ;(emit-c-code "static RTYPE ~a( CONT_PARAMS );~%" name)
      (if entry?
          (set! entry name)))))

(define (dump-codevector! heap cv)
  (emit-c-code "~a~%" cv)
  $imm.false)

(define (dump-startup-procedure! h)

  (define (dump-init-thunks)
    (emit-c-code "~%~%; File init procedures~%~%")
    (for-each (lambda (e)
                (emit-c-code "extern ~a~%" e))
              (reverse *entrypoints*))
    (emit-c-code "global ~a~%" *init-thunk-array-name*)
    (emit-c-code "~%~a:~%" *init-thunk-array-name*)
    (for-each (lambda (e)
                (emit-c-code "  dd ~a~%" e))
              (reverse *entrypoints*)))

  ; The twobit_load_table is defined by the program that uses the
  ; library that contains the startup heap.

  (define (dump-loadable-thunks)
    (emit-c-code "extern twobit_load_table~%"))

  (let ((r #f))
    (delete-file *temp-file*)
    (call-with-output-file *temp-file*
      (lambda (out)
        (set! *asm-output* out)
        (emit-c-code "%include \"i386-machine.ah\"~%")
        (emit-c-code "%include \"i386-instr.asm\"~%~%")
        (let ((thunks  (dump-list-spine! h (heap.thunks h)))
              (symbols (dump-list-spine! h (symbol-locations h))))
          (set! r (dump-segment! h
                                 (construct-startup-procedure symbols thunks)
                                 #t))
          (dump-init-thunks)
          (dump-loadable-thunks))))
    (set! *asm-output* #f)
    (c-compile-file *temp-file* (rewrite-file-type *temp-file* ".asm" (obj-suffix)))
    (if *delete-temp-files*
        (delete-file *temp-file*))
    r))

(define (dump-loadable-thunks filename)
  (delete-file filename)
  (call-with-output-file filename
    (lambda (f)
      (set! *asm-output* f)
      (emit-c-code "%include \"i386-machine.ah\"~%")
      (emit-c-code "%include \"i386-instr.asm\"~%")
      (emit-c-code "global main~%")
      (emit-c-code "global twobit_load_table~%")
      (emit-c-code "extern larceny_main~%")
      (emit-c-code "main:~%")
      (emit-c-code "jmp larceny_main~%~%")
      (emit-c-code "; Loadable segments' code~%~%")
      (let ((l (reverse *loadables*)))
        ; Print prototypes
        (do ((l l (cdr l)))
            ((null? l))
          (do ((f (cdar l) (cdr f)))
              ((null? f))
            (emit-c-code "extern ~a~%" (car f))))
        ; Print inner tables
        (do ((l l (cdr l))
             (i 0 (+ i 1)))
            ((null? l))
          (emit-c-code "twobit_load_table~a:~%" i)
          (do ((x (cdar l) (cdr x)))
              ((null? x))
            (emit-c-code "  dd ~a~%" (car x))))
        ; Print outer table
        (emit-c-code "twobit_load_table:~%")
        (do ((l l (cdr l))
             (i 0 (+ i 1)))
            ((null? l))
          (emit-c-code "  dd twobit_load_table~a~%" i)))))
  (set! *asm-output* #f))

(define (emit-c-code fmt . args)
  (if *asm-output*
      (apply twobit-format *asm-output* fmt args)))

; Startup procedure is same as standard except for the patch instruction.

(define init-proc
  `((,$.proc)
    (,$args= 1)
    (,$reg 1)
    (,$setreg 2)
    (,$const (thunks))
    (,$op1 petit-patch-boot-code)       ; Petit larceny
    (,$setreg 1)
    (,$.label 1001)
    (,$reg 1)
    (,$op1 null?)                       ; (null? l)
    (,$branchf 1003)
    (,$const (symbols))                 ; dummy list of symbols
    (,$setreg 1)
    (,$global go)
    (,$invoke 2)                        ; (go <list of symbols> argv)
    (,$.label 1003)
    (,$save 2)
    (,$store 0 0)
    (,$store 1 1)
    (,$store 2 2)
    (,$setrtn 1004)
    (,$reg 1)
    (,$op1 car)
    (,$invoke 0)                        ; ((car l))
    (,$.align 4)
    (,$.label 1004)
    (,$.cont)
    (,$restore 2)
    (,$pop 2)
    (,$reg 1)
    (,$op1 cdr)
    (,$setreg 1)
    (,$branch 1001)))                      ; (loop (cdr l))

; C compiler interface
;
; We shold move the definitions of *c-linker* and *c-compiler* to
; a compatibility library, and *petit-libraries* also.

(define *available-compilers* '())  ; Assigned below -- ((tag name obj functions) ...)
(define *current-compiler* #f)      ; Assigned below -- (tag name obj functions)

(define optimize-c-code
  (make-twobit-flag "optimize-c-code"))

(define (insert-space l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        (else (cons (car l) (cons " " (insert-space (cdr l)))))))

(define (execute cmd)
  (display cmd)
  (newline)
  (if (not (= (system cmd) 0))
      (error "COMMAND FAILED.")))
  
(define (c-compile-file c-name o-name)
  ((cdr (assq 'compile (cadddr *current-compiler*))) c-name o-name))

(define (c-link-library output-name object-files libraries)
  ((cdr (assq 'link-library (cadddr *current-compiler*))) output-name object-files libraries))

(define (c-link-executable output-name object-files libraries)
  ((cdr (assq 'link-executable (cadddr *current-compiler*))) output-name object-files libraries))

(define (c-link-shared-object output-name object-files libraries)
  ((cdr (assq 'link-shared-object (cadddr *current-compiler*))) output-name object-files libraries))

(define (obj-suffix)
  (caddr *current-compiler*))

(define (*append-file-shell-command* x y)
  ((cdr (assq 'append-files (cadddr *current-compiler*))) x y))

(define (define-compiler name tag extension commands)
  (set! *available-compilers*
	(cons (list tag name extension commands)
	      *available-compilers*)))

(define (select-compiler . rest)
  (if (null? rest)
      (begin
	(display "Available compilers:")
	(newline)
	(for-each (lambda (c) 
		    (display (car c))
		    (display "   ")
		    (display (cadr c))
		    (newline))
		  *available-compilers*))
      (let ((probe (assq (car rest) *available-compilers*)))
	(if probe
	    (begin
	      (display "Selecting compiler: ")
	      (display (cadr probe))
	      (newline)
	      (set! *current-compiler* probe))
	    (error "No such compiler: " (car rest)))))
  (unspecified))

(define-compiler 
  "No compiler at all"
  'none
  ".o"
  `((compile 
     . ,(lambda (c-name o-name)
	  (display ">>> MUST COMPILE ")
	  (display c-name)
	  (newline)))
    (link-library
     . ,(lambda (output-name object-files libs)
	  (display ">>> MUST LINK LIBRARY ")
	  (display output-name)
	  (newline)))
    (link-executable
     . ,(lambda (output-name object-files libs)
	  (display ">>> MUST LINK EXECUTABLE ")
	  (display output-name)
	  (newline)))
    (link-shared-object
     . ,(lambda (output-name object-files libs)
	  (display ">>> MUST LINK SHARED OBJECT ")
	  (display output-name)
	  (newline)))
    (append-files 
     . ,append-file-shell-command-portable)))

(select-compiler 'none)

; eof
