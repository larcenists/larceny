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

; Constants
(define *init-function-name* "twobit_start")
(define *init-thunk-array-name* "twobit_start_procedures")
(define *temp-file* "HEAPDATA.c")
(define *delete-temp-files* #f)

; Variables
(define *c-output* #f)
(define *segment-number* 0)
(define *already-compiled* '())
(define *seed* #f)
(define *live-seeds* '())
(define *entrypoints* '())
(define *additional-files* '())
(define *loadables* '())

; Alternate entry point to the heap dumper.

(define (build-extended-heap-image output-file input-files additional)
  (set! *additional-files* additional)
  (build-heap-image output-file input-files))

(define (before-all-files heap output-file-name input-file-names)
  (set! *segment-number* 0)
  (set! *seed* #f)
  (set! *live-seeds* '())
  (set! *entrypoints* '())
  (set! *loadables* '())
  (set! *already-compiled* '())
  (for-each create-loadable-file *additional-files*))

(define (create-loadable-file filename)

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
                                 *seed*
                                 "_"
                                 (number->string *segment-number*))))
        (emit-c-code 
         "RTYPE ~a(CONT_PARAMS) {~%  RETURN_RTYPE(~a(CONT_ACTUALS));~%}~%"
         name
         entrypoint)
        name)))

  (define (dump-fasl segment out)
    (display "((.petit-patch-procedure " out)
    (display (length *loadables*) out)
    (display " " out)
    (display *segment-number* out)
    (newline out)
    (display "'" out)
    (dump-fasl-segment-to-port segment out 'no-code)
    (display "))" out)
    (newline out))

  (display "Loading code for ") (display filename) (newline)
  (let ((entrypoints '())
        (fasl-file (rewrite-file-type filename ".lop" ".fasl")))
    (before-dump-file #f filename)
    (call-with-output-file fasl-file
      (lambda (out)
        (call-with-input-file filename
          (lambda (in)
            (do ((segment (read in) (read in)))
                ((eof-object? segment) 
                 (set! *loadables* (cons (cons *seed* (reverse entrypoints))
                                         *loadables*)))
              (set! entrypoints (cons (dump-code segment) entrypoints))
              (dump-fasl (cons (segment.code segment)
                               (segment.constants segment))
                         out)
              (set! *segment-number* (+ *segment-number* 1)))))))
    (after-dump-file #f filename)))

(define (after-all-files heap output-file-name input-file-names)
  (c-link-executable "petit-larceny"
                     (remove-duplicates
                      (append (map (lambda (x)
                                     (rewrite-file-type x ".lop" ".o"))
                                   input-file-names)
                              (list (rewrite-file-type *temp-file* ".c" ".o"))
                              (map (lambda (x)
                                     (rewrite-file-type x ".lop" ".o"))
                                   *additional-files*))
                      string=?)))

; Returns a seed and an indication of whether the seed is new.

(define (compute-seed c-name)

  (define seed-name (rewrite-file-type c-name ".c" ".seed"))

  (define (adjust-seed seed n)
    (cond ((not (memv seed *live-seeds*))
           (set! *live-seeds* (cons seed *live-seeds*))
           (call-with-output-file seed-name
             (lambda (out)
               (write seed out)))
           (values (number->string seed 16) (positive? n)))
          ((member c-name *already-compiled*)
           (values (number->string seed 16) #f))
          (else
           (adjust-seed (remainder (+ seed (an-arbitrary-number)) 65536)
                        (+ n 1)))))

  (if (file-exists? seed-name)
      (adjust-seed (call-with-input-file seed-name read) 0)
      (adjust-seed (remainder (string-hash c-name) 65536) 1)))

(define (before-dump-file h filename)
  (set! *segment-number* 0)
  (let ((c-name (rewrite-file-type filename ".lop" ".c")))
    (call-with-values 
     (lambda () (compute-seed c-name))
     (lambda (seed updated?)
       (set! *seed* seed)
       (set! *already-compiled* (cons c-name *already-compiled*))
       (if (and (not updated?)
                (file-exists? c-name)
                (compat:file-newer? c-name filename))
           (set! *c-output* #f)
           (let ((c-file (open-output-file c-name)))
             (set! *c-output* c-file)
             (emit-c-code "/* Generated from ~a */~%" filename)
             (emit-c-code "#include \"twobit.h\"~%~%")))))))

(define (after-dump-file h filename)
  (if *c-output*
      (close-output-port *c-output*))
  (let ((c-name (rewrite-file-type filename ".lop" ".c"))
	(o-name (rewrite-file-type filename ".lop" ".o")))
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
                                      *seed*
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
                (emit-c-code "extern void ~a( CONT_PARAMS );~%" e))
              (reverse *entrypoints*))
    (emit-c-code "~%cont_t ~a[] = { ~%" *init-thunk-array-name*)
    (for-each (lambda (e)
                (emit-c-code "  ~a,~%" e))
              (reverse *entrypoints*))
    (emit-c-code "};~%~%"))

  (define (dump-loadable-thunks)
    (emit-c-code "~%/* Loadable segments' code */~%~%")
    (let ((l (reverse *loadables*)))
      ; Print prototypes
      (do ((l l (cdr l)))
          ((null? l))
        (do ((f (cdar l) (cdr f)))
            ((null? f))
          (emit-c-code "extern void ~a( CONT_PARAMS );~%" (car f))))
      ; Print inner tables
      (do ((l l (cdr l))
           (i 0 (+ i 1)))
          ((null? l))
        (emit-c-code "cont_t twobit_load_table~a[] = { ~%" i)
        (do ((x (cdar l) (cdr x)))
            ((null? x))
          (emit-c-code "  ~a,~%" (car x)))
        (emit-c-code "};~%~%"))
      ; Print outer table
      (emit-c-code "cont_t *twobit_load_table[] = { ~%")
      (do ((l l (cdr l))
           (i 0 (+ i 1)))
          ((null? l))
        (emit-c-code "  twobit_load_table~a,~%" i))
      (emit-c-code 
       "  0  /* The table may be empty; some compilers complain */~%};~%")))

  (let ((r #f))
    (call-with-output-file *temp-file*
      (lambda (out)
	(set! *c-output* out)
        (emit-c-code "/* Generated heap bootstrap code */~%")
	(emit-c-code "#include \"twobit.h\"~%~%")
	(let ((thunks  (dump-list-spine! h (heap.thunks h)))
	      (symbols (dump-list-spine! h (symbol-locations h))))
	  (set! r (dump-segment! h
				 (construct-startup-procedure symbols thunks)
                                 #t))
          (dump-init-thunks)
          (dump-loadable-thunks))))
    (set! *c-output* #f)
    (c-compile-file *temp-file* (rewrite-file-type *temp-file* ".c" ".o"))
    (if *delete-temp-files*
        (delete-file *temp-file*))
    r))

(define (emit-c-code fmt . args)
  (if *c-output*
      (display (apply twobit-format fmt args) *c-output*)))

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

; C compiler interface
;
; We shold move the definitions of *c-linker* and *c-compiler* to
; a compatibility library, and *petit-libraries* also.

(define *petit-libraries* '("Rts/libpetit.so"))  ; List of file names
(define *c-compiler* #f)                         ; Assigned below
(define *c-linker* #f)                           ; Assigned below

(define optimize-c-code
  (make-twobit-flag "optimize-c-code"))

(define (c-compile-file c-name o-name)
  (*c-compiler* c-name o-name))

(define (c-link-executable output-name object-files)
  (*c-linker* output-name object-files))

(define (insert-space l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        (else (cons (car l) (cons " " (insert-space (cdr l)))))))

(define (execute cmd)
  (display cmd)
  (newline)
  (if (not (= (system cmd) 0))
      (error "COMMAND FAILED.")))
  
(define (c-compiler:gcc-unix c-name o-name)
  (let ((cmd (twobit-format "gcc -c -g -IRts/Sys -IRts/Standard-C -IRts/Build -D__USE_FIXED_PROTOTYPES__ -Wpointer-arith -Wimplicit ~a -o ~a ~a"
                            (if (optimize-c-code) "-O3 -DNDEBUG" "")
                            o-name
                            c-name)))
    (execute cmd)))

(define (c-linker:gcc-unix output-name object-files)
  (let ((cmd (twobit-format "gcc -g -o ~a ~a ~a"
                            output-name
			    (apply string-append (insert-space object-files))
			    (apply string-append 
                                   (insert-space *petit-libraries*)))))
    (execute cmd)))

(define (c-compiler:lcc-unix c-name o-name)
  (let ((cmd (twobit-format "lcc -c -g -IRts/Sys -IRts/Standard-C -IRts/Build -DSTDC_SOURCE ~a -o ~a ~a"
                            (if (optimize-c-code) "-DNDEBUG" "")
                            o-name
                            c-name)))
    (execute cmd)))

(define (c-linker:lcc-unix output-name object-files)
  (let ((cmd (twobit-format "lcc -g -o ~a ~a ~a"
                            output-name
			    (apply string-append (insert-space object-files))
			    (apply string-append 
                                   (insert-space *petit-libraries*)))))
    (execute cmd)))

(define *c-compiler* c-compiler:gcc-unix)
(define *c-linker* c-linker:gcc-unix)

; eof
