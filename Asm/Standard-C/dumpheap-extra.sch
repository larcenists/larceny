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
(define *loadables* '())

(define (init-variables)
  (set! *segment-number* 0)
  (set! *seed* #f)
  (set! *live-seeds* '())
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
    (delete-file fasl-file)
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

; This is probably not the right thing, because the library may need
; to be moved to its destination location before the executable is
; built.

(define (after-all-files heap output-file-name input-file-names)
  (build-petit-larceny heap output-file-name input-file-names))

; Returns a seed and an indication of whether the seed is new.

(define (compute-seed c-name)

  (define seed-name (rewrite-file-type c-name ".c" ".seed"))

  (define (adjust-seed seed n)
    (cond ((not (memv seed *live-seeds*))
           (set! *live-seeds* (cons seed *live-seeds*))
	   (delete-file seed-name)
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
        (o-name (rewrite-file-type filename ".lop" (obj-suffix))))
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
        (emit-c-code "#include \"twobit.h\"~%~%")
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
      (emit-c-code "#include \"twobit.h\"~%~%")
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

(define *c-compiler* #f)                         ; Assigned below
(define *c-linker* #f)                           ; Assigned below
(define *c-library-linker* #f)                   ; Assigned below
(define *obj-suffix* #f)                         ; Assigned below

(define optimize-c-code
  (make-twobit-flag "optimize-c-code"))

(define (c-compile-file c-name o-name)
  (*c-compiler* c-name o-name))

(define (c-link-library output-name object-files libraries)
  (*c-library-linker* output-name object-files libraries))

(define (c-link-executable output-name object-files libraries)
  (*c-linker* output-name object-files libraries))

(define (obj-suffix)
  *obj-suffix*)

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
  (let ((cmd (twobit-format #f
                            "gcc -c -g -IRts/Sys -IRts/Standard-C -IRts/Build -D__USE_FIXED_PROTOTYPES__ -Wpointer-arith -Wimplicit ~a -o ~a ~a"
                            (if (optimize-c-code) "-O -DNDEBUG" "")
                            o-name
                            c-name)))
    (execute cmd)))

(define (c-library-linker:gcc-unix output-name object-files libs)
  (let ((cmd (twobit-format #f
                            "gcc -g -shared -o ~a ~a ~a"
                            output-name
                            (apply string-append (insert-space object-files))
                            (apply string-append (insert-space libs)))))
    (execute cmd)))

(define (c-linker:gcc-unix output-name object-files libs)
  (let ((cmd (twobit-format #f
                            "gcc -g -o ~a ~a ~a"
                            output-name
                            (apply string-append (insert-space object-files))
                            (apply string-append (insert-space libs)))))
    (execute cmd)))

(define (c-compiler:lcc-unix c-name o-name)
  (let ((cmd (twobit-format #f
                            "lcc -c -g -IRts/Sys -IRts/Standard-C -IRts/Build -DSTDC_SOURCE ~a -o ~a ~a"
                            (if (optimize-c-code) "-DNDEBUG" "")
                            o-name
                            c-name)))
    (execute cmd)))

(define (c-library-linker:lcc-unix output-name object-files libs)
  (error "Must figure out how to create shared libraries with LCC."))

(define (c-linker:lcc-unix output-name object-files libs)
  (let ((cmd (twobit-format #f
                            "lcc -g -o ~a ~a ~a"
                            output-name
                            (apply string-append (insert-space object-files))
                            (apply string-append (insert-space libs)))))
    (execute cmd)))

(define (c-compiler:no-compiler c-name o-name)
  (display ">>> MUST COMPILE ")
  (display c-name)
  (newline))

(define (c-library-linker:no-linker output-name object-files libs)
  (display ">>> MUST LINK LIBRARY ")
  (display output-name)
  (newline))

(define (c-linker:no-linker output-name object-files libs)
  (display ">>> MUST LINK EXECUTABLE ")
  (display output-name)
  (newline))


; Somewhat general architecture for selecting compilers and linkers,
; although I dread the day when all options for all operating systems
; are included in this file.

(define (select-compiler . rest)

  (define compilers
    `((gcc-unix 
       "GCC on Unix systems" 
       ,c-compiler:gcc-unix 
       ,c-library-linker:gcc-unix
       ,c-linker:gcc-unix 
       ,append-file-shell-command-unix
       ".o")
      (lcc-unix 
       "LCC on Unix systems" 
       ,c-compiler:lcc-unix 
       ,c-library-linker:lcc-unix
       ,c-linker:lcc-unix
       ,append-file-shell-command-unix
       ".o")
      (none 
       "No compiler at all" 
       ,c-compiler:no-compiler 
       ,c-library-linker:no-linker
       ,c-linker:no-linker
       ,append-file-shell-command-portable
       ".o")))

  (if (null? rest)
      (begin (display "Select one of these: ")
             (newline)
             (for-each (lambda (x)
                         (display "  ")
                         (display (car x))
                         (display 
                          (make-string (- 12 (string-length 
                                              (symbol->string (car x))))
                                       #\space))
                         (display (cadr x))
                         (newline))
                       compilers))
      (let ((probe (assq (car rest) compilers)))
        (if (not probe)
            (select-compiler)
	    (apply set-compiler! (cdr probe))))))

(define (set-compiler! name compiler lib-linker linker append-cmd obj-suffix)
  (set! *c-compiler* compiler)
  (set! *c-library-linker* lib-linker)
  (set! *c-linker* linker)
  (set! *append-file-shell-command* append-cmd)
  (set! *obj-suffix* obj-suffix)
  (newline)
  (display "Selecting compiler: ")
  (display name)
  (newline))

(select-compiler 'none)

; eof
