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

(define fun.id car)
(define fun.il-namespace cadr)
(define fun.definite? caddr)
(define fun.entry? cadddr)

;; ----------------------
;; IL code
;; ----------------------

;; il:build-constant : value -> ilpackage
(define (il:build-constant x)
  (il:load-constant x))

;; dump-nested-codevectors : (listof TaggedConstant) -> void
;; Creates classes for each codevector in the constant vector.
;; Does not create a class for the top-level codevector.
(define (dump-nested-codevectors cv)
  (for-each (lambda (x)
              (case (car x)
                ((codevector)
                 (dump-nested-codevector (cadr x)))
                ((constantvector)
                 (dump-nested-codevectors (cadr x)))
                (else #f)))
            (vector->list cv)))

;; dump-codevector : cvclass -> void
;; Takes a cvclass (defined in pass5p2) and emits a full class.
(define (dump-nested-codevector codevector)
  (let* ((id (cvclass-id codevector))
         (il-namespace (cvclass-il-namespace codevector))
         (instrs (cvclass-instrs codevector))
         (constants (cvclass-constants codevector))
         (name (codevector-name id))
         (fullname (il-class #f il-namespace name)))
    (class-start name il-namespace il-codevector 
                 '(private auto ansi beforefieldinit))
    (field-add "instance" iltype-codevector '(public static initonly))
    
    (method-start ".ctor" iltype-void '() 
                  '(private hidebysig specialname 
                            rtspecialname instance cil managed))
    (emit ilc 
          (il 'ldarg 0)
          (il:call '(instance) iltype-void il-codevector ".ctor" '())
          (il 'ret))
    (method-finish)

    (method-start "id" iltype-int32 '()
                  '(public hidebysig virtual instance cil managed))
    (emit ilc
          (il 'ldc.i4 (+ (* (car id) #x10000)
                         (cdr id)))
          (il 'ret))
    (method-finish)

    (method-add "call" iltype-void (list iltype-int32) 
                '(public hidebysig virtual instance cil managed)
                instrs)

    (method-start ".cctor" iltype-void '()
                  '(private hidebysig specialname 
                            rtspecialname static cil managed))
    (emit ilc
          (il:directive 
           'maxstack 
           (apply max (cons 4 (map il:constant-max-stack constants))))
          (il:call '(new instance) iltype-void fullname ".ctor" '())
          (il 'stsfld (il-field iltype-codevector fullname "instance"))
          
          (let loop ((n 0) (constants constants))
            (cond ((null? constants) '())
                  ((pair? constants)
                   (list (il:build-constant (car constants))
                         (il:stsfld iltype-schemeobject 
                                    fullname
                                    (twobit-format #f "constant~s" n))
                         (loop (+ 1 n) (cdr constants))))))
          (il 'ret))
    (method-finish)

    (class-finish)))

;; dump-segment : Segment -> (cons string string)
;; Dumps all codevectors in the segment. In addition, a Loader is defined 
;; which creates a Procedure representing the top-level form of the segment.
;; Returns the pair of Loader classname and namespace.
(define (dump-segment segment)
  (let* ((entrypoint (dump-codevector-prototypes
                      (segment.function-info segment)))
         (il-namespace (cvclass-il-namespace (segment.code segment)))
         (max-stack (il:constant-vector-max-stack
                     (segment.constants segment))))
    (dump-nested-codevector (segment.code segment))
    (dump-nested-codevectors (segment.constants segment))

    (let ((loadable-classname (il:loader-name *segment-number*)))
      (class-start loadable-classname il-namespace il-object 
                   '(public auto ansi beforefieldinit))
      (method-start "constants" iltype-schemeobject '()
                    '(public hidebysig static cil managed))
      (emit ilc 
            (il:directive 'maxstack max-stack)
            (il:constant-vector 
             (vector->list (segment.constants segment)))
            (il 'ret))
      (method-finish)
      
      (field-add "entrypoint" iltype-codevector '(public static initonly))
      
      (method-start ".cctor" iltype-void '()
                    '(private hidebysig specialname 
                              rtspecialname static cil managed))
      (emit ilc 
            (il 'ldsfld 
                (il-field iltype-codevector
                          (il-class #f
                                    il-namespace
                                    (codevector-name entrypoint))
                          "instance"))
            (il 'stsfld 
                (il-field iltype-codevector
                          (il-class #f il-namespace loadable-classname)
                          "entrypoint"))
            (il 'ret))
      (method-finish)
      
      (method-start ".ctor" iltype-void '()
                    '(private hidebysig specialname 
                              rtspecialname instance cil managed))
      (emit ilc
            (il 'ldarg 0)
            (il:call '(instance)
                     iltype-void il-object ".ctor" '())
            (il 'ret))
      (method-finish)

      (method-start "load" iltype-procedure '()
                    '(public hidebysig static cil managed))
      (emit ilc
            (il:ldsfld iltype-codevector 
                       (il-class #f il-namespace loadable-classname)
                       "entrypoint")
            (il:call '() iltype-schemeobject
                     (il-class #f il-namespace loadable-classname)
                     "constants" '())
            (il:call '(new instance) iltype-void 
                     il-procedure ".ctor" 
                     (list iltype-codevector
                           iltype-schemeobject))
            (il 'ret))
      (method-finish)

      (class-finish)
      (cons loadable-classname il-namespace))))

;; dump-fasl : segment string output-port -> void
;; Dumps a fasl file containing the file base (no extension) of the
;; source .lop file, the generated namespace for all classes in that
;; segment, 0 (?), and the number of the segment.
(define (dump-fasl segment filename out)
  (twobit-format 
   out
   "((.common-patch-procedure ~s ~s ~s ~s "
   (rewrite-file-type filename ".lop" "")
   (cvclass-il-namespace (segment.code segment))
   (length *loadables*)
   (car (cvclass-id (segment.code segment)));;*segment-number*
   )
  (dump-fasl-segment-to-port segment out 'no-code)
  (twobit-format out "))~%"))

;; dump-manifest : segment string output-port -> void
(define (dump-manifest code filename out)
  (write `(,(rewrite-file-type filename ".lop" "")
           ,(cvclass-il-namespace code)
           ,(length *loadables*)
           ,*segment-number*)
         out)
  (newline out))

;; create-loadable-file : string -> void
;; ENTRY POINT for creating .il files in larceny-csharp
;; Turns a single .lop file into a single .il file, without
;; an assembly manifest.
(define (create-loadable-file filename)
  (init-variables)
  (let ((entrypoints '())
        (fasl-file (rewrite-file-type filename ".lop" ".fasl"))
        (il-file-name (rewrite-file-type filename ".lop" ".code-il"))
        (manifest-file (rewrite-file-type filename ".lop" ".manifest")))
    (set! *c-output* (open-output-file il-file-name))
    (call-with-output-file fasl-file
      (lambda (out)
        (call-with-output-file manifest-file
          (lambda (manifest-out)
            (call-with-input-file filename
              (lambda (in)
                (do ((segment (read in) (read in)))
                  ((eof-object? segment) 
                   (set! *loadables* (cons (cons *seed* (reverse entrypoints))
                                           *loadables*)))
                  (set! entrypoints (cons (dump-segment segment) entrypoints))
                  (dump-fasl (cons (segment.code segment)
                                   (segment.constants segment))
                             filename out)
                  (dump-manifest (segment.code segment)
                                 filename manifest-out)
                  (set! *segment-number* (+ *segment-number* 1)))))))))
    (il-finalize *c-output*)
    (close-output-port *c-output*)
    (set! *c-output* #f)))

;; create-assembly : string (listof string) -> string
(define (create-assembly file manifests)
  (init-variables)
  (let ((il-name (rewrite-file-type file ".exe" ".asm-il"))
        (assembly-name (rewrite-file-type file ".exe" "")))
    (set! *c-output* (open-output-file il-name))
    (emit topc
          (il:directive 'assembly-extern il:scheme-assembly-name '())
          (il:directive 'assembly-extern "mscorlib" '())
          (il:directive 'assembly assembly-name '())
          (il:directive 'module file '()))
    (class-start "SchemeManifest" #f "[mscorlib]System.Object"
                 '(public auto ansi beforefieldinit))
    (dump-toplevels (apply append (map manifest-get-loaders manifests)))
    (dump-debug-info (map manifest-get-namespace manifests))
    (class-finish)
    
    (dump-main-function '() #f)
    (il-finalize *c-output*)
    (close-output-port *c-output*)
    (set! *c-output* #f)
    il-name))

;; dump-main-function : (listof string) string -> void
(define (dump-main-function classes filename)
  (begin
    (class-start "Main"
                 #f
                 "[mscorlib]System.Object"
                 '(public auto ansi beforefieldinit))
    (method-start "Main" iltype-void (list iltype-string-array)
                  '(public hidebysig static cil managed))
    (emit ilc
          (il:directive 'entrypoint)
          (il 'ldarg 0)
          (il:call '() iltype-void il-load "MainHelper"
                   (list iltype-string-array))
          (il 'ret))
    (method-finish)
    (class-finish)))

;; manifest-get-loaders : string -> (listof string)
(define (manifest-get-loaders manifest)
  (with-input-from-file manifest
    (lambda ()
      (read/map 
       (lambda (line)
         (let ((file-base (car line))
               (il-namespace (cadr line))
               (segment (cadddr line)))
           (il-class #f il-namespace (il:loader-name segment))))))))

;; dump-debug-info : (listof (string . string)) -> void
(define (dump-debug-info nspairs)
  (method-add
   "DebugInfo"
   iltype-void
   '()
   '(public static cil managed)
   (let ((instrs '()))
     (emit (lambda (instr) (set! instrs (cons instr instrs)))
           (map il:set-namespace-map nspairs)
           (il 'ret))
     (reverse instrs))))

;; il:set-namespace-map : (string . string) -> ilpackage
(define (il:set-namespace-map nspair)
  (list (il:ldstr (car nspair))
        (il:ldstr (cdr nspair))
        (il:call '() iltype-void il-exn "registerNamespace"
                 (list iltype-string iltype-string))))

;; manifest-get-namespace : string -> (string . string)
(define (manifest-get-namespace fasl)
  (with-input-from-file fasl
    (lambda ()
      (let* ((entry0 (read))
             (file-base (car entry0))
             (il-namespace (cadr entry0)))
        (cons il-namespace file-base)))))

;; dump-toplevels : (listof string) -> void
(define (dump-toplevels loaders)
  (method-add
   "TopLevel"
   iltype-procedure-array 
   '()
   '(public static cil managed)
   (let ((instrs '()))
     (emit (lambda (instr) (set! instrs (cons instr instrs)))
           (list (il 'ldc.i4 (length loaders))
                 (il 'newarr iltype-procedure)
                 (let loop ((index 0) (loaders loaders))
                   (if (null? loaders) 
                       (list (il 'ret))
                       (list (il 'dup)
                             (il 'ldc.i4 index)
                             (il:call '() iltype-procedure (car loaders)
                                      "load" '())
                             (il 'stelem.ref)
                             (loop (+ 1 index) (cdr loaders)))))))
     (reverse instrs))))


(define (il:call-invoke-procedure class)
  (list
   (il 'ldsfld (il-field iltype-codevector class "entrypoint"))
   (il:call '() iltype-constantvector class "constants" '())
   (il:call '() iltype-void il-call "invokeProcedure"
            (list iltype-codevector iltype-constantvector))))

; Returns a seed and an indication of whether the seed is new.
(define (compute-seed c-name)

  (define seed-name (rewrite-file-type c-name ".il" ".seed"))

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


;; dump-codevector-prototypes : (listof function-info?) -> (cons num num)
;;   where function-info is (name il-namespace definite? entry?)
;; (Potentially print empty class declarations for each codevector class).
;; Returns the unique entry point.
(define (dump-codevector-prototypes funs)
;  (twobit-format (current-output-port)
;                 "dump-codevector-prototypes: ~s~%" funs)
  (let loop ((funs funs) (entry #f))
    (cond ((null? funs)
           entry)
          (else 
           (let* ((fun (car funs))
                  (id (fun.id fun))
                  (il-namespace (fun.il-namespace fun))
                  (definite? (fun.definite? fun))
                  (entry? (fun.entry? fun)))
             ;; We can emit class prototypes here if we choose.
;             (class-add 
;              (make-class (codevector-name id) 
;                          il-namespace
;                          il-codevector
;                          '(private auto ansi beforefieldinit)
;                          '()))
             (loop (cdr funs) 
                   (if entry? id entry)))))))

;; /C# code -------------

;; ----------------------
;; Attic 
;; ----------------------

;;; Constants
;;(define *init-function-name* "twobit_start")
;;(define *init-thunk-array-name* "twobit_start_procedures")
;;(define *temp-file* "HEAPDATA.c")
;;(define *delete-temp-files* #f)

;;; Very Unix
;;(define *petit-executable-src-name* "petit-larceny.c")
;;(define *petit-executable-obj-name* "petit-larceny.o")
;;(define *petit-executable-name* "petit-larceny")
;;
;;(define *petit-library-path* "")
;;(define *petit-heap-library-name* "petit-larceny.so")
;;(define *petit-rts-libraries* 
;;  (list (string-append *petit-library-path* "libpetit.so")))
;;(define *petit-executable-libraries* 
;;  (append *petit-rts-libraries* 
;;          (list (string-append *petit-library-path* "petit-larceny.so"))))

;;; Build an _application_: an executable that contains additional object
;;; files to load, and a set of FASL files that can be loaded to link 
;;; the object code to Scheme procedures.
;;;
;;; FIXME: The use of ".exe" and ".o" here are pretty arbitrary and not
;;; right on all (most) platforms; must parameterize.  The use of .exe
;;; is OK on both Unix and Mac, however, as rewrite-file-type also matches
;;; on the empty extension.
;;
;;(define (build-application executable-name additional-files)
;;  (let ((src-name (rewrite-file-type executable-name '(".exe") ".c"))
;;        (obj-name (rewrite-file-type executable-name '(".exe") ".o")))
;;    (init-variables)
;;    (for-each create-loadable-file additional-files)
;;    (dump-loadable-thunks src-name)
;;    (c-compile-file src-name obj-name)
;;    (c-link-executable executable-name
;;                       (cons obj-name
;;                             (map (lambda (x)
;;                                    (rewrite-file-type x ".lop" ".o"))
;;                                  additional-files))
;;                       *petit-executable-libraries*)
;;    executable-name))

;;; Link all the files in Lib, Repl, Eval, and the macro expander
;;; with HEAPDATA.o and create the (shared) library petit-larceny.so.
;;
;;(define (build-petit-library library-name input-file-names)
;;  (c-link-library *petit-heap-library-name*
;;                  (remove-duplicates
;;                   (append (map (lambda (x)
;;                                  (rewrite-file-type x ".lop" ".o"))
;;                                input-file-names)
;;                           (list (rewrite-file-type *temp-file* ".c" ".o")))
;;                   string=?)
;;                  *petit-rts-libraries*))
;;
;;(define (before-all-files heap output-file-name input-file-names)
;;  (init-variables))
;;
;;; This is probably not the right thing, because the library may need
;;; to be moved to its destination location before the executable is
;;; built.
;;
;;(define (after-all-files heap output-file-name input-file-names)
;;  (build-petit-library *petit-heap-library-name* input-file-names)
;;  (build-application *petit-executable-name* '()))

;;; It's important that segment-number is updated afterwards to correspond 
;;; to segment number update in dumping of loadables (because some code may
;;; be shared).
;;
;;(define (dump-segment! h segment . rest)
;;  (let ((entrypoint
;;         (dump-function-prototypes (segment.function-info segment)))
;;        (startup?
;;         (not (null? rest)))
;;        (template
;;         "RTYPE ~a(CONT_PARAMS) {~%  RETURN_RTYPE(~a(CONT_ACTUALS));~%}~%"))
;;    (dump-codevector! h (segment.code segment))
;;    (let* ((the-consts
;;            (dump-constantvector! h (segment.constants segment)))
;;           (t
;;            (if (not startup?)
;;                (let ((name
;;                       (string-append "twobit_thunk_"
;;                                      *seed*
;;                                      "_"
;;                                      (number->string *segment-number*))))
;;                  (emit-c-code template name entrypoint)
;;                  (set! *entrypoints* (cons name *entrypoints*))
;;                  (dump-thunk! h $imm.false the-consts))
;;                (begin
;;                  (emit-c-code template *init-function-name* entrypoint)
;;                  (dump-thunk! h $imm.false the-consts)))))
;;      (set! *segment-number* (+ *segment-number* 1))
;;      t)))

;;(define (dump-startup-procedure! h)
;;
;;  (define (dump-init-thunks)
;;    (emit-c-code "~%~%/* File init procedures */~%~%")
;;    (for-each (lambda (e)
;;                (emit-c-code "extern RTYPE ~a( CONT_PARAMS );~%" e))
;;              (reverse *entrypoints*))
;;    (emit-c-code "~%codeptr_t ~a[] = { ~%" *init-thunk-array-name*)
;;    (for-each (lambda (e)
;;                (emit-c-code "  ~a,~%" e))
;;              (reverse *entrypoints*))
;;    (emit-c-code "};~%~%"))
;;
;;  ; The twobit_load_table is defined by the program that uses the
;;  ; library that contains the startup heap.
;;
;;  (define (dump-loadable-thunks)
;;    (emit-c-code "extern codeptr_t *twobit_load_table[];~%"))
;;
;;  (let ((r #f))
;;    (call-with-output-file *temp-file*
;;      (lambda (out)
;;        (set! *c-output* out)
;;        (emit-c-code "/* Generated heap bootstrap code */~%")
;;        (emit-c-code "#include \"twobit.h\"~%~%")
;;        (let ((thunks  (dump-list-spine! h (heap.thunks h)))
;;              (symbols (dump-list-spine! h (symbol-locations h))))
;;          (set! r (dump-segment! h
;;                                 (construct-startup-procedure symbols thunks)
;;                                 #t))
;;          (dump-init-thunks)
;;          (dump-loadable-thunks))))
;;    (set! *c-output* #f)
;;    (c-compile-file *temp-file* (rewrite-file-type *temp-file* ".c" ".o"))
;;    (if *delete-temp-files*
;;        (delete-file *temp-file*))
;;    r))

;;(define (dump-loadable-thunks filename)
;;  (call-with-output-file filename
;;    (lambda (f)
;;      (set! *c-output* f)
;;      (emit-c-code "#include \"twobit.h\"~%~%")
;;      (emit-c-code "~%/* Loadable segments' code */~%~%")
;;      (let ((l (reverse *loadables*)))
;;        ; Print prototypes
;;        (do ((l l (cdr l)))
;;            ((null? l))
;;          (do ((f (cdar l) (cdr f)))
;;              ((null? f))
;;            (emit-c-code "extern void ~a( CONT_PARAMS );~%" (car f))))
;;        ; Print inner tables
;;        (do ((l l (cdr l))
;;             (i 0 (+ i 1)))
;;            ((null? l))
;;          (emit-c-code "codeptr_t twobit_load_table~a[] = { ~%" i)
;;          (do ((x (cdar l) (cdr x)))
;;              ((null? x))
;;            (emit-c-code "  ~a,~%" (car x)))
;;          (emit-c-code "};~%~%"))
;;        ; Print outer table
;;        (emit-c-code "codeptr_t *twobit_load_table[] = { ~%")
;;        (do ((l l (cdr l))
;;             (i 0 (+ i 1)))
;;            ((null? l))
;;          (emit-c-code "  twobit_load_table~a,~%" i))
;;        (emit-c-code 
;;         "  0  /* The table may be empty; some compilers complain */~%};~%"))))
;;  (set! *c-output* #f))

;;; Startup procedure is same as standard except for the patch instruction.
;;
;;(define init-proc
;;  `((,$.proc)
;;    (,$args= 1)
;;    (,$reg 1)
;;    (,$setreg 2)
;;    (,$const (thunks))
;;    (,$op1 petit-patch-boot-code)       ; Petit larceny
;;    (,$setreg 1)
;;    (,$.label 1001)
;;    (,$reg 1)
;;    (,$op1 null?)                       ; (null? l)
;;    (,$branchf 1003)
;;    (,$const (symbols))                 ; dummy list of symbols
;;    (,$setreg 1)
;;    (,$global go)
;;    (,$invoke 2)                        ; (go <list of symbols> argv)
;;    (,$.label 1003)
;;    (,$save 2)
;;    (,$store 0 0)
;;    (,$store 1 1)
;;    (,$store 2 2)
;;    (,$setrtn 1004)
;;    (,$reg 1)
;;    (,$op1 car)
;;    (,$invoke 0)                        ; ((car l))
;;    (,$.label 1004)
;;    (,$.cont)
;;    (,$restore 2)
;;    (,$pop 2)
;;    (,$reg 1)
;;    (,$op1 cdr)
;;    (,$setreg 1)
;;    (,$branch 1001)))                      ; (loop (cdr l))

; C compiler interface
;
; We shold move the definitions of *c-linker* and *c-compiler* to
; a compatibility library, and *petit-libraries* also.

(define *c-compiler* #f)                         ; Assigned below
(define *c-linker* #f)                           ; Assigned below
(define *c-library-linker* #f)                   ; Assigned below

(define optimize-c-code
  (make-twobit-flag "optimize-c-code"))

(define (c-compile-file c-name o-name)
  (*c-compiler* c-name o-name))

(define (c-link-library output-name object-files libraries)
  (*c-library-linker* output-name object-files libraries))

(define (c-link-executable output-name object-files libraries)
  (*c-linker* output-name object-files libraries))

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
                            "ld -G -o ~a -L/usr/lib -lc ~a"
                            output-name
                            (apply string-append
                                   (insert-space object-files)))))
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

(define (c-library-linker:lcc-unix output-name object-files)
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

  (define compiler caddr)
  (define lib-linker cadddr)
  (define (linker x) (car (cddddr x)))
  (define (append-cmd x) (cadr (cddddr x)))
  
  (define compilers
    `((gcc-unix 
       "GCC on Unix systems" 
       ,c-compiler:gcc-unix 
       ,c-library-linker:gcc-unix
       ,c-linker:gcc-unix 
       ,append-file-shell-command-unix)
      (lcc-unix 
       "LCC on Unix systems" 
       ,c-compiler:lcc-unix 
       ,c-library-linker:lcc-unix
       ,c-linker:lcc-unix
       ,append-file-shell-command-unix)
      (none 
       "No compiler at all" 
       ,c-compiler:no-compiler 
       ,c-library-linker:no-linker
       ,c-linker:no-linker
       ,append-file-shell-command-portable)))

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
            (begin (set! *c-compiler* (compiler probe))
                   (set! *c-library-linker* (lib-linker probe))
                   (set! *c-linker* (linker probe))
                   (set! *append-file-shell-command* (append-cmd probe))
                   (car probe))))))

(select-compiler 'none)

; eof
