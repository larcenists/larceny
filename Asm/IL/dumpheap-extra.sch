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
  (class-finish))

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

;; ===============================================
;; Helpers for building programs

(define (create-application app src-manifests)
  (let* ((app-exe (string-append app ".exe"))
         (assembly-il 
          (create-assembly app-exe src-manifests))
         (ordered-il-files
          (map (lambda (f) (rewrite-file-type f ".manifest" ".code-il"))
               src-manifests)))
    (ilasm app-exe (cons assembly-il ordered-il-files))
    app-exe))

;; Override Asm/Common/dumpheap.sch's definition so
;; the make-system will do the right thing
(define (build-heap-image output-file input-files)
  (create-application output-file input-files))

(define (invoke-ilasm exe-file il-files)
  (let ((options
         (if (codegen-option 'ilasm-debug)
             "/nologo /quiet /debug"
             "/nologo /quiet")))
    (system (twobit-format #f "ilasm ~a /output:~a ~a" 
                           options
                           exe-file
                           (apply string-append
                                  (map/separated
                                   values
                                   (lambda () " ")
                                   il-files))))))

(define (ilasm exe-file il-files)
  (if (member (nbuild-parameter 'host-system) '("Larceny"))
      ;; Petit Larceny seems to fail with no explanation on long
      ;; command strings.
      (let ((big-il-file (rewrite-file-type exe-file ".exe" ".il")))
        (concatenate-files big-il-file il-files)
        (invoke-ilasm exe-file (list big-il-file)))
      (invoke-ilasm exe-file il-files)))

(define (concatenate-files target sources)
  (with-output-to-file target
    (lambda ()
      (for-each display-file sources))))

(define (display-file source)
  (with-input-from-file source
    (lambda ()
      (let loop ()
        (let ((next (read-char)))
          (if (eof-object? next)
              #t
              (begin
                (write-char next)
                (loop))))))))

;; -----------------------------------------------

(define (scheme->app file)
  (let ((base (rewrite-file-type file '(".sch" ".scm" ".mal") "")))
    (scheme->il file)
    (let ((app (create-application 
                base
                (list (string-append base ".manifest")))))
      (twobit-format (current-output-port)
                     "  application file -> ~s~%" app))))

(define (scheme->il filename)
  (twobit-format (current-output-port) "Source file: ~s~%" filename)
  (if (file-type=? filename ".mal")
      (mal->il filename)
      (sch->il filename)))

(define (sch->il filename)
  (let ((lap-name (rewrite-file-type filename '(".scm" ".sch") ".lap")))
    (compile313 filename)
    (twobit-format (current-output-port) "  compiled -> ~s~%" lap-name)
    (mal->il lap-name)))

(define (mal->il filename)
  (let* ((base (rewrite-file-type filename '(".lap" ".mal") ""))
	 (listing-name (rewrite-file-type base "" ".list"))
	 (lop-name (rewrite-file-type base "" ".lop"))
	 (il-name (rewrite-file-type base "" ".code-il")))
    (if (codegen-option 'listify-write-list-file)
	(begin (listify-reset)
	       (set! listify-filename listing-name)
	       (set! listify-oport (open-output-file listing-name))))
    (assemble313 filename)
    (if (codegen-option 'listify-write-list-file)
	(begin ;; (flush-output-port listify-oport) ;; FIXME: WHY WAS THIS HERE?
	       (close-output-port listify-oport)
	       (listify-reset)
	       (twobit-format (current-output-port)
			      "  listing -> ~s~%" listing-name)))
    (twobit-format (current-output-port) "  assembled -> ~s~%" lop-name)
    
    (create-loadable-file lop-name)
    (twobit-format (current-output-port) "  IL dumped -> ~s~%" il-name)))
