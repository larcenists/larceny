; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny development system -- makefile for compiling Scheme files.
;
; Procedures to call:
;  make-sparc-heap
;  make-petit-heap
;  make-dotnet-heap
;  make-extended-petit-heap
;  make-auxlib
;  make-petit-auxlib
;  make-compat
;  make-petit-compat
;  make-compiler
;  make-petit-compiler
;  make-sparcasm
;  make-petitasm
;  make-petit-petitasm
;  make-gc-testsuite
;  make-regression-test
;  make-development-environment
;     makes auxlib, compat, compiler, sparcasm, petitasm
;  make-petit-development-environment
;  make-petit-heap-with-compiler

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Action procedures for the rules.

(define *make-verbose* #t)
(define (make-mesg . args)
  (cond (*make-verbose* (for-each display args) (newline))))

(define (make-compile target deps)
  (make-mesg "Compiling " target)
  (compile313 (car deps) target))

(define (make-assemble target deps)
  (make-mesg "Assembling " target)
  (assemble313 (car deps) target))

(define (make-compile-file target deps)
  (make-mesg "Compiling File " target)
  (compile-file (car deps)))

(define (make-compile-file/with-syntax target deps)
  (make-mesg "Compiling File (with Syntax) " target)
  (compile-file/with-syntax (car deps) (cdr deps) target))

(define (make-compile-and-assemble target deps)
  (make-mesg "Compiling and Assembling " target)
  (compile-and-assemble313 (car deps) target))

(define (make-compile-and-assemble/no-keywords target deps)
  (make-mesg "Compiling and Assembling (no keywords) " target)
  (parameterize ((recognize-keywords? #f))
    (compile-and-assemble313 (car deps) target)))

(define (make-compile-and-assemble/keywords target deps)
  (make-mesg "Compiling and Assembling (w/ keywords) " target)
  (parameterize ((recognize-keywords? #t))
    (compile-and-assemble313 (car deps) target)))

(define (make-assemble-file target deps)
  (make-mesg "Assembling " target)
  (assemble-file (car deps)))

(define (make-dumpheap target files)
  (make-mesg "Dumping " target)
  (delete-file target)
  (build-heap-image target files))

(define *extended-files* '())

(define (make-copy target src)
  (make-mesg "Copying " target)
  (call-with-input-file (car src)
    (lambda (inp)
      (delete-file target)
      (call-with-output-file target
        (lambda (outp)
          (let loop ((item (read-char inp)))
            (if (eof-object? item)
                #t
                (begin (write-char item outp)
                       (loop (read-char inp))))))))))

(define (objects path ext files . rest)
  (let ((substitutions (if (null? rest) '() (car rest))))
    (define (loop files)
      (cond ((null? files)
             '())
            ((string? (car files))
             (cons (string-append path (car files) ext)
                   (loop (cdr files))))
            ((symbol? (car files))
             (let ((probe (assq (car files) substitutions)))
               (cond ((not probe)
                      (error "objects: No substitutions found for "
                             (car files)))
                     ((not (cdr probe))
                      (loop (cdr files)))
                     (else
                      (cons (cdr probe) (loop (cdr files)))))))
            (else
             ???)))
    (loop files)))

(define (replace-extension ext files)
  (map (lambda (file)
         (rewrite-file-type file '(".sch") ext))
       files))

(define (machine-relative x)
  (param-filename 'machine-source x))

(define (common-relative x)
  (param-filename 'common-source x))

(define (auxlib-relative x)
  (param-filename 'auxiliary x))

(define (common-endian x . rest)
  (param-filename 'common-source
		  (string-append
		    x
		    (if (eq? (nbuild-parameter 'target-endianness) 'little)
		      "-el"
		      "-be")
		    (if (null? rest)
		      ".lop"
		      (car rest)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Projects for building the basic heap images.

; These are the base names of all the files that make up the basic heap.

(define common-heap-files
  '(
    ; Fundamental

    "malcode"           ; really basic things
    "typetags"          ; type tags
    "syscall-id"        ; syscall ID numbers
    osdep               ; OS dependent system interface (to be substituted!)
    "system-interface"  ; OS independent system interface
    "error0"            ; Boot-time 'error' procedure.
    primops             ; primop procedures (to be substituted!)

    ; General library

    "sysparam"          ; system parameters
    "struct"            ; structures
    "command-line"      ; command line arguments
    "list"              ; list procedures
    "vector"            ; vector procedures
    "bytevector"        ; bytevector procedures
    "string"            ; string and bytevector procs
    "control"           ; control procedures
    "hash"              ; hash functions
    "preds"             ; some predicates
    "oblist"            ; symbol table
    "mcode"             ; millicode support
    "memstats"          ; runtime stats
    "record"            ; records
    "condition"         ; conditions
    "ecodes"            ; exception codes
    "ehandler"          ; exception handler
    "error"             ; error/reset system
    "raise"             ; R6RS exception mechanism
    "timer"             ; timer interrupts
    "exit"              ; exit procedure; exit/init hooks
    "dump"              ; dump-heap procedure
    "secret"            ; some "hidden" top-level names
    "hashtable"         ; hashtables
    "enum"              ; enumeration sets
    "unicode0"          ; general utility procedures for binary search
    "unicode1"          ; char-* procedures
    "unicode2"          ; the word-breaking algorithm defined by UAX #29
    "unicode3"          ; string-* procedures (casing, -ci comparisons)
    "unicode4"          ; four string-normalize-* procedures
    "require"           ; rudimentary library system

    ; New I/O subsystem

    "iosys"             ; basic system
    "fileio"            ; file ports
    "conio"             ; console ports, i.e., terminal
    "bytevectorio"      ; bytevector ports
    "stringio"          ; string ports
    "customio"          ; custom ports
    "utf16"             ; UTF-16 transcoding
;    "transio"           ; transcript ports
    "portio"            ; user-level procedures for R6RS
    "stdio"             ; user-level procedures
    "print"             ; write/display
    "ioboot"            ; one-time initialization

    "format"            ; `format' procedure.
    "sort"              ; `sort' and `sort!' (used by new macro expander)
    "number"            ; arithmetic
    "fx"                ; fixnum arithmetic
    "fl"                ; flonum arithmetic
    "bitwise"           ; bitwise operations on exact integers
    "globals"           ; `global' offsets (for memstats)

    ; It's important for bellerophon to be loaded as late as possible
    ; because it depends on much of the rest of the system.

    "profile"           ; Profiling code
    bignum-endian       ; Endian-dependent bignum support (to be substituted!)
    "bignums"           ; Bignum support
    "ratnums"           ; Ratnum support
    "rectnums"          ; Rectnum support
    flonum-endian       ; Endian-dependent flonum support (to be substituted!)
    "flonums"           ; Flonum support
    "contag"            ; Contagion
    "num2str"           ; Number printer
    "belle"             ; Algorithm bellerophon
    "str2num"           ; Number parser
    "javadot-symbol"    ; reader depends on this
    "javadot-syntax"    ;        and this
    "reader"            ; Reader
    "env"               ; R5RS environments
    "procinfo"          ; Heuristic procedure information
    "load"              ; Loader
    "eval"              ; Eval procedure
    "syshooks"          ; System functions
    "gcctl"             ; Garbage collector policy control
    "toplevel"          ; Common top-level environment
    toplevel-target     ; Target-specific top-level environment extensions (to be substituted!)
    extra               ; any private libraries (to be substituted!)
    "go"                ; Driver

    ))

; Files in the bootstrap evaluator, repl, and toplevel env.

(define eval-files
  (append
   (param-filename 'repl-source '("main" "reploop"))
   (param-filename 'interp-source '("interp" "interp-prim" "switches"))
   (param-filename 'compiler
                 '("pass1" "pass1.aux" "pass2.aux" "prefs"
                   "syntaxenv" "syntaxrules2" "syntaxrules" "lowlevel"
                   "expand" "usual"
                   ))
   (param-filename 'interp-source '("macro-expand"))
   ))

; Files which implement MzScheme features.
; For now, these can be bundled up with Larceny, but eventually
; this will need to be a separate project.

(define mzscheme-files
  (param-filename 'mzscheme-source
                '("init" "hash-compat"
                  "misc"
                  "inspector"
                  "struct-proc0" "struct-proc" "struct" "struct-macros"
                  ;; N.B.: class, generic, gprint, and dotnet need (recognize-keywords?) on
                  ;; See targets clause in dotnet heap below.
                  "instance0" "instance" "class" "generic" "gprint" ; Ripoff
                  "dotnet-ffi" "dotnet" ; dotnet support
                  ;; under development
                  "envaux"     ;; Auxiliary elements for environments
                  "identifier" ;; for hygienic macros
                  "compress" ;; environment compression for macros
                  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building the sparc-larceny heap image.

(define sparc-heap-project
  (let ((sparc-heap-files
         (objects (nbuild-parameter 'common-source)
                  ".lop"
                  common-heap-files
                  `((primops . ,(param-filename 'source "Arch" "Sparc" "primops.lop"))
                    (toplevel-target . ,(param-filename 'source "Arch" "Sparc" "toplevel-target.lop"))
                    (flonum-endian . ,(common-relative "flonums-be.lop"))
                    (bignum-endian . ,(common-relative "bignums-be.lop"))
                    (osdep . ,(common-relative "sys-unix.lop"))
                    (extra . #f))))
        (sparc-eval-files
         (objects "" ".lop" eval-files)))
    (make:project "sparc.heap"
      `(rules
        (".lop" ".mal" ,make-assemble)
        (".lop" ".lap" ,make-assemble)
        (".lap" ".sch" ,make-compile))
      `(targets
        ("sparc.heap" ,make-dumpheap))
      `(dependencies                    ; Order matters.  [Why??!]
        ("sparc.heap" ,sparc-heap-files)
        ("sparc.heap" ,sparc-eval-files)))))

(define (make-sparc-heap . rest)
  (make:pretend (not (null? rest)))
  (parameterize ((integrate-procedures 'larceny)
                 (compat:read-case-sensitive? #t))
    (make:make sparc-heap-project "sparc.heap" 
               (lambda (tgt) (error 'make-sparc-heap tgt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building the dotNet-larceny heap image

;; set within def. of dotnet-heap-project because that's where
;; the filenames are available with the least effort.

(define remove-dotnet-heap-objects #f)

(define dotnet-heap-project
  (let* ((osdep-file (case (nbuild-parameter 'target-os)
                       ((unix macosx) "sys-unix.manifest")
                       ((win32) "sys-win32.manifest")))
         (dotnet-heap-files
          (objects (nbuild-parameter 'common-source)
                   ".manifest"
                   common-heap-files
                   `((primops
                      . ,(param-filename 'source "Arch" "IL"
                                         "primops.manifest"))
                     (toplevel-target
                      . ,(param-filename 'source "Arch" "IL"
                                         "toplevel-target.manifest"))
                     (flonum-endian
                      . ,(common-endian "flonums" ".manifest"))
                     (bignum-endian
                      . ,(common-endian "bignums" ".manifest"))
                     (osdep
                      . ,(common-relative osdep-file))
                     (extra
                      . ,(param-filename 'source "Arch" "IL"
                                         "loadable.manifest")))))
         (dotnet-eval-files
          (objects "" ".manifest" eval-files))
         (mzscheme-source-target/keywords
          (lambda (name)
            (list (string-append (nbuild-parameter 'mzscheme-source) name)
                  (lambda (tgt deps)
                    (parameterize ((recognize-keywords? #t))
                      (sch->il (car deps)))))))
         (mzscheme-source-dependency
          (lambda (name source)
            (list (string-append (nbuild-parameter 'mzscheme-source) name)
                  (list (string-append (nbuild-parameter 'mzscheme-source)
                                       source)))))
	 (common-larceny/debug
	  (string-append (pathname-append "bin" "Debug") "CommonLarceny"))
	 (common-larceny/release
	  (string-append (pathname-append "bin" "Release") "CommonLarceny"))
         (dotnet-mzscheme-files
          (objects "" ".manifest" mzscheme-files)))

    ;; a handy procedure to delete all the intermediate files

    (set! remove-dotnet-heap-objects
          (lambda ()
            (let* ((manifest-files (append dotnet-mzscheme-files
                                           dotnet-heap-files
                                           dotnet-eval-files))
                   (all-files
                    (cons "dotnet.heap.asm-il"
                    (apply append
                           (cons manifest-files
                                 (map
                                  (lambda (ext)
                                    (map
                                     (lambda (f)
                                       (rewrite-file-type
                                        f
                                        '(".manifest")
                                        ext))
                                     manifest-files))
                                  '(".fasl" ".lap" ".lop" ".code-il")))))))

              (for-each (lambda (f) (if (file-exists? f) (delete-file f)))
                        all-files))))
    (make:project "dotnet.heap"
      `(rules
        (".manifest" ".mal" ,(lambda (tgt deps) (mal->il (car deps))))
        (".manifest" ".sch" ,(lambda (tgt deps) (sch->il (car deps)))))
      `(targets
        (,(common-relative "ecodes.sch") ,make-copy)
        (,(common-relative "globals.sch") ,make-copy)
        ,(mzscheme-source-target/keywords "class.manifest")
        ,(mzscheme-source-target/keywords "compress.manifest")
        ,(mzscheme-source-target/keywords "dotnet.manifest")
        ,(mzscheme-source-target/keywords "envaux.manifest")
        ,(mzscheme-source-target/keywords "generic.manifest")
        ,(mzscheme-source-target/keywords "gprint.manifest")
        ,(mzscheme-source-target/keywords "identifier.manifest")
        ("dotnet.heap" ,make-dumpheap)
	(,common-larceny/debug ,make-dumpheap)
	(,common-larceny/release ,make-dumpheap))
      `(dependencies                    ; Order matters.  [Why??!]
        ,(mzscheme-source-dependency "class.manifest"      "class.sch")
        ,(mzscheme-source-dependency "compress.manifest"   "compress.sch")
        ,(mzscheme-source-dependency "dotnet.manifest"     "dotnet.sch")
        ,(mzscheme-source-dependency "envaux.manifest"     "envaux.sch")
        ,(mzscheme-source-dependency "generic.manifest"    "generic.sch")
        ,(mzscheme-source-dependency "gprint.manifest"     "gprint.sch")
        ,(mzscheme-source-dependency "identifier.manifest" "identifier.sch")
        ("dotnet.heap" ,dotnet-heap-files)
        ("dotnet.heap" ,dotnet-eval-files)
        ("dotnet.heap" ,dotnet-mzscheme-files)

        (,common-larceny/debug ,dotnet-heap-files)
        (,common-larceny/debug ,dotnet-eval-files)
        (,common-larceny/debug ,dotnet-mzscheme-files)

        (,common-larceny/release ,dotnet-heap-files)
        (,common-larceny/release ,dotnet-eval-files)
        (,common-larceny/release ,dotnet-mzscheme-files)
        ))))

(define (make-dotnet-heap . rest)
  (make:pretend (not (null? rest)))
  (parameterize ((integrate-procedures 'larceny)
                 (compat:read-case-sensitive? #t))
    (make:make dotnet-heap-project "dotnet.heap"
               (lambda (tgt) (error 'make-dotnet-heap tgt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building the petit-larceny and IAssassin heap images.

(define petit-heap-files '())
(define petit-eval-files '())

(define (petit-select-target target)
  (define (select target)
    (set! petit-heap-files
          (objects (nbuild-parameter 'common-source)
                   ".lop"
                   common-heap-files
                   `((primops  . ,(machine-relative "primops.lop"))
                     (toplevel-target . ,(machine-relative "toplevel-target.lop"))
                     (flonum-endian . ,(common-endian "flonums"))
                     (bignum-endian . ,(common-endian "bignums"))
                     (osdep    . ,(common-relative target))
                     (extra    . ,(machine-relative "loadable.lop")))))
    (set! petit-eval-files
          (objects "" ".lop" eval-files))
    (unspecified))

  (case target
    ((unix macosx) (select "sys-unix.lop"))
    ((macos) (select "sys-macos.lop"))
    ((win32) (select "sys-win32.lop"))
    ((help)
     (display "Targets are unix, macosx, macos, win32.")
     (newline))
    (else
     (error "Unsupported target "
            target
            "; try one of unix, macosx, macos, win32."))))

(define (make-petit-heap-project heap-name heap-dumper)
  (make:project "petit.heap"
    `(rules
      (".lop" ".mal" ,make-assemble)
      (".lop" ".lap" ,make-assemble)
      (".lap" ".sch" ,make-compile))
    `(targets
      (,heap-name ,heap-dumper))
    `(dependencies                    ; Order matters [why??!]
      (,heap-name ,petit-heap-files)
      (,heap-name ,petit-eval-files))))

(define (make-petit-heap . rest)
  (if (null? petit-heap-files)
      (petit-select-target (nbuild-parameter 'target-os)))
  (let ((petit-heap-project
         (make-petit-heap-project "petit.heap" make-dumpheap)))
    (make:pretend (not (null? rest)))
    (parameterize ((integrate-procedures 'larceny)
                   (compat:read-case-sensitive? #t))
      (make:make petit-heap-project "petit.heap"
                 (lambda (tgt) (error 'make-petit-heap tgt))))))

(define (make-sasstrap-heap . rest)
  (if (null? petit-heap-files)
      (petit-select-target (nbuild-parameter 'target-os)))
  (let ((petit-heap-project
         (make-petit-heap-project "sasstrap.heap" make-dumpheap)))
    (make:pretend (not (null? rest)))
    (parameterize ((integrate-procedures 'larceny)
                   (compat:read-case-sensitive? #t))
      (make:make petit-heap-project "sasstrap.heap"
                 (lambda (tgt) (error 'make-sasstrap-heap tgt))))))

(define (make-sassy-project . rest)
  (define (srfi-file ext)
    (lambda (name)
      (param-filename 'srfi (string-append name ext))))
  (define (sassy-file ext)
    (lambda (name)
      (param-filename 'source "Sassy" (string-append name ext))))
  (define sassy-deps
    (map (lambda (file-and-deps)
	   (list
	     ((sassy-file ".fasl") (car file-and-deps))
	     (apply list
	       ((sassy-file ".scm") "extras")
	       (map (srfi-file ".sch") (cdr file-and-deps)))))
	 '(("push-stacks")
	   ("api" "srfi-9")
	   ("intern")
	   ("macros" "srfi-9")
	   ("numbers")
	   ("other/srfi-56-pieces")
	   ("operands")
	   ("text-block" "srfi-9")
	   ("opcodes")
	   ("text")
	   ("parse")
	   ("main")
	   ("flat-bin")
	   ("elf"))))
  (define srfi-deps
    (map (lambda (file-and-deps)
           (list
	     ((srfi-file ".fasl") (car file-and-deps))
	     (map (srfi-file ".sch") (cdr file-and-deps))))
      '(("srfi-1" "srfi-0" "srfi-8")
        ("srfi-56")
        ("srfi-60")
        ("srfi-66")
        ("srfi-69" "srfi-9"))))
  (make:project "sassy.date"
    `(rules
       (".fasl" ".scm" ,make-compile-file/with-syntax)
       (".fasl" ".sch" ,make-compile-file/with-syntax))
    `(targets
       ("sassy.date" ,(lambda args #t)))
    `(dependencies
       ("sassy.date" ,(map car sassy-deps))
       ("sassy.date" ,(map car srfi-deps))
       ,@sassy-deps
       ,@srfi-deps)))

(define (make-sassy . rest)
  (make:pretend (not (null? rest)))
  (make:make (make-sassy-project) "sassy.date"
             (lambda (tgt) (error 'make-sassy tgt))))

(define (make-petit-libclean)
  (let ((files (append petit-heap-files petit-eval-files)))
    (for-each (lambda (x)
                (if (file-exists? x)
                    (delete-file x)))
              (append files
                      (map (lambda (x) (rewrite-file-type x '(".lop") ".c"))
                           files)
                      (map (lambda (x) (rewrite-file-type x '(".lop") (obj-suffix)))
                           files)
                      (map (lambda (x) (rewrite-file-type x '(".lop") ".lap"))
                           files)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building all the files in the Compiler subdirectory.

(define (make-compiler-project file-type)
  (let ((compiler-files
         (append
          (replace-extension file-type (nbuild:twobit-files))
          (objects (nbuild-parameter 'compiler)
                   file-type
                   '("driver-larceny"))))
        (comp-util-files
         (replace-extension file-type (nbuild:utility-files)))
        (other-util-files
         (append
          (objects (nbuild-parameter 'common-source)
                   file-type
                   '("toplevel"))
          (objects (nbuild-parameter 'machine-source)
                   file-type
                   '("toplevel-target"))
          (objects (nbuild-parameter 'util)
                   file-type
                   '("r5rs-heap"
		     "sparc-larceny-heap" "sparc-twobit-heap"
                     "iasn-larceny-heap" "iasn-twobit-heap"
                     "petit-larceny-heap" "petit-twobit-heap"))))
        (compiler-target/no-keywords
         (lambda (name)
           (list (string-append (nbuild-parameter 'compiler) name)
                 make-compile-and-assemble/no-keywords)))
        (compiler-dependency
         (lambda (name source)
           (list (string-append (nbuild-parameter 'compiler) name)
                 (list (string-append (nbuild-parameter 'compiler) source))))))
    (make:project "compiler.date"
      `(rules
        (".lop" ".sch" ,make-compile-and-assemble)
        (".fasl" ".sch" ,make-compile-file))
      `(targets
        ,(compiler-target/no-keywords "standard-C.imp.lop")
        ,(compiler-target/no-keywords "sparc.imp.lop")
        ("compiler.date" ,(lambda args #t)))
      `(dependencies
        ;; explicit targets need explicit dependencies
        ,(compiler-dependency "standard-C.imp.lop" "standard-C.imp.sch")
        ,(compiler-dependency "sparc.imp.lop" "sparc.imp.sch")
        ("compiler.date" ,compiler-files)
        ("compiler.date" ,comp-util-files)
        ("compiler.date" ,other-util-files)))))

(define compiler-project/fasl (make-compiler-project ".fasl"))

(define (make-compiler . rest)
  (make:pretend (not (null? rest)))
  (make:make compiler-project/fasl "compiler.date"
             (lambda (tgt) (error 'make-compiler tgt))))

(define compiler-project/lop (make-compiler-project ".lop"))

(define (make-petit-compiler . rest)
  (make:pretend (not (null? rest)))
  (make:make compiler-project/lop "compiler.date"
             (lambda (tgt) (error 'make-petit-compiler tgt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building all the files in the new generic assembler and
; the Sparc assembler.

; Note that this project also "works" for non sparc targets.
; That is, the magic here may trick you into thinking that you have
; instant support for your non-sparc backend.

(define sparcasm-project
  (let ((common-asm-files
         (append
          (replace-extension ".fasl" (nbuild:common-asm-files))
          (param-filename 'common-asm '("link-lop.fasl"))))
        (sparcasm-files
         (replace-extension ".fasl" (nbuild:machine-asm-files))))
    (make:project "sparcasm.date"
      `(rules
        (".fasl" ".sch" ,make-compile-file))
      `(targets
        ("sparcasm.date" ,(lambda args #t)))
      `(dependencies
        ("sparcasm.date" ,common-asm-files)
        ("sparcasm.date" ,sparcasm-files)))))

(define (make-sparcasm . rest)
  (make:pretend (not (null? rest)))
  (make:make sparcasm-project "sparcasm.date"
             (lambda (tgt) (error 'make-sparcasm tgt))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building all the files in the standard-C assembler.

(define (make-petit-asm-project file-type)
  (let ((common-asm-files
         (replace-extension ".lop" (nbuild:common-asm-files)))
        (petit-asm-files
         (replace-extension ".lop" (nbuild:machine-asm-files))))
    (make:project "petitasm.date"
      `(rules
        (".lop" ".sch" ,make-compile-and-assemble)
        (".fasl" ".sch" ,make-compile-file))
      `(targets
        ("petitasm.date" ,(lambda args #t)))
      `(dependencies
        ("petitasm.date" ,common-asm-files)
        ("petitasm.date" ,petit-asm-files)))))

(define petit-asm-project/fasl (make-petit-asm-project ".fasl"))

(define (make-petitasm . rest)
  (make:pretend (not (null? rest)))
  (make:make petit-asm-project/fasl "petitasm.date"
             (lambda (tgt) (error 'make-petitasm tgt))))

(define petit-asm-project/lop (make-petit-asm-project ".lop"))

(define (make-petit-petitasm . rest)
  (make:pretend (not (null? rest)))
  (make:make petit-asm-project/lop "petitasm.date"
             (lambda (tgt) (error 'make-petit-petitasm tgt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building all the files in the Common Larceny assembler

(define dotnetasm-project
  (let ((common-asm-files
         (append
          (replace-extension ".fasl" (nbuild:common-asm-files))
          (param-filename 'common-asm '("link-lop.fasl"))))
        (dotnetasm-files
         (replace-extension ".fasl" (nbuild:machine-asm-files))))
    (make:project "dotnetasm.date"
      `(rules
        (".fasl" ".sch" ,make-compile-file))
      `(targets
        ("dotnetasm.date" ,(lambda args #t)))
      `(dependencies
        ("dotnetasm.date" ,common-asm-files)
        ("dotnetasm.date" ,dotnetasm-files)))))

(define (make-dotnetasm . rest)
  (make:pretend (not (null? rest)))
  (make:make dotnetasm-project "dotnetasm.date"
             (lambda (tgt) (error 'make-dotnetassm tgt))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building the Larceny compatibility files.

(define (make-compat-project file-type)
  (make:project "compat.date"
    `(rules
      (".lop" ".sch" ,make-compile-and-assemble)
      (".fasl" ".sch" ,make-compile-file))
    `(targets
      ("compat.date" ,(lambda args #t)))
    `(dependencies
      ("compat.date" (,(string-append (nbuild-parameter 'compatibility)
                                      "compat2"
                                      file-type))))))

(define compat-project/fasl (make-compat-project ".fasl"))
(define compat-project/lop (make-compat-project ".lop"))

(define (make-compat . rest)
  (make:pretend (not (null? rest)))
  (make:make compat-project/fasl "compat.date"
             (lambda (tgt) (error 'make-compat tgt))))

(define (make-petit-compat . rest)
  (make:pretend (not (null? rest)))
  (make:make compat-project/lop "compat.date"
             (lambda (tgt) (error 'make-petit-compat tgt))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building the Auxiliary libraries.

(define auxlib-files
  '("misc" "list" "vector" "string" "pp" "io" "format" "load"
    "osdep-unix" "osdep-win32"))

(define experimental-files
  ; (param-filename 'root "Lib" "Experimental
  ;   '("libapplyhook" "applyhook0" "apropos" "system-stuff"))
  '()
  )

(define debugger-files
  '("debug" "countcalls" "trace" "inspect-cont"))

(define (make-auxlib-project file-type)

  (define (auxfile fn)
    (string-append (nbuild-parameter 'auxiliary)
                   fn
                   file-type))
  (define (debfile fn)
    (string-append (nbuild-parameter 'debugger)
                   fn
                   file-type))

  (let ((auxlib-files
         (map auxfile auxlib-files))
        (debugger-files
         (map debfile debugger-files)))
    (make:project "Auxiliary library"
    `(rules
      (".lop" ".sch" ,make-compile-and-assemble)
      (".lop" ".mal" ,make-assemble)
      (".fasl" ".sch" ,make-compile-file)
      (".fasl" ".mal" ,make-assemble-file))
    `(targets
      ("auxlib.date" ,(lambda args #t)))
    `(dependencies
      ("auxlib.date" ,auxlib-files)
      ("auxlib.date" ,experimental-files)
      ("auxlib.date" ,debugger-files)))))

(define auxlib-project/fasl (make-auxlib-project ".fasl"))
(define auxlib-project/lop (make-auxlib-project ".lop"))

(define (make-auxlib . rest)
  (make:pretend (not (null? rest)))
  (make:make auxlib-project/fasl "auxlib.date"
             (lambda (tgt) (error 'make-auxlib tgt))))

(define (make-petit-auxlib . rest)
  (make:pretend (not (null? rest)))
  (make:make auxlib-project/lop "auxlib.date"
             (lambda (tgt) (error 'make-petit-auxlib tgt))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building the GC test suite.

(define gc-testsuite-project
  (let ((gc-testsuite-files
         '("dynamic" "gcbench0" "gcbench1" "grow" "lattice" "nbody"
           "nboyer" "nucleic2" "permsort" "sboyer" "dummy")))
    (make:project "GC Testsuite"
      `(rules
        (".fasl" ".sch" ,make-compile-file))
      `(targets
        ("gc-testsuite.date" ,(lambda args #t)))
      `(dependencies
        ("gc-testsuite.date"
         ,(objects "Testsuite/GC/" ".fasl" gc-testsuite-files))))))

(define (make-gc-testsuite . rest)
  (make:pretend (not (null? rest)))
  (make:make gc-testsuite-project "gc-testsuite.date"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Regression tests.

(define regression-test-project
  (let ((regression-test-files
         '("test" "bool" "char" "ctak" "dynamic-wind" "fact" "fib" "fixnums"
           "number" "pred" "regression")))
    (make:project "Regression tests"
      `(rules
        (".fasl" ".sch" ,make-compile-file)
        (".fasl" ".mal" ,make-assemble-file))
      `(targets
        ("regression-test.date" ,(lambda args #t)))
      `(dependencies
        ("regression-test.date"
         ,(objects "Testsuite/Lib/" ".fasl" regression-test-files))))))

(define (make-regression-test . rest)
  (make:pretend (not (null? rest)))
  (make:make regression-test-project "regression-test.date"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Rebuild the entire development system, for all targets.

; Compile to FASL

(define (make-development-environment . rest)
  (parameterize ((compat:read-case-sensitive? #t))
    (apply make-auxlib rest)
    (apply make-compiler rest)
    (apply make-sparcasm rest)
    (apply make-compat rest)
    (compile-file (param-filename 'source "makefile.sch"))))

; Compile to LOP

(define (make-petit-development-environment . rest)
  (parameterize ((compat:read-case-sensitive? #t))
    (apply make-petit-auxlib rest)
    (apply make-petit-compiler rest)
    (apply make-petit-petitasm rest)
    (apply make-petit-compat rest)
    (compile-and-assemble313 (param-filename 'source "makefile.sch"))))

(define (petit-development-environment-lop-files)

  (define (fix files)
    (replace-extension ".lop" files))

  (append (list
           (param-filename 'compatibility "compat2.lop")
           (param-filename 'auxiliary "list.lop")
           (param-filename 'auxiliary "pp.lop"))
          (fix (nbuild:twobit-files))
          (fix (nbuild:common-asm-files))
          (fix (nbuild:machine-asm-files))
          (fix (nbuild:heap-dumper-files))
          (fix (nbuild:utility-files))))

(define (make-dotnet-development-environment . rest)
  (parameterize ((compat:read-case-sensitive? #t))
    (apply make-auxlib rest)
    (apply make-compiler rest)
    (apply make-dotnetasm rest)
    (apply make-compat rest)
    (compile-file (param-filename 'source "makefile.sch"))))

; eof
