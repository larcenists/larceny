; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny development system -- makefile for compiling Scheme files.
;
; Procedures to call:
;  make-sparc-heap
;  make-petit-heap
;  make-auxlib
;  make-compat
;  make-compiler
;  make-sparcasm
;  make-petitasm
;  make-gc-testsuite
;  make-regression-test
;  make-development-environment
;     makes auxlib, compat, compiler, sparcasm, petitasm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Action procedures for the rules.

(define (make-compile target deps) 
  (display "Compiling ") (display target) (newline)
  (compile313 (car deps) target))

(define (make-assemble target deps)
  (display "Assembling ") (display target) (newline)
  (assemble313 (car deps) target))

(define (make-compile-file target deps)
  (display "Making ") (display target) (newline)
  (compile-file (car deps)))

(define (make-assemble-file target deps)
  (display "Making ") (display target) (newline)
  (assemble-file (car deps)))

(define (make-dumpheap target files)
  (display "Dumping ") (display target) (newline)
  (delete-file target)
  (apply build-heap-image target files))

(define (make-copy target src)
  (display "Copying ") (display target) (newline)
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
    (map (lambda (n)
	   (cond ((string? n)
		  (string-append path n ext))
		 ((symbol? n) 
		  (let ((probe (assq n substitutions)))
		    (if probe
			(cdr probe)
			(error "objects: No substitution found for " n))))
		 (else ???)))
	 files)))

(define (replace-extension ext files)
  (map (lambda (file)
	 (rewrite-file-type file '(".sch" ".h") ext))
       files))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Projects for building the basic heap images.

; These are the base names of all the files that make up the basic heap.

(define common-heap-files
  '(
    ; Fundamental

    "malcode"           ; really basic things
    "typetags"          ; type tags
    "unix"              ; OS primitives for Unix; $$trace procedure.
    "error0"            ; Boot-time 'error' procedure.
    primops             ; primop procedures (to be substituted!)

    ; General library

    "sysparam"		; system parameters
    "struct"            ; structures
    "argv"              ; command line arguments
    "list"              ; list procedures
    "vector"            ; vector procedures
    "string"            ; string and bytevector procs
    "control"           ; control procedures
    "preds"             ; some predicates
    "oblist"            ; symbol table
    "mcode"             ; millicode support
    "memstats"          ; runtime stats
    "ecodes"            ; exception codes
    "ehandler"          ; exception handler
    "error"             ; error/reset system
    "timer"             ; timer interrupts
    "exit"              ; exit procedure; exit/init hooks
    "dump"              ; dump-heap procedure
    "secret"            ; some "hidden" top-level names

    ; New I/O subsystem

    "iosys"             ; basic system
    "fileio"            ; file ports
    "conio"             ; console ports, i.e., terminal
    "stringio"          ; string ports
    "transio"		; transcript ports
    "stdio"             ; user-level procedures
    "print"             ; write/display
    "ioboot"            ; one-time initialization

    "format"            ; `format' procedure.
    "sort"              ; `sort' and `sort!' (used by new macro expander)
    "number"            ; arithmetic
    "globals"           ; `global' offsets (for memstats)

    ; It's important for bellerophon to be loaded as late as possible
    ; because it depends on much of the rest of the system.

    "profile"           ; Profiling code
    "bignums"           ; Bignum support
    "ratnums"           ; Ratnum support
    "rectnums"          ; Rectnum support
    "flonums"           ; Flonum support
    "contag"            ; Contagion
    "num2str"           ; Number printer
    "belle"             ; Algorithm bellerophon
    "str2num"           ; Number parser
    "reader"            ; Reader
    "env"               ; R5RS environments
    "procinfo"          ; Heuristic procedure information
    "load"              ; Loader
    "eval"              ; Eval procedure
    "syshooks"          ; System functions
    "gcctl"             ; Garbage collector policy control
    toplevel            ; top-level environment (to be substituted!)
    "go"                ; Driver

    ))

; Files in the bootstrap evaluator, repl, and toplevel env.

(define eval-files
  '("Repl/reploop"          ; Read-eval-print loop
    "Eval/eval"             ; Simple eval procedure (interpreter)
    "Eval/evalprim"         ; Primitives for interpreter
    "Eval/switches"	    ; Switches for macro expander
    "Compiler/pass1"
    "Compiler/pass1.aux"
    "Compiler/pass2.aux"
    "Compiler/prefs"
    "Compiler/syntaxenv"
    "Compiler/syntaxrules"
    "Compiler/lowlevel"
    "Compiler/expand"
    "Compiler/usual"
    "Eval/macro-expand"     ; Macro expander wrapper
    ))

; Files that hold system constants.

(define build-files
  '("Rts/Build/except.sh"   ; Exception codes (autogenerated)
    "Rts/Build/globals.sh"  ; Global values (autogenerated)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building the sparc-larceny heap image.

(define sparc-heap-project
  (let ((sparc-heap-files 
	 (objects "Lib/Common/" ".lop" common-heap-files
		  '((primops . "Lib/Sparc/primops.lop")
		    (toplevel . "Lib/Sparc/toplevel.lop"))))
	(sparc-eval-files
	 (objects "" ".lop" eval-files)))
    (make:project "sparc.heap"
      `(rules
	(".lop" ".mal" ,make-assemble)
	(".lop" ".lap" ,make-assemble)
	(".lap" ".sch" ,make-compile)
	(".sch" ".sh"  ,make-copy))
      `(targets 
	("sparc.heap" ,make-dumpheap))
      `(dependencies			; Order matters.
	("sparc.heap" ,sparc-heap-files)
	("sparc.heap" ,sparc-eval-files)
	("Lib/Common/ecodes.sch" ("Rts/Build/except.sh"))
	("Lib/Common/globals.sch" ("Rts/Build/globals.sh"))))))
     
(define (make-sparc-heap . rest)
  (make:pretend (not (null? rest)))
  (make:make sparc-heap-project "sparc.heap"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building the petit-larceny heap image.

(define petit-heap-project 
  (let ((petit-heap-files
	 (objects "Lib/Common/" ".lop" common-heap-files
		  '((primops . "Lib/Standard-C/primops.lop")
		    (toplevel . "Lib/Standard-C/toplevel.lop"))))
	(petit-eval-files
	 (objects "" ".lop" eval-files)))
    (make:project "petit.heap"
      `(rules 
	(".lop" ".mal" ,make-assemble)
	(".lop" ".lap" ,make-assemble)
	(".lap" ".sch" ,make-compile)
	(".sch" ".sh"  ,make-copy))
      `(targets
	("petit.heap" ,make-dumpheap))
      `(dependencies			; Order matters.
	("petit.heap" ,petit-heap-files)
	("petit.heap" ,petit-eval-files)
	("Lib/Common/ecodes.sch" ("Rts/Build/except.sh"))
	("Lib/Common/globals.sch" ("Rts/Build/globals.sh"))))))
	
(define (make-petit-heap . rest)
  (make:pretend (not (null? rest)))
  (make:make petit-heap-project "petit.heap"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building all the files in the Compiler subdirectory.

(define compiler-project
  (let ((compiler-files 
	 (replace-extension ".fasl" (nbuild:twobit-files)))
	(comp-util-files 
	 '("make" "make-support" "init-comp" "compile-always")))
    (make:project "compiler.date"
      `(rules
	(".fasl" ".sch" ,make-compile-file))
      `(targets
	("compiler.date" ,(lambda args #t)))
      `(dependencies
	("compiler.date" ,compiler-files)
	("compiler.date" ,(objects "Util/" ".fasl" comp-util-files))))))

(define (make-compiler . rest)
  (make:pretend (not (null? rest)))
  (make:make compiler-project "compiler.date"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building all the files in the new generic assembler and
; the Sparc assembler.

(define sparcasm-project
  (let ((common-asm-files
	 (append
	  (replace-extension ".fasl" (nbuild:common-asm-files))
	  (nbuild-files 'common-asm '("link-lop.fasl"))))
	(sparcasm-files
	 (replace-extension ".fasl" (nbuild:machine-asm-files))))
    (make:project "sparcasm.date"
      `(rules
	(".fasl" ".h" ,make-compile-file)
	(".fasl" ".sch" ,make-compile-file))
      `(targets
	("sparcasm.date" ,(lambda args #t)))
      `(dependencies
	("sparcasm.date" ,common-asm-files)
	("sparcasm.date" ,sparcasm-files)))))

(define (make-sparcasm . rest)
  (make:pretend (not (null? rest)))
  (make:make sparcasm-project "sparcasm.date"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building all the files in the standard-C assembler.

(define petit-asm-project
  (let ((common-asm-files
	 '("pass5p1" "asmutil" "asmutil32" "asmutil32be" "asmutil32el" 
           "link-lop"))
	(petit-asm-files
	 '("pass5p2" "switches" "dumpheap-extra")))
    (make:project "petitasm.date"
      `(rules
	(".fasl" ".sch" ,make-compile-file))
      `(targets
	("petitasm.date" ,(lambda args #t)))
      `(dependencies
	("petitasm.date" ,(objects "Asm/Common/" ".fasl" common-asm-files))
	("petitasm.date" ,(objects "Asm/Standard-C/" ".fasl"
				   petit-asm-files))))))

(define (standard-C-present?)
  (file-exists? "Asm/Standard-C"))

(define (make-petitasm . rest)
  (make:pretend (not (null? rest)))
  (make:make petit-asm-project "petitasm.date"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building the Larceny compatibility files.

(define compat-project
  (make:project "compat.date"
    `(rules
      (".fasl" ".sch" ,make-compile-file))
    `(targets
      ("compat.date" ,(lambda args #t)))
    `(dependencies
      ("compat.date" ("Compat/Larceny/compat2.fasl")))))

(define (make-compat . rest)
  (make:pretend (not (null? rest)))
  (make:make compat-project "compat.date"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Project for building the Auxiliary libraries.

(define auxlib-project 
  (let ((auxlib-files
	 '("misc" "list" "vector" "string" "pp" "io" "format" "load" 
	   "osdep-unix"))
	(experimental-files
	 '("applyhook" "applyhook0" "apropos" "system-stuff"))
	(debugger-files
	 '("debug" "countcalls" "trace" "inspect-cont")))
    (make:project "Auxiliary library"
    `(rules
      (".fasl" ".sch" ,make-compile-file)
      (".fasl" ".mal" ,make-assemble-file))
    `(targets
      ("auxlib.date" ,(lambda args #t)))
    `(dependencies
      ("auxlib.date" ,(objects "Auxlib/" ".fasl" auxlib-files))
      ("auxlib.date" ,(objects "Experimental/" ".fasl" experimental-files))
      ("auxlib.date" ,(objects "Debugger/" ".fasl" debugger-files))))))

(define (make-auxlib . rest)
  (make:pretend (not (null? rest)))
  (make:make auxlib-project "auxlib.date"))


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

(define (make-development-environment . rest)
  (apply make-auxlib rest)
  (apply make-compiler rest)
  (apply make-sparcasm rest)
  (if (standard-C-present?)
      (apply make-petitasm rest))
  (apply make-compat rest)
  (compile-file "Lib/makefile.sch"))

; eof
