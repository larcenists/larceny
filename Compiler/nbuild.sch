;; -*- Scheme -*-
;;
;; Load the heap with the Scheme 313 compiler; Chez Scheme version.
;; This file assumes that the following variables are defined:
;;
;;  "compilerdir" is the absolute name of the directory which has
;;  the compiler files; it ends with a "/".
;;
;;  "sourcedir" is the absolute name of the directory which has the
;;  source files for the libraries as well as the makefile for building
;;  the initial heap; it ends with a "/".
;;
;;  "targetdir" is the absolute name of the directory which has or will have
;;  the intermediate and final output files from the compiler and the 
;;  assembler; it, too, ends with a "/".
;;
;;  "sparcdir" is the absolute name of the directory with the sparc specific
;;  files; it ends with a "/".
;;
;;  "chezdir" is the absolute name of the directory which has the chez
;;  Scheme support and compatibility files; it ends with a "/".

(print-vector-length #f)          ; a curse on Dybvig!

(define (loadfile path file)
  (load (string-append path file)))

(display "Larceny development environment under Chez scheme")
(newline)
(newline)

(display "Loading Chez Scheme/MacScheme compatibility package...") (newline)
(loadfile chezdir "compat.ss")
(load-foreign (string-append chezdir "bitpattern.o"))
(loadfile chezdir "bytevec.ss")
(loadfile chezdir "misc2bytevector.ss")
(if (not (bound? 'values))
    (loadfile chezdir "values.ss"))
(display "Loading make utility...") (newline)
(loadfile compilerdir "make.sch")

(display "Loading compiler proper...") (newline)
(loadfile compilerdir "sets.sch")
(loadfile compilerdir "switches.sch")    ; @@ Will
;(loadfile compilerdir "pass1.imp.sch")  ; @@ Will
(loadfile compilerdir "pass1.aux.sch")
(loadfile compilerdir "twobit.imp.sch")  ; @@ Will
(loadfile compilerdir "pass1.sch")
(loadfile compilerdir "pass2.aux.sch")
(loadfile compilerdir "pass2p1.sch")     ; @@ Will
(loadfile compilerdir "pass2p2.sch")     ; @@ Will
;(loadfile compilerdir "pass4.imp.sch")  ; @@ Will
(loadfile compilerdir "pass4.aux.sch")
(loadfile compilerdir "pass4p1.sch")
(loadfile compilerdir "pass4p2.sch")
(loadfile compilerdir "pass4p3.sch")     ; @@ Will

(display "Loading generic assembler...") (newline)
(parameterize ((optimize-level 2))
  (loadfile compilerdir "assembler.sch")
  (Loadfile compilerdir "peepopt.sch"))

(display "Loading SPARC header files...") (newline)
(loadfile builddir "schdefs.h")

(display "Loading SPARC assembler and code generator...") (newline)
(parameterize ((optimize-level 2))
  (loadfile sparcdir "sparcasm.sch"))
(loadfile sparcdir "gen-msi.sch")
(loadfile sparcdir "gen-prim.sch")
;Patches have been folded into the code 950729 / lth
;(loadfile compilerdir "patches.sch")     ; @@ Will

(display "Loading SPARC disassembler...") (newline)
(loadfile sparcdir "sparcdis.sch")

(display "Loading bootstrap heap dumper...") (newline)
(parameterize ((optimize-level 3))
  (loadfile compilerdir "dumpheap.sch"))

(display "Loading drivers and utilities...") (newline)
(loadfile compilerdir "compile313.sch")
(loadfile compilerdir "printlap.sch")
(loadfile compilerdir "utils.sch")
(loadfile compilerdir "makefasl.sch")

;; The next two do magic things for the top level compilation.

(loadfile compilerdir "expand313.sch")
(loadfile compilerdir "rewrite.sch")  ; HACK! FIXME!

(display "Loading makefile...") (newline)
(loadfile sourcedir "makefile.sch")

(display "Loading help...") (newline)
(loadfile compilerdir "help.sch")

; Compiler switches!

(define unsafe-mode #f)         ; turn off checking on a lot of primitives
(define inline-cons #f)         ; inline CONS 
; @@ Will -- fast-pop is obsolete (see patches.sch)
;(define fast-pop #f)            ; assumes no spill frames
(define inline-assignment #f)   ; inline gen. check in assignments
(define assume-short-distance-to-call #f)  ; faster SAVE/SETRTN
(set!   emit-undef-check? #t)   ; undefined globals are caught
(set!   listify? #f)            ; generate listing; not very useful any more
(set!   generate-global-symbols? #t)  ; each cell has the name

; In addition, there are other switches:
;   the nullary procedure benchmark-mode is defined in Chez/compat.ss
;   register-transactions-for-side-effects is def'd in Sparc/gen-primops.sch
;   enable-peephole? is defined in Compiler/assembler.sch
;   enable-singlestep? is defined in Compiler/assembler.sch
;   emit-undef-check? is defined in Compiler/assembler.sch
;   listify? is defined in Compiler/assembler.sch
;   generate-global-symbols? is defined in Compiler/dumpheap.sch

(define (compiler-switches)

  (define (display-switch caption value)
    (display #\tab)
    (display caption)
    (display " is ")
    (display (if value "on" "off"))
    (newline))

  (display "Summary of compiler switches:" ) (newline)
  (display-switch "Integrate-usual-procedures"                    ; @@ Will
                  (integrate-usual-procedures))                   ; @@ Will
  (display-switch "Local-optimizations"                           ; @@ Will
                  (local-optimizations))                          ; @@ Will
  (display-switch "Benchmark-mode" (benchmark-mode))
  (display-switch "Unsafe-mode" unsafe-mode)
  (display-switch "Inline-cons" inline-cons) 
  (display-switch "Listify?" listify?)
;  (display-switch "Fast-pop (not always safe)" fast-pop)         ; @@ Will
  (display-switch "Inline-assignment" inline-assignment)
  (display-switch "Register-transactions-for-side-effects"
		  register-transactions-for-side-effects)
  (display-switch "Enable-peephole?" enable-peephole?)
  (display-switch "Enable-singlestep?" enable-singlestep?)
  (display-switch "Assume-short-distance-to-call (not always safe)" 
		  assume-short-distance-to-call)
  (display-switch "Emit-undef-check?" emit-undef-check?)
  (display-switch "Generate-global-symbols?" generate-global-symbols?))

;(compiler-switches)
;(newline)
(display "Welcome. Type (help) for help.")
(newline)

(define (roundup8 n)
  (* (quotient (+ n 7) 8) 8))

;; eof

