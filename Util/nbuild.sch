; Util/nbuild.sch
; Loader for Larceny development system; portable!
;
; $Id: nbuild.sch,v 1.3 1997/02/11 21:53:13 lth Exp lth $
;
; This file assumes that the following variables are defined:
;
;  "compilerdir" is the absolute name of the directory which has
;  the compiler files; it ends with a "/".
;
;  "sourcedir" is the absolute name of the directory which has the
;  source files for the libraries as well as the makefile for building
;  the initial heap; it ends with a "/".
;
;  "targetdir" is the absolute name of the directory which has or will have
;  the intermediate and final output files from the compiler and the 
;  assembler; it, too, ends with a "/".
;
;  "sparcdir" is the absolute name of the directory with the sparc specific
;  files; it ends with a "/".
;
;  "hostdir" is the absolute name of the directory which has the 
;  host system support and compatibility files; it ends with a "/".

; Note: the compatibility package has already been loaded by the build script.

(compat:initialize)

(define (loadfile path file)
  (let ((fn (string-append path file)))
    (compat:load fn)))

(display "Loading make utility...") (newline)
(loadfile compilerdir "make.sch")

(display "Loading compiler proper...") (newline)
(loadfile compilerdir "sets.sch")
(loadfile compilerdir "switches.sch")    ; @@ Will
(loadfile compilerdir "pass1.aux.sch")
(loadfile compilerdir "twobit.imp.sch")  ; @@ Will
(loadfile compilerdir "pass1.sch")
(loadfile compilerdir "pass2.aux.sch")
(loadfile compilerdir "pass2p1.sch")     ; @@ Will
(loadfile compilerdir "pass2p2.sch")     ; @@ Will
(loadfile compilerdir "pass4.aux.sch")
(loadfile compilerdir "pass4p1.sch")
(loadfile compilerdir "pass4p2.sch")
(loadfile compilerdir "pass4p3.sch")     ; @@ Will

(display "Loading generic assembler...") (newline)
(with-optimization 2
  (lambda ()
    (loadfile compilerdir "assembler.sch")
    (Loadfile compilerdir "peepopt.sch")))

(display "Loading SPARC header files...") (newline)
(loadfile builddir "schdefs.h")

(display "Loading SPARC assembler and code generator...") (newline)
(with-optimization 2
  (lambda ()
    (loadfile sparcdir "sparcasm.sch")))
(loadfile sparcdir "gen-msi.sch")
(loadfile sparcdir "gen-prim.sch")
(loadfile sparcdir "asmutil.sch")
(loadfile sparcdir "switches.sch")

(display "Loading SPARC disassembler...") (newline)
(loadfile sparcdir "sparcdis.sch")

(display "Loading bootstrap heap dumper...") (newline)
(with-optimization 3
  (lambda ()
    (loadfile compilerdir "dumpheap.sch")))

(display "Loading drivers and utilities...") (newline)
(loadfile compilerdir "compile313.sch")
(loadfile compilerdir "printlap.sch")
(loadfile compilerdir "utils.sch")
(loadfile compilerdir "makefasl.sch")

(display "Loading makefile...") (newline)
(loadfile sourcedir "makefile.sch")

(display "Loading help...") (newline)
(loadfile compilerdir "help.sch")

; The switches can be found in Compiler/switches.sch, Compiler/assembler.sch,
; and Sparcasm/switches.sch.
;
; FIXME: each of the mentioned files should contain a procedure which
; prints its own switches, so this procedure won't have to know.

(issue-warnings #f)
(include-source-code #f)
(include-variable-names #f)

(define (compiler-switches)

  (define (display-switch caption value)
    (display #\tab)
    (display caption)
    (display " is ")
    (display (if value "on" "off"))
    (newline))

  (display "Summary of compiler switches:" ) (newline)

; Compiler switches -- defined in Compiler/switches.sch

  (display-switch "Integrate-usual-procedures"                    ; @@ Will
                  (integrate-usual-procedures))                   ; @@ Will
  (display-switch "Local-optimizations"                           ; @@ Will
                  (local-optimizations))                          ; @@ Will
  (display-switch "Benchmark-mode" (benchmark-mode))
  (display-switch "Issue-warnings" (issue-warnings))
  (display-switch "Include-source-code" (include-source-code))
  (display-switch "Include-variable-names" (include-variable-names))
  (display-switch "Include-procedure-names" (include-procedure-names))
  (display-switch "Empty-list-is-true" (empty-list-is-true))

; Sparc assembler switches -- define in Sparcasm/switches.sch

;  (display-switch "Fast-pop (not always safe)" fast-pop)         ; @@ Will
  (display-switch "Unsafe-code" (unsafe-code))
  (display-switch "Inline-cons" (inline-cons)) 
  (display-switch "Inline-assignment" (inline-assignment))
  (display-switch "Write-barrier" (write-barrier))
  (display-switch "Catch-undefined-globals" (catch-undefined-globals))

; Heap dumper switch -- defined in Compiler/switches.sch

  (display-switch "Generate-global-symbols" 
		  (generate-global-symbols))

; Generic assembler switches -- defined in Compiler/assembler.sch.

  (display-switch "Listify?" listify?)
  (display-switch "Enable-peephole?" enable-peephole?)
  (display-switch "Enable-singlestep?" enable-singlestep?)

; Sparc assembler switch

  (display-switch "Assume-short-distance-to-call (not always safe)" 
		  assume-short-distance-to-call)

  )

(display "Welcome. Type (help) for help.")
(newline)

; eof

