; Util/nbuild.sch
; Loader for Larceny development system; portable!
;
; $Id: nbuild.sch,v 1.6 1997/09/23 20:07:36 lth Exp lth $
;
; All directory names *must* end with "/" (or whatever is appropriate for
; the current operating system), and they should all be absolute (ditto).
;
; This file assumes that the following variables are defined:
;
; * `compilerdir' is the name of the directory that has the compiler files.
;
; * `sparc-olddir' is the name of the directory that has all the
;   files (target-independent and -dependent, both) for the old SPARC 
;   assembler.
;
; * `common-asmdir' is the name of the directory that has all the 
;   target-independent files for the new assembler.
;
; * `sparc-asmdir' is the name of the directory that has all the 
;   SPARC-specific files for the new SPARC assembler.
;
; * `sourcedir' is the name of the directory that has the
;   Larceny makefile.
;
; * `utildir' is the name of the directory that has some utilities, like
;   the make system.
;
; Note: the compatibility package has already been loaded by the build script.

(compat:initialize)

(define (loadfile path file)
  (let ((fn (string-append path file)))
    (compat:load fn)))

(display "Loading make utility...") (newline)
(loadfile utildir "make.sch")

(display "Loading compiler proper...") (newline)
(loadfile compilerdir "sets.sch")
(loadfile compilerdir "switches.sch")
(loadfile compilerdir "pass1.aux.sch")
(loadfile compilerdir "twobit.imp.sch")
(loadfile compilerdir "pass1.sch")
(loadfile compilerdir "pass2.aux.sch")
(loadfile compilerdir "pass2p1.sch")
(loadfile compilerdir "pass2p2.sch")
(loadfile compilerdir "pass4.aux.sch")
(loadfile compilerdir "pass4p1.sch")
(loadfile compilerdir "pass4p2.sch")
(loadfile compilerdir "pass4p3.sch")
(loadfile compilerdir "pass4patch.sch")  ; @@ Lars

(if (not new-assembler?)
    (begin 
      (display "Loading old generic assembler...")
      (newline)
      (with-optimization 2
        (lambda ()
	  (loadfile sparc-olddir "assembler.sch")
	  (loadfile sparc-olddir "peepopt.sch"))))
    (begin
      (display "Loading new generic assembler...") (newline)
      (loadfile common-asmdir "pass5p1.sch")
      (loadfile common-asmdir "asmutil.sch")
      (loadfile common-asmdir "asmutil32be.sch")  ; For now
      (loadfile common-asmdir "asmutil32.sch")))

(display "Loading SPARC header files...") (newline)
(loadfile builddir "schdefs.h")

(if (not new-assembler?)
    (begin
      (display "Loading old SPARC assembler and code generator...") (newline)
      (with-optimization 2
	(lambda ()
	  (loadfile sparc-olddir "sparcasm.sch")))
      (loadfile sparc-olddir "gen-msi.sch")
      (loadfile sparc-olddir "gen-prim.sch")
      (loadfile sparc-olddir "asmutil.sch")
      (loadfile sparc-olddir "switches.sch"))
    (begin 
      (display "Loading new SPARC assembler and code generator...") (newline)
      (loadfile sparc-asmdir "pass5p2.sch")
      (loadfile sparc-asmdir "peepopt.sch")
      (loadfile sparc-asmdir "sparcutil.sch")
      (loadfile sparc-asmdir "sparcasm.sch")
      (loadfile sparc-asmdir "gen-msi.sch")
      (loadfile sparc-asmdir "gen-prim.sch")
      (loadfile sparc-asmdir "switches.sch")))

(if (not new-assembler?)
    (begin 
      (display "Loading old SPARC disassembler...") (newline)
      (loadfile sparc-olddir "sparcdis.sch"))
    (begin
      (display "Loading new SPARC disassembler...") (newline)
      (loadfile sparc-asmdir "sparcdis.sch")
      ))

(display "Loading bootstrap heap dumper...") (newline)
(with-optimization 3
  (lambda ()
    (loadfile common-asmdir "dumpheap.sch")))

(display "Loading drivers and utilities...") (newline)
(loadfile compilerdir "compile313.sch")
(loadfile compilerdir "printlap.sch")
(loadfile compilerdir "utils.sch")
(loadfile common-asmdir "makefasl2.sch")
(loadfile utildir "init-comp.sch")

(display "Loading makefile...") (newline)
(loadfile sourcedir "makefile.sch")

(display "Loading help...") (newline)
(loadfile compilerdir "help.sch")

; The switches can be found in Compiler/switches.sch and 
; Asm/Sparc/switches.sch.
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

  (display-switch "Benchmark-mode" (benchmark-mode))
  (display-switch "Catch-undefined-globals" (catch-undefined-globals))
  (display-switch "Empty-list-is-true" (empty-list-is-true))
  (display-switch "Fill-delay-slots" (fill-delay-slots))
  (display-switch "Generate-global-symbols" (generate-global-symbols))
  (display-switch "Include-procedure-names" (include-procedure-names))
  (display-switch "Include-source-code" (include-source-code))
  (display-switch "Include-variable-names" (include-variable-names))
  (display-switch "Inline-assignment" (inline-assignment))
  (display-switch "Inline-cons" (inline-cons)) 
  (display-switch "Integrate-usual-procedures" (integrate-usual-procedures))
  (display-switch "Issue-warnings" (issue-warnings))
  (display-switch "Listify?" listify?)
  (display-switch "Local-optimizations" (local-optimizations))
  (display-switch "Peephole-optimization" (peephole-optimization))
  (display-switch "Single-stepping" (single-stepping))
  (display-switch "Unsafe-code" (unsafe-code))
  (display-switch "Write-barrier" (write-barrier))

  )

(compat:initialize2)

(display "Welcome. Type (help) for help.")
(newline)

; eof

