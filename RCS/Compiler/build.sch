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

(define (loadfile path file)
  (load (string-append path file)))

(display "Larceny develompment environment under Chez scheme")
(newline)
(newline)

(display "Loading Chez Scheme/MacScheme compatibility package...") (newline)
(loadfile chezdir "compat.ss")
(load-foreign (string-append chezdir "bitpattern.o"))
(loadfile chezdir "bytevec.ss")
(loadfile chezdir "misc2bytevector.ss")

(display "Loading make utility...") (newline)
(loadfile compilerdir "make.sch")

(display "Loading compiler proper...") (newline)
(loadfile compilerdir "sets.sch")
(loadfile compilerdir "pass1.imp.sch")
(loadfile compilerdir "pass1.aux.sch")
(loadfile compilerdir "pass1.sch")
(loadfile compilerdir "pass2.aux.sch")
(loadfile compilerdir "pass2.sch")
(loadfile compilerdir "pass4.imp.sch")
(loadfile compilerdir "pass4.aux.sch")
(loadfile compilerdir "pass4p1.sch")
(loadfile compilerdir "pass4p2.sch")

(display "Loading generic assembler...") (newline)
(parameterize ((optimize-level 2))
  (loadfile compilerdir "assembler.sch"))

(display "Loading SPARC header files...") (newline)
(loadfile sparcdir "registers.sch.h")
(loadfile sparcdir "offsets.sch.h")
(loadfile sparcdir "layouts.sch.h")
(loadfile sparcdir "millicode.sch.h")
(loadfile sourcedir "exceptions.sch")

(display "Loading SPARC assembler and code generator...") (newline)
(parameterize ((optimize-level 2))
  (loadfile sparcdir "asm.sparc.sch"))
(loadfile sparcdir "gen-msi.sch")
(loadfile sparcdir "gen-primops.sch")

(display "Loading SPARC disassembler...") (newline)
(loadfile sparcdir "disasm.sparc.sch")

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
(load "/home/systems/lth/lib/rewrite.sch")  ; HACK! FIXME!

(display "Loading makefile...") (newline)
(loadfile sourcedir "newmakefile.sch")

(set! listify? #f)
(display "Listing is off") (newline)
(newline)
