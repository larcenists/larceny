; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; 23 August 1999 / lth
; 
; Host-independent loader for the development system.
;
; The only parameter to this module is a procedure "nbuild-parameter"
; that accepts a key and returns a value for the key, or #f if the key
; is not known.
;
; The keys used by nbuild directly are
;   source          the directory that contains makefile.sch
;   compiler        the directory for Twobit and the help system
;   common-asm      the directory for the target-independent assembler
;   util            the directory for utilities and the make system
;   build           the directory that contains schdefs.h
;   compatibility   the directory for the compatibility code
;   host-system     the name of the host system
;   target-machine  a symbol that specifies the target architecture
;                      SPARC           Sparc v8  (32-bit, big endian)
;                      Standard-C      ANSI C
;   endianness      endianness of target, either 'big' or 'little'.
;
; If target-machine = SPARC:
;   sparc-asm       the directory the SPARC assembler
;
; If target-machine = Standard-C
;   standard-C-asm  the directory for the standard-C assembler
;
; There might be other keys used by the compatibility packages.

; Sanity check: normally we do not want always-source? to be true,
; so warn if it is.  This decreases the likelihood that a true value
; for this parameter escapes into a release.

(if (nbuild-parameter 'always-source?)
    (begin
      (newline)
      (newline)
      (display "*** WARNING: Loading source code only")
      (newline)
      (newline)))

(load (make-filename (nbuild-parameter 'util) "nbuild-files.sch"))

; File lists and procedures

(define (writeln . x)
  (for-each display x) (newline))

(define (nbuild-load-files files)
  (for-each compat:load files))

(writeln "Loading Twobit.")
(nbuild-load-files (nbuild:twobit-files))

(writeln "Loading the common assembler.")
(nbuild-load-files (nbuild:common-asm-files))

(writeln "Loading " (nbuild-parameter 'target-machine) " machine assembler.")
(nbuild-load-files (nbuild:machine-asm-files))

(writeln "Loading bootstrap heap dumper.")
(nbuild-load-files (nbuild:heap-dumper-files))

(writeln "Loading make utility, makefile, and help.")
(nbuild-load-files (nbuild:utility-files))


; Initialize Twobit and help system.

(compiler-switches 'default)
(compiler-switches 'fast-safe)
(initialize-help (nbuild-parameter 'compiler) 
		 (if (eq? 'sparc (nbuild-parameter 'target-machine))
		     'native
		     'petit))

; Initialize assembler (Nothing yet -- must eventually adjust endianness.)

; Initialize heap dumper.

(dumpheap.set-endianness! (nbuild-parameter 'target-endianness))

; And they're off!

(newline)
(writeln "Welcome. Type (help) for help.")

; eof
