; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; File lists for nbuild et al.  Don't rearrange the lists -- order matters.

(define (nbuild-files path-ident files)
  (let ((path (nbuild-parameter path-ident)))
    (if path
	(map (lambda (fn)
	       (string-append path fn))
	     files)
	'())))

(define *nbuild:twobit-files-1*
  (nbuild-files 'compiler
    `("sets.sch" "hash.sch" "hashtable.sch" "hashtree.sch"
      "switches.sch" "pass1.aux.sch" "pass2.aux.sch"
      "prefs.sch" "syntaxenv.sch" "syntaxrules.sch" "lowlevel.sch"
      "expand.sch" "usual.sch"
      "pass1.sch"
      "copy.sch" "pass3commoning.aux.sch" "pass3rep.aux.sch")))

(define *nbuild:sparc/twobit-files*
  (nbuild-files 'compiler
		'("common.imp.sch" "sparc.imp.sch" "sparc.imp2.sch")))

(define *nbuild:petit/twobit-files*
  (nbuild-files 'compiler
		'("common.imp.sch" "standard-C.imp.sch" "standard-C.imp2.sch")))

(define *nbuild:dotnet/twobit-files*
  (append 
   (nbuild-files 'compiler
                 '("common.imp.sch"))
   ;;; FIXME:  Compiler is going to need to know about
   ;;;         Lib/Common/javadot-syntax.sch to implement .javadot macro
   (nbuild-files 'dotnet-asm
                 '("il.imp.sch" "il.imp2.sch"))))

(define *nbuild:twobit-files-2*
  (nbuild-files 'compiler
    `("pass2p1.sch" "pass2p2.sch" "pass2if.sch"
      "pass3callgraph.sch" "pass3inlining.sch" "pass3folding.sch"
      "pass3anormal.sch" "pass3anormal2.sch" "pass3commoning.sch"
      "pass3rep.sch" "pass3.sch"
      "pass4.aux.sch" "pass4p1.sch" "pass4p2.sch" "pass4let.sch"
      "pass4special.sch"
      "pass4p3.sch"
      "driver-common.sch"
      ,(if (nbuild-parameter 'development?)
           "driver-twobit.sch"
           "driver-larceny.sch")
      "printlap.sch"
      )))

(define *nbuild:common-asm-be*
  (nbuild-files 'common-asm
		'("pass5p1.sch" "asmutil.sch" "asmutil32be.sch" "asmutil32.sch"
		  "makefasl.sch" "dumpheap.sch")))

(define *nbuild:common-asm-el*
  (nbuild-files 'common-asm
		'("pass5p1.sch" "asmutil.sch" "asmutil32el.sch" "asmutil32.sch"
                  "makefasl.sch" "dumpheap.sch")))

(define *nbuild:build-files*
  (nbuild-files 'build '("schdefs.h")))

(define *nbuild:sparcasm-files*
  (nbuild-files 'sparc-asm
		'("pass5p2.sch" "peepopt.sch" "sparcutil.sch" "sparcasm.sch"
                  "sparcasm2.sch"
		  "gen-msi.sch" "sparcprim-part1.sch" "sparcprim-part2.sch"
		  "sparcprim-part3a.sch" "sparcprim-part3b.sch"
                  "sparcprim-part4.sch"
                  "switches.sch" "sparcdis.sch")))

(define *nbuild:petitasm-files*
  (append
   (nbuild-files 'common-asm
		 '("external-assembler.sch"))
   (nbuild-files 'standard-C-asm
		 `("pass5p2.sch" 
		   "peepopt.sch" 
		   "asm-switches.sch" 
		   "dumpheap-overrides.sch" 
		   "petit-init-proc.sch"
		   "md5.sch"
		   ,@(case (nbuild-parameter 'host-os)
		       ((macosx unix) '("dumpheap-unix.sch"))
		       ((win32)       '("dumpheap-win32.sch"))
		       (else          '()))))))

(define *nbuild:x86-nasm-files*
  (append
   (nbuild-files 'common-asm
		 '("external-assembler.sch"))
   (nbuild-files 'x86-nasm-asm
		 `("pass5p2-nasm.sch"
		   "peepopt.sch"
		   "dumpheap-overrides.sch" 
		   ,@(case (nbuild-parameter 'host-os)
		      ((unix)  '("dumpheap-unix.sch"))
		      ((win32) '("dumpheap-win32.sch"))
		      (else    '()))))
   (nbuild-files 'standard-C-asm
		 '("asm-switches.sch"
		   "petit-init-proc.sch"
		   "md5.sch"))))

(define *nbuild:dotnetasm-files* 
  (nbuild-files 'dotnet-asm
                '("asm-switches.sch"
                  "config.sch"
                  "util.sch"
                  "il-gen.sch"
                  "il-rtif.sch"
                  "pass5p2.sch"
                  "pass5p2-instructions.sch"
                  "pass5p2-ops.sch"
                  "pass5p2-listify.sch"
                  "peepopt.sch"
                  "il-src2string.sch"
                  "il-sourcefile.sch"
                  "dumpheap-il.sch"
                  "dumpheap-extra.sch")))

(define *nbuild:make-files*
  `(,@(nbuild-files 'util '("make.sch"))
    ,@(if (nbuild-parameter 'development?)
          (nbuild-files 'source '("makefile.sch"))
          '())))

(define *nbuild:help-files*
  (nbuild-files 'compiler '("help.sch")))

(define *nbuild:sparc-heap-dumper-files* 
  '())

(define *nbuild:petit-heap-dumper-files*
  (nbuild-files 'standard-C-asm '()))

(define *nbuild:dotnet-heap-dumper-files*
  (nbuild-files 'dotnet-asm '()))

(define (nbuild:twobit-files)
  (append *nbuild:twobit-files-1*
          ; The target-specific tables may need these constants.
          *nbuild:build-files*
	  (case (nbuild-parameter 'target-machine)
	    ((SPARC)      *nbuild:sparc/twobit-files*)
	    ((Standard-C) *nbuild:petit/twobit-files*)
	    ((x86-nasm)   *nbuild:petit/twobit-files*)  ; for now
            ((dotnet)     *nbuild:dotnet/twobit-files*) ; FIXME
	    (else (error "nbuild:twobit-files: bad architecture.")))
	  *nbuild:twobit-files-2*))

(define (nbuild:common-asm-files)
  (case (nbuild-parameter 'endianness)
    ((big)    ;(append *nbuild:common-asm-be* *nbuild:build-files*)
              *nbuild:common-asm-be*)
    ((little) ;(append *nbuild:common-asm-el* *nbuild:build-files*)
              *nbuild:common-asm-el*)
    (else (error "nbuild:common-asm-files: big endianness."))))

(define (nbuild:machine-asm-files)
  (case (nbuild-parameter 'target-machine)
    ((SPARC)      *nbuild:sparcasm-files*)
    ((Standard-C) *nbuild:petitasm-files*)
    ((x86-nasm)   *nbuild:x86-nasm-files*)
    ((dotnet)     *nbuild:dotnetasm-files*) ; FIXME
    (else (error "nbuild:machine-asm-files: bad architecture."))))

(define (nbuild:heap-dumper-files)
  (case (nbuild-parameter 'target-machine)
    ((SPARC)      *nbuild:sparc-heap-dumper-files*)
    ((standard-C) *nbuild:petit-heap-dumper-files*)
    ((x86-nasm)   *nbuild:petit-heap-dumper-files*)
    ((dotnet)     *nbuild:dotnet-heap-dumper-files*) ; FIXME
    (else (error "nbuild:heap-dumper-files: bad architecture."))))

(define (nbuild:utility-files)
  (append *nbuild:make-files* *nbuild:help-files*))

; eof
