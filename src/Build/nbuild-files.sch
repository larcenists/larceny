; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; File lists for nbuild et al.  Don't rearrange the lists -- order matters.

; Twobit shouldn't redefine Larceny's hash functions and hashtable
; procedures when running under Larceny.

(define (*nbuild:twobit-files-1*)
  (param-filename 'compiler
   (append
    (if (and (string=? (nbuild-parameter 'host-system) "Larceny")
             (not (nbuild-parameter 'development?)))
        '()
        '("hash.sch" "hashtable.sch"))
    `("hashtree.sch" "sets.sch"
      "switches.sch" "pass1.aux.sch" "pass2.aux.sch"
      "prefs.sch" "pass0.sch"
      "syntaxenv.sch" "syntaxrules2.sch" "syntaxrules.sch" "lowlevel.sch"
      "expand.sch" "usual.sch" "pass1.sch"
      "copy.sch" "pass3commoning.aux.sch" "pass3rep.aux.sch"))))

(define *nbuild:sparc/twobit-files*
  (param-filename
   'compiler
   '("common.imp.sch" "sparc.imp.sch" "sparc.imp2.sch")))

(define *nbuild:petit/twobit-files*
  (param-filename
   'compiler
   '("common.imp.sch" "standard-C.imp.sch" "standard-C.imp2.sch")))

(define *nbuild:dotnet/twobit-files*
  (if (eq? 'dotnet (nbuild-parameter 'target-machine))
      (append 
       (param-filename 'compiler
                     '("common.imp.sch"))

       ;; FIXME:  Compiler is going to need to know about
       ;;         Lib/Common/javadot-syntax.sch to implement .javadot macro
       ;; The macro expander handles that, not the compiler proper.

       (param-filename 'dotnet-asm
                       '("il.imp.sch" "il.imp2.sch")))))

(define *nbuild:iasn/twobit-files*
  (param-filename
   'compiler
   '("common.imp.sch" "iasn.imp.sch" "iasn.imp2.sch")))

(define *nbuild:twobit-files-2*
  (param-filename 'compiler
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
      "printlap.sch")))

(define *nbuild:common-asm-be*
  (param-filename
   'common-asm
   '("pass5p1.sch" "asmutil.sch" "asmutil32be.sch" "asmutil32.sch"
     "makefasl.sch" "dumpheap.sch")))

(define *nbuild:common-asm-el*
  (param-filename
   'common-asm
   '("pass5p1.sch" "asmutil.sch" "asmutil32el.sch" "asmutil32.sch"
     "makefasl.sch" "dumpheap.sch")))

(define *nbuild:build-files*
  (param-filename
   'common-source
   `("globals.sch"
     "ecodes.sch"
     "layouts.sch"
     ,@(case *runtime-type* 
        ((sparc-native sassy-native) '("regs.sch"))
        (else '())))))

(define *nbuild:sparcasm-files*
  (if (eq? 'sparc (nbuild-parameter 'target-machine))
      (param-filename 'sparc-asm
                      '("pass5p2.sch" 
                        "peepopt.sch"
                        "sparcutil.sch"
                        "sparcasm.sch"
                        "sparcasm2.sch"
                        "gen-msi.sch"
                        "sparcprim-part1.sch"
                        "sparcprim-part2.sch"
                        "sparcprim-part3a.sch"
                        "sparcprim-part3b.sch"
                        "sparcprim-part4.sch"
                        "switches.sch"
                        "sparcdis.sch"))))

(define *nbuild:petitasm-files*
  (if (eq? 'standard-c (nbuild-parameter 'target-machine))
      (append
       (param-filename 'common-asm
                       '("external-assembler.sch"))
       (param-filename
        'standard-c-asm
        `("pass5p2.sch" 
          "peepopt.sch" 
          "asm-switches.sch" 
          "dumpheap-overrides.sch" 
          "petit-init-proc.sch"
          "md5.sch"
          ,@(case (nbuild-parameter 'host-os)
             ((macosx macosx-el unix cygwin solaris linux-el)
              '("dumpheap-unix.sch"))
             ((win32)
              '("dumpheap-win32.sch"))
             (else
              (error '*nbuild:petitasm-files*
                     "Unknown value for nbuild-parameter 'host-os"))))))
      '()))

(define *nbuild:x86-sass-files*
  (if (eq? 'x86-sass (nbuild-parameter 'target-machine))
      (append
       (param-filename 'common-asm
                       '("external-assembler.sch"))
       (param-filename 'x86-sass-asm
                       `("sassy-machine.sch"
                         "sassy-instr.sch"
                         "sassy-invoke.sch"
                         "pass5p2-sassy.sch"
                         "peepopt.sch"
                         ;;,@(case (nbuild-parameter 'host-os)
                         ;;    ((unix cygwin linux-el)  '("dumpheap-unix.sch"))
                         ;;    ((win32) '("dumpheap-win32.sch"))
                         ;;    (else    '()))
                         "asm-switches.sch"
                         ;;"petit-init-proc.sch"
                         ;;"md5.sch"
                         )))
      '()))

(define *nbuild:x86-nasm-files*
  (if (eq? 'x86-nasm (nbuild-parameter 'target-machine))
      (append
       (param-filename 'common-asm
                       '("external-assembler.sch"))
       (param-filename 'x86-nasm-asm
                       `("pass5p2-nasm.sch"
                         "peepopt.sch"
                         "dumpheap-overrides.sch" 
                         ,@(case (nbuild-parameter 'host-os)
                            ((unix cygwin linux-el) '("dumpheap-unix.sch"))
                            ((win32)                '("dumpheap-win32.sch"))
                            (else                   '()))))
       (param-filename 'standard-c-asm
                      '("asm-switches.sch"
                        "petit-init-proc.sch"
                        "md5.sch")))
      '()))

(define *nbuild:dotnetasm-files* 
  (if (eq? 'dotnet (nbuild-parameter 'target-machine))
      (param-filename 'dotnet-asm
                      '("asm-switches.sch"
                        "config.sch"
                        "util-structs.sch"
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
                        "dumpheap-extra.sch"))))

(define *nbuild:make-files*
  `(,@(param-filename 'util '("make.sch" "compile-tools.sch"))
    ,@(if (nbuild-parameter 'development?)
          (param-filename 'source '("makefile.sch"))
          '())))

(define *nbuild:help-files*
  (param-filename 'compiler '("help.sch")))

(define *nbuild:sparc-heap-dumper-files* 
  '())

(define *nbuild:petit-heap-dumper-files*
  (if (eq? 'standard-c (nbuild-parameter 'target-machine))
      (param-filename 'standard-c-asm '())
      '()))

(define *nbuild:dotnet-heap-dumper-files*
  (if (eq? 'dotnet (nbuild-parameter 'target-machine))
      (param-filename 'dotnet-asm '())))

(define (nbuild:twobit-files)
  (append (*nbuild:twobit-files-1*)

          ; The target-specific tables may need these constants.

          *nbuild:build-files*

          (case (nbuild-parameter 'target-machine)
            ((sparc)      *nbuild:sparc/twobit-files*)
            ((standard-c) *nbuild:petit/twobit-files*)
            ((x86-sass)   *nbuild:iasn/twobit-files*)
            ((x86-nasm)   *nbuild:petit/twobit-files*)  ; for now
            ((dotnet)     *nbuild:dotnet/twobit-files*) ; FIXME
            (else (error "nbuild:twobit-files: bad architecture.")))

          *nbuild:twobit-files-2*))

(define (nbuild:common-asm-files)
  (case (nbuild-parameter 'target-endianness)
    ((big)    ;(append *nbuild:common-asm-be* *nbuild:build-files*)
              *nbuild:common-asm-be*)
    ((little) ;(append *nbuild:common-asm-el* *nbuild:build-files*)
              *nbuild:common-asm-el*)
    (else (error "nbuild:common-asm-files: big endianness."))))

(define (nbuild:machine-asm-files)
  (case (nbuild-parameter 'target-machine)
    ((sparc)      *nbuild:sparcasm-files*)
    ((standard-c) *nbuild:petitasm-files*)
    ((x86-nasm)   *nbuild:x86-nasm-files*)
    ((x86-sass)   *nbuild:x86-sass-files*)
    ((dotnet)     *nbuild:dotnetasm-files*) ; FIXME
    (else (error "nbuild:machine-asm-files: bad architecture."))))

(define (nbuild:heap-dumper-files)
  (case (nbuild-parameter 'target-machine)
    ((sparc)      *nbuild:sparc-heap-dumper-files*)
    ((standard-c) *nbuild:petit-heap-dumper-files*)
    ((x86-sass)   *nbuild:petit-heap-dumper-files*)
    ((x86-nasm)   *nbuild:petit-heap-dumper-files*)
    ((dotnet)     *nbuild:dotnet-heap-dumper-files*) ; FIXME
    (else (error "nbuild:heap-dumper-files: bad architecture."))))

(define (nbuild:utility-files)
  (append *nbuild:make-files* *nbuild:help-files*))

; eof
