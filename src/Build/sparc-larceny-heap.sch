; Copyright 1998-2004 Lars T Hansen.    -*- mode: scheme; mode: font-lock -*-
;
; $Id$
;
; SPARC Larceny:
; Script for building the full heap image "larceny.heap"
;
; 1  Evaluate (BUILD-LARCENY-FILES) in the development environment
; 2  From the command line run
;        larceny.bin -stopcopy -heap sparc.heap
; 3  Load this script.  It will create larceny.heap.
;
; BUGS:
; - The FFI and record package internals are not hidden.
; - Some files that are no longer in Auxlib and Experimental (they
;   have been moved to an external library) are not loaded

(define ($$trace x) #f)                 ; Some code uses this

(define toplevel-macro-expand #f)       ; A hack for the benefit of 
                                        ; init-toplevel-environment

;;; First load the compiler and seal its namespace.

(let ()
    
  (load "setup.sch")
  (setup 'native
         'string-rep: (cdr (assq 'string-representation (system-features))))
  (load-compiler 'release)
  (load (param-filename 'common-asm "link-lop.fasl"))

  (let ((interaction-environment interaction-environment)
        (compile-expression compile-expression)
        (link-lop-segment link-lop-segment)
        (evaluator evaluator)
        (macro-expand-expression macro-expand-expression))

    (define twobit
      (lambda (expr . rest)
        (let ((env (if (null? rest)
                       (interaction-environment)
                       (car rest))))
          ((link-lop-segment (compile-expression expr env) env)))))

    (evaluator twobit))

  ; Install twobit's macro expander as the interpreter's ditto

  (macro-expander (lambda (form environment)
		    (let ((switches (compiler-switches 'get)))
		      (dynamic-wind
			  (lambda ()
			    (compiler-switches 'standard))
			  (lambda ()
			    (twobit-expand
                             form
                             (environment-syntax-environment environment)))
			  (lambda ()
			    (compiler-switches 'set! switches))))))

  ; Kids, don't try this at home

  (vector-like-set! (interaction-environment) 
		    4
		    (the-usual-syntactic-environment))

  ; Replace and populate the top-level environment
  
  (compat:load (param-filename 'common-source "toplevel.sch"))
  (compat:load (param-filename 'source "Arch" "Sparc" "toplevel-target.sch"))

  (let ((e (interaction-environment)))
    (letrec ((install-procedures 
              (lambda (x procs)
                (if (not (null? procs))
                    (begin
                      (environment-set! x
                                        (car procs)
                                        (environment-get e (car procs)))
                      (install-procedures x (cdr procs)))))))
  
      ; Replace the existing environments with fresh environments
      ; containing the bindings for the standard names; those bindings are 
      ; initialized with values taken from the current interaction
      ; environment.

      (init-toplevel-environment)
      
      (install-procedures (interaction-environment)
                          '(; Compilation

                            compile-file
                            assemble-file
                            compile-expression
                            macro-expand-expression
                            make-assembly-procedure

                            ; On-line help

                            help

                            ; Disassembler

                            disassemble-codevector
                            print-instructions
                            disassemble-file
                            disassemble

                            ; Compiler and assembler switches

                            compiler-switches
                            compiler-flags
                            global-optimization-flags
                            runtime-safety-flags
                            compile-despite-errors
                            issue-warnings
                            include-procedure-names
                            include-source-code
                            include-variable-names
                            single-stepping
                            hygienic-literals
                            avoid-space-leaks
                            write-barrier
                            runtime-safety-checking
                            catch-undefined-globals
                            integrate-procedures
                            faster-arithmetic
                            control-optimization
                            parallel-assignment-optimization
                            lambda-optimization
                            benchmark-mode
                            benchmark-block-mode
                            global-optimization
                            interprocedural-inlining
                            interprocedural-constant-propagation
                            common-subexpression-elimination
                            representation-inference
                            local-optimization
                            peephole-optimization
                            inline-allocation
                            fill-delay-slots

                            ; Temporary to assist this file

                            param-filename
                            compat:load

                            ; Make utility

                            make:project
                            make:new-project
                            make:project?
                            make:rule
                            make:deps
                            make:targets
                            make:pretend
                            make:debug
                            make:make))))


  (unspecified))

;;; Redefine MACRO-EXPAND in the interaction environment.

(define macro-expand
  (lambda (expr . rest)
    (let ((env (if (null? rest)
                   (interaction-environment)
                   (car rest))))
      (macro-expand-expression expr env))))

;;; Helpful procedure for hiding irrelevant names

;;; FIXME: this is unsafe because definitions within the loaded files
;;; will override any previous definitions made at Larceny's top level.
;;; That leads to mysterious bugs when adding new standard procedures
;;; or enhanced versions of previously standard procedures.

(define (load-in-private-namespace files procs)
  (let ((e (environment-copy (interaction-environment))))
    (for-each (lambda (f) (load f e)) files)
    (let ((x (interaction-environment)))
      (do ((procs procs (cdr procs)))
          ((null? procs))
        (environment-set! x
                          (car procs)
                          (environment-get e (car procs)))))))

;;; Load a bunch of useful procedures

(load-in-private-namespace
 (map (lambda (fasl) (param-filename 'auxiliary fasl))
      `("pp.fasl"              ; defines pretty-print, pretty-line-length
        ;; "misc.fasl"         ; defines nothing that matters here
        "list.fasl"            ; defines aremq!, aremv!, aremove!
                               ;     and redefines list-head (FIXME)
                               ;     and defines some stuff needed by load.fasl
        ;; "string.fasl"       ; defines nothing that matters here
        ;; "vector.fasl"       ; defines nothing at all now
        ;; "pp.fasl"           ; was a duplicate
        ;; "io.fasl"           ; was redefining file-newer?

        ;; These two define some stuff needed by load.fasl.

        ,(if (string=? (cdr (assq 'os-name (system-features)))
                       "Win32")
           "osdep-win32.fasl"
           "osdep-unix.fasl")

        "load.fasl"            ; redefines load (deliberately), defines load-*
        ))
 '(;; remq! remv! remove!      ; defined at toplevel (FIXME: but shouldn't be)
   aremq! aremv! aremove!      ; FIXME: what are these? and why?
   ;; filter find              ; defined at toplevel
   reduce reduce-right         ; FIXME: who uses these?
   ;; fold-left fold-right     ; FIXME: are these R6RS-compatible?
   ;; make-list                ; defined at toplevel for R7RS
   ;; vector-copy              ; defined at toplevel for R7RS
   ;; read-line                ; defined at toplevel for R7RS
   pretty-print pretty-line-length
   load load-noisily load-quietly))

;;; Load and install the debugger

(load-in-private-namespace
 (param-filename 'debugger
   '("debug.fasl"
     "inspect-cont.fasl"
     "trace.fasl"))
 '(debug trace trace-entry trace-exit untrace break-entry unbreak 
   trace-entry-printer trace-exit-printer
   install-debugger))

(install-debugger)
(define install-debugger)

;;; Install pretty printer as default printer.

(repl-printer
 (lambda (x port)
   (if (not (eq? x (unspecified)))
       (pretty-print x port))))

;;; Load a bunch of useful things.  
;;; FIXME: Some of these files could usefully be loaded in private namespaces.

(compat:load (param-filename 'auxiliary "macros.sch"))

; The record system is now defined in Lib/Common/record.sch.

;(compat:load (param-filename 'auxiliary "record.sch"))
;(load "Experimental/define-record.sch") ; DEFINE-RECORD syntax
;(load "Experimental/exception.sch")

(compat:load (param-filename 'auxiliary "std-ffi.sch"))
(compat:load (param-filename 'auxiliary "unix-functions.sch"))
(compat:load "lib/Experimental/system-stuff.sch")
;(load "Experimental/applyhook0.fasl")
;(load "Experimental/applyhook.fasl")

;;; Improve some definitions

(define (procedure-documentation-string p)
  (let ((e (procedure-expression p)))
    (if (and (list? e)
             (> (length e) 2)
             (string? (caddr e))
             (not (null? (cdddr e))))
        (caddr e)
        #f)))

;;; Give names to all procedures in the interaction environment.

(let* ((env (interaction-environment))
       (vars (environment-variables env))
       (cells (map (lambda (var) (environment-get-cell env var))
                   vars)))
  (for-each (lambda (var cell)
              (let ((val (.cell-ref cell)))
                (if (and (procedure? val)
                         (not (procedure-name val)))
                    (procedure-name-set! val var))))
            vars
            cells))

;;; Set parameters to their defaults.

(compat:load (param-filename 'auxiliary "defaults.sch"))
(set-parameter-defaults-for-a-standard-heap!)
(set! set-parameter-defaults-for-a-standard-heap! (undefined))

;;; Remove the functions that we only exported to assist on this script

(set! param-filename (undefined))
(set! compat:load (undefined))

;;; Dump the heap

(gctwa)
(dump-interactive-heap "larceny.heap")
(system "./larceny.bin -reorganize-and-dump -heap larceny.heap")
(system "/bin/mv larceny.heap.split larceny.heap")

; eof
