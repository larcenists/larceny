; Copyright 1998-2004 Lars T Hansen.    -*- mode: scheme; mode: font-lock -*-
;
; $Id$
;
; Petit Larceny:
; Script for building the full heap image "petit-larceny.heap"
;
; 1  Evaluate (BUILD-LARCENY-FILES) in the development environment
; 2  From the command line run
;        petit-larceny -stopcopy petit.heap
; 3  Load this script.  It will create petit-larceny.heap.

(load "setup.sch")
(setup)
(load-compiler 'release)

(define ($$trace x) #f)                 ; Some code uses this

(define toplevel-macro-expand #f)       ; A hack for the benefit of 
                                        ; init-toplevel-environment

;;; Need compile-files to find Petit includes
(require 'petit-compile-file)

;;; First load the compiler and seal its namespace.

(let ()
    
  ; Install twobit's macro expander as the interpreter's ditto

  (macro-expander (lambda (form environment)
		    (let ((switches (compiler-switches 'get)))
		      (dynamic-wind
			  (lambda ()
			    (compiler-switches 'standard))
			  (lambda ()
			    (twobit-expand form (environment-syntax-environment environment)))
			  (lambda ()
			    (compiler-switches 'set! switches))))))
  ; Kids, don't try this at home
  (vector-like-set! (interaction-environment) 
		    4
		    (the-usual-syntactic-environment))

  ; Replace and populate the top-level environment
  
  (compat:load (param-filename 'common-source "toplevel.sch"))
  (compat:load (param-filename 'source "Arch" "Standard-C" "toplevel-target.sch"))

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
			    *scheme-file-types*
                            *fasl-file-type*
			    rewrite-file-type
			    compile-files
                            macro-expand-expression
                            ; On-line help
                            help
                            ; Compiler and assembler switches
                            compiler-switches
                            compiler-flags
                            global-optimization-flags
                            runtime-safety-flags
                            issue-warnings
                            include-procedure-names
                            include-source-code
                            include-variable-names
                            single-stepping
                            avoid-space-leaks
                            runtime-safety-checking
                            catch-undefined-globals
                            integrate-procedures
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
			    optimize-c-code
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
                            make:make
			    ; Other
			    pretty-print
			    ))))


  (unspecified))

;;; Redefine MACRO-EXPAND in the interaction environment.

(define macro-expand
  (lambda (expr . rest)
    (let ((env (if (null? rest)
                   (interaction-environment)
                   (car rest))))
      (macro-expand-expression expr env))))

;;; Redefine COMPILE-FILES to pick up the syntactic environment
;;; from the interaction environment

(define compile-files
  (let ((compile-files compile-files))
    (lambda (inputs output)
      (compile-files inputs output (environment-syntax-environment
				    (interaction-environment))))))

;;; Redefine COMPILE-FILE to use the above redefinition of COMPILE-FILES
;;; (the one that we required from petit-compile-file was sealed off with
;;;  the old compile-files definition)
(define (compile-file infile . rest)
  (let ((outfile
          (if (null? rest)
            (rewrite-file-type infile
                               *scheme-file-types*
                               *fasl-file-type*)
            (car rest))))
    (compile-files (list infile) outfile)))

;;; Helpful procedure for hiding irrelevant names

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

'(load-in-private-namespace
  (list
    (param-filename 'auxiliary "pp.fasl")
   ;"Auxlib/misc.fasl"
    (param-filename 'auxiliary "list.fasl")
   ;"Auxlib/string.fasl"
   ;"Auxlib/vector.fasl"
   ;"Auxlib/io.fasl"
   ;"Auxlib/osdep-unix.fasl"
   ;"Auxlib/load.fasl"
   )
 '(remq! remv! remove! aremq! aremv! aremove! filter find make-list 
   reduce reduce-right fold-left fold-right vector-copy read-line
   pretty-print pretty-line-length file-newer? read-line 
   load load-noisily load-quietly))

;;; Load and install the debugger

'(load-in-private-namespace
  (param-filename 'debugger
                  '("debug.fasl"
                    "inspect-cont.fasl"
                    "trace.fasl"))
 '(debug trace trace-entry trace-exit untrace break-entry unbreak 
   install-debugger))

'(install-debugger)
'(define install-debugger)

;;; Install pretty printer as default printer.

(repl-printer
 (lambda (x port)
   (if (not (eq? x (unspecified)))
       (pretty-print x port))))

;;; Load a bunch of useful things.  
;;; FIXME: Some of these files could usefully be loaded in private namespaces.

(load (param-filename 'auxiliary "macros.sch"))
;(load "Auxlib/record.sch")              ; Record package
;(load "Experimental/define-record.sch") ; DEFINE-RECORD syntax
;(load "Experimental/exception.sch")
;(load "Auxlib/std-ffi.sch")
;(load "Auxlib/unix-functions.sch")
;(load "Experimental/system-stuff.fasl")
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

"Remove the functions that we only exported to assist on this script"
(set! param-filename (undefined))
(set! compat:load (undefined))

;;; Dump the heap

;(gctwa)

; Dump a unified heap as petit-std.heap

(dump-interactive-heap "petit-larceny.heap")

; Reorganize and redump the heap and give it the name "petit-std.heap"

(define mv-command)
(define petit-command)

(if (string=? "Win32" (cdr (assq 'os-name (system-features))))
    (begin
      (set! mv-command "rename")
      (set! petit-command ".\\petit-larceny.bin.exe"))
    (begin
      (set! mv-command "mv")
      (set! petit-command "./petit-larceny.bin")))

(system
 (string-append petit-command " -reorganize-and-dump -heap petit-larceny.heap"))
(system
 (string-append mv-command " petit-larceny.heap.split petit-larceny.heap"))

; eof
