; Copyright 2007 William D Clinger
;
; $Id$
;
; Larceny's R6RS-compatible (D'Argo) mode.



; FIXME:  This is for compiling by hand.  It should be moved into
; test/Scripts/package-bin-release.sh
;
; FIXME:  This assumes the current directory is lib/R6RS

(define (larceny:compile-r6rs-runtime)
  (compile-file "r6rs-compat-larceny.sch")
  (compile-file "r6rs-runtime.sch")
  (compile-file "r6rs-expander.sch")
  (load "r6rs-compat-larceny.fasl")
  (load "r6rs-runtime.fasl")
  (load "r6rs-expander.fasl")
  (ex:expand-file "r6rs-standard-libraries.sch" "r6rs-standard-libraries.exp")
  (compile-file "r6rs-standard-libraries.exp" "r6rs-standard-libraries.fasl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (larceny:load-r6rs-runtime)
  (require 'r6rs-compat-larceny)
  (require 'r6rs-runtime)
  (require 'r6rs-standard-libraries))

(define (larceny:load-r6rs-package)
  (require 'r6rs-compat-larceny)
  (require 'r6rs-runtime)
  (require 'r6rs-expander)
  (require 'r6rs-standard-libraries))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Called by interactive-entry-point in Lib/Repl/main.sch

(define (run-r6rs-forms forms)
  (larceny:load-r6rs-package)
  (ex:run-r6rs-sequence forms))

; Called by interactive-entry-point in Lib/Repl/main.sch

(define (run-r6rs-program filename)
  (larceny:load-r6rs-package)
  (ex:run-r6rs-program filename))

; Expands an R6RS program into an R5RS program that can be
; loaded by load-r6rs-program.  The target-filename can be
; compiled before it is loaded.

(define (expand-r6rs-program filename target-filename)
  (larceny:load-r6rs-package)
  (ex:expand-file filename target-filename))

; Loads (thereby running) an expanded R6RS program
; or a .fasl file compiled from an expanded R6RS program.

(define (load-r6rs-program filename)
  (larceny:load-r6rs-runtime)
  (load filename))

