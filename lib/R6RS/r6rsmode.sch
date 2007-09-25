; FIXME:  Temporary hack for experimentation.


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

(define (run-r6rs-program filename)
  (larceny:load-r6rs-package)
  (ex:run-r6rs-program filename))

(define (load-r6rs-program filename)
  (larceny:load-r6rs-runtime)
  (load filename))

(define (expand-r6rs-program filename target-filename)
  (ex:expand-file filename target-filename))

