;; $Id$
;; Lists exported names. Make sure you also put the files in 
;; Lib/makefile.scm in the dotnet-mzscheme-files list.

(define-syntax export
  (syntax-rules ()
    ((_ name ...)
     (let ((old initialize-larceny-environment-target-specific))
       (set! initialize-larceny-environment-target-specific
             (lambda (larc)
               (environment-set! larc 'name name) ...
               (old larc)))))))

;; Exports

;; continuation marks
(export call-with-continuation-mark
        current-continuation-marks
        continuation-marks
        continuation-marks/structure
        continuation-mark-set->list)

;; structs
(export)

