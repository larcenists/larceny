;;; This is a script for compiling the SRFI's of interest.

;;; Note that you should *not* compile any files that contain syntax
;;; defintions that are intended for export, because those do not get
;;; propagated into the FASL file.  Any such SRFI's should first be
;;; factored into two separate files, one for the syntax definitions
;;; and a second for the runtime code.

(load (string-append (current-larceny-root) "/Util/compile-tools.sch"))

(parameterize ((current-directory (string-append (current-larceny-root)
                                                 "/Lib/SRFI")))
  (let ((c (lambda (f . reqs) 
             (begin (display `(compiling ,f)) (newline))
             (compile-file/requiring f reqs))))

    (c "srfi-1.sch"  'srfi-0 'srfi-8)
    (c "srfi-56.sch" )
    (c "srfi-60.sch" )
    (c "srfi-69.sch" 'srfi-9)

    ))
