; Very Unix
(define *petit-executable-src-name* "petit-larceny.c")
(define *petit-executable-obj-name* "petit-larceny.o")
(define *petit-executable-name* "petit-larceny")

(define *petit-library-path* "Rts/")
(define *petit-heap-library-name* "petit-larceny.so")
(define *petit-rts-libraries* 
  (list (string-append *petit-library-path* "libpetit.so")))
(define *petit-executable-libraries* 
  (append *petit-rts-libraries* 
          (list (string-append *petit-library-path* "petit-larceny.so"))))

; Build an _application_: an executable that contains additional object
; files to load, and a set of FASL files that can be loaded to link 
; the object code to Scheme procedures.
;
; FIXME: The use of ".exe" and ".o" here are pretty arbitrary and not
; right on all (most) platforms; must parameterize.  The use of .exe
; is OK on both Unix and Mac, however, as rewrite-file-type also matches
; on the empty extension.

(define (build-application executable-name additional-files)
  (let ((src-name (rewrite-file-type executable-name '(".exe") ".c"))
        (obj-name (rewrite-file-type executable-name '(".exe") (obj-suffix))))
    (init-variables)
    (for-each create-loadable-file additional-files)
    (dump-loadable-thunks src-name)
    (c-compile-file src-name obj-name)
    (c-link-executable executable-name
                       (cons obj-name
                             (map (lambda (x)
                                    (rewrite-file-type x ".lop" (obj-suffix)))
                                  additional-files))
                       *petit-executable-libraries*)
    executable-name))

; Link all the files in Lib, Repl, Eval, and the macro expander
; with HEAPDATA.o and create the library with the requested name.

(define (build-petit-library library-name input-file-names)
  (c-link-library library-name
		  (remove-duplicates
		   (append (map (lambda (x)
				  (rewrite-file-type x ".lop" (obj-suffix)))
				input-file-names)
			   (list (rewrite-file-type *temp-file* ".c" (obj-suffix))))
		   string=?)
		  *petit-rts-libraries*))

(define (build-petit-lareny heap output-file-name input-file-names)
  (build-petit-library *petit-heap-library-name* input-file-names)
  (build-application *petit-executable-name* '()))
