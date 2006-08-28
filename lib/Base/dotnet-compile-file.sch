;; This file defines a (hackish) implementation of COMPILE-FILE that
;; should work out-of-the-box.

;; pass1-block must be modified to deal with new stxenv's
(benchmark-block-mode #f)

;; This is a hack to ensure that we're consistent with the codegen
;; options that the user established in larceny-setup, even when we're
;; loading up Twobit directly; see the crock stuff in Util/dotnet.sch
(for-each set-codegen-option! option:codegen-options)

;; remove-fasl-if-present : string -> string
(define (remove-fasl-if-present s)
  (let* ((len (string-length s))
	 (mid (- len 5))
	 (end len))
    (if (string=? (substring s mid end)
		  ".fasl")
	(substring s 0 mid)
	s)))

;; compile-files : [Listof String] String -> void
(define (compile-files infiles outfile)
  (compile-application (remove-fasl-if-present outfile)
		       infiles))

;; compile-file : String [String] -> void
(define (compile-file infile . rest)
  (let ((outfile
	 (if (null? rest)
	     (rewrite-file-type infile
				*scheme-file-types*
				*fasl-file-type*)
	     (car rest))))
    (compile-files (list infile) outfile)))

