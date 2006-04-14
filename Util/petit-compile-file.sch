;; This file defines a (hackish) implementation of COMPILE-FILE that
;; should work out-of-the-box, provided CURRENT-LARCENY-ROOT is correct.
;; Essentially, it provides a way to get around having to use
;; INSTALL-TWOBIT if you want to use the compiler on source files in a
;; different directory (e.g. the Testsuite)

(define compile-files
  (let ((compile-files compile-files))
    (lambda (infiles outfile)
      (let* ((compile-root (current-directory))
             (fix-path
               (lambda (path)
                 (if (absolute-path-string? path)
                   path
                   (make-filename compile-root path))))
             (infiles-fixed (map fix-path infiles))
             (outfile-fixed (fix-path outfile)))
        (parameterize ((current-directory (current-larceny-root)))
          (compile-files infiles-fixed outfile-fixed))))))

(define (compile-file infile . rest)
  (let ((outfile
          (if (null? rest)
            (rewrite-file-type infile
                               *scheme-file-types*
                               *fasl-file-type*)
            (car rest))))
    (compile-files (list infile) outfile)))
