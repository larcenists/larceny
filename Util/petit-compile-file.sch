;; This file defines a (hackish) implementation of COMPILE-FILE that
;; should work out-of-the-box, provided CURRENT-LARCENY-ROOT is correct.
;; Essentially, it provides a way to get around having to use
;; INSTALL-TWOBIT if you want to use the compiler on source files in a
;; different directory (e.g. the Testsuite)

(define (compile-file infilename . rest)
  (let* ((compile-root (current-directory))
         (fix-path
           (lambda (path)
             (if (absolute-path-string? path)
               path
               (make-filename compile-root path))))
         (infilename-fixed (fix-path infilename))
         (outfilename-fixed
           (fix-path
             (if (not (null? rest))
               (car rest)
               (rewrite-file-type infilename
                                  *scheme-file-types*
                                  *fasl-file-type*)))))
    (parameterize ((current-directory (current-larceny-root)))
                  (compile-files (list infilename-fixed) outfilename-fixed))))
