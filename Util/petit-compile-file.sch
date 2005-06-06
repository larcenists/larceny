;; This file defines a (hackish) implementation of COMPILE-FILE that
;; should work out-of-the-box if the file is loaded when the current
;; directory is the root of the Larceny source tree.  Essentially, it
;; provides a way to get around having to use INSTALL-TWOBIT if you
;; want to use the compiler on source files in a different directory
;; (e.g. the Testsuite)

(define compile-file 
  (let ((larceny-root (current-directory))
        (absolute-path? (lambda (string)
                            (char=? #\/ (string-ref string 0)))))
      (lambda (infilename . rest)
        (let* ((compile-root (current-directory))
               (fix-path
                (lambda (path)
                  (if (absolute-path? path)
                      path
                      (string-append compile-root "/" path))))
               (infilename (fix-path infilename))
               (outfilename 
                (fix-path
                 (if (not (null? rest))
                     (car rest)
                     (rewrite-file-type infilename 
                                        *scheme-file-types*
                                        *fasl-file-type*)))))
          (parameterize ((current-directory larceny-root))
            (compile-files (list infilename) outfilename))))))
