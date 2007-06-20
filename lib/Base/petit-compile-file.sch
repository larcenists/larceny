;; This file defines a (hackish) implementation of COMPILE-FILE that
;; should work out-of-the-box, provided CURRENT-LARCENY-ROOT is correct.
;; Essentially, it provides a way to get around having to use
;; INSTALL-TWOBIT if you want to use the compiler on source files in a
;; different directory (e.g. the Testsuite)

(define (preserve-reader-state func)
  (lambda args
    ;; FIXME: The reader modes are parameterized here to protect the
    ;; interactive session's modes from changes made while reading the
    ;; compiled file.
    ;; FIXME: This needs to be kept in sync with the preserved
    ;; parameters in src/Lib/Common/load.sch until we adopt a more
    ;; robust solution.
    (parameterize ((recognize-keywords?          (recognize-keywords?))
                   (recognize-javadot-symbols?   (recognize-javadot-symbols?))
                   (read-square-bracket-as-paren (read-square-bracket-as-paren))
                   (case-sensitive?              (case-sensitive?))
                   (read-r6rs-flags?             #t)
                   (read-larceny-weirdness?      (read-larceny-weirdness?))
                   (read-traditional-weirdness?  (read-traditional-weirdness?))
                   (read-mzscheme-weirdness?     (read-mzscheme-weirdness?)))
      (apply func args))))

(define compile-files
  (let ((compile-files compile-files))
    (lambda (infiles outfile . stxenv)
      (let* ((compile-root (current-directory))
             (fix-path
               (lambda (path)
                 (if (absolute-path-string? path)
                   path
                   (make-filename compile-root path))))
             (infiles-fixed (map fix-path infiles))
             (outfile-fixed (fix-path outfile)))
        (parameterize ((current-directory (current-larceny-root)))
          (apply (preserve-reader-state compile-files)
                 infiles-fixed outfile-fixed stxenv))))))

(define (compile-file infile . rest)
  (let ((outfile
          (if (null? rest)
            (rewrite-file-type infile
                               *scheme-file-types*
                               *fasl-file-type*)
            (car rest))))
    (compile-files (list infile) outfile)))
