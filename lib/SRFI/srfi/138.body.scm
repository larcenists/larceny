;;; Given the pathname for an R7RS top-level program,
;;; the pathname for an executable file to be produced,
;;; a list of directories to be searched before the standard directories,
;;; a list of directories to be searched after the standard directories,
;;; and a list of feature identifiers to be recognized by cond-expand,
;;; compiles the program and produces an executable file that will run
;;; the program.
;;;
;;; The first argument is required.  All others are optional.
;;;
;;; The outfile may be #f, in which case a default outfile is constructed
;;; from the name of the pgm or, as a last resort, assumed to be a.out
;;;
;;; Assumes all libraries needed by the program have been pre-compiled,
;;; don't need to be compiled, or are contained within the program file.
;;;
;;; Works only for Unix systems (including Linux, MacOS X).

(define compile-r7rs
  (case-lambda
   ((pgm)
    (compile-r7rs-shared pgm #f '() '() '()))
   ((pgm outfile)
    (compile-r7rs-shared pgm outfile '() '() '()))
   ((pgm outfile dirs)
    (compile-r7rs-shared pgm outfile dirs '() '()))
   ((pgm outfile dirs dirs2)
    (compile-r7rs-shared pgm outfile dirs dirs2 '()))
   ((pgm outfile dirs dirs2 features)
    (compile-r7rs-shared pgm outfile dirs dirs2 features))))

(define (compile-r7rs-shared pgm outfile dirs dirs2 features)
  (let ((pgmfile (name-of-pgmfile pgm))
        (outfile (name-of-outfile pgm outfile)))
    (assert (not (string=? pgmfile "")))
    (assert (not (string=? pgmfile outfile)))
    (parameterize ((current-require-path
                    (append dirs (current-require-path) dirs2))
                   (larceny:current-declared-features
                    (append (larceny:current-declared-features)
                            features)))
     (compile-file pgm pgmfile))
    (write-outfile outfile pgmfile dirs dirs2 features)))

;;; SRFI 138 says the .scm suffix is to be treated specially,
;;; with implementations free to do as they like with other suffixes.

(define recognized-program-suffixes
  '(".scm" ".sps"))

(define (name-of-pgmfile pathname)
  (let* ((suffixes (filter (lambda (suffix)
                             (textual-suffix? suffix pathname))
                           recognized-program-suffixes))
         (suffix (if (null? suffixes) #f (car suffixes))))
    (string-append (if suffix
                       (substring pathname
                                  0
                                  (- (string-length pathname)
                                     (string-length suffix)))
                       pathname)
                   ".slfasl")))

(define (name-of-outfile pgm outfile)
  (let* ((suffixes (filter (lambda (suffix)
                             (textual-suffix? suffix pgm))
                           recognized-program-suffixes))
         (suffix (if (null? suffixes) #f (car suffixes))))
    (cond (outfile outfile)
          (suffix (substring pgm
                             0
                             (- (string-length pgm)
                                (string-length suffix))))
          (else "a.out"))))

(define (write-outfile outfile pgmfile dirs dirs2 features)
  (delete-file outfile)
  (call-with-output-file
   outfile
   (lambda (p)
     (define (write-opts opt things)
       (for-each (lambda (thing)
                   (display opt p)
                   (display " " p)
                   (display thing p)
                   (display " " p))
                 things))
     (display "#!/bin/sh\n\n" p)
     (display "LARCENY=${LARCENY:-\"larceny\"}\n\n" p)
     (display "${LARCENY} --r7rs " p)
     (write-opts "-I" dirs)
     (write-opts "-A" dirs2)
     (write-opts "-D" features)
     (display "--program " p)
     (display pgmfile p)
     (display " -- $@ \n" p)))
  (system (string-append "chmod 775 " outfile)))
