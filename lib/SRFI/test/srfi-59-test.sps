; Test suite for SRFI-59
;
; $Id$

(import (rnrs base)
        (rnrs io simple)
        (rnrs exceptions)
        (srfi :59 vicinities))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define vicinity? string?)

(or (call-with-current-continuation
     (lambda (k)
       (guard (c (#t (k #t)))
         (or (vicinity? (program-vicinity))
             #f))))
    (fail 'program-vicinity))

(or (vicinity? (library-vicinity))
    (fail 'library-vicinity))

(or (vicinity? (implementation-vicinity))
    (fail 'implementation-vicinity))

(or (vicinity? (user-vicinity))
    (fail 'user-vicinity))

(or (vicinity? (home-vicinity))
    (fail 'home-vicinity))

(or (string? (in-vicinity (home-vicinity) "foo.txt"))
    (fail 'in-vicinity))

(or (vicinity? (sub-vicinity (home-vicinity) "subsystem"))
    (fail 'sub-vicinity))

(or (vicinity? (make-vicinity "foo/bar/"))
    (fail 'make-vicinity))

(or (vicinity? (pathname->vicinity "/usr/local/lib/scm/Link.scm"))
    (fail 'pathname->vicinity))

(or (and (vicinity:suffix? #\/)
         (not (vicinity:suffix? #\a)))
    (fail 'vicinity:suffix?))

#;
(for-each (lambda (x) (write x) (newline))
          (list ;(program-vicinity)
                (library-vicinity)
                (implementation-vicinity)
                (user-vicinity)
                (home-vicinity)
                (in-vicinity (home-vicinity) "foo.txt")
                (sub-vicinity (make-vicinity "foo/bar/") "subsystem")
                (make-vicinity "foo/bar/")
                (pathname->vicinity "/usr/local/lib/scm/Link.scm")
                (vicinity:suffix? #\/)
                (vicinity:suffix? #\a)))

(writeln "Done.")
