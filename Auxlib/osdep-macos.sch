; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Operating-system-dependent functionality for Macintosh.

; A "file name" is a string of length > 0.
; A "directory name" is a string of length > 0.

(define *current-directory-designator* ":")
(define *directory-separators* '(#\:))

(define (absolute-pathname? fn)

  (define (string-contains? fn c)
    (let loop ((i 0))
      (cond ((= i (string-length fn)) #f)
	    ((char=? (string-ref fn i) c) #t)
	    (else (loop (+ i 1))))))

  (and (string-contains? fn #\:)
       (not (char=? #\: (string-ref fn 0)))))

(define (relative-pathname? fn)
  (not (absolute-pathname? fn)))

(define (make-pathname dir fn)
  (if (= (string-length dir) 0)
      (error "append-directory-and-filename: \"" dir "\" is not a directory."))
  (if (= (string-length fn) 0)
      (error "append-directory-and-filename: \"" fn "\" is not a file name."))
  (if (char=? #\: (string-ref dir (- (string-length dir) 1)))
      (string-append dir fn)
      (string-append dir ":" fn)))

; eof
