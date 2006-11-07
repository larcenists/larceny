; Copyright 2006 Jesse A Tov and Felix S Klock.
; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Operating-system-dependent functionality for Unix.

; A "file name" is a string of length > 0.
; A "directory name" is a string of length > 0.

(define *current-directory-designator* ".")   ; a string
(define *directory-separators* '(#\\ #\/))    ; char list, default first

(define (absolute-pathname? fn)
  (or (and (>= (string-length fn) 2)
           (char=? #\: (string-ref fn 1)))
      (memq (string-ref fn 0) *directory-separators*)))

(define (relative-pathname? fn)
  (not (absolute-pathname? fn)))

(define (make-pathname dir fn)
  (if (= (string-length dir) 0)
      (error "append-directory-and-filename: \"\" is not a directory."))
  (if (= (string-length fn) 0)
      (error "append-directory-and-filename: \"\" is not a file name."))
  (if (memq (string-ref dir (- (string-length dir) 1)) *directory-separators*)
      (string-append dir fn)
      (string-append dir (car *directory-separators*) fn)))

; Experimental, but working, code.

; A pathname-list is a list where the first element is a symbol 
; ("absolute" or "relative") and the rest are strings.  The last element
; is the full file name including type and version number, if any.

(define (string->pathname-list fn)

  ; Unix pathname semantics: ...//... == .../...

  (define (split-pathname fn i start)
    (cond ((= i (string-length fn))
	   (if (= i start)
	       '()
	       (list (substring fn start i))))
	  ((memq (string-ref fn i) *directory-separators*)
	   (if (= i start)
	       (split-pathname fn (+ i 1) (+ i 1))
	       (cons (substring fn start i)
		     (split-pathname fn (+ i 1) (+ i 1)))))
	  (else
	   (split-pathname fn (+ i 1) start))))

  (if (absolute-pathname? fn)
      (cons 'absolute (split-pathname fn 0 0))
      (cons 'relative (split-pathname fn 0 0))))

(define (pathname-list->string l)
  (case (car l)
    ((absolute)
     (fold-left (lambda (x y)
                  (string-append x (car *directory-separators*) y))
                ""
                (cdr l)))
    ((relative)
     (fold-left (lambda (x y)
                  (string-append x (car *directory-separators*) y))
                (cadr l)
                (cddr l)))
    (else
     (error "Illegal keyword " (car l) " in pathname list."))))

; eof
