; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Compatibility library for Gambit-C interpreter -- fundamental stuff.
;
; Foreign functions defined in gsi-ffs.scm:
;  double-bits
;  file-modification-time

(set-case-conversion! #t)		; Monocase only
(set-keywords-allowed! #f)		; make :blah no longer magic

(define host-system 'gambit-c)		; Canonical name

(define *gambit-files* '())		; List of _source_ files loaded

(define ($$trace . args) #t)            ; Used in parts of the compiler

(define (compat:initialize)
  (let ((dir (nbuild-parameter 'compatibility)))
    (load (string-append dir "gsi-ffs"))
    (compat:load (string-append dir "compat2.sch"))
    (compat:load (string-append dir "format.sch"))
    (compat:load (string-append dir "../../Auxlib/list.sch"))
    (compat:load (string-append dir "../../Lib/Common/sort.sch"))))

(define (compat:sort list less?) (sort list less?))

(define (compat:load filename)

  (define (larc-new-extension fn ext)
    (let* ((l (string-length fn))
	   (x (let loop ((i (- l 1)))
		(cond ((< i 0) #f)
		      ((char=? (string-ref fn i) #\.) (+ i 1))
		      (else (loop (- i 1)))))))
      (if (not x)
	  (string-append fn "." ext)
	  (string-append (substring fn 0 x) ext))))

  (define (loadit fn)
    (if (nbuild-parameter 'verbose-load?)
	(begin (display fn) (newline)))
    (load fn))

  (let ((new-fn (larc-new-extension filename "o1")))
    (if (and (file-exists? new-fn)
	     (compat:file-newer? new-fn filename)
	     (not (nbuild-parameter 'always-source?)))
	(loadit new-fn)
	(begin (set! *gambit-files* (cons filename *gambit-files*))
	       (loadit filename)))))

; Multiple return values

(define (call-with-values thunk receiver)
  (apply receiver (thunk)))

(define (values . x) x)

; Dynamic-wind placeholder -- ok in the absence of errors.

(define (dynamic-wind before during after)
  (before)
  (let ((x (during)))
    (after)
    x))

; This isn't entirely adequate but it's OK for now.  unwind-protect or
; dynamic-wind would be sufficient but Gambit doesn't have them.

(define (call-with-error-control thunk1 thunk2)
  (thunk1))

; The reason for the hair is twofold: first, compile-file puts the output
; in the current working directory, not in the directory of the source file,
; and second, gsc can pigheadedly only compile files with a .scm suffix.

(define (gambit-compile-file path)
  (let* ((dir (##path-directory path))
	 (file (##path-strip-directory path))
	 (scm  (string-append (##path-strip-extension file) ".scm"))
	 (obj  (string-append (##path-strip-extension file) ".o"))
	 (prelude "'(declare (standard-bindings))'"))
    (let ((cmd (string-append "cd " dir "; "
			      "rm -f " scm "; "           ; delete link
			      "rm -f " obj "* ;"          ; delete old objs
			      "ln -s " file " " scm "; "  ; create link
			      "gsc -dynamic -prelude " prelude " " scm)))
      (display cmd)
      (newline)
      (system cmd))))

(define (gambit-compile-files)
  (for-each gambit-compile-file *gambit-files*))

(define (delete-file filename)
  (system (string-append "rm -f " filename))
  #t)

(define (compat:file-newer? a b)
  (and (file-exists? a)
       (file-exists? b)
       (> (file-modification-time a) (file-modification-time b))))

(define (system command)
  (##shell-command command))

(define (make-parameter name value)
  (lambda v
    (cond ((null? v) value)
	  ((null? (cdr v))
	   (set! value (car v))
	   value)
	  (else
	   (error "Too many params to make-parameter.")))))

(define-macro (parameterize params . body)
  (let* ((ps (map car params))
         (vs (map cadr params))
         (saves (map gensym ps)))
    `(let ,@(map (lambda (s p) `(,s (,p))) saves ps)
       (begin ,@(map (lambda (p v) `(,p ,v)) ps vs))
       (begin ,@ body)
       (begin ,@(map (lambda (p s) `(,p ,s)) ps saves)))))


; eof
