; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Some Unix functionality imported as foreign function.

; CD and PWD
; Simulate a directory stack in 'dirs' to keep Emacs happy.  HACK!!

(define pwd
  (let ((getcwd (foreign-procedure "getcwd" '(boxed int) 'int)))
    (lambda ()
      (let ((s (make-bytevector 1024)))
	(getcwd s 1024)
	(ffi/asciiz->string s)))))

(define cd
  (let ((chdir (foreign-procedure "chdir" '(string) 'int)))
    (lambda (newdir)
      (if (not (zero? (chdir newdir)))
	  (error "cd: " newdir " is not a valid directory name."))
      ; This supports emacs M-x dirs in a crude way.
      (set! dirs (string->symbol (pwd)))
      (unspecified))))

(define dirs (string->symbol (pwd)))

; Returns the number of seconds since Jan 1, 1970 00:00:00 GMT.
; If the argument is non-#f then it should be a bytevector of length
; at least 4, in which to store the time.  See time(2).

(define unix:time
  (let ((_time (foreign-procedure "time" '(boxed) 'int)))
    (lambda (arg)
      (if (and arg
	       (not (and (bytevector? arg)
			 (>= (bytevector-length arg) 4))))
	  (error "Invalid parameter to unix:time"))
      (_time arg))))

; Format a time stamp in a string.
; Takes a format string or #f (meaning "default format") and a clock buffer
; as filled-in by unix:time (above), and returns a fresh string with the
; formatted time.  For the form of formats, see cftime(3).

(define unix:cftime
  (let ((_cftime (foreign-procedure "cftime" '(boxed string boxed) 'int)))
    (lambda (format clock)
      (let ((buf (make-bytevector 128)))
	(_cftime buf format clock)
	(ffi/asciiz->string buf)))))

(define (unix:current-timestamp)
  (unix:cftime #f (let ((x (make-bytevector 4))) (unix:time x) x)))

; eof
