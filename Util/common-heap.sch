; Common things for standard heap and twobit heap.

; Set up things so that the compiler is used for all evaluation.

(if (and (file-exists? "Util/compile-always.fasl")
	 (file-newer? "Util/compile-always.fasl" "Util/compile-always.sch"))
    (load "Util/compile-always.fasl")
    (load "Util/compile-always.sch"))

	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Things will be compiled from this point on.


(include-source-code #t)
(include-variable-names #t)
(generate-global-symbols #t)
(issue-warnings #f)			;Annoying

(define *optimize-level* 'unknown)

(define (optimize-level . rest)
  (if (null? rest)
      *optimize-level*
      (case (car rest)
	((0) (benchmark-mode #f)
	     (catch-undefined-globals #t)
	     (fill-delay-slots #f)
	     (inline-assignment #f)
	     (inline-allocation #f)
	     (integrate-usual-procedures #f)
	     (local-optimizations #f)
	     (peephole-optimization #f)
	     (unsafe-code #f)
	     (set! *optimize-level* (car rest)))
	; Should really disable lambda lifting at optimize-level 1.
	((1) (benchmark-mode #f)
	     (catch-undefined-globals #t)
	     (fill-delay-slots #t)
	     (inline-assignment #t)
	     (inline-allocation #t)
	     (integrate-usual-procedures #f)
	     (local-optimizations #t)
	     (peephole-optimization #t)
	     (unsafe-code #f)
	     (set! *optimize-level* (car rest)))
	((2) (benchmark-mode #t)
	     (catch-undefined-globals #t)
	     (fill-delay-slots #t)
	     (inline-assignment #t)
	     (inline-allocation #t)
	     (integrate-usual-procedures #t)
	     (local-optimizations #t)
	     (peephole-optimization #t)
	     (unsafe-code #f)
	     (set! *optimize-level* (car rest)))
	((3) (benchmark-mode #t)
	     (catch-undefined-globals #f)
	     (fill-delay-slots #t)
	     (inline-assignment #t)
	     (inline-allocation #t)
	     (integrate-usual-procedures #t)
	     (local-optimizations #t)
	     (peephole-optimization #t)
	     (unsafe-code #t)
	     (set! *optimize-level* (car rest)))
	(else
	 (error "Invalid value for optimize-level")))))

(optimize-level 2)

; Some useful extensions.

(define pp pretty-print)

(define apropos
  (let ((apropos apropos))
    (lambda (x)
      (sort (apropos x) 
	    (lambda (a b)
	      (string<? (symbol->string a) (symbol->string b)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign-Function Interface

(load "Auxlib/std-ffi.sch")

; CD and PWD [and a simulated directory stack to keep Emacs happy.]

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

; More examples, but useful!

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
