; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny -- compatibility library for Twobit running under Larceny.

(define ($$trace x) #t)

(define host-system 'larceny)

; Temporary?

(define (.check! flag exn . args)
  (if (not flag)
      (apply error "Runtime check exception: " exn args)))

; Temporary until all versions of Larceny have migrated to the new
; environment system.

(call-with-error-handler
 (lambda args
   (set! environment-syntax-environment
         (lambda (env)
           usual-syntactic-environment)))
 (lambda ()
   environment-syntax-environment))

; The compatibility library loads Auxlib if compat:initialize is called
; without arguments.  Compat:load will load fasl files when appropriate.

(define (compat:initialize . rest)
  (if (null? rest)
      (let ((dir (nbuild-parameter 'compatibility))
            (auxlib (nbuild-parameter 'auxiliary)))
	(compat:load (string-append dir "compat2.sch"))
	(compat:load (string-append auxlib "list.sch"))
	;(compat:load (string-append auxlib "pp.sch"))
        )))

(define (with-optimization level thunk) 
  (thunk))

; Calls thunk1, and if thunk1 causes an error to be signalled, calls thunk2.

(define (call-with-error-control thunk1 thunk2) 
  (let ((eh (error-handler)))
    (error-handler (lambda args
		     (error-handler eh)
		     (thunk2)
		     (apply eh args)))
    (thunk1)
    (error-handler eh)))

(define (larc-new-extension fn ext)
  (let* ((l (string-length fn))
	 (x (let loop ((i (- l 1)))
	      (cond ((< i 0) #f)
		    ((char=? (string-ref fn i) #\.) (+ i 1))
		    (else (loop (- i 1)))))))
    (if (not x)
	(string-append fn "." ext)
	(string-append (substring fn 0 x) ext))))

(define (compat:load filename)
  (define (loadit fn)
    (if (nbuild-parameter 'verbose-load?)
	(format #t "~a~%" fn))
    (load fn))
  (if (nbuild-parameter 'always-source?)
      (loadit filename)
      (let ((fn (larc-new-extension filename "fasl")))
	(if (and (file-exists? fn)
		 (compat:file-newer? fn filename))
	    (loadit fn)
	    (loadit filename)))))

(define (compat:file-newer? a b)
  (let* ((ta    (file-modification-time a))
	 (tb    (file-modification-time b))
	 (limit (vector-length ta)))
    (let loop ((i 0))
      (cond ((= i limit)
	     #f)
	    ((= (vector-ref ta i) (vector-ref tb i))
	     (loop (+ i 1)))
	    (else
	     (> (vector-ref ta i) (vector-ref tb i)))))))

; eof
