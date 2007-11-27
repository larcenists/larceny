;;; Copyright (C) Aubrey Jaffer (2004). All Rights Reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the
;;; Software, and to permit persons to whom the Software is furnished
;;; to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modified for Larceny by William D Clinger.
;;;
;;; $Id$
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#!fold-case

(define (software-type)
  (let ((os (cdr (assq 'os-name (system-features)))))
    (cond ((string=? os "Win32")
           'ms-dos)
          (else 'unix))))

;;@ (implementation-vicinity) should be defined to be the pathname of
;;; the directory where any auxillary files to your Scheme
;;; implementation reside.

(define (implementation-vicinity)
  (sub-vicinity (current-larceny-root) ""))

;;@ (library-vicinity) should be defined to be the pathname of the
;;; directory where files of Scheme library functions reside.
;;;
;;; In Larceny, this doesn't really make sense because there are
;;; normally 7 or 8 such directories.  What is intended, I think,
;;; is that (library-vicinity) return the directory in which SLIB
;;; files reside.

(define (library-vicinity)
  (let ((library-path
	 (or

	  ;; Use this getenv if your implementation supports it.

	  (getenv "SCHEME_LIBRARY_PATH")

          ;; Larceny-specific

          (do ((dirs (current-require-path) (cdr dirs)))
              ((or (null? dirs)
                   (let* ((dir (car dirs))
                          (n (string-length dir)))
                     (and (>= n 4)
                          (string-ci=? "slib" (substring dir (- n 4) n)))))
               (if (null? dirs)
                   #f
                   (sub-vicinity (implementation-vicinity)
                                 (car dirs)))))

	  ;; Use this path if your scheme does not support GETENV
	  ;; or if SCHEME_LIBRARY_PATH is not set.

	  (case (software-type)
	    ((UNIX) "/usr/local/lib/slib/")
	    ((VMS) "lib$scheme:")
	    ((MS-DOS) "C:\\SLIB\\")
	    (else "")))))

    library-path))

;;@ (home-vicinity) should return the vicinity of the user's HOME
;;; directory, the directory which typically contains files which
;;; customize a computer environment for a user.

(define (home-vicinity)
  (let ((home (getenv "HOME")))
    (and home
	 (case (software-type)
	   ((UNIX COHERENT MS-DOS)	;V7 unix has a / on HOME
	    (if (eqv? #\/ (string-ref home (+ -1 (string-length home))))
		home
		(string-append home "/")))
	   (else home)))))

;@

(define (in-vicinity . args)
  (apply string-append args))

;@

(define (user-vicinity)
  (case (software-type)
    ((VMS)	"[.]")
    (else	"")))

;@

(define vicinity:suffix?
  (let ((suffi
	 (case (software-type)
	   ((AMIGA)				'(#\: #\/))
	   ((MACOS THINKC)			'(#\:))
	   ((MS-DOS WINDOWS ATARIST OS/2)	'(#\\ #\/))
	   ((NOSVE)				'(#\: #\.))
	   ((UNIX COHERENT PLAN9)		'(#\/))
	   ((VMS)				'(#\: #\]))
	   (else
            (raise-continuable
             (condition (make-warning)
                        (make-who-condition 'software-type)
                        (make-message-condition "unknown software-type")
                        (make-irritants-condition (software-type))))
	    "/"))))
    (lambda (chr) (and (memv chr suffi) #t))))

;@

(define (pathname->vicinity pathname)
  (let loop ((i (- (string-length pathname) 1)))
    (cond ((negative? i) "")
	  ((vicinity:suffix? (string-ref pathname i))
	   (substring pathname 0 (+ i 1)))
	  (else (loop (- i 1))))))

; Larceny does not support program-vicinity except in combination
; with with-load-pathname.  See SRFII 96.

(define *load-pathname* #f)

(define (with-load-pathname path thunk)
  (let ((old-load-pathname *load-pathname*))
    (dynamic-wind
     (lambda () (set! *load-pathname* path))
     thunk
     (lambda () (set! *load-pathname* old-load-pathname)))))

(define (program-vicinity)
  (if *load-pathname*
      (pathname->vicinity *load-pathname*)
      (error 'program-vicinity "called while not within load")))

;@

(define sub-vicinity
  (case (software-type)
    ((VMS) (lambda
	       (vic name)
	     (let ((l (string-length vic)))
	       (if (or (zero? (string-length vic))
		       (not (char=? #\] (string-ref vic (- l 1)))))
		   (string-append vic "[" name "]")
		   (string-append (substring vic 0 (- l 1))
				  "." name "]")))))
    (else (let ((*vicinity-suffix*
		 (case (software-type)
		   ((NOSVE) ".")
		   ((MACOS THINKC) ":")
		   ((MS-DOS WINDOWS ATARIST OS/2) "\\")
		   ((UNIX COHERENT PLAN9 AMIGA) "/"))))
	    (lambda (vic name)
	      (string-append vic name *vicinity-suffix*))))))

;@

(define (make-vicinity pathname) pathname)

