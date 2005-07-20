; Drop-in replacement for the md5 algorithm that allows one to
; use old versions of Larceny (containing a bug that prevents the
; real md5 implementation from working) to build current Larceny
; sources, since the new build system uses md5 to hash file names.

(define *md5-counter* 100000)
(define *md5-list* '())

(define (md5 x)
  (let ((probe (assoc x *md5-list*)))
    (if probe
	(cdr probe)
	(let ((y (number->string *md5-counter* 16)))
	  (set! *md5-counter* (+ *md5-counter* 1))
	  (set! *md5-list* (cons (cons x y) *md5-list*))
	  y))))

