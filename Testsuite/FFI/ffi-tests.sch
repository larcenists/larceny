; Make sense for Unix, at least.
; These should work in dumped heaps too.

(define time
  (let ((_time (foreign-procedure "time" '(boxed) 'int)))
    (lambda (buf)
      (_time buf))))

(define x (make-bytevector 4))

(time x)

(define cftime
  (let ((_cftime (foreign-procedure "cftime" '(boxed string boxed) 'int)))
    (lambda (format clock)
      (let ((buf (make-bytevector 128)))
	(_cftime buf format clock)
	(ffi/asciiz->string buf)))))

(cftime #f x)				; "Default" format -- #f for string

(cftime "%c" x)

(cftime "%d-%h-%y %T" x)
