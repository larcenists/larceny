; Compatibility library for the new Twobit running under Larceny 0.25.
; $Id$


(define host-system 'larceny)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Compatibility library initialization

(define (compat:initialize)
  #t)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Multiple values
; Larceny 0.25 does not have multiple values.

(define (values . x) x)

(define (call-with-values proc receiver)
  (apply receiver (proc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; A well-defined sorting procedure.

(define compat:sort (lambda (list less?) (sort less? list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Loading

; for now; should later prefer .fasl over .sch

;(define (compat:load fn)
;  (display fn) (newline)
;  (load fn))


(define (compat:load filename)
  (let ((fn (larc-new-extension filename "fasl")))
    (if (and (file-exists? fn)
	     (compat:file-newer? fn filename))
	(begin ; (display (format "; ~a~%" cfn))
	       (load fn))
	(load filename))))

(define (compat:file-newer? a b)
  (let ((mta (file-modification-time a))
	(mtb (file-modification-time b)))
    (compat:timestamp-newer? mta mtb)))

(define (compat:timestamp-newer? ta tb)
  (cond ((and (number? ta) (number? tb))
	 (> ta tb))
	((and (vector? ta) (vector? tb))
	 (let ((limit (vector-length ta)))
	   (let loop ((i 0))
	     (cond ((= i limit) #f)
		   ((= (vector-ref ta i) (vector-ref tb i)) (loop (+ i 1)))
		   (else (> (vector-ref ta i) (vector-ref tb i)))))))
	(else
	 (make:error 'newer-than? "Internal error: " ta tb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Input and output
;
; For Chez compatibility, bytevectors are dumped as vectors for now.

(define (write-lop item port)
  (cond ((pair? item)
	 (compat:write-list item port))
	((vector? item) 
	 (write-char #\# port)
	 (compat:write-list (vector->list item) port))
	((bytevector? item)
	 (compat:write-bytevector item port))
	(else
	 (write item port))))

(define (compat:write-list item port)
  (write-char #\( port)
  (let loop ((item item))
    (cond ((null? item)
	   (write-char #\) port))
	  ((and (pair? item)
		(or (null? (cdr item))
		    (pair? (cdr item))))
	   (write-lop (car item) port)
	   (write-char #\space port)
	   (loop (cdr item)))
	  ((pair? item)
	   (write-lop (car item) port)
	   (display " . " port)
	   (write-lop (cdr item) port)
	   (write-char #\) port)))))
	  
(define (compat:write-bytevector-as-vector item port)
  (let ((limit (bytevector-length item)))
    (display "#(" port)
    (do ((i 0 (+ i 1)))
	((= i limit) (write-char #\) port))
      (display (bytevector-ref item i) port)
      (display " " port))))

(define (compat:write-bytevector-as-bytevector item port)
  (let ((limit (bytevector-length item)))
    (write-char #\# port)
    (write-char (integer->char 2) port)
    (write-char #\" port)
    (do ((i 0 (+ i 1)))
	((= i limit) (write-char #\" port))
      (let ((c (integer->char (bytevector-ref item i))))
	(cond ((eq? c #\") (write-char #\\ port))
	      ((eq? c #\\) (write-char #\\ port)))
	(write-char c port)))))

(define compat:write-bytevector compat:write-bytevector-as-bytevector)

; The power of self-hosting ;-)

(define (misc->bytevector x)
  (let ((bv (bytevector-like-copy x)))
    (typetag-set! bv $tag.bytevector-typetag)
    bv))

(define string->bytevector misc->bytevector)
(define flonum->bytevector misc->bytevector)
(define compnum->bytevector misc->bytevector)
(define bignum->bytevector misc->bytevector)

(define (list->bytevector l)
  (let ((b (make-bytevector (length l))))
    (do ((i 0 (+ i 1))
	 (l l (cdr l)))
	((null? l) b)
      (bytevector-set! b i (car l)))))

(define bytevector-word-ref 
  (let ((two^8  (expt 2 8))
	(two^16 (expt 2 16))
	(two^24 (expt 2 24)))
    (lambda (bv i)
      (+ (* (bytevector-ref bv i) two^24)
	 (* (bytevector-ref bv (+ i 1)) two^16)
	 (* (bytevector-ref bv (+ i 2)) two^8)
	 (bytevector-ref bv (+ i 3))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Miscellaneous

(define (roundup8 n)
  (* (quotient (+ n 7) 8) 8))


; eof
