; Copyright 1998 Lars T Hansen.
; 2002-09-28 / lth
;
; (format <port> <format-string> <arg> ...)
;
; <port> can be: an output port 
;                #t, denoting (current-output-port)
;                #f, denoting an output string (that is returned)
;
; The following formatting characters are valid:
;   ~a   - write as with 'display'
;   ~b   - write bytevector elements (decimal)
;   ~B   - write bytevector elements (hexadecimal)
;   ~c   - write character as with 'write-char'
;   ~f   - write number as with 'display', though see below
;   ~x   - write integer in base 16
;   ~o   - write integer in base 8
;   ~s   - write as with 'write', compatible with Chez Scheme
;   ~w   - write as with 'write'
;   ~?   - next two args are another format string and a list of
;          arguments, respectively; recursively invoke format on
;          those arguments.
;   ~%   - newline
;   ~~   - write a ~
;
; ~a and ~w admit a field width specifier
;   - an integer n formats the datum left-justified in a field that 
;     is at least n characters wide, inserting spaces on the right 
;     if necessary
;   - an integer n followed by @ formats the datum right-justified,
;     ditto.
;
; ~f admits a formatting specifier
;   - integers w and d, comma separated, that format the datum
;     left justified in a field at least w characters wide rounded
;     to d decimal places.  w is optional in which case no leading
;     spaces are printed; ,d is optional in which case the number 
;     is formatted as with number->string and an attempt is made to
;     fit it in the field (FIXME: should probably round it to fit,
;     if that's possible.)

(require 'number)

(define (format port-spec format-string . args)
  (let ((port (cond ((output-port? port-spec) 
                     port-spec)
		    ((eq? port-spec #t) 
                     (current-output-port))
                    ((eq? port-spec #f) 
                     (open-output-string))
		    (else 
                     (error "format: not a port: " port-spec))))
	(n    (string-length format-string)))

    (define (format-justified width side obj p)
      (let ((x     (let ((s (open-output-string)))
                     (p obj s)
                     (get-output-string s))))
        (if (< (string-length x) width)
            (let ((padding (make-string (- width (string-length x)) #\space)))
              (if (eq? side 'left)
                  (begin (display x port)
                         (display padding port))
                  (begin (display padding port)
                       (display x port))))
            (display x port))))

    (define (format-error)
      (error "format: error in specifier: " format-string))

    (define (convert-width cs)
      (string->number (list->string (reverse cs))))
            
    (define (get-field-width i)
      (let loop ((cs '()) (i i))
        (if (= i n)
            (format-error)
            (let ((c (string-ref format-string i)))
              (cond ((char-numeric? c)
                     (loop (cons c cs) (+ i 1)))
                    ((memq c '(#\a #\w #\, #\f))
                     (values (convert-width cs) 'left c (+ i 1)))
                    ((char=? c #\@)
                     (if (= (+ i 1) n)
                         (format-error)
                         (let ((c (string-ref format-string (+ i 1))))
                           (cond ((char=? c #\a)
                                  (values (convert-width cs) 'right c (+ i 2)))
                                 ((char=? c #\w)
                                  (values (convert-width cs) 'right c (+ i 2)))
                                 (else
                                  (format-error))))))
                    (else
                     (format-error)))))))

    (define (format-loop i args)
      (cond ((= i n))
	    ((char=? (string-ref format-string i) #\~)
	     (let ((c (string-ref format-string (+ i 1))))
	       (cond ((char=? c #\~)
		      (write-char #\~ port)
		      (format-loop (+ i 2) args))
		     ((char=? c #\%)
		      (newline port)
		      (format-loop (+ i 2) args))
                     ((char=? c #\?)
                      (apply format port (car args) (cadr args))
                      (format-loop (+ i 2) (cddr args)))
		     ((char=? c #\a)
		      (display (car args) port)
		      (format-loop (+ i 2) (cdr args)))
		     ((or (char=? c #\b)
			  (char=? c #\B))
		      (let ((bv    (car args))
			    (radix (if (char=? c #\b) 10 16)))
			(if (not (bytevector? bv))
			    (error "format: not a bytevector: " bv))
			(do ((k 0 (+ k 1)))
			    ((= k (bytevector-length bv)))
                          (if (> k 0)
                              (write-char #\space port))
                          (display (number->string (bytevector-ref bv k) radix)
                                   port)))
                      (format-loop (+ i 2) (cdr args)))
		     ((char=? c #\c)
		      (write-char (car args) port)
		      (format-loop (+ i 2) (cdr args)))
                     ((char=? c #\f)
                      (display (exact->inexact (car args)) port)
                      (format-loop (+ i 2) (cdr args)))
		     ((char=? c #\x)
		      (if (not (integer? (car args)))
			  (error "format: not an integer: " (car args)))
		      (display (number->string (car args) 16) port)
		      (format-loop (+ i 2) (cdr args)))
		     ((char=? c #\o)
		      (if (not (integer? (car args)))
			  (error "format: not an integer: " (car args)))
		      (display (number->string (car args) 8) port)
		      (format-loop (+ i 2) (cdr args)))
		     ((or (char=? c #\w) (char=? c #\s))
		      (write (car args) port)
		      (format-loop (+ i 2) (cdr args)))
                     ((char=? c #\,)
                      (let-values (((width side spec i)
                                    (get-field-width (+ i 2))))
                        (case spec
                          ((#\f) 
                           (display (list (car args) width)) (newline)
                           (display (flonum->string (car args) width) port)
                           (format-loop i (cdr args)))
                          (else 
                           (format-error)))))
                     ((char-numeric? c)
                      (let-values (((width side spec i) 
                                    (get-field-width (+ i 1))))
                        (case spec
                          ((#\a) 
                           (format-justified width side (car args) display)
                           (format-loop i (cdr args)))
                          ((#\w)
                           (format-justified width side (car args) write)
                           (format-loop i (cdr args)))
                          ((#\,)
                           (let-values (((width2 side2 spec2 i2)
                                         (get-field-width i)))
                             (case spec2
                               ((#\f)
                                (format-justified width 'right
                                                  (flonum->string (car args)
                                                                  width2)
                                                  display)
                                (format-loop i2 (cdr args)))
                               (else 
                                (format-error)))))
                          (else
                           (format-error)))))
		     (else
                      (format-error)))))
	    (else
	     (write-char (string-ref format-string i) port)
	     (format-loop (+ i 1) args))))

    (format-loop 0 args)
    (if (not port-spec)
        (get-output-string port)
        (unspecified))))

; eof
