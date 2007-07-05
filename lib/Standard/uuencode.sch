; uuencode and uudecode
;
; For spec see
;   http://www.opengroup.org/onlinepubs/007908799/xcu/uuencode.html

; FIXME: This code has fallen victim to bit rot.
; It needs conversion for both fixnum arithmetic and binary output.

(require 'io)
(require 'string)

; (uudecode input-filename [override-output-filename])

(define (uudecode filename . rest)

  ; Return the code that the char represents.
  (define (mapchar c)
    (cond ((eq? c #\`) 0)
	  (else        (- (char->integer c) 32))))

  (define (decode-string data out)
    (let ((cnt (mapchar (string-ref data 0))))     ; Number of output chars to produce
      (let loop ((cnt cnt) (i 1))
	(if (> cnt 0)
	    (let* ((charsleft (- (string-length data) i))
		   (bits (cond ((>= charsleft 4)
				(logior (lsh (mapchar (string-ref data i)) 18)
					(logior (lsh (mapchar (string-ref data (+ i 1))) 12)
						(logior (lsh (mapchar (string-ref data (+ i 2))) 6)
							(mapchar (string-ref data (+ i 3)))))))
			       ((= charsleft 3)
				(logior (lsh (mapchar (string-ref data i)) 18)
					(logior (lsh (mapchar (string-ref data (+ i 1))) 12)
						(lsh (mapchar (string-ref data (+ i 2))) 6))))
			       ((= charsleft 2)
				(logior (lsh (mapchar (string-ref data i)) 18)
					(lsh (mapchar (string-ref data (+ i 1))) 12)))
			       ((= charsleft 1)
				(lsh (mapchar (string-ref data i)) 18)))))
	      (write-char (integer->char (rshl bits 16)) out)
	      (if (> cnt 1)
		  (begin 
		    (write-char (integer->char (logand (rshl bits 8) 255)) out)
		    (if (> cnt 2)
			(write-char (integer->char (logand bits 255)) out))))
	      (loop (- cnt 3) (+ i 4)))))))

  ; Try to cope with line termination problems.
  ;
  ; It is not entirely robust, but it will work on Mac, Unix, and DOS
  ; if the file comes with CRLF endings: 
  ;   - on Unix, the CR is ignored (trailing junk on the line)
  ;   - on DOS, CRLF is native
  ;   - on Mac, the CR is consumed by read-line and the LF is consumed here.

  (define (get-line in)
    (let ((l (read-line in)))
      (if (eq? #\linefeed (peek-char in))
	  (read-char in))
      l))

  (call-with-input-file filename
    (lambda (in)
      (let ((hdr (read-line in)))
	(if (eof-object? hdr)
	    (error "Missing header"))
	(let ((fields (string-split hdr (lambda (c) (not (char-whitespace? c))))))
	  (if (not (and (= (length fields) 3)
			(string=? (car fields) "begin")))
	      (error "Bogus header: " hdr))
	  (let ((outputfile (if (null? rest)
				(caddr fields)
				(car rest))))
	    (delete-file outputfile)
	    (call-with-binary-output-file outputfile
	      (lambda (out)
		(do ((data (get-line in) (get-line in)))
		    ((string=? data "end"))
		  (decode-string data out))))))))))

; eof
