; Probably simplistic.  The ECMAScript spec has a more complete
; definition of how to encode and decode urls and url components.

(define (decode-url s)
  (let loop ((l (string->list s)) (r '()))
    (cond ((null? l)
	   (list->string (reverse r)))
	  ((and (char=? (car l) #\%) 
		(not (null? (cdr l)))
		(char-hexdigit? (cadr l))
		(not (null? (cddr l)))
		(char-hexdigit? (caddr l)))
	   (loop (cdddr l) 
		 (cons (integer->char (+ (* 16 (hexdigit-value (cadr l)))
					 (hexdigit-value (caddr l))))
		       r)))
	  (else
	   (loop (cdr l) (cons (car l) r))))))

(define (char-hexdigit? c)
  (or (char-numeric? c)
      (and (char<=? #\a c) (char<=? c #\f))
      (and (char<=? #\A c) (char<=? c #\F))))

(define (hexdigit-value c)
  (if (char-numeric? c)
      (- (char->integer c) (char->integer #\0))
      (+ 10 (- (char->integer (char-upcase c)) (char->integer #\A)))))

