; This needs to be a global feature

; Unix specific

; *Directory-separators* is a list of characters: the valid directory
; separators for the current operating system.

(define *directory-separators* '(#\/))
(define *current-directory-designator* ".")

(define (relative-filename? fn)
  (or (= (string-length fn) 0)
      (not (char=? #\/ (string-ref fn 0)))))


; Utility functions for Auxlib procedures.

(define (string-search-reverse s chars)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) #f)
	  ((memq (string-ref s i) chars) i)
	  (else (loop (- i 1))))))

(define (smallest less? l)
  (cond ((null? l) ???)
	((null? (cdr l)) (car l))
	(else (let loop ((l (cdr l)) (min (car l)))
		(cond ((null? l) min)
		      ((less? (car l) min) (loop (cdr l) (car l)))
		      (else (loop (cdr l) min)))))))

(define (filter pred? l)
  (cond ((null? l) '())
	((pred? (car l)) (cons (car l) (filter pred? (cdr l))))
	(else (filter pred? (cdr l)))))

; eof
