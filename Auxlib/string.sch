; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Useful string functions

; (substring-match s1 s2) => i iff 
;     (string=? s2 (substring s1 i (+ i (string-length s2))))
; (substring-match s1 s2) => #f otherwise
; (substring-match s1 s2 k) => i >= k iff
;     (string=? s2 (substring s1 i (+ i (string-length s2))))
; (substring-match s1 s2 k) => #f otherwise

; FIXME: this is a dumb implementation -- it conses more than it
; needs to.  But it's asymptotically the same complexity as one that
; doesn't cons at all :-)  
;
; [Better solution: use a compiled matcher and the Boyer-Moore algorithm.]

(define (substring-match s subs . rest)
  (let* ((start (if (null? rest) 0 (car rest)))
	 (ls    (string-length s))
	 (lu    (string-length subs))
	 (limit (- ls lu)))
    (let loop ((i start))
      (cond ((> i limit)
	     #f)
	    ((string=? subs (substring s i (+ i lu)))
	     i)
	    (else (loop (+ i 1)))))))

; Ditto, but case-insensitive.

(define (substring-match-ci s subs . rest)
  (let* ((start (if (null? rest) 0 (car rest)))
	 (ls    (string-length s))
	 (lu    (string-length subs))
	 (limit (- ls lu)))
    (let loop ((i start))
      (cond ((> i limit)
	     #f)
	    ((string-ci=? subs (substring s i (+ i lu)))
	     i)
	    (else (loop (+ i 1)))))))

(define (->string x)
  (cond ((char? x) (string x))
	((number? x) (number->string x))
	((symbol? x) (symbol->string x))
	((bytevector? x)
	 (let ((y (bytevector-copy x)))
	   (typetag-set! y (typetag ""))
	   y))
	((string? x) x)
	(else
	 (error "->string: Cannot convert " x " to a string."))))

(define (string-insert! target loc src)
  (let ((l (string-length src)))
    (do ((i 0 (+ i 1))
	 (n loc (+ n 1)))
	((= i l) target)
      (string-set! target n (string-ref src i)))))

(define (string-prefix=? s1 s2)
  (and (>= (string-length s1) (string-length s2))
       (string=? (substring s1 0 (string-length s2)) s2)))

(define (string-split s constituent?)
  (let ((limit (string-length s)))
    (let loop ((i 0) (words '()))
      (cond ((>= i limit) 
             (reverse words))
            ((constituent? (string-ref s i))
             (let ((start i))
               (let loop2 ((i (+ i 1)))
                 (if (and (< i limit) (constituent? (string-ref s i)))
                     (loop2 (+ i 1))
                     (loop (+ i 1) (cons (substring s start i) words))))))
            (else
             (loop (+ i 1) words))))))

; eof
