; Auxlib/string.sch
; Larceny auxiliary library -- string functions

; $Id$

; (substring-match s1 s2) => i iff 
;     (string=? s2 (substring s1 i (+ i (string-length s2))))
; (substring-match s1 s2) => #f otherwise
; (substring-match s1 s2 k) => i >= k iff
;     (string=? s2 (substring s1 i (+ i (string-length s2))))
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

; eof
