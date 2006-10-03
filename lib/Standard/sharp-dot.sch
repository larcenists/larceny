; #.<expression> syntax
; 2003-11-14 / lth
;
; The #.<expression> is present in Common Lisp, though I have not
; read the spec before implementing it here.
;
; Semantics: #.<expression> reads the <expression>, evaluates it,
; and returns the result of the expression as the datum.

(let* ((sharp-reader (readtable-ref #\#))
       (sharp-class (car sharp-reader))
       (sharp-dispatch (cadr sharp-reader))
       (sharp-dispatch-list (caddr sharp-reader)))

  (define (read-evaluation-prefix p)
    (let ((c (peek-char p)))
      (if (eqv? c #\.)
          (begin
            (read-char p)
	    (if (eof-object? (peek-char p))
		(error "EOF found following #."))
            #t)
          #f)))

  (define (new-sharp-dispatch c p)
    (if (read-evaluation-prefix p)
	(let* ((c (read-char p))
	       (e ((cadr (readtable-ref c)) c p)))
	  (eval e))
        (sharp-dispatch c p)))

  ; The default behavior for sharp-dispatch-list is to call on the
  ; installed non-list sharp-dispatcher, so rely on that.

  (readtable-set! #\# (list sharp-class
                            new-sharp-dispatch
                            sharp-dispatch-list))
  'sharp-dot)

; eof
