; Apropos
; April 1, 1998 / lth
;
; Idea from Alexander Taranov <tay@jet.msk.su>
; Should really use regular expressions, but OK for now.

; (apropos substring)
; (apropos substring environment)

(define (apropos substr . rest)
  (let ((env (if (null? rest)
		 (interaction-environment)
		 (car rest))))
    (select (lambda (name)
	      (and (environment-gettable? env name)
		   (cond ((symbol? name)
			  (has-substring? (symbol->string name) substr))
			 ((string? name)
			  (has-substring? name substr)))))
	    (environment-variables env))))

(define (has-substring? s subs)
  (let* ((ls    (string-length s))
	 (lu    (string-length subs))
	 (limit (- ls lu)))
    (let loop ((i 0))
      (cond ((> i limit)
	     #f)
	    ((string=? subs (substring s i (+ i lu)))
	     i)
	    (else (loop (+ i 1)))))))

; This is general and belongs elsewhere.

(define (select wanted? l)
  (cond ((null? l) '())
	((wanted? (car l)) (cons (car l) (select wanted? (cdr l))))
	(else (select wanted? (cdr l)))))

; eof
