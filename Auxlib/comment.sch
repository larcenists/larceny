; More interesting comment facilities.
; 2003-11-14 / lth
;
; The #| ... |# comment is common in many Lisp and Scheme systems.
; The #;<expr> comment is from Chez Scheme version 6
;
; Semantics: either comment is equivalent to exactly one space.

; #| ... |# comments
; One of these comments is replaced by a single whitespace character.
; Nested comments are allowed, to be compatible with SRFI 30

(let* ((sharp-reader (readtable-ref #\#))
       (sharp-class (car sharp-reader))
       (sharp-dispatch (cadr sharp-reader))
       (sharp-dispatch-list (caddr sharp-reader))
       (space-reader (readtable-ref #\space))
       (space-dispatch (cadr space-reader))
       (space-dispatch-list (caddr space-reader)))

  (define (read-comment p)
    (let ((c (peek-char p)))
      (if (eqv? c #\|)
          (begin
            (read-char p)
            (let loop ((c (read-char p)) (level 1))
              (cond ((eof-object? c)
                     (error "EOF in #|...|# comment."))
                    ((char=? c #\|)
                     (let ((c (read-char p)))
                       (if (eqv? c #\#) 
			   (if (= level 1)
			       #t
			       (loop (read-char p) (- level 1)))
                           (loop c level))))
		    ((char=? c #\#)
		     (let ((c (read-char p)))
		       (if (eqv? c #\|)
			   (loop (read-char p) (+ level 1))
			   (loop c))))
                    (else
                     (loop (read-char p) level)))))
          #f)))

  (define (new-sharp-dispatch c p)
    (if (read-comment p)
        (space-dispatch #\space p)
        (sharp-dispatch c p)))

  (define (new-sharp-dispatch-list match c p)
    (if (read-comment p)
        (space-dispatch-list match #\space p)
        (sharp-dispatch-list match c p)))

  (readtable-set! #\# (list sharp-class
                            new-sharp-dispatch
                            new-sharp-dispatch-list))
  'sharp-bar-comments)


; #;<expr> comments

(let* ((sharp-reader (readtable-ref #\#))
       (sharp-class (car sharp-reader))
       (sharp-dispatch (cadr sharp-reader))
       (sharp-dispatch-list (caddr sharp-reader))
       (space-reader (readtable-ref #\space))
       (space-dispatch (cadr space-reader))
       (space-dispatch-list (caddr space-reader)))

  ; This is correct for correct programs and will signal an error 
  ; in incorrect programs.

  (define (read-comment p)
    (let ((c (peek-char p)))
      (if (eqv? c #\;)
          (begin
            (read-char p)
            (if (eof-object? (read p))
                (error "EOF in #; comment."))
            #t)
          #f)))

  (define (new-sharp-dispatch c p)
    (if (read-comment p)
        (space-dispatch #\space p)
        (sharp-dispatch c p)))

  (define (new-sharp-dispatch-list match c p)
    (if (read-comment p)
        (space-dispatch-list match #\space p)
        (sharp-dispatch-list match c p)))

  (readtable-set! #\# (list sharp-class
                            new-sharp-dispatch
                            new-sharp-dispatch-list))
  'sharp-semi-comments)

; eof
