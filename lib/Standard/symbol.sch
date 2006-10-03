; |...| read syntax for symbols.
; 2002-03-18 / lth
;
; The initial | acts as a separator, so ab|cde| reads as 
; "ab" followed by "cde".

(let* ((x (readtable-ref #\$))
       (y (readtable-ref #\())
       (separator-class (car y))
       (symbol-list-reader (caddr x)))

  (define (bar-reader c p)
    (let loop ((c (read-char p)) (l '()))
      (cond ((eof-object? c)
             (error "EOF in |...| symbol."))
            ((char=? c #\|)
             (string->symbol (list->string (reverse l))))
            ((char=? c #\\)
             (let ((c (read-char p)))
               (cond ((eof-object? c)
                      (error "EOF in |...| symbol."))
                     (else
                      (loop (read-char p) (cons c l))))))
            (else
             (loop (read-char p) (cons c l))))))

  (readtable-set! #\| (list separator-class
			    bar-reader
			    symbol-list-reader))
  'bar-reader)

; eof
