; Reader syntax for square brackets as synonyms for parentheses.
; 2000-05-25 / lth

(let* ((lparen-reader (readtable-ref #\())
       (rparen-reader (readtable-ref #\))))
  (readtable-set! #\[ lparen-reader)
  (readtable-set! #\] rparen-reader)
  'brackets)

; eof

