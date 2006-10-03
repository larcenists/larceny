; Test code for lib/string.sch
; 2002-03-18 / lth
;
; FIXME: test substring-match-replace

(require 'string)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(or (eqv? 0 (string-index "abra" #\a)) (fail 'string-index:1))
(or (eqv? 2 (string-index "abra" #\r)) (fail 'string-index:2))
(or (eqv? #f (string-index "abra" #\c)) (fail 'string-index:3))

(or (equal? (string-split "" char?) '())
    (fail 'string-split:1))
(or (equal? (string-split "abc" char?) '("abc"))
    (fail 'string-split:2))
(or (equal? (string-split "a12b34cd56e" char-numeric?) '("12" "34" "56"))
    (fail 'string-split:3))

(or (eqv? (substring-match "abracadabra" "ra") 2)
    (fail 'substring-match:1))
(or (eqv? (substring-match "abracadabra" "ra" 3) 9)
    (fail 'substring-match:2))
(or (eqv? (substring-match "abracadabra" "ra" 10) #f)
    (fail 'substring-match:3))
(or (eqv? (substring-match "abracadabra" "rab") #f)
    (fail 'substring-match:4))

