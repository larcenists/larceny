; The posn data type for the beginning student language.
;
; Uses ERR5RS records.

(define rtd:posn (make-rtd 'posn '#((immutable x) (immutable y))))
(define make-posn (rtd-constructor rtd:posn))
(define posn-x (rtd-accessor rtd:posn 'x))
(define posn-y (rtd-accessor rtd:posn 'y))
(define posn? (rtd-predicate rtd:posn))
