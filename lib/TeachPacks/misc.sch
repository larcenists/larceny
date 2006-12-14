;; Some miscellaneous functions

;; format-string : string string ... -> string
;; formats the string, uses ~a placeholders for the args
(define (format-string str . args)
  (let ((out (open-output-string)))
    (apply format (list* out str args))
    (get-output-string out)))

;; check-arg : sym bool str str TST -> void
;; checks the argument against condition
(define (check-arg pname condition expected arg-posn given)
  (if (not condition)
    (error pname (format-string "expected <~a> as ~a argument, given: ~e"
                                expected 
                                arg-posn 
                                given))))