; 2002-10-02 / lth

; (fluid-let ((v1 e1) ...) b1 b2 ...)
; sets the variables v1 ... to the values of e1 ... in the dynamic scope
; of b1 b2 ... .  usually, v1 ... are global, in which case they must
; already have a value.

(define-syntax fluid-let
  (syntax-rules ()
    ((_ ((v1 e1) ...) b1 b2 ...)
     (fluid-let "temps" () ((v1 e1) ...) b1 b2 ...))
    ((_ "temps" (t ...) ((v1 e1) x ...) b1 b2 ...)
     (let ((temp e1))
       (fluid-let "temps" ((temp e1 v1) t ...) (x ...) b1 b2 ...)))
    ((_ "temps" ((t e v) ...) () b1 b2 ...)
     (let-syntax ((swap!
		   (syntax-rules ()
		     ((swap! a b)
		      (let ((tmp a))
			(set! a b)
			(set! b tmp))))))
       (dynamic-wind
	(lambda ()
	  (swap! t v) ...)
	(lambda ()
	  b1 b2 ...)
	(lambda ()
	  (swap! t v) ...))))))

