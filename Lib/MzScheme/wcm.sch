
;; call-with-continuation-mark : value (-> 'a) -> 'a
;; Evaluates thunk with the mark added to the continuation marks.
;; Thunk is not precisely evaluated in tail position, but should
;; be effectively the same. Based on semantics from 
;; "Modeling an Algebraic Stepper", Clements et al.

; (define (call-with-continuation-mark mark proc)
;   ...)

(define-syntax with-continuation-mark
  (syntax-rules ()
    ((_ mark . body)
     (call-with-continuation-mark mark (lambda () . body)))))

(define (current-continuation-marks)
  (define (loop cob)
    (cond ((and (vector? cob) 
                (eq? (vector-ref cob 2) ;; Slot 0
                     call-with-continuation-mark))
           (cons (vector-ref cob 3)
                 (loop (vector-ref cob 1))))
          ((vector? cob)
           (loop (vector-ref cob 1)))
          (else '())))
  (call-with-current-continuation
   (lambda (k)
     (loop (sys$continuation-data-structure k)))))
