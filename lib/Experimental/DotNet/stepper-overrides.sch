;; Will's "hooks" of display-step and really-display-step are not
;; parameterized over *anything*; they're just thunks.
;; 
;; Anyway, the way they work is they extract the state to 
;; render from the car and cadr of the stepping-history,
;; and pass that to a helper, display-configuration, 
;; which itself invokes 
;; configuration->psuedocode on the elements of 
;; stepping-history
;;
;; This file defines a way to make the monothilic interpreter 
;; act like an incremental stepper.

;; A StepFcn is a (-> Boolean);
;; the returned thunk has a side-effect of overwriting
;; really-display-step so that its next invocation will push the
;; computation forward.
;; The return value represents whether there are further steps
;; to take (when it returns #f, then computation has completed)

;; Sexp -> StepFcn
(define make-incremental-stepper
  (let ((really-display-step-orig really-display-step))
    (lambda (initial-program . handle-step-optional)
      (let ((handle-step! (if (null? handle-step-optional) 
                              (lambda (c1 c2) (really-display-step-orig))
                              (car handle-step-optional)))
            (jump-back-into-interpreter #f)
            (jump-back-from-stepper #f))
        (define my-really-display-step
          (lambda () 
            '(begin 
               (write `(override of really-display-step))
               (newline))
            (call-with-current-continuation
             (lambda (back-to-the-interpreter)
               (set! jump-back-into-interpreter
                     (lambda () 
                       (back-to-the-interpreter (unspecified))))
               (handle-step! (cadr stepping-history)
                             (car stepping-history))
               (jump-back-from-stepper)))))
        (define step!
          (lambda ()
            (call-with-current-continuation
             (lambda (back-to-user)
               (set! jump-back-from-stepper 
                     (lambda () 
                       (set! really-display-step really-display-step-orig)
                       (back-to-user #t)))
               (cond
                ((not jump-back-into-interpreter)
                 ;; set up machinery so that displaying a step returns
                 ;; control to the UI
                 (set! really-display-step my-really-display-step)
                 ;; Now with the machinery in place, start the program
                 ;; evaluation.
                 (call-with-values 
                     (lambda () 
                       (interpret-beginning-program initial-program))
                   (lambda args
                     ;; If we ever reach this point, then the 
                     ;; program has been completely evaluated, and 
                     ;; thus there are no further steps to take.
                     (set! really-display-step really-display-step-orig)
                     (back-to-user #f))))
                (else
                 (set! really-display-step my-really-display-step)
                 (let ((val (jump-back-into-interpreter)))
                   ;; I do not think control should ever reach here
                   (display `(whoa the interpreter returned ,val two))
                   (newline)
                   val)))))))
        step!))))

;; Example usage:
(define step! 
  (let ((handle-step!
         (lambda (c1 c2) 
           ;; discards extra return values for textual display
           (begin (write `(,(configuration->pseudocode c1 #t) 
                           ==> 
                           ,(configuration->pseudocode c2 #f))) 
                  (newline)))))
    (make-incremental-stepper '((+ (if (< (+ 1 2) 3) 4 5) 6))
                              handle-step!)))
