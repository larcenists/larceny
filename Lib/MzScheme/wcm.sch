
;; call-with-continuation-mark : value (-> 'a) -> 'a
;; Evaluates thunk with the mark added to the continuation marks.
;; Thunk is not precisely evaluated in tail position, but should
;; be effectively the same. Based on semantics from 
;; "Modeling an Algebraic Stepper", Clements et al.

(define-syntax with-continuation-mark
  (syntax-rules ()
    ((_ key value . body)
     (call-with-continuation-mark key value (lambda () . body)))))

(define (sys$replace-mark-functional key value alist)
  (define (replace key value alist)
    (cond ((null? alist)
           (list (cons key value)))
          ((pair? alist)
           (cond ((eq? key (caar alist))
                  (cons (cons key value) (cdr alist)))
                 (else
                  (cons (car alist) 
                        (replace key value (cdr alist))))))))
  (replace key value alist))

(define (continuation-marks/structure cob)
  (define (loop cob)
    (cond ((and (vector? cob) 
                (eq? (vector-ref cob 2) ;; Slot 0
                     call-with-continuation-mark))
           (cons (vector-ref cob 3)
                 (loop (vector-ref cob 1))))
          ((vector? cob)
           (cons '() (loop (vector-ref cob 1))))
          (else '())))
  (loop cob))

(define (continuation-marks k)
  (continuation-marks/structure
   (sys$continuation-data-structure k)))

(define (current-continuation-marks)
  (continuation-marks/structure
   (current-continuation-structure)))

;; The following doesn't work, probably because
;; call/cc has been redefined to be dynamic-wind-safe call/cc,
;; and no longer has the structure expected by 
;; sys$continuation-data-structure.
;;
;;    (call-with-current-continuation continuation-marks))

;; Initialization

(let ((old-init initialize-larceny-environment-target-specific))
  (set! initialize-larceny-environment-target-specific
        (lambda (larc)
          (environment-set! larc 
                            'call-with-continuation-mark
                            call-with-continuation-mark)
          (environment-set! larc
                            'current-continuation-marks
                            current-continuation-marks)
          (environment-set! larc
                            'continuation-marks
                            continuation-marks)
          (environment-set! larc
                            'continuation-marks/structure
                            continuation-marks/structure)
          (environment-set! larc
                            'sys$continuation-data-structure
                            sys$continuation-data-structure)
          (old-init larc))))
