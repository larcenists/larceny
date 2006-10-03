; Tests for lib/generator.sch
; 2000-07-20 / lth

(require 'generator)
(require 'control)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(or (equal? (let ((g (make-generator
                      (lambda (return)
                        37))))
              (g))
            37)
    (fail 'generator:1))

(or (equal? (let ((g (make-generator
                      (lambda (return)
                        (for-each return '(1 2 3))
                        'done))))
              (call-with-accumulator
               (lambda (acc)
                 (acc (g))
                 (acc (g))
                 (acc (g))
                 (acc (g)))))
            '(1 2 3 done))
    (fail 'generator:2))

(or (equal? (call-with-accumulator
             (lambda (acc)
               (generators-for-each (lambda xs (acc xs)) 
                                    not
                                    (generate-list '(1 2 3) #f)
                                    (generate-list '(4 5 6) #f))))
            '((1 4) (2 5) (3 6)))
    (fail 'generators-for-each:1))

(or (equal? (generators-map (lambda xs xs) 
                            not
                            (generate-list '(1 2 3) #f)
                            (generate-list '(4 5 6) #f))
            '((1 4) (2 5) (3 6)))
    (fail 'generators-map:1))

    
; eof
