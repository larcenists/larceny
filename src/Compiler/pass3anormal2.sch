(define (post-simplify-anf L0 T1 E0 E1 free regbindings L2)
  
  (define (return-normally)
    (values (make-call L0 (list E1))
            free
            regbindings))
  
  (return-normally))
