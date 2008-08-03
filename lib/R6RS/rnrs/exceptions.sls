(library (rnrs exceptions (6))

  (export with-exception-handler raise raise-continuable guard)

  (import
   (rnrs base)
   (primitives
    with-exception-handler raise raise-continuable))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; The definitions of guard and guard-aux come from the
  ; reference implementation for SRFI 34, whose copyright
  ; notice is reproduced below.  See
  ; http://srfi.schemers.org/srfi-34/srfi-34.html
  ;
  ; Copyright (C) Richard Kelsey, Michael Sperber (2002). All Rights Reserved.
  ;
  ; Permission is hereby granted, free of charge, to any
  ; person obtaining a copy of this software and associated
  ; documentation files (the "Software"), to deal in the
  ; Software without restriction, including without
  ; limitation the rights to use, copy, modify, merge,
  ; publish, distribute, sublicense, and/or sell copies of
  ; the Software, and to permit persons to whom the Software
  ; is furnished to do so, subject to the following conditions:
  ; 
  ; The above copyright notice and this permission notice
  ; shall be included in all copies or substantial portions
  ; of the Software. 

  ; The original call to raise has been changed to a call to
  ; raise-continuable.  See
  ; http://www.r6rs.org/r6rs-errata.html
  
  (define-syntax guard
    (syntax-rules ()
      ((guard (var clause ...) e1 e2 ...)
       ((call-with-current-continuation
         (lambda (guard-k)
           (with-exception-handler
            (lambda (condition)
              ((call-with-current-continuation
                 (lambda (handler-k)
                   (guard-k
                    (lambda ()
                      (let ((var condition))      ; clauses may SET! var
                        (guard-aux (handler-k (lambda ()
                                                (raise-continuable condition)))
                                   clause ...))))))))
            (lambda ()
              (call-with-values
               (lambda () e1 e2 ...)
               (lambda args
                 (guard-k (lambda ()
                            (apply values args)))))))))))))
  
  (define-syntax guard-aux
    (syntax-rules (else =>)
      ((guard-aux reraise (else result1 result2 ...))
       (begin result1 result2 ...))
      ((guard-aux reraise (test => result))
       (let ((temp test))
         (if temp 
             (result temp)
             reraise)))
      ((guard-aux reraise (test => result) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             (result temp)
             (guard-aux reraise clause1 clause2 ...))))
      ((guard-aux reraise (test))
       test)
      ((guard-aux reraise (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             temp
             (guard-aux reraise clause1 clause2 ...))))
      ((guard-aux reraise (test result1 result2 ...))
       (if test
           (begin result1 result2 ...)
           reraise))
      ((guard-aux reraise (test result1 result2 ...) clause1 clause2 ...)
       (if test
           (begin result1 result2 ...)
           (guard-aux reraise clause1 clause2 ...)))))
  
  ; End of copyrighted extract from the reference implementation
  ; for SRFI 34.
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )

