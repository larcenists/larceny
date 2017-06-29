;;; Copyright (C) William D Clinger (2017).
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

(define-library (srfi 145)
  (export assume)
  (import (scheme base)
          (only (rnrs base) assert))

  (begin

   (define-syntax assume
     (syntax-rules ()
      ((_ expr msg1 ...)
       (let ((x expr))
         (if (not x)
             (assumption-failed 'expr
                                (lambda () (list msg1 ...))))
         (assert x)
         x))))

   (define (assumption-failed expr thunk)
     (let* ((info (guard (c (#t c))
                   (thunk))))
       (if (error-object? info)
           (apply error
                  "assumption failed, and then raised an exception: "
                  expr
                  (error-object-message info)
                  (error-object-irritants info))
           (apply error
                  "assumption failed: "
                  expr
                  info))))))
