;;; SRFI 45: Primitives for expressing iterative lazy algorithms
;;;
;;; $Id$
;;;
;;; Copyright (C) Andre van Tonder (2003). All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the Software), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify,
;;; merge, publish, distribute, sublicense, and/or sell copies of the
;;; Software, and to permit persons to whom the Software is furnished
;;; to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;; Modified in January 2015 by Will Clinger for R7RS and Larceny
;;; by commenting out the "Boxes" section.

;=========================================================================
; Boxes

;(define (box x) (list x))
;(define unbox car)
;(define set-box! set-car!)

;=========================================================================
; Primitives for lazy evaluation:

(define-syntax lazy
  (syntax-rules ()
    ((lazy exp)
     (box (cons 'lazy (lambda () exp))))))

(define (eager x)
  (box (cons 'eager x)))

(define-syntax delay
  (syntax-rules ()
    ((delay exp) (lazy (eager exp)))))

(define (force promise)
  (let ((content (unbox promise)))
    (case (car content)
      ((eager) (cdr content))
      ((lazy)  (let* ((promise* ((cdr content)))        
                      (content  (unbox promise)))                      ; * 
                 (if (not (eqv? (car content) 'eager))                 ; *
                     (begin (set-car! content (car (unbox promise*)))
                            (set-cdr! content (cdr (unbox promise*)))
                            (set-box! promise* content)))
                 (force promise))))))

; (*) These two lines re-fetch and check the original promise in case 
;     the first line of the let* caused it to be forced.  For an example  
;     where this happens, see reentrancy test 3 below.

