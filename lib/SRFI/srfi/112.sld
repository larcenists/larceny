;; SRFI 112: Environment Inquiry
;; Copyright (C) William D Clinger 2015. All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define-library (srfi 112)

  (export
   implementation-name
   implementation-version
   cpu-architecture
   machine-name
   os-name
   os-type ; FIXME: os-type was probably a mistake in the SRFI
   os-version
   )

  (import (scheme base)
          (primitives system-features))
  
  (begin

   (define (get-feature feature-name)
     (let ((probe (assq feature-name (system-features))))
       (if probe
           (cdr probe)
           #f)))

   (define (implementation-name)
     (let ((arch (get-feature 'arch-name)))
       (cond ((string=? arch "IAssassin")
              "Larceny")
             ((string=? arch "Standard-C")
              "Petit Larceny")
             (else arch))))

   (define (implementation-version)
     (let ((major (get-feature 'larceny-major-version))
           (minor (get-feature 'larceny-minor-version)))
       (cond ((and (number? major) (number? minor))
              (string-append (number->string major)
                             "."
                             (number->string minor)))
             ((number? major)
              (number->string major))
             (else #f))))

   (define (cpu-architecture)
     (let ((arch (get-feature 'arch-name)))
       (cond ((string=? arch "IAssassin")
              "IA32")
             (else
              #f))))

   (define (machine-name)
     #f)

   (define (os-name)
     (get-feature 'os-name))

   (define (os-type)
     (get-feature 'os-name))

   (define (os-version)
     (let ((major (get-feature 'os-major-version))
           (minor (get-feature 'os-minor-version)))
       (cond ((and (number? major) (number? minor))
              (string-append (number->string major)
                             "."
                             (number->string minor)))
             ((number? major)
              (number->string major))
             (else #f))))

   ))

; eof
