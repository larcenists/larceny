;;; Copyright (c) 2010-2014 PLT Design Inc.
;;; 
;;; This package is distributed under the GNU Lesser General Public
;;; License (LGPL).  This means that you can link this package into proprietary
;;; applications, provided you follow the rules stated in the LGPL.  You
;;; can also modify this package; if you distribute a modified version,
;;; you must distribute it under the terms of the LGPL, which in
;;; particular means that you must release the source code for the
;;; modified software.  See http://www.gnu.org/copyleft/lesser.html
;;; for more information.

;;; Copy of test/R7RS/Lib/tests/scheme/test.sld,
;;; which is derived from Racket's R6RS test suite.

(define-library (larceny r7rs test)
  (export test
          test/approx
          test/alts
          test/exn
          test/values
          test/output
          test/unspec
          test/unspec-or-exn
          test/unspec-flonum-or-exn
          test/output/unspec
          run-test
          report-test-results

          ;; FIXME: this is a hack

          &assertion
          &implementation-restriction
          &who
          &message
          &irritants
          &error
          &syntax
          &violation
          &non-continuable

          condition-message
          condition-who
          condition-irritants
          who-condition?
          error?
          )

  (import (scheme base)
          (scheme cxr)
          (scheme write))

  (cond-expand
   ((library (scheme inexact))
    (import (scheme inexact)))
   ((not (library (scheme inexact)))
    (begin (define (infinite? x) (= x (/ x 2.0)))
           (define (nan? x) (not (= x x))))))

  (cond-expand
   ((library (scheme complex))
    (import (scheme complex)))
   ((not (library (scheme complex)))
    (begin (define (magnitude x) (abs x))
           (define (real-part x) x)
           (define (imag-part x) 0))))

  (cond-expand
   ((library (scheme file))
    (import (scheme file)))
   ((not (library (scheme file)))
    (begin (define (delete-file fname) #t)
           (define (file-exists? fname) #t)
           (define (call-with-input-file fname proc)
             (proc (current-input-port)))
           (define (with-output-to-file fname thunk) (thunk)))))

  (begin

   ;; Fake condition system; see test/exn and test/unspec-or-exn.

   (define &assertion '&assertion)
   (define &implementation-restriction '&implementation-restriction)
   (define &who '&who)
   (define &message '&message)
   (define &irritants '&irritants)
   (define &error '&error)
   (define &syntax '&syntax)
   (define &violation '&violation)
   (define &non-continuable '&non-continuable)

   (define condition-message   error-object-message)
   (define condition-who       error-object-message)
   (define condition-irritants error-object-irritants)
   (define who-condition?      error-object?)
   (define error?              error-object?)

   )

  (begin

   ;; Good enough for this file.

   (define (for-all f xs . others)
     (cond ((null? xs)
            #t)
           ((apply f (car xs) (map car others))
            (apply for-all f (cdr xs) (map cdr others)))
           (else
            #f)))

   (define (exists f xs . others)
     (cond ((null? xs)
            #f)
           ((apply f (car xs) (map car others))
            #t)
           (else
            (apply exists f (cdr xs) (map cdr others)))))

   (define (get-string-n p n)
     (let loop ((chars '())
                (i 0))
       (if (= i n)
           (list->string (reverse chars))
           (let ((c (read-char p)))
             (if (char? c)
                 (loop (cons c chars)
                       (+ i 1))
                 (loop chars n))))))

   (define-record-type multiple-results
     (make-multiple-results values)
     multiple-results?
     (values multiple-results-values))

   (define-record-type approx
     (make-approx value)
     approx?
     (value approx-value))

   (define-record-type alts (make-alts values) alts?
     (values alts-values))

   (define-syntax test
     (syntax-rules ()
       ((_ expr expected)
        (begin
         ;; (write 'expr) (newline)
         (run-test 'expr
                   (catch-exns (lambda () expr))
                   expected)))))

   (define (catch-exns thunk)
     (guard (c (#t &error))
      (call-with-values thunk
       (lambda x
         (if (= 1 (length x))
             (car x)
             (make-multiple-results x))))))

   (define-syntax test/approx
     (syntax-rules ()
      ((_ expr expected)
       (run-test 'expr
                 (make-approx expr)
                 (make-approx expected)))))

   (define-syntax test/alts
     (syntax-rules ()
      ((_ expr expected0 expected ...)
       (run-test 'expr
                 expr
                 (make-alts (list expected0 expected ...))))))

   (define (good-enough? x y)
     ;; relative error should be with 0.1%, but greater
     ;; relative error is allowed when the expected value
     ;; is near zero.
     (cond ((not (number? x)) #f)
           ((not (number? y)) #f)
           ((or (not (real? x))
                (not (real? y)))
            (and (good-enough? (real-part x) (real-part y))
                 (good-enough? (imag-part x) (imag-part y))))
           ((infinite? x)
            (= x (* 2.0 y)))
           ((infinite? y)
            (= (* 2.0 x) y))
           ((nan? y)
            (nan? x))
           ((> (magnitude y) 1e-6)
            (< (/ (magnitude (- x y))
                  (magnitude y))
               1e-3))
           (else
            (< (magnitude (- x y)) 1e-6))))

   ;; FIXME

   (define-syntax test/exn
     (syntax-rules ()
      ((_ expr condition)
       (test (guard (c (#t
                        condition))
                    expr)
             condition))))

   (define-syntax test/values
     (syntax-rules ()
      ((_ expr val ...)
       (run-test 'expr
                 (catch-exns (lambda () expr))
                 (make-multiple-results (list val ...))))))

   (define-syntax test/output
     (syntax-rules ()
      ((_ expr expected str)
       (run-test 'expr
                 (capture-output
                  (lambda ()
                    (run-test 'expr
                              (guard (c (#t &error))
                                     expr)
                              expected)))
                 str))))

   (define-syntax test/unspec
     (syntax-rules ()
      ((_ expr)
       (test (begin expr 'unspec) 'unspec))))

   ;; FIXME

   (define-syntax test/unspec-or-exn
     (syntax-rules ()
      ((_ expr condition)
       (test (guard (c (#t
                        'unspec))
                    (begin expr 'unspec))
             'unspec))))

   ;; FIXME

   (define-syntax test/unspec-flonum-or-exn
     (syntax-rules ()
      ((_ expr condition)
       (test (guard (c (#t
                        'unspec-or-flonum))
                     (let ((v expr))
                       (if (and (number? v)
                                (inexact? v)
                                (real? v))
                           'unspec-or-flonum
                           (if (eq? v 'unspec-or-flonum)
                               (list v)
                               v))))
             'unspec-or-flonum))))

   (define-syntax test/output/unspec
     (syntax-rules ()
      ((_ expr str)
       (test/output (begin expr 'unspec) 'unspec str))))

   (define checked 0)
   (define failures '())

   (define (capture-output thunk)
     (if (file-exists? "tmp-catch-out")
         (delete-file "tmp-catch-out"))
     (dynamic-wind
      (lambda () 'nothing)
      (lambda ()
        (with-output-to-file "tmp-catch-out"
         thunk)
        (call-with-input-file "tmp-catch-out"
         (lambda (p)
           (get-string-n p 1024))))
      (lambda ()
        (if (file-exists? "tmp-catch-out")
            (delete-file "tmp-catch-out")))))
  
   (define (same-result? got expected)
     (cond
      ((and (real? expected) (nan? expected))
       (and (real? got) (nan? got)))
      ((approx? expected)
       (and (approx? got)
            (good-enough? (approx-value expected)
                          (approx-value got))))
      ((multiple-results? expected)
       (and (multiple-results? got)
            (= (length (multiple-results-values expected))
               (length (multiple-results-values got)))
            (for-all same-result?
                     (multiple-results-values expected)
                     (multiple-results-values got))))
      ((alts? expected)
       (exists (lambda (e) (same-result? got e))
               (alts-values expected)))
      (else (equal? got expected))))
    
   (define (run-test expr got expected)
     ;; (write expr) (newline)
     (set! checked (+ 1 checked))
     (unless (same-result? got expected)
      (set! failures
            (cons (list expr got expected)
                  failures))))

   (define (write-result prefix v)
     (cond
      ((multiple-results? v)
       (for-each (lambda (v)
                   (write-result prefix v))
                 (multiple-results-values v)))
      ((approx? v)
       (display prefix)
       (display "approximately ")
       (write (approx-value v)))
      ((alts? v)
       (write-result (string-append prefix "   ")
                     (car (alts-values v)))
       (for-each (lambda (v)
                   (write-result (string-append prefix "OR ")
                                 v))
                 (cdr (alts-values v))))
      (else
       (display prefix)
       (write v))))

   (define (report-test-results)
     (if (null? failures)
         (begin
          (display checked)
          (display " tests passed\n"))
         (begin
          (display (length failures))
          (display " tests failed:\n\n")
          (for-each (lambda (t)
                      (display "Expression:\n ")
                      (write (car t))
                      (display "\nResult:")
                      (write-result "\n " (cadr t))
                      (display "\nExpected:")
                      (write-result "\n " (caddr t))
                      (display "\n\n"))
                    (reverse failures))
           (display (length failures))
           (display " of ")
           (display checked)
           (display " tests failed.\n"))))))
