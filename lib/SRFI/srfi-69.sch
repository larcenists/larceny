;;; Copyright © Panu Kalliokoski (2005). All Rights Reserved.
;;; (with Larceny-specific updates by Felix Klock (2006))
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

;;; Modification history:
;;; 2006 Larceny-specific updates by Felix Klock
;;; 2008 completely rewritten by William D Clinger
;;; 
;;; This implementation deviates from SRFI 69 in several ways:
;;; 
;;;     The hash table type is not disjoint from all other types,
;;;     because hash tables are the same as R6RS hashtables.
;;; 
;;;     The default hash functions do not accept a second
;;;     argument.  This restriction makes it easier to use
;;;     Larceny's usual hash functions.  This restriction
;;;     could easily be lifted at some cost in performance,
;;;     but few clients of SRFI 69 will even notice.
;;; 
;;;     The string-hash and string-ci-hash procedures do not
;;;     accept a second argument.  This restriction allows
;;;     them to be the same as the corresponding R6RS procedures
;;;     in Larceny's R5RS top level.  In an ERR5RS/R6RS library
;;;     version of SRFI 69, there would be no clash so this
;;;     restriction could easily be removed.

;; Hack to allow use of Larceny's built-in string-hash implementation
;; and to keep it in the face of multiple loads of this srfi.
;;
;; The hack isn't needed here because we're going to restrict
;; SRFI 69 instead of redefining string-hash and string-ci-hash.

;(define %%string-hash 
;  (if (environment-variable? (interaction-environment) '%%string-hash)
;      %%string-hash
;      string-hash))

(define larceny:string-hash string-hash)
(define larceny:string-ci-hash string-ci-hash)

(define (make-hash-table . args)
  (cond ((null? args)
         (make-r6rs-hashtable hash equal?))
        ((null? (cdr args))
         (let ((same? (car args)))
           (cond ((eq? same? eq?)
                  (make-eq-hashtable))
                 ((eq? same? eqv?)
                  (make-eqv-hashtable))
                 ((eq? same? equal?)
                  (make-r6rs-hashtable hash equal?))
                 ((eq? same? string=?)
                  (make-r6rs-hashtable string-hash string=?))
                 ((eq? same? string-ci=?)
                  (make-r6rs-hashtable string-ci-hash string-ci=?))
                 (else
                  (assertion-violation 'make-hash-table
                                       "unable to infer hash function"
                                       args)))))
        ((null? (cddr args))
         (let ((same? (car args))
               (hash (cadr args)))
           (cond ((and (eq? same? eq?)
                       (eq? hash %srfi69:hash-on-eq))
                  (make-eq-hashtable))
                 ((and (eq? same? eqv?)
                       (eq? hash %srfi69:hash-on-eqv))
                  (make-eqv-hashtable))
                 (else
                  (make-r6rs-hashtable hash same?)))))
        (else
         (assertion-violation 'make-hash-table
                              "too many arguments"
                              args))))

(define hash-table? hashtable?)

(define (alist->hash-table alist . rest)
  (let ((ht (apply make-hash-table rest))
        (ralist (reverse alist)))
    (for-each (lambda (entry)
                (hashtable-set! ht (car entry) (cdr entry)))
              ralist)
    ht))

(define (hash-table-equivalence-function ht)
  (hashtable-equivalence-function ht))

(define (hash-table-hash-function ht)
  (let ((f (hashtable-hash-function ht)))
    (if f
        f
        (let ((same? (hashtable-equivalence-function ht)))
          (cond ((eq? same? eq?)
                 %srfi69:hash-on-eq)
                (else
                 %srfi69:hash-on-eqv))))))

(define (hash-table-ref ht key . rest)
  (let ((value (hashtable-ref ht key %srfi69:missing)))
    (cond ((eq? value %srfi69:missing)
           (if (null? rest)
               (%srfi69:error:missing 'hash-table-ref ht key)
               ((car rest))))
          (else value))))

(define hash-table-ref/default hashtable-ref)

(define hash-table-set! hashtable-set!)

(define hash-table-delete! hashtable-delete!)

(define hash-table-exists? hashtable-contains?)

; FIXME: this could be more efficient, if anyone cares

(define (hash-table-update! ht key f . rest)
  (hash-table-set! ht key (f (apply hash-table-ref ht key rest))))

(define (hash-table-update!/default ht key f default)
  (hash-table-set! ht key (f (hash-table-ref/default ht key default))))

(define hash-table-size hashtable-size)

(define (hash-table-keys ht)
  (vector->list (hashtable-keys ht)))

(define (hash-table-values ht)
  (call-with-values
   (lambda () (hashtable-entries ht))
   (lambda (keys values)
     (vector->list values))))

(define (hash-table-walk ht f)
  (call-with-values
   (lambda () (hashtable-entries ht))
   (lambda (keys values)
     (vector-for-each f keys values))))

(define (hash-table-fold ht f init)
  (call-with-values
   (lambda () (hashtable-entries ht))
   (lambda (keys values)
     (let ((n (vector-length keys)))
       (do ((i 0 (+ i 1))
            (accum init
                   (f (vector-ref keys i)
                      (vector-ref values i)
                      accum)))
           ((= i n) accum))))))

(define (hash-table->alist ht)
  (call-with-values
   (lambda () (hashtable-entries ht))
   (lambda (keys values)
     (vector->list
      (vector-map cons keys values)))))

(define (hash-table-copy ht)
  (hashtable-copy ht #t))

(define (hash-table-merge! ht1 ht2)
  (hash-table-walk ht
                   (lambda (key value)
                     (hash-table-set! ht1 key value)))
  ht1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Private stuff.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (%srfi69:error name msg irritants)
  (error name msg irritants))

(define (%srfi69:error:missing name ht key)
  (error name "key not in hash-table" (list ht key)))

(define %srfi69:missing (list '%srfi69:missing))

(define %srfi69:counter 14151210)
(define %srfi69:increment 268421)
(define %srfi69:modulus 300489088)

(define %srfi69:eq-table (make-eq-hashtable))
(define %srfi69:eqv-table (make-eqv-hashtable))

(define (%srfi69:hash-on-eq obj . rest)
  (cond ((not (null? rest))
         (mod (%srfi69:hash-on-eq obj) (car rest)))
        ((symbol? obj)
         (symbol-hash obj))
        ((fixnum? obj)
         (equal-hash obj))
        ((char? obj)
         (equal-hash obj))
        ((boolean? obj)
         (equal-hash obj))
        ((null? obj)
         (equal-hash obj))
        (%srfi69:eq-table
         (let ((h (hashtable-ref %srfi69:eq-table obj #f)))
           (or h
               (let ((h (mod (+ %srfi69:counter %srfi69:increment)
                             %srfi69:modulus)))
                 (set! %srfi69:counter h)
                 h))))
        (else
         (set! %srfi69:eq-table (make-eq-table))
         (%srfi69:hash-on-eq obj))))

(define (%srfi69:hash-on-eqv obj . rest)
  (cond ((not (null? rest))
         (mod (%srfi69:hash-on-eqv obj) (car rest)))
        ((symbol? obj)
         (symbol-hash obj))
        ((number? obj)
         (equal-hash obj))
        ((char? obj)
         (equal-hash obj))
        ((boolean? obj)
         (equal-hash obj))
        ((null? obj)
         (equal-hash obj))
        (%srfi69:eqv-table
         (let ((h (hashtable-ref %srfi69:eqv-table obj #f)))
           (or h
               (let ((h (mod (+ %srfi69:counter %srfi69:increment)
                             %srfi69:modulus)))
                 (set! %srfi69:counter h)
                 h))))
        (else
         (set! %srfi69:eqv-table (make-eqv-table))
         (%srfi69:hash-on-eqv obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SRFI 69 hash functions.
;;;
;;; It's hard to implement hash-on-eq and hash-on-eqv well
;;; without resorting to weak pointers or other sophisticated
;;; techniques.  That's why the R6RS API doesn't let those
;;; hash functions leak out.
;;;
;;; But SRFI 69 exposes them, directly via hash-on-identity
;;; and indirectly via hash-table-hash-function applied to
;;; a hash table whose hash function was inferred for the
;;; eqv? predicate.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hash obj . rest)
  (if (null? rest)
      (equal-hash obj)
      (mod (equal-hash obj) (car rest))))

;(define (string-hash obj . rest)
;  (if (null? rest)
;      (larceny:string-hash obj)
;      (mod (larceny:string-hash obj) (car rest))))

;(define (string-ci-hash obj . rest)
;  (if (null? rest)
;      (larceny:string-ci-hash obj)
;      (mod (larceny:string-ci-hash obj) (car rest))))

(define hash-by-identity %srfi69:hash-on-eq)

;eof
