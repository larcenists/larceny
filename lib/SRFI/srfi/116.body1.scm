;; SRFI 116: Immutable List Library
;; Copyright (C) John Cowan 2014. All Rights Reserved.
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

;;;; Enhancements and hooks in Olin's SRFI-1 code to make it work for ilists

;;; The basic ilist cell

(define-record-type <ilist> (ipair icar icdr) ipair? (icar icar) (icdr icdr))

;;; SRFI 8 syntax for receiving multiple values

(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
                       (lambda formals body ...)))))

;;; Syntax for quoting ilists

(define-syntax iq
  (syntax-rules ()
    ((iq . tree) (gtree->itree 'tree))))

;;; Replacers

(define (replace-icar old new)
  (ipair new (icdr old)))

(define (replace-icdr old new)
  (ipair (icar old) new))

;;; Conversion between lists and ilists

(define (pair->ipair pair)
  (ipair (car pair) (cdr pair)))

(define (ipair->pair ipair)
  (cons (icar ipair) (icdr ipair)))

(define (list->ilist list)
  (let lp ((list list))
    (if (pair? list)
      (ipair (car list) (lp (cdr list)))
      list)))

(define (ilist . objs)
  (list->ilist objs))

(define (ilist->list ilist)
  (let lp ((ilist ilist))
    (if (ipair? ilist)
      (cons (icar ilist) (lp (icdr ilist)))
      ilist)))

(define (tree->itree obj)
  (if (pair? obj)
    (ipair (tree->itree (car obj)) (tree->itree (cdr obj)))
    obj))

(define (itree->tree obj)
  (if (ipair? obj)
    (cons (itree->tree (icar obj)) (itree->tree (icdr obj)))
    obj))

(define (gtree->itree obj)
  (cond
    ((pair? obj)
     (ipair (gtree->itree (car obj)) (gtree->itree (cdr obj))))
    ((ipair? obj)
     (ipair (gtree->itree (icar obj)) (gtree->itree (icdr obj))))
    (else
     obj)))

(define (gtree->tree obj)
  (cond
    ((pair? obj)
     (cons (gtree->tree (car obj)) (gtree->tree (cdr obj))))
    ((ipair? obj)
     (cons (gtree->tree (icar obj)) (gtree->tree (icdr obj))))
    (else
     obj)))

;; Apply a function to (arguments and) an ilist
;; If ilists are built in, optimize this!
;; Need a few SRFI-1 routines

(define (take! ls i)
  (if (<= i 0)
      '()
      (let ((tail (list-tail ls (- i 1))))
        (set-cdr! tail '())
        ls)))

(define (drop-right! ls i)
  (take! ls (- (length ls) i)))

(define (last ls) (if (null? (cdr ls)) (car ls) (last (cdr ls))))

(define (iapply proc . ilists)
  (cond
    ((null? ilists)
     (apply proc '()))
    ((null? (cdr ilists))
     (apply proc (ilist->list (car ilists))))
    (else
      (let ((final (ilist->list (last ilists))))
        (apply proc (append (drop-right! ilists 1) final))))))

;;; Printer for debugging

(define (write-ipair ipair port)
  (write (gtree->tree ipair) port))

(cond-expand
  (chicken (define-record-printer <ilist> write-ipair))
  (else #t))

;;; Stuff needed by Olin's code

(define-syntax :optional
  (syntax-rules ()
    ((:optional rest default)
     (cond
       ((null? rest) default)
       ((null? (cdr rest)) (car rest))
       (else (error "Too many arguments"))))))

;;; Analogues of R5RS list routines (not defined by Olin)

(define (iassq x lis)
    (ifind (lambda (entry) (eq? x (icar entry))) lis))

(define (iassv x lis)
    (ifind (lambda (entry) (eqv? x (icar entry))) lis))

#;
(define (imap proc lis1 . lists)
  (apply imap-in-order proc lis1 lists))

(define (ifor-each proc lis1 . lists)
  (check-arg procedure? proc ipair-for-each)
  (if (pair? lists)

      (let lp ((lists (cons lis1 lists)))
        (let ((tails (%cdrs lists)))
          (if (pair? tails)
              (begin (apply proc (map icar lists))
                     (lp tails)))))

      ;; Fast path.
      (let lp ((lis lis1))
        (if (not (null-ilist? lis))
            (let ((tail (icdr lis)))    ; Grab the icdr now,
              (proc (icar lis))                ; even though it's unnecessary
              (lp tail))))))

