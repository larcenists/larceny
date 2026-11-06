;;; Set benchmark for (scheme set).

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time)
        (scheme list)
        (scheme sort)
        (scheme comparator)
        (scheme set))

(define symbol-comparator
  (make-comparator symbol?
                   eq?
                   (lambda (sym1 sym2)
                     (string<? (symbol->string sym1)
                               (symbol->string sym2)))
                   symbol-hash))

(define (permutation<? perm1 perm2)
  (cond ((null? perm1)
         (not (null? perm2)))
        ((null? perm2)
         #f)
        ((<? symbol-comparator (car perm1) (car perm2))
         #t)
        ((<? symbol-comparator (car perm2) (car perm1))
         #f)
        (else
         (permutation<? (cdr perm1) (cdr perm2)))))

(define permutation-comparator
  (make-comparator list?
                   equal?
                   permutation<?
                   default-hash))

;;; Returns the union of the sets contained in s,
;;; which must be non-empty (else there's no way to know what
;;; the comparator should be for the result).

(define (big-set-union s)
  (define (loop s partial-result)
    (if (set-empty? s)
        partial-result
        (let* ((s0 (set-find (lambda (x) #t) s 'ignored))
               (s  (set-remove (lambda (y) (set=? s0 y)) s)))
          (loop s (set-union s0 partial-result)))))
  (let* ((s0 (set-find (lambda (x) #t) s 'ignored))
         (s  (set-remove (lambda (y) (set=? s0 y)) s)))
    (loop s s0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SRFI 113 now includes a post-finalization note that says
;;; the order of arguments to set-unfold should be
;;;
;;;     (set-unfold stop? mapper successor seed comparator)
;;;
;;; but WG2 says that change has not yet been made.

(define (symbols n)
  (set-unfold symbol-comparator
              (lambda (i) (= i n))
              (lambda (i)
                (string->symbol (string-append "x" (number->string i))))
              (lambda (i) (+ i 1))
              0))

;;; SRFI 113 now includes a post-finalization note that says
;;; the order of arguments to set-map should be
;;;
;;;    (set-map proc comparator hash-table)
;;;
;;; but WG2 says that change has not yet been made.

(define (powerset universe)
  (if (set-empty? universe)
      (set set-comparator universe)
      (let* ((x (set-find (lambda (x) #t) universe 'ignored))
             (u2 (set-remove (lambda (y) (eq? x y)) universe))
             (pu2 (powerset u2)))
        (set-union pu2
                   (set-map set-comparator
                            (lambda (y)
                              (set-adjoin y x))
                            pu2)))))

;;; SRFI 113 now includes a post-finalization note that says
;;; the order of arguments to set-map and set-unfold should be
;;;
;;;    (set-map proc comparator hash-table)
;;;
;;; but WG2 says that change has not yet been made.

(define (permutations universe)
  (if (set-empty? universe)
      (set permutation-comparator '())
      (let* ((x (set-find (lambda (x) #t) universe 'ignored))
             (u2 (set-remove (lambda (y) (equal? x y)) universe))
             (perms2 (permutations u2)))
        (big-set-union
         (set-map set-comparator
                  (lambda (perm)
                    (set-map permutation-comparator
                             (lambda (i)
                               (append (take perm i)
                                       (cons x (drop perm i))))
                             (list->set
                              (make-default-comparator)
                              (iota (+ 1 (length perm))))))
                  perms2)))))

(define (go n)
  (let* ((universe (symbols n))
         (subsets (powerset universe))
         (perms (permutations universe))
         (syms (set-filter (lambda (syms)
                             (set-contains? perms (set->list syms)))
                           subsets)))
    (list-sort (lambda (sym1 sym2) (<? symbol-comparator sym1 sym2))
               (set->list (car (set->list syms))))))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "set"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (go (hide count input1)))
     (lambda (result) (equal? result output)))))
