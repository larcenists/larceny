;;; Copyright 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:

;;; The R6RS speaks of universes and enumeration types, but doesn't define these
;;; as Scheme types or objects; it only defines the <enum-set> type.
;;;
;;; The closest thing one can get to a universe object is the <enum-set> object
;;; returned by `enum-set-universe'.  Therefore, in our implementation, a
;;; universe is simply an <enum-set> whose universe field contains itself, thus
;;; `enum-set-universe' is just a record field access.
;;;
;;; The closest thing one can get to the enumeration type of an <enum-set> is
;;; the universe of that <enum-set>, although there can be separate enumeration
;;; types with equal universes.  In our implementation, there is one universe
;;; (<enum-set> object) per enumeration type, as created by `make-enumeration',
;;; that will be in the universe field of any <enum-set> of that enumeration
;;; type, so we can use the object identity of that universe (<enum-set> object)
;;; as the enumeration type of an <enum-set>.

(define (assert bool message . irritants)
  (unless bool
    (apply error message irritants)))

(define (sort-uniq list order)
  (let loop ((order order)
             (result '()))
    (cond
     ((null? order)
      (reverse result))
     ((memq (car order) list)
      (loop (cdr order) (cons (car order) result)))
     (else
      (loop (cdr order) result)))))

;;; Given a list of symbols, sorts it into some standard order.
;;; The standard order doesn't matter, so long as the same standard
;;; order is used whenever sets of symbols are compared.

(define (sorted-symbols symbols)
  (list-sort (lambda (sym1 sym2)
               (string<? (symbol->string sym1)
                         (symbol->string sym2)))
             symbols))

;;; Given two lists that have been sorted with respect to the same
;;; total order, returns true if and only if the elements of the
;;; first list are a subset of the elements of the second list.

(define (sorted-subset? list1 list2)
  (let loop ((list1 list1)
             (list2 list2))
    (or (null? list1)
        (let ((list2-rest (memq (car list1) list2)))
          (and list2-rest (loop (cdr list1) (cdr list2-rest)))))))

(define (sorted-intersection list1 list2)
  (let loop ((list1 list1)
             (list2 list2)
             (result '()))
    (if (null? list1)
        (reverse result)
        (let ((list2-rest (memq (car list1) list2)))
          (if list2-rest
              (loop (cdr list1) (cdr list2-rest) (cons (car list1) result))
              (loop (cdr list1) list2 result))))))

(define (sorted-difference list1 list2)
  (let loop ((list1 list1)
             (list2 list2)
             (result '()))
    (if (null? list1)
        (reverse result)
        (let ((list2-rest (memq (car list1) list2)))
          (if list2-rest
              (loop (cdr list1) (cdr list2-rest) result)
              (loop (cdr list1) list2 (cons (car list1) result)))))))

(define-record-type <enum-set>
  (make-enum-set universe indexer constructor elements)
  enum-set?
  (universe enum-set-universe set-enum-set-universe!)
  (indexer enum-set-indexer)
  (constructor enum-set-constructor)
  (elements enum-set->list))

(define (make-enumeration symbol-list)
  (assert (for-all symbol? symbol-list)
          "Enumeration values must be symbols." symbol-list)
  (let ()
    (define (indexer symbol)
      (let loop ((i 0)
                 (p symbol-list))
        (cond ((null? p) #f)
              ((eq? (car p) symbol) i)
              (else (loop (+ i 1) (cdr p))))))
    (define (constructor elements)
      (assert (for-all (lambda (e) (memq e symbol-list)) elements)
              "Some symbols not in enumeration set universe." elements)
      (let ((elements (sort-uniq elements symbol-list)))
        (make-enum-set universe indexer constructor elements)))
    (define universe (make-enum-set #f indexer constructor symbol-list))
    (set-enum-set-universe! universe universe)
    universe))

(define (enum-set-member? symbol set)
  (not (not (memq symbol (enum-set->list set)))))

(define (enum-set-subset? set1 set2)
  (let* ((universe1 (enum-set->list (enum-set-universe set1)))
         (universe2 (enum-set->list (enum-set-universe set2)))
         (elements1 (enum-set->list set1))
         (elements2 (enum-set->list set2))
         (universe1 (sorted-symbols universe1))
         (universe2 (sorted-symbols universe2))
         (elements1 (sorted-symbols elements1))
         (elements2 (sorted-symbols elements2)))
    (and (sorted-subset? universe1 universe2)
         (sorted-subset? elements1 elements2))))

(define (enum-set=? set1 set2)
  (let* ((universe1 (enum-set->list (enum-set-universe set1)))
         (universe2 (enum-set->list (enum-set-universe set2)))
         (elements1 (enum-set->list set1))
         (elements2 (enum-set->list set2))
         (universe1 (sorted-symbols universe1))
         (universe2 (sorted-symbols universe2))
         (elements1 (sorted-symbols elements1))
         (elements2 (sorted-symbols elements2)))
    (and (equal? universe1 universe2)
         (equal? elements1 elements2))))

(define (assert-same-type set1 set2)
  (assert (eq? (enum-set-universe set1)
               (enum-set-universe set2))
          "Enumeration sets have distinct type." set1 set2))

(define (enum-set-union set1 set2)
  (assert-same-type set1 set2)
  ((enum-set-constructor set1)
   (append (enum-set->list set1) (enum-set->list set2))))

(define (enum-set-intersection set1 set2)
  (assert-same-type set1 set2)
  ((enum-set-constructor set1)
   (sorted-intersection (enum-set->list set1) (enum-set->list set2))))

(define (enum-set-difference set1 set2)
  (assert-same-type set1 set2)
  ((enum-set-constructor set1)
   (sorted-difference (enum-set->list set1) (enum-set->list set2))))

(define (enum-set-complement set)
  ((enum-set-constructor set)
   (sorted-difference (enum-set->list (enum-set-universe set))
                      (enum-set->list set))))

(define (enum-set-projection set1 set2)
  ((enum-set-constructor set2)
   (let ((set1-elements (enum-set->list set1))
         (set2-universe (enum-set->list (enum-set-universe set2))))
     (filter (lambda (e) (memq e set2-universe)) set1-elements))))

(define-syntax define-enumeration
  (syntax-rules ()
    ((define-enumeration <type-name>
       (<symbol> ...)
       <constructor-syntax>)
     (begin
       (define universe (make-enumeration '(<symbol> ...)))
       (define constructor (enum-set-constructor universe))
       ;; Since `syntax-rules' does hygienic matching of its input symbols, the
       ;; macro-expansion time validation of symbols done by the following
       ;; commented-out implementation is broken in that it will error when a
       ;; symbol matches by name but not by binding.
       ;; 
       ;; (define-syntax <type-name>
       ;;   (syntax-rules (<symbol> ...)
       ;;     ((_ <symbol>)
       ;;      '<symbol>)
       ;;     ...
       ;;     ((_ <invalid>)
       ;;      (syntax-error "Invalid enumeration value." <invalid>))))
       ;;
       ;; Given the absence of a procedural macro system, the only other choice
       ;; is to do the validation at run-time.
       (define-syntax <type-name>
         (syntax-rules ()
           ((_ <obj>)
            (let ((obj '<obj>))
              (assert (symbol? obj) "Invalid enumeration value." obj)
              (assert (memq obj '(<symbol> ...))
                      "Symbol not in enumeration universe." obj '(<symbol> ...))
              obj))))

       ;; FIXME: Larceny v0.98 doesn't support the R7RS optional ellipsis
       ;; feature, so Will Clinger rewrote this into an equivalent macro.
       #;
       (define-syntax <constructor-syntax>
         (syntax-rules ___ ()
           ((_ <element> ___)
            (begin
              ;; Validate the elements.
              (<type-name> <element>)
              ___
              (constructor '(<element> ___))))))
       (define-syntax <constructor-syntax>
         (syntax-rules ()
           ((_ <element> (... ...))
            (begin
              ;; Validate the elements.
              (<type-name> <element>)
              (... ...)
              (constructor '(<element> (... ...)))))))
       ))))
