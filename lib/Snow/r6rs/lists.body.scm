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

(define (identity x) x)

(define (find proc list)
  (let loop ((rest list))
    (if (null? rest)
        #f
        (let ((car (car rest)))
          (if (proc car)
              car
              (loop (cdr rest)))))))

(define-syntax define-quantifier
  (syntax-rules ()
    ((_ <name> <base-value> <terminating-value?>)
     (define (<name> proc list1 . lists)
       (define (length-error)
         (error "Lists don't have the same length." (cons list1 lists)))
       (if (null? list1)
           ;; Careful with recursion on for-all.
           (if (or (null? lists) (for-all null? lists))
               <base-value>
               (length-error))
           (let loop ((car1 (car list1))
                      (cars (map car lists))
                      (cdr1 (cdr list1))
                      (cdrs (map cdr lists)))
             (if (null? cdr1)
                 (if (for-all null? cdrs)
                     (apply proc car1 cars)
                     (length-error))
                 (let ((value (apply proc car1 cars)))
                   (if (<terminating-value?> value)
                       value
                       (loop (car cdr1) (map car cdrs)
                             (cdr cdr1) (map cdr cdrs)))))))))))

(define-quantifier for-all #t not)
(define-quantifier exists #f identity)

(define-syntax define-segregator
  (syntax-rules ()
    ((_ <name> <gather-negatives?>)
     (define (<name> proc list)
       (let loop ((rest list)
                  (positives '())
                  (negatives '()))
         (if (null? rest)
             (if <gather-negatives?>
                 (values (reverse positives) (reverse negatives))
                 (reverse positives))
             (let* ((car (car rest))
                    (pass? (proc car)))
               (loop (cdr rest)
                     (if pass?
                         (cons car positives)
                         positives)
                     (if (and <gather-negatives?> (not pass?))
                         (cons car negatives)
                         negatives)))))))))

(define-segregator filter #f)
(define-segregator partition #t)

(define (fold-left combine nil list1 . lists)
  (define (length-error)
    (error "Lists don't have the same length." (cons list1 lists)))
  (let loop ((acc nil)
             (list1 list1)
             (lists lists))
    (if (null? list1)
        (if (for-all null? lists)
            acc
            (length-error))
        (loop (apply combine acc (car list1) (map car lists))
              (cdr list1)
              (map cdr lists)))))

(define (fold-right combine nil list1 . lists)
  (define (length-error)
    (error "Lists don't have the same length." (cons list1 lists)))
  (let recur ((list1 list1)
              (lists lists))
    (if (null? list1)
        (if (for-all null? lists)
            nil
            (length-error))
        (let* ((acc (recur (cdr list1) (map cdr lists))))
          (apply combine
                 (car list1) (append (map car lists) (list acc)))))))

(define-syntax define-remover
  (syntax-rules ()
    ((_ <name> <pred>)
     (define (<name> obj list)
       (let loop ((rest list)
                  (result '()))
         (if (null? rest)
             (reverse result)
             (loop (cdr rest)
                   (let ((car (car rest)))
                     (if (not (<pred> obj car))
                         (cons car result)
                         result)))))))))

(define-remover remp (lambda (pred x) (pred x)))
(define-remover remove equal?)
(define-remover remv eqv?)
(define-remover remq eq?)

(define (memp pred list)
  (let loop ((rest list))
    (if (null? rest)
        #f
        (if (pred (car rest))
            rest
            (loop (cdr rest))))))

(define (assp pred alist)
  (let loop ((rest alist))
    (if (null? rest)
        #f
        (let ((entry (car rest)))
          (if (pred (car entry))
              entry
              (loop (cdr rest)))))))

(define cons*
  (case-lambda
    ((obj)
     obj)
    (objs
     (cons (car objs) (apply cons* (cdr objs))))))
