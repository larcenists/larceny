
;;(load "simple-macros.scm")
;;(load "simple-syntax-case.scm")

;(repl '
; (

  (module m scheme (v)

    (import syntax-case-module)

    (define-syntax u (lambda (form) (syntax 1)))
    (define-syntax v (lambda (form) (with-syntax ((n (+ (f)
                                                        (u))))
                                      (syntax (g n)))))
    (define (f) 2)
    (define (g n) (/ n))

    ) ; m

  (import m)

  (v)        ;==> 1/3

;  ))

;(repl '
; (

  (module records (define-record record-ref)
    (import syntax-case-module)

    (display "Creating fresh table")
    (newline)
    (define registry '())

    (define (register name fields)
      (display "Registering: ") (display name)
      (newline)
      (set! registry (cons (cons name fields) registry)))

    (define-syntax define-record
      (syntax-rules ()
        ((define-record name pred? field ...)
         (begin
           (register 'name '(field ...))
           (define (pred? x) (and (pair? x) (eq? (car x) 'name)))
           (define (name field ...)
             (list 'name field ...))))))

    (define-syntax record-ref
      (lambda (form)
        (syntax-case form ()
          ((_ exp name field)
           (let ((entry (assq (syntax-object->datum (syntax name)) registry)))
             (if entry
                 (let ((maybe-member (member (syntax-object->datum field) entry)))
                   (if maybe-member
                       (with-syntax
                           ((index (- (length entry) (length maybe-member))))
                         (syntax
                          (list-ref exp index)))
                       (syntax-error "Unknown field" (syntax field))))
                 (syntax-error "Unknown record type" (syntax name))))))))
    ) ; records

  (module savannah (giraffe giraffe? lion lion?)
    (import records)                                      ;==> Creating fresh table

    (define-record giraffe giraffe? height speed weight)  ;==> Registering: giraffe
    (define-record lion    lion?    speed weight))        ;==> Registering: lion

  (module metrics (weight)
    (import records)                ;==> Creating fresh table
    (import savannah)               ;==> Registering: giraffe
                                        ;==> Registering: lion
    (define (weight animal)
      (cond ((lion? animal)    (record-ref animal lion weight))
            ((giraffe? animal) (record-ref animal giraffe weight)))))

  (module main ()
    (import savannah)               ;==> Creating fresh table
                                        ;==> Registering: giraffe
                                        ;==> Registering: lion
    (import metrics)

    (display (weight (giraffe 25 51 1000))))   ;==> 1000
;;  ))

;;(repl '
;; (
  (module interface-even (even)
    (define-syntax even #f))

  (module interface-odd (odd)
    (define-syntax odd #f))

  (module mod-even (even)
    (import syntax-case-module)
    (import interface-even)
    (import interface-odd)
    (set-syntax! even
                 (syntax-rules ()
                   ((even)         #t)
                   ((even x y ...) (odd y ...)))))

  (module mod-odd (odd)
    (import syntax-case-module)
    (import interface-even)
    (import interface-odd)
    (set-syntax! odd
                 (syntax-rules ()
                   ((odd)         #f)
                   ((odd x y ...) (even y ...)))))

  (import mod-even)
  (import mod-odd)

  (even a a a)   ;==> #f
  (odd  a a a)   ;==> #t

;;  ))

;;(repl '
;; (
  (module eager-kons (kons kar kdr)
    (define kons cons)
    (define kar car)
    (define kdr cdr))

  (module lazy-kons (kons kar kdr)
    (import syntax-case-module)
    (define-syntax kons
      (syntax-rules ()
        ((kons x y) (delay (cons x y)))))
    (define (kar x) (car (force x)))
    (define (kdr x) (cdr (force x))))

  (import syntax-case-module)
  (define-syntax make-list-module
    (syntax-rules ()
      ((make-list-module name operations-module)
       (module name (kons kar kdr kadr)
         (import operations-module)
         (define (kadr x) (kar (kdr x)))))))

  (make-list-module eager-lists eager-kons)
  (make-list-module lazy-lists lazy-kons)

  (import eager-lists)

  (kons 1 (kons 2 3))                 ;==> (1 2 . 3)
  (kadr (kons 1 (kons 2 3)))          ;==> 2

  (import lazy-lists)

  (kons 1 (kons 2 3))                 ;==> PROMISE
  (kadr (kons 1 (kons 2 3)))          ;==> 2

;;  ))

;;(repl '
;; (
  (define global 1)

  ; (module m scheme ()
  ;  (display global))   ;==> reference to undefined identifier: global#g16206

;;  ))
