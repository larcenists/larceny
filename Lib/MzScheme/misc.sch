($$trace "misc")

;;; A bunch of miscellaneous functions that I can't figure out where
;;; else to put.

;;; Cheesy implementation of arity-at-least.
(define (make-arity-at-least x)
  (cons 'arity-at-least x))

(define (arity-at-least? thing)
  (and (pair? thing)
       (eq? (car thing) 'arity-at-least)))

(define arity-at-least-value cdr)

(define (arity-plus arity n)
  (cond ((number? arity) (+ arity n))
        ((arity-at-least? arity) (make-arity-at-least (+ (arity-at-least-value arity) n)))
        (else (error "arity-plus: not an arity" arity))))

; taken from slib/comlist.scm
; modified to check only up to the
; length of the shortest list.
; The -ct suffix means `co-truncate'
(define (every-ct pred l . rest)

  (define (every-n cars cdrs lists)
    (cond ((pair? lists) (cond ((pair? (car lists)) (every-n (cons (caar lists) cars)
                                                             (cons (cdar lists) cdrs)
                                                             (cdr lists)))
                               ((null? (car lists)) #t)
                               (else (error "EVERY:  improper list:  " lists))))
          ((null? lists) (and (apply pred (reverse! cars))
                              (every-n '() '() (reverse! cdrs))))
          (else (error "EVERY:  improper list:  " lists))))

  (cond ((pair? rest) (every-n '() '() (cons l rest)))
        ((null? rest) (every? pred l))
        (else (error "EVERY:  bad rest arg?" rest))))

(define false-func (lambda args #f))

(define (getarg args keyword . default)
  (define (scan tail)
    (cond ((pair? tail) (let ((key   (car tail))
                              (ttail (cdr tail)))
                          (if (pair? ttail)
                              (if (eq? key keyword)
                                  (car ttail)
                                  (scan (cdr ttail)))
                              (error "GETARG:  Unbalanced arglist" args))))
          ((null? tail) (and (pair? default) (car default)))
          (else (error "GETARG:  Improper arglist" args))))
  (scan args))

(define (getargs args keyword)
  (cond ((pair? args) (let ((key (car args))
                            (tail (cdr args)))
                        (if (pair? tail)
                            (if (eq? key keyword)
                                (cons (car tail) (getargs (cdr tail) keyword))
                                (getargs (cdr tail) keyword))
                            (error "GETARGS:  Unbalanced list:  " args))))
        ((null? args) '())
        (else (error "GETARGS:  Improper list:  " args))))

(define (identity x) x)

(define (keys/args args)
  (define (scan tail accum)
    (cond ((pair? tail) (if (keyword? (car tail))
                            (if (pair? (cdr tail))
                                (scan (cddr tail) (list* (cadr tail) (car tail) accum))
                                (error "KEYS/ARGS:  Unbalanced list:  " tail))
                            (values (reverse! accum) tail)))
          ((null? tail) (values (reverse! accum) tail))
          (else (error "KEYS/ARGS:  Improper list:  " tail))))
  (scan args '()))

(define (keyword? thing)
  (and (symbol? thing)
       (colon-prefix? thing)))

(define (mapadd f l last)
  (define (helper l)
    (cond ((pair? l) (cons (f (car l)) (helper (cdr l))))
          ((null? l) (list last))
          (else (error "MAPADD:  Improper list:  " l))))
  (helper l))


