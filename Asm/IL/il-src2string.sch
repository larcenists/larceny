;; il-src2string
;; Formats ilpackages into strings

;; ----------------------------------

;; pickle-il : il -> il
(define (pickle-il il)
  (if (il? il)
      (let ((bytecode (il.code il))
            (args (il.args il)))
        (case bytecode
          ((comment)
           (apply pickle-comment args))
          ((directive)
           (apply pickle-directive il args))
          ((label)
           il)
          ((switch)
           il)
          ((ldc.i4)
           (apply pickle-ldc-i4 bytecode args))
          ((ldloc stloc ldarg starg)
           (apply pickle-short-form bytecode args))
          (else (pickle-instr bytecode args))))
      il))

;; pickle-comment : string -> il
(define (pickle-comment str)
  str)

;; pickle-directive : symbol il . (listof string)  -> il
(define (pickle-directive same directive . args)
  (case directive
    ((entrypoint)
     ".entrypoint")
    ((maxstack)
     (twobit-format #f ".maxstack ~a" 80)) ;;(car args))) ;; 80?
    ((module)
     (twobit-format #f ".module '~a'" (car args)))
    ((line)
     (twobit-format #f ".line ~a ~a" (car args)
                    (if (cadr args) (twobit-format #f "'~a'" (cadr args)) "")))
    (else same)))

;; pickle-instr : symbol (listof string) -> il
(define (pickle-instr bytecode args)
  (cond ((null? args)
         (twobit-format #f "~a" bytecode))
        ((null? (cdr args))
         (twobit-format #f "~a ~a" bytecode (car args)))
        ((null? (cddr args))
         (twobit-format #f "~a ~a, ~a" bytecode (car args) (cadr args)))
        (else (error 'unimplemented "unknown instruction format: "
                     (cons bytecode args)))))

(define (pickle-ldc-i4 bytecode datum)
  (cond ((memv datum '(0 1 2 3 4 5 6 7 8))
         (pickle-instr (string-append (symbol->string bytecode) "."
                                      (number->string datum))
                       '()))
        ((equal? datum -1)
         (pickle-instr 'ldc.i4.m1 '()))
        ((and (number? datum) (>= datum 0) (< datum 128))
         (pickle-instr 'ldc.i4.s (list datum)))
        (else (pickle-instr bytecode (list datum)))))

(define (pickle-short-form bytecode datum)
  (cond ((and (eqv? bytecode 'ldloc) (memv datum '(0 1 2 3)))
         (pickle-instr (string-append (symbol->string bytecode) "."
                                      (number->string datum))
                       '()))
        ((and (>= datum 0) (< datum 128))
         (pickle-instr (string-append (symbol->string bytecode) ".s")
                       (list datum)))
        (else (pickle-instr bytecode (list datum)))))
