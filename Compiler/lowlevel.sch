; Low-level macro facility based on explicit renaming.  See
; William D Clinger. Hygienic macros through explicit renaming.
; In Lisp Pointers IV(4), 25-28, December 1991.

(define (m-transcribe-low-level exp env-use k transformer env-def)
  (let ((rename0 (make-rename-procedure))
        (renamed '())
        (ok #t))
    (define (lookup sym)
      (let loop ((alist renamed))
        (cond ((null? alist)
               (syntactic-lookup env-use sym))
              ((eq? sym (cdr (car alist)))
               (syntactic-lookup env-def (car (car alist))))
              (else
               (loop (cdr alist))))))
    (let ((rename
           (lambda (sym)
             (if ok
                 (let ((probe (assq sym renamed)))
                   (if probe
                       (cdr probe)
                       (let ((sym2 (rename0 sym)))
                         (set! renamed (cons (cons sym sym2) renamed))
                         sym2)))
                 (m-error "Illegal use of a rename procedure" sym))))
          (compare
           (lambda (sym1 sym2)
             (same-denotation? (lookup sym1) (lookup sym2)))))
      (let ((exp2 (transformer exp rename compare)))
        (set! ok #f)
        (k exp2
           (syntactic-alias env-use renamed env-def))))))

(define identifier? symbol?)

(define (identifier->symbol id)
  (m-strip id))
