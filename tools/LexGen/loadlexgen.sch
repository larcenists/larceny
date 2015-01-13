; Loads LexGen, such as it is.
;
; First you have to load ParseGen.

; The following definition assumes the Larceny top-level directory
; is the current working directory.
; Edit this definition to accomodate your circumstances.

(define *lexgen-directory* "./tools/LexGen/")

(let ((load (lambda (filename)
              (load (string-append *lexgen-directory* filename)))))
  (load "select.sch")
  (load "accessible.sch")
  (load "regexp.sch")
  (load "lexgen.c.sch")
  (load "lexgen.java.sch")
  (load "lexgen.scheme.sch"))

(define (generate-lexer generator description . rest)
  (cond ((null? rest)
         (generate-lexer generator description (current-output-port)))
        ((string? (car rest))
         (call-with-output-file
          (car rest)
          (lambda (out)
            (generate-lexer generator description out))))
        ((output-port? (car rest))
         (set! output-port1 (car rest))
         (generator (relabel-states (regular->minimal description))))
        (else
         (display "Ignoring illegal third argument to generate-lexer")
         (newline)
         (generate-lexer generator description))))

(define (generate-c-lexer description . rest)
  (apply generate-lexer generate-c-for-lexer description rest))

(define (generate-java-lexer description . rest)
  (apply generate-lexer generate-java-for-lexer description rest))

(define (generate-scheme-lexer description . rest)
  (apply generate-lexer generate-scheme-for-lexer description rest))

; Where should these go?

(define (make-set x)
  (define (loop x y)
    (cond ((null? x) y)
          ((member (car x) y)
           (loop (cdr x) y))
          (else
           (loop (cdr x) (cons (car x) y)))))
  (loop x '()))

(define (set-equal? x y)
  (and (= (length x) (length y))
;      (set-subset? x y)
       (set-subset? y x)))

(define (set-subset? x y)
  (cond ((null? x) #t)
        ((member (car x) y)
         (set-subset? (cdr x) y))
        (else #f)))
