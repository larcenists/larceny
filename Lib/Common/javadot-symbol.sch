(let ((javadot-symbol-prop-name (gensym "javadot-symbol-prop-name")))

  ;; Mark the given symbol as a javadot symbol
  (set! symbol->javadot-symbol
        (lambda (sym)
          (putprop sym javadot-symbol-prop-name #t)
          sym))

  ;; Mark the given symbol as _not_ a javadot symbol
  (set! javadot-symbol->symbol
        (lambda (sym)
          (putprop sym javadot-symbol-prop-name #f)
          sym))

  ;; Consumes a string and produces a javadot symbol
  (set! string->javadot-symbol
        (lambda (str)
          (symbol->javadot-symbol (string->symbol str))))

  ;; any -> boolean
  (set! javadot-symbol?
        (lambda (sym)
          (and (symbol? sym)
               (getprop sym javadot-symbol-prop-name)))))

(define (leading? char symbol)
      ;; Return #t if the symbol begins with CHAR.
      (char=? (string-ref (symbol->string symbol) 0) char))

(define (trailing? char symbol)
  ;; Return #t if the symbol ends with CHAR.
  (let ((string (symbol->string symbol)))
    (char=? (string-ref string (- (string-length string) 1))
            char)))

(define (leading-dot? symbol)
  ;; Return #T if the `symbol' has a leading dot.
  (leading? #\. symbol))

(define (trailing-dollar? symbol)
  ;; Return #T if the symbol has a trailing dollar.
  (trailing? #\$ symbol))

(define (trailing-dot? symbol)
  (trailing? #\. symbol))

(define (dot-dollar? symbol)
  ;; Return #T if the symbol has a leading dot and a trailing
  ;; dollar sign.
  (and (leading-dot? symbol)
       (trailing-dollar? symbol)))

(define (set-dot-dollar-excl? symbol)
  ;; Return #T if the symbol begins with SET-.  and ends
  ;; with $!
  ;; These symbols are created from the SETF! macro.
  ;; Kinda gross.
  (let* ((string (symbol->string symbol))
         (length (string-length string)))
    (and (> length 7)
         (char=? (string-ref string 0) #\s)
         (char=? (string-ref string 1) #\e)
         (char=? (string-ref string 2) #\t)
         (char=? (string-ref string 3) #\-)
         (char=? (string-ref string 4) #\.)
         (char=? (string-ref string (- length 2)) #\$)
         (char=? (string-ref string (- length 1)) #\!))))

(define (trailing-dot-class? symbol)
  (let* ((string (symbol->string symbol))
         (length (string-length string)))
    (and (> length 6)
         (char=? (string-ref string (- length 6)) #\.)
         (char=? (string-ref string (- length 5)) #\c)
         (char=? (string-ref string (- length 4)) #\l)
         (char=? (string-ref string (- length 3)) #\a)
         (char=? (string-ref string (- length 2)) #\s)
         (char=? (string-ref string (- length 1)) #\s))))

(define (split-on-dots string)
  (let loop ((scan 0)
             (start 0)
             (accum '()))
    (cond ((>= scan (string-length string))
           (reverse (cons (substring string start scan) accum)))
          ((char=? (string-ref string scan) #\.)
           (loop (add1 scan)
                 (add1 scan)
                 (cons (substring string start scan) accum)))
          (else (loop (add1 scan) start accum)))))

(define (embedded-dot? symbol)
  ;; return #T if the `symbol' has embedded dots
  ;; leading and trailing dots are not allowed, however.
  (let* ((string (symbol->string symbol))
         (length (string-length string)))
    (and (> length 2)
         (not (char=? (string-ref string 0) #\.))
         (not (char=? (string-ref string (- length 1)) #\.))
         (let loop ((scan 1))
           (cond ((>= scan length) #f)
                 ((char=? (string-ref string scan) #\.) #t)
                 (else (loop (+ scan 1))))))))

(define (javadot-syntax? sym)
  (and (symbol? sym)
       (some? (lambda (f) (f sym))
              (list leading-dot?
                    trailing-dot?
                    trailing-dollar?
                    dot-dollar?
                    set-dot-dollar-excl?
                    trailing-dot-class?
                    embedded-dot?))))
                      