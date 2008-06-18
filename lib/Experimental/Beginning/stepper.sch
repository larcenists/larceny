; Prototype of stepper for the beginning language.
;
; Redefines four procedures that were defined as stubs
; by the interpreter.

(define stepping? #t)

; FIXME: maintains history in this global variable.

(define stepping-history '())

; Redefinitions

(define (interpret-beginning-program pgm)
  (set! stepping-history '())
  (interpret-beginning-program-loop pgm
                                    pgm
                                    (make-hashtable symbol-hash eq?)
                                    '()))

(define (evaluate-beginning-expression exp env cont)
  (if stepping? (display-step1 exp cont))
  (step-beginning-expression exp env cont))

(define (apply-beginning-continuation cont val)
  (if stepping? (display-step2 val cont))
  (step-beginning-continuation cont val))

(define (apply-beginning-procedure proc args cont)
  (if stepping? (display-step3 proc args cont))
  (step-beginning-procedure proc args cont))

; These procedures add a configuration to the history.

(define (display-step1 exp cont)
  (set! stepping-history
        (cons (make-beginning-configuration-exp cont exp)
              stepping-history))
  (display-step))

(define (display-step2 value cont)
  (set! stepping-history
        (cons (make-beginning-configuration-value cont value)
              stepping-history))
  (display-step))

(define (display-step3 proc args cont)
  (set! stepping-history
        (cons (make-beginning-configuration-call cont proc args)
              stepping-history))
  (display-step))

; This procedure displays a step.
;
; FIXME
;
; PLT Scheme appears to display redexes only when they
; are of one of the following forms:
;     applications whose arguments have been evaluated
;     if expressions whose arguments have been evaluated
;
; Capturing only the expansion of these redexes could be
; done by changing the interpreter.  Better yet, it could
; be done in the stepper with only slight cleverness.

(define (display-step)
  (if (and (pair? stepping-history)
           (pair? (cdr stepping-history))
           (let ((config0 (cadr stepping-history)))
             (or (and (beginning-configuration-call? config0)
                      (let ((cont (beginning-configuration-cont config0)))
                        (and (call-cont? cont)
                             (null? (call-cont-exps cont)))))
                 (and (beginning-configuration-value? config0)
                      (if-cont? (beginning-configuration-cont config0))))))
      (really-display-step)))

(define (really-display-step)
  (define (display-configuration config)
    (call-with-values
     (lambda () (configuration->pseudocode config))
     (lambda (s range)
       (let ((s (fixme-highlighted s (car range) (cadr range))))
         (display s)
         (newline)))))
  (display-configuration (cadr stepping-history))
  (display-configuration (car stepping-history))
  (newline)
  (newline))

; Converts a configuration to pseudocode.
;
; FIXME: we want to do the highlighting differently
; depending on whether it's a redex or a result.

(define (configuration->pseudocode config)
  (cond ((beginning-configuration-exp? config)
         (wrap-highlighted-with-continuation
          (beginning-configuration-exp config)
          (beginning-configuration-cont config)))
        ((beginning-configuration-value? config)
         (let* ((code (value->pseudocode
                       (beginning-configuration-value config)))
                (cont (beginning-configuration-cont config)))
           (cond ((if-cont? cont)
                  (let ((code (wrap-with-continuation1 code cont))
                        (cont (if-cont-cont cont)))
                    (wrap-highlighted-with-continuation code cont)))
                 ((call-cont? cont)
                  (let ((code (wrap-with-continuation1 code cont))
                        (cont (call-cont-cont cont)))
                    (wrap-highlighted-with-continuation code cont)))
                 (else
                  (wrap-highlighted-with-continuation code cont)))))
        ((beginning-configuration-call? config)
         (wrap-highlighted-with-continuation
          (cons (beginning-closure-name
                 (beginning-configuration-proc config))
                (map value->pseudocode
                     (beginning-configuration-args config)))
          (beginning-configuration-cont config)))
        (else
         (beginning:error "bad configuration" config))))

; Given pseudocode to be highlighted and a continuation
; representing its context, returns two values:
;     a string s containing the pretty-printed code and context
;     a list of the form (i j) indicating that (substring s i j)
;         should be highlighted
;
; FIXME
;
; PLT Scheme displays code with variables replaced by
; their R-values.

(define (wrap-highlighted-with-continuation code cont)
  (let* ((p1 (wrap-with-continuation code cont))
         (fake (if (string? code) '() "fake"))
         (p2 (wrap-with-continuation fake cont))
         (s1 (call-with-output-string
              (lambda (out) (pretty-print p1 out beginning-pp-width))))
         (s2 (call-with-output-string
              (lambda (out) (pretty-print p2 out beginning-pp-width))))
         (i (first-diff-forward s1 s2))
         (j-1 (first-diff-backward s1 s2)))
    (values s1 (list i (+ j-1 1)))))

; Given two strings, returns the index of their first difference,
; ignoring whitespace.  The index that is returned is an index
; into the first string, or is the length of the first string
; if no difference is found.

(define (first-diff-forward s1 s2)
  (let ((n1 (string-length s1))
        (n2 (string-length s2)))
    (define (loop i j)
      (cond ((= i n1)
             i)
            ((= j n2)
             i)
            ((char-whitespace? (string-ref s1 i))
             (loop (+ i 1) j))
            ((char-whitespace? (string-ref s2 j))
             (loop i (+ j 1)))
            ((char=? (string-ref s1 i) (string-ref s2 j))
             (loop (+ i 1) (+ j 1)))
            (else
             i)))
    (loop 0 0)))

; Given two strings, returns the index of their first difference,
; ignoring whitespace, when scanning backward from their ends.
; The index that is returned is an index into the first string,
; or is the length of the first string if no difference is found.

(define (first-diff-backward s1 s2)
  (let ((n1 (string-length s1))
        (n2 (string-length s2)))
    (define (loop i j)
      (cond ((< i 0)
             n1)
            ((< j 0)
             i)
            ((char-whitespace? (string-ref s1 i))
             (loop (- i 1) j))
            ((char-whitespace? (string-ref s2 j))
             (loop i (- j 1)))
            ((char=? (string-ref s1 i) (string-ref s2 j))
             (loop (- i 1) (- j 1)))
            (else
             i)))
    (loop (- n1 1) (- n2 1))))

; Desired maximum width of the pretty-printed string.
;
; FIXME: Larceny's pretty-printer may exceed that width,
; so be careful.

(define beginning-pp-width 39)

; FIXME
;
; This performs character-level highlighting just so
; I can debug the stepper without using Common Larceny.

(define (fixme-highlighted s i j)
  (define (loop s i j)
    (if (= i j)
        s
        (let ((c (string-ref s i)))
          (cond ((char=? c #\()
                 (string-set! s i #\[))
                ((char=? c #\))
                 (string-set! s i #\]))
                (else
                 (string-set! s i (char-upcase c))))
          (loop s (+ i 1) j))))
  (loop (string-copy s) i j))

(define (ignored exp)
  (cond ((pair? exp)
         (list->vector exp))
        ((symbol? exp)
         (string->symbol
          (string-upcase
           (symbol->string exp))))
        ((number? exp)
         (+ 0.0 exp))
        (else exp)))

(define (value->pseudocode x)
  (cond ((null? x) 'empty)
        ((eq? x #t) 'true)
        ((eq? x #f) 'false)
        ((symbol? x) (list 'quote x))
        ((beginning-closure-base? x)
         (beginning-closure-name x))
        (else x)))

; Wraps pseudocode with context from the first continuation frame only.

(define (wrap-with-continuation1 code cont)
  (cond ((if-cont? cont)
         (let* ((exp1 (if-cont-exp1 cont))
                (exp2 (if-cont-exp2 cont))
                (exp (cond ((cond-cont? cont)
                            (if (and (pair? exp2)
                                     (eq? (car exp2) 'cond))
                                (cons 'cond
                                      (cons (list code exp1)
                                            (cdr exp2)))
                                (list 'cond
                                      (list code exp1)
                                      (list 'else exp2))))
                           ((and-cont? cont)
                            (if (and (pair? exp1)
                                     (eq? (car exp1) 'and))
                                (cons 'and (cons code (cdr exp1)))
                                (list 'and code exp1)))
                           ((or-cont? cont)
                            (if (and (pair? exp2)
                                     (eq? (car exp2) 'or))
                                (cons 'or (cons code (cdr exp2)))
                                (list 'or code exp2)))
                           (else
                            (list 'if code exp1 exp2)))))
            exp))
        ((call-cont? cont)
         (let* ((proc (call-cont-val0 cont))
                (vals (call-cont-vals cont))
                (exps (cons code (call-cont-exps cont)))
                (call (cons (value->pseudocode proc)
                            (append (map value->pseudocode vals)
                                    exps))))
           call))
        (else
         code)))

; Wraps pseudocode with context from the entire continuation.

(define (wrap-with-continuation code cont)
  (cond ((if-cont? cont)
         (wrap-with-continuation (wrap-with-continuation1 code cont)
                                 (if-cont-cont cont)))
        ((call-cont? cont)
         (wrap-with-continuation (wrap-with-continuation1 code cont)
                                 (call-cont-cont cont)))
        (else
         code)))

; Given two pseudocodes, compares them to find their difference.
; Returns three values:
;     a string containing the pretty-printed pseudocodes side by side
;     a list of substring indices that highlight the first pseudocode
;     a list of substring indices that highlight the second pseudocode
;
; Each list of substring indices is of the form ((i j) ...),
; indicating the half-open intervals <i,j> ...

; Configurations that can appear in the stepping-history.

(define beginning-configuration
  (make-rtd 'beginning-configuration
            '#((immutable cont))))
(define beginning-configuration-cont
  (rtd-accessor beginning-configuration 'cont))

(define beginning-configuration-exp
  (make-rtd 'beginning-configuration-exp
            '#((immutable exp))
            beginning-configuration))
(define make-beginning-configuration-exp
  (rtd-constructor beginning-configuration-exp))
(define beginning-configuration-exp?
  (rtd-predicate beginning-configuration-exp))
(define beginning-configuration-exp
  (rtd-accessor beginning-configuration-exp 'exp))

(define beginning-configuration-value
  (make-rtd 'beginning-configuration-value
            '#((immutable value))
            beginning-configuration))
(define make-beginning-configuration-value
  (rtd-constructor beginning-configuration-value))
(define beginning-configuration-value?
  (rtd-predicate beginning-configuration-value))
(define beginning-configuration-value
  (rtd-accessor beginning-configuration-value 'value))

(define beginning-configuration-call
  (make-rtd 'beginning-configuration-call
            '#((immutable proc)
               (immutable args))
            beginning-configuration))
(define make-beginning-configuration-call
  (rtd-constructor beginning-configuration-call))
(define beginning-configuration-call?
  (rtd-predicate beginning-configuration-call))
(define beginning-configuration-proc
  (rtd-accessor beginning-configuration-call 'proc))
(define beginning-configuration-args
  (rtd-accessor beginning-configuration-call 'args))

