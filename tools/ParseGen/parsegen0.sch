; Lexical analyzer, parser, and action procedures for inputs to ParseGen.

(define (string-downcase s)
  (list->string
   (map char-downcase (string->list s))))

; Main entry points.

(define (make-generator thunk)
  (lambda (file0 . rest)
    (let ((grammar (parse-file file0))
          (generator (thunk)))
      (cond ((null? rest)
             (generator grammar))
            ((null? (cdr rest))
             (call-with-output-file
              (car rest)
              (lambda (p1)
                (generator grammar p1))))
            (else
             (call-with-output-file
              (car rest)
              (lambda (p1)
                (call-with-output-file
                 (cadr rest)
                 (lambda (p2)
                   (generator grammar p1 p2))))))))))

(define generate-c       (make-generator (lambda () generate-c-parser)))
(define generate-java    (make-generator (lambda () generate-java-parser)))
(define generate-modula3 (make-generator (lambda () generate-modula3-parser)))
(define generate-pascal  (make-generator (lambda () generate-pascal-parser)))
(define generate-scheme  (make-generator (lambda () generate-scheme-parser)))

; Lexical analyzer.
; See Appendix C of Fischer and LeBlanc for the lexical syntax of LLGen.
; Restriction:  The "..." feature of LLGen is not yet supported here.

(define (init-all)
  (init-scanner)
  (init-parse-error))

(define (parse-file filename)
  (call-with-input-file filename parse-port))

(define (parse-port p)
  (init-all)
  (set! parser-input p)
  (let ((ast (parse-input)))
    (if (positive? total-errors)
        (mk-error)
        ast)))

; LEXICAL ANALYZER.  Public entry points.

(define (consume-token!)
  (set! next-token-is-ready #f))

(define (next-token)
  (if next-token-is-ready
      kind-of-next-token
      (begin (set! size-of-next-token 0)
             (scanner0))))

; These global variables are used to communicate with
; make-identifier and make-number,
; and should otherwise be considered private.

(define token-value 'a)

; These global variables are used to communicate with parse-error,
; and should otherwise be considered private.

(define line-number 1)
(define line-number-of-last-error 0)

; Private to the lexical analyzer.

(define parser-input (current-input-port))

; Identifiers and numerals are limited to maxtokensize characters, sorry.
; Overflow is indeed caught.

(define maxtokensize 1024)
(define maxtokensize-1 (- maxtokensize 1))
(define eoftoken 'eof)

(define kind-of-next-token eoftoken)
(define text-of-next-token (make-string maxtokensize))
(define size-of-next-token 0)
(define next-token-is-ready #f)

; This must be called before next-token is called.

(define (init-scanner)
  (set! line-number 1)
  (set! line-number-of-last-error 0)
  (set! kind-of-next-token eoftoken)
  (set! size-of-next-token 0)
  (set! next-token-is-ready #f)
  (set! last-lhs #f))

(define (scan-char)
  (peek-char parser-input))

(define (consume-char)
  (if (< size-of-next-token maxtokensize-1)
      (let ((c (read-char parser-input)))
        (if (not (eof-object? c))
            (begin (if (char=? c #\newline)
                       (set! line-number (+ line-number 1)))
                   (string-set! text-of-next-token size-of-next-token c)
                   (set! size-of-next-token (+ size-of-next-token 1))))
        c)
      (scanner-error "amazingly long token" text-of-next-token)))

(define (accept kind)
  (set! kind-of-next-token kind)
  (set! next-token-is-ready #t)
  kind)

(define (accept-constant kind)
  (set! token-value
        (string->symbol
         (string-downcase
          (substring text-of-next-token 0 size-of-next-token))))
  (accept kind))

(define (accept-id)
  (let* ((s (substring text-of-next-token 0 size-of-next-token))
         (name (string->symbol (string-downcase s)))
         (probe (assq name reserved-words)))
    (if probe
        (accept (cdr probe))
        (begin (set! token-value (string->symbol s))
               (accept 'id)))))

(define reserved-words
  '((*end . end)
    (*productions . productions)
    (*terminals . terminals)))

; Initial state for lexical analyzer's state machine.

(define (scanner0)
  (let ((c (consume-char)))
    (cond ((eof-object? c)      (accept eoftoken))
          ((char=? c #\newline) (accept 'newline))
          ((char-whitespace? c) (set! size-of-next-token 0) (scanner0))
          ((char=? c #\#)       (accept 'sharpsign))
          ((char=? c #\<)       (scanner1))
          ((char=? c #\:)       (scanner2))
          ((char-numeric? c)    (scanner3))
          (else                 (scanner4)))))

; Scanning an identifier that began with a left angle bracket.

(define (scanner1)
  (let ((c (scan-char)))
    (cond ((eof-object? c)      (scanner-error "incomplete identifier"))
          ((char=? c #\>)       (consume-char)
                                (let ((c (scan-char)))
                                  (if (or (eof-object? c)
                                          (char-whitespace? c))
                                      (accept-id)
                                      (begin (scanner-warning)
                                             (consume-char)
                                             (scanner1)))))
          (else                 (consume-char) (scanner1)))))

; Scanning a token that began with a colon.

(define (scanner2)
  (let ((c (scan-char)))
    (cond ((eof-object? c)
           (accept-id))
          ((char=? c #\:)
           (consume-char)
           (let ((c (scan-char)))
             (cond ((eof-object? c)
                    (accept-id))
                   ((char=? c #\=)
                    (consume-char)
                    (let ((c (scan-char)))
                      (if (or (eof-object? c)
                              (char-whitespace? c))
                          (accept 'goesto)
                          (scanner4))))
                   (else (scanner4)))))
          (else (scanner4)))))

; Scanning an int or identifier.

(define (scanner3)
  (let ((c (scan-char)))
    (cond ((eof-object? c)      (accept-constant 'number))
          ((char-whitespace? c) (accept-constant 'number))
          ((char-numeric? c)    (consume-char) (scanner3))
          (else                 (scanner4)))))

; Scanning an identifier or reserved word.

(define (scanner4)
  (let ((c (scan-char)))
    (cond ((eof-object? c)      (accept-id))
          ((char-whitespace? c) (accept-id))
          ((char=? c #\<)       (consume-char) (scanner-warning) (scanner4))
          ((char=? c #\>)       (consume-char) (scanner-warning) (scanner4))
          (else                 (consume-char) (scanner4)))))

; Reporting and recovery of lexical errors.

(define (scanner-error msg . values)
  (display "ERROR: ")
  (display msg)
  (display " ")
  (for-each (lambda (v)
              (display v)
              (newline))
            values)
  (consume-token!)
  (next-token))

(define (scanner-warning)
  (display "WARNING: Possible misuse of < or >: ")
  (display (substring text-of-next-token 0 size-of-next-token))
  (newline))

; Parser (generated by ParseGen from parsegen0.pg).

(define (parse-input)
  (case (next-token)
    ((goesto newline sharpsign end productions id number terminals)
     (let ((ast1 (parse-garbage)))
          (if (eq? (next-token) 'terminals)
              (begin (consume-token!)
                     (let ((ast2 (parse-terminals)))
                          (if (eq? (next-token) 'productions)
                              (begin (consume-token!)
                                     (let
                                      ((ast3 (parse-productions)))
                                      (if (eq? (next-token) 'end)
                                          (begin (consume-token!)
                                                 (make-grammar0 ast1
                                                                ast2
                                                                ast3))
                                          (parse-error '<input>
                                                       '(end)))))
                              (parse-error '<input>
                                           '(productions)))))
              (parse-error '<input> '(terminals)))))
    (else
     (parse-error '<input>
                  '(end goesto
                        id
                        newline
                        number
                        productions
                        sharpsign
                        terminals)))))

(define (parse-garbage)
  (case (next-token)
    ((terminals) (list))
    ((goesto newline sharpsign end productions id number)
     (let ((ast1 (parse-anything_but_terminals)))
          (let ((ast2 (parse-garbage))) (ignore2 ast1 ast2))))
    (else
     (parse-error '<garbage>
                  '(end goesto
                        id
                        newline
                        number
                        productions
                        sharpsign
                        terminals)))))

(define (parse-anything_but_terminals)
  (case (next-token)
    ((goesto) (begin (consume-token!) (list)))
    ((newline) (begin (consume-token!) (list)))
    ((sharpsign) (begin (consume-token!) (list)))
    ((end) (begin (consume-token!) (list)))
    ((productions) (begin (consume-token!) (list)))
    ((id) (begin (consume-token!) (list)))
    ((number) (begin (consume-token!) (list)))
    (else
     (parse-error '<anything_but_terminals>
                  '(end goesto
                        id
                        newline
                        number
                        productions
                        sharpsign)))))

(define (parse-terminals)
  (case (next-token)
    ((productions) (list))
    ((newline)
     (begin (consume-token!)
            (let ((ast1 (parse-terminals))) (identity ast1))))
    ((id)
     (let ((ast1 (parse-id)))
          (let ((ast2 (parse-terminals))) (cons ast1 ast2))))
    (else (parse-error '<terminals> '(id newline productions)))))

(define (parse-productions)
  (case (next-token)
    ((end) (list))
    ((newline)
     (begin (consume-token!)
            (let ((ast1 (parse-productions))) (identity ast1))))
    ((id goesto)
     (let ((ast1 (parse-production)))
          (if (eq? (next-token) 'newline)
              (begin (consume-token!)
                     (let ((ast2 (parse-productions)))
                          (cons ast1 ast2)))
              (parse-error '<productions> '(newline)))))
    (else (parse-error '<productions> '(end goesto id newline)))))

(define (parse-production)
  (case (next-token)
    ((id)
     (let ((ast1 (parse-id)))
          (if (eq? (next-token) 'goesto)
              (begin (consume-token!)
                     (let ((ast2 (parse-rhs)))
                          (make-production ast1 ast2)))
              (parse-error '<production> '(goesto)))))
    ((goesto)
     (begin (consume-token!)
            (let ((ast1 (parse-rhs))) (make-default-lhs ast1))))
    (else (parse-error '<production> '(goesto id)))))

(define (parse-rhs)
  (case (next-token)
    ((newline) (list))
    ((sharpsign)
     (begin (consume-token!)
            (let ((ast1 (parse-actionproc)))
                 (let ((ast2 (parse-rhs))) (cons ast1 ast2)))))
    ((id)
     (let ((ast1 (parse-id)))
          (let ((ast2 (parse-rhs))) (cons ast1 ast2))))
    (else (parse-error '<rhs> '(id newline sharpsign)))))

(define (parse-actionproc)
  (case (next-token)
    ((id) (let ((ast1 (parse-id))) (make-actionproc ast1)))
    ((number) (let ((ast1 (parse-number))) (make-actionnum ast1)))
    (else (parse-error '<actionproc> '(id number)))))

(define (parse-id)
  (case (next-token)
    ((id) (begin (consume-token!) (make-identifier)))
    (else (parse-error '<id> '(id)))))

(define (parse-number)
  (case (next-token)
    ((number) (begin (consume-token!) (make-number)))
    (else (parse-error '<number> '(number)))))

; PARSE-ERROR.

; These variables are assigned.

(define total-errors 0)

(define (init-parse-error)
  (set! total-errors 0))

; To avoid a cascade of spurious error messages, only one error
; is counted or reported for any one line of the program.

(define (parse-error nonterminal expected)
  (if (< line-number-of-last-error line-number)
      (begin (set! line-number-of-last-error line-number)
             (set! total-errors (+ total-errors 1))
             (display "ERROR in line ")
             (display line-number)
             (display " while parsing ")
             (display nonterminal)
             (display ":")
             (newline)
             (display "    Encountered ")
             (display (next-token))
             (display " while expecting: ")
             (display expected)
             (newline)))
  (parse-error-recovery nonterminal expected)
  (mk-error))

; For now the error recovery has to be hand-coded.
; This is the simplest possible error recovery: exit!

(define (parse-error-recovery nonterminal expected)
  (reset))


; Action procedures.

(define (identity x) x)

(define (ignore2 x y) '())

(define (make-actionnum <number>)
  (make-actionproc
   (string->symbol
    (string-append "action-" (symbol->string <number>)))))

(define (make-actionproc <id>)
  (list 'action <id>))

(define (make-default-lhs <rhs>)
  (if last-lhs
      (make-production last-lhs <rhs>)
      (semantic-error "LHS cannot be omitted from first production")))

(define (mk-error)
  (make-grammar '() (make-production 'S '())))

(define (make-grammar0 <garbage> <terminals> <productions>)
  (make-grammar <terminals> <productions>))

(define (make-identifier)
  token-value)

(define (make-number)
  token-value)

(define (make-production <id> <rhs>)
  (set! last-lhs <id>)
  (list <id> <rhs>))

(define last-lhs #f)

; Conversion to the internal representation.

(define (make-grammar terminals productions)
  (define (terminal? x) (memq x terminals))
  (define (action? x) (pair? x))
  (define (action-name x)
    (let* ((name (cadr x))
           (n (string->number (symbol->string name))))
      (if (and n
               (exact? n)
               (integer? n)
               (not (negative? n)))
          (string->symbol (string-append "action-" name))
          name)))
  (define (generate-nonterminal)
    (set! nonterminal-counter (+ nonterminal-counter 1))
    (generate-identifier "nonterminal" nonterminal-counter))
  (define (generate-action-name)
    (if (actions-take-arguments)
        (set! action-counter (+ action-counter 1)))
    (generate-identifier "action" action-counter))
  (define (generate-identifier s n)
    (string->symbol
     (string-append "anonymous_" s "_" (number->string n))))
  (define (do-production! lhs rhs)
    (let* ((entry (assq lhs grammar)))
      (if entry
          (set-cdr! entry
                    (cons (rhs-loop rhs '())
                          (cdr entry)))
          (begin (set! grammar
                       (cons (list lhs) grammar))
                 (do-production! lhs rhs)))))
  (define (rhs-loop rhs new-rhs)
    (cond ((null? rhs)
           (rhs-loop (list (list 'action (generate-action-name)))
                     new-rhs))
          ((and (action? (car rhs))
                (null? (cdr rhs)))
           (list (reverse new-rhs)
                 (action-name (car rhs))))
          ((action? (car rhs))
           (let ((nt (generate-nonterminal)))
             (set! grammar
                   (cons (list nt
                               (list (reverse new-rhs)
                                     (action-name (car rhs))))
                         grammar))
             (rhs-loop (cons nt (cdr rhs)) '())))
          (else (rhs-loop (cdr rhs)
                          (cons (car rhs) new-rhs)))))
  (define grammar '())
  (define nonterminal-counter 0)
  (define action-counter 0)
  (for-each do-production!
            (map car productions)
            (map cadr productions))
  (reverse (append (map (lambda (t)
                          (list t (list (list (symbol->string t)) '*)))
                        terminals)
                   grammar)))
