; Code generator for C.

(define (generate-c-parser . args)
  (set! parser-language 'c)
  (apply generate-parser
         generate-c-declarations
         generate-c-for-nonterminal
         generate-c-finalize
         args))

; Comment to indicate that code must be inserted here.

(define c-blank "/* whatever you like */")

; Maximum length of a line of c code.

(define c-maxlinelength 80)

; Given a table of tokens and the simplified grammar (in which there is
; an action procedure associated with each production), writes C
; declarations for tokens, nonterminals, and so forth.

(define (generate-c-declarations
         token-kinds calls-to-actionprocs grammar)
  (let ((s "enum token0 { "))
    (gen-c 0 s)
    (gen-c-list-long (string-length s)
                     (sort1 (lambda (x y)
                              (string<? (symbol->string x)
                                        (symbol->string y)))
                            (map translate-token
                                 (map token.kind token-kinds))))
    (gen-c0 " }" #\; #\newline)
    (gen-c 0 #\newline)
    (gen-c 0 "typedef enum token0 token" #\; #\newline)
    (gen-c 0 #\newline))
  
  (let ((s "enum nonterminal0 { "))
    (gen-c 0 s)
    (gen-c-list-long (string-length s)
                     (map translate-nonterminal
                          (map entry.nonterminal grammar)))
    (gen-c0 " }" #\; #\newline)
    (gen-c 0 #\newline)
    (gen-c 0 "typedef enum nonterminal0 nonterminal" #\; #\newline)
    (gen-c 0 #\newline))
  
  (gen-c 0 "struct tokens0 {" #\; #\newline)
  (gen-c 0 "    token first" #\; #\newline)
  (gen-c 0 "    struct tokens0 *rest" #\; #\newline)
  (gen-c 0 "}" #\; #\newline)
  (gen-c 0 #\newline)
  (gen-c 0 "typedef struct tokens0 *tokens" #\; #\newline)
  (gen-c 0 #\newline)
  
  (gen-c 0 #\newline)
  (gen-c 0 "/*************************************************/" #\newline)
  (gen-c 0 "/*                                               */" #\newline)
  (gen-c 0 "/*    Each action procedure must return an ast.  */" #\newline)
  (gen-c 0 "/*    An ast may be any type whatsoever.         */" #\newline)
  (gen-c 0 "/*                                               */" #\newline)
  (gen-c 0 "/*************************************************/" #\newline)
  (gen-c 0 #\newline)
  
  (generate-c-actionprocs calls-to-actionprocs)
  (gen-c 0 #\newline)
  
  (gen-c 0 "extern ast "
           (generate-parser-procname (grammar.start grammar))
           "()" #\; #\newline)
  (gen-c 0 #\newline)
  
  (gen-c 0 "/*  End of parser.h  */" #\newline)
  (gen-c 0 #\newline)
  
  (for-each (lambda (nonterminal)
              (gen-c 0 "ast "
                       (generate-parser-procname nonterminal)
                       "()" #\; #\newline))
            (map entry.nonterminal (cdr grammar)))
  (gen-c 0 #\newline))

(define (generate-c-actionprocs calls)
  (do ((calls calls (cdr calls)))
      ((null? calls))
      (let ((call (car calls)))
        (if (not (assq (car call) (cdr calls)))
            (begin
             (gen-c 0 "extern ast "
                      (translate-procname (car call)))
             (if (null? (cdr call))
                 (gen-c0 "()" #\; #\newline)
                 (begin
                  (gen-c0 "(")
                  (gen-c-list
                   (do ((args (map translate-nonterminal-to-variable
                                   (cdr call))
                              (cdr args))
                        (new '()
                             (cons (string->symbol
                                    (string-append
                                     "ast "
                                     (symbol->string (car args))
                                     "_"
                                     (number->string i)))
                                   new))
                        (i 1 (+ i 1)))
                       ((null? args) (reverse new))))
                  (gen-c0 ")" #\; #\newline))))))))

; Given a nonterminal, the list of right hand sides for that nonterminal,
; the list of director sets for that nonterminal, and the list of
; names of procedures that create abstract syntax trees for those
; productions,
; generates the code for a procedure that parses that nonterminal.

(define (generate-c-for-nonterminal nonterminal rhss dsets creators)
  (gen-c0 "ast " (generate-parser-procname nonterminal) "() {" #\newline)
  (let ((n (apply max
                  (map (lambda (rhs)
                         (length (filter symbol? rhs)))
                       rhss))))
    (if (positive? n)
        (begin (init-temp-generator)
               (gen-c 4 "ast ")
               (gen-c-list (generate-temps n))
               (gen-c0 #\; #\newline))))
  (gen-c 4 "switch (nextToken()) {" #\newline)
  (for-each (lambda (rhs dset creator)
              (init-temp-generator)
              (let ((kinds (map lookup-token-kind dset)))
                (for-each (lambda (kind)
                            (gen-c 8 "case " kind ":" #\newline))
                          kinds))
              (generate-c-for-production
               nonterminal rhs creator)
              (gen-c 12 "break" #\; #\newline))
            rhss
            dsets
            creators)
  (gen-c 8 "default:" #\newline)
  (let ((s1 "return parseError(")
        (name2 (translate-nonterminal nonterminal))
        (s2 ", ")
        (items (sorted-union
                (map (lambda (dset)
                       (map lookup-token-kind dset))
                     dsets))))
    (gen-c 12 s1 name2 s2)
    (gen-c-set-long (+ 12 (string-length s1) 
                          (string-length (symbol->string name2))
                          (string-length s2))
                    items)
    (gen-c0 ")" #\; #\newline))
  
  (gen-c 4 "}" #\newline)
  (gen-c 0 "} /* end of "
           (generate-parser-procname nonterminal)
           " */" #\newline)
  (gen-c 0 #\newline))

; Given a nonterminal, a rhs for that nonterminal
; the director set for that nonterminal,
; and the name of the procedure that creates abstract syntax trees
; for that nonterminal,
; generates code to parse that rhs and return an abstract syntax tree.
;
; If the right hand side begins with a terminal,
; then the dispatch on the lookahead token has already confirmed that
; that terminal is the lookahead token.

(define (generate-c-for-production nonterminal rhs creator)
  (if (and (not (null? rhs))
           (string? (car rhs)))
      (begin (gen-c 12 "consumeToken()" #\; #\newline)
             (generate-c-for-production1
              nonterminal (cdr rhs) creator '() 12))
      (generate-c-for-production1 nonterminal rhs creator '() 12)))

(define (generate-c-for-production1 nonterminal rhs creator trees i)
  (cond ((null? rhs)
         (gen-c i "return " (translate-procname creator) "(")
         (if (actions-take-arguments)
             (gen-c-list (reverse trees)))
         (gen-c0 ")" #\; #\newline))
        ((string? (car rhs))
         (let ((kind (lookup-token-kind (car rhs))))
           (gen-c i "if (nextToken() == " kind ") {" #\newline)
           (gen-c (+ i 4) "consumeToken()" #\; #\newline)
           (generate-c-for-production1
            nonterminal
            (cdr rhs)
            creator
            trees
            (+ i 4))
           (gen-c i "}" #\newline)
           (gen-c i "else return parseError(")
           (gen-c0 (translate-nonterminal nonterminal) ", ")
           (gen-c-set (list kind))
           (gen-c0 ")" #\; #\newline)))
        (else (let ((temp (generate-temp))
                    (proc (generate-parser-procname (car rhs))))
                (gen-c i temp " = " proc "()" #\; #\newline)
                (generate-c-for-production1
                 nonterminal
                 (cdr rhs)
                 creator
                 (cons temp trees)
                 i)))))

(define (generate-c-finalize) #t)

; Output routine for c code.
; The first argument indicates the number of spaces to indent.

(define (gen-c indent . args)
  (display (make-string indent #\space) output-port1)
  (for-each (lambda (x)
              (display x output-port1))
            args))

; Output routine for use in the middle of a line, when indentation
; is not called for.

(define (gen-c0 . args)
  (apply gen-c 0 args))

; Outputs a comma-separated list.

(define (gen-c-list items)
  (if (not (null? items))
      (begin (gen-c0 (car items))
             (for-each (lambda (item)
                         (gen-c0 ", " item))
                       (cdr items)))))

; Outputs a comma-separated list, using more than one line if the list
; is long enough.  The first argument is the indentation of the first
; element in the list.  Each item in the list must be a symbol.

(define (gen-c-list-long indent items)
  (let ((sizes (map (compose1 string-length symbol->string) items)))
    (define (loop n items sizes)
      (if (not (null? sizes))
          (let ((item (car items))
                (n (+ n 2 (car sizes))))
            (if (< n c-maxlinelength)
                (begin (gen-c0 ", " item)
                       (loop n (cdr items) (cdr sizes)))
                (begin (gen-c0 "," #\newline)
                       (gen-c indent item)
                       (loop (+ indent (car sizes))
                             (cdr items)
                             (cdr sizes)))))))
    (if (not (null? items))
        (begin (gen-c0 (car items))
               (loop (+ indent (car sizes)) (cdr items) (cdr sizes))))))

; Outputs an expression that constructs a value of type "tokens"
; that represents a set of the given tokens.
; These sets are constructed only when a syntax error is detected.
; The storage allocated for these lists may be reclaimed by parseError,
; but why bother?

(define (gen-c-set tokens)
  (if (null? tokens)
      (gen-c0 "NULL")
      (begin (gen-c0 "makeTokens(" (car tokens) ", ")
             (gen-c-set (cdr tokens))
             (gen-c0 ")"))))

; temporary definition

(define (gen-c-set-long i tokens)
  (gen-c-set tokens))
