; Code generator for Java.

(define (generate-java-parser . args)
  (set! parser-language 'java)
  (apply generate-parser
         generate-java-declarations
         (make-generate-java-for-nonterminal)
         generate-java-finalize
         args))

; Maximum length of a line of Java code.

(define java-maxlinelength 72)

; Given a table of tokens and the simplified grammar (in which there is
; an action procedure associated with each production), writes Java
; declarations for tokens, nonterminals, and so forth.

(define (generate-java-declarations
         token-kinds calls-to-actionprocs grammar)
  
  (gen-java0 "package parser" #\; #\newline)
  (gen-java0 #\newline)
  
  (gen-java0 "public class Parser {" #\newline)
  (gen-java0 #\newline)
  
  (gen-java0 "    public Parser (Scanner s, Actions a) {" #\newline)
  (gen-java0 "        scanner = s" #\; #\newline)
  (gen-java0 "        actions = a" #\; #\newline)
  (gen-java0 "    }" #\newline)
  (gen-java0 #\newline)
  (gen-java0 "    private Scanner scanner" #\; #\newline)
  (gen-java0 "    private Actions actions" #\; #\newline)
  (gen-java0 #\newline)
  
  (gen-java 4 "/*  Tokens.  */" #\newline #\newline)
  (do ((i 1 (+ i 1))
       (tokens (sort1 (lambda (x y)
                        (string<? (symbol->string x)
                                  (symbol->string y)))
                      (map translate-token
                           (map token.kind token-kinds)))
               (cdr tokens)))
      ((null? tokens))
      (gen-java 4 "public static final int "
                  (car tokens) " = " i #\; #\newline))
  (gen-java0 #\newline)
  
  (gen-java 4 "/*  Nonterminals.  */" #\newline #\newline)
  (do ((i 1 (+ i 1))
       (nonterminals (sort1 (lambda (x y)
                              (string<? (symbol->string x)
                                        (symbol->string y)))
                            (map translate-nonterminal
                                 (map entry.nonterminal grammar)))
                     (cdr nonterminals)))
      ((null? nonterminals))
      (gen-java 4 "public static final int "
                  (car nonterminals) " = " i #\; #\newline))
  (gen-java0 #\newline)
  
  (gen-java 4 "/*  Scanner routines.  */" #\newline #\newline)
  (gen-java 4 "private final int nextToken () { ")
  (gen-java 0 "return scanner.nextToken()" #\; " }" #\newline)
  (gen-java 4 "private final void consumeToken () { ")
  (gen-java 0 "scanner.consumeToken()" #\; " }" #\newline)
  (gen-java0 #\newline)
  
  (gen-java 4 "/*  Error routine.  */" #\newline #\newline)
  (gen-java 4 "private final Ast parseError (int x, Tokens y) {" #\newline)
  (gen-java 4 "    return scanner.parseError (x, y)" #\; #\newline)
  (gen-java 4 "}" #\newline)
  (gen-java0 #\newline)
  
  (gen-java 4 "/*  Action routines.  */" #\newline #\newline)
  (generate-java-actionprocs calls-to-actionprocs)
  (gen-java0 #\newline)
  
  (gen-java 4 "/*  Parsing routines.  */" #\newline)
  (gen-java0 #\newline))

(define (generate-java-actionprocs calls)
  (do ((calls calls (cdr calls)))
      ((null? calls))
      (let ((call (car calls)))
        (if (not (assq (car call) (cdr calls)))
            (let ((procname (translate-procname (car call)))
                  (args
                   (do ((args (map translate-nonterminal-to-variable
                                   (cdr call))
                              (cdr args))
                        (new '()
                             (cons (string->symbol
                                    (string-append
                                     (symbol->string (car args))
                                     "_"
                                     (number->string i)))
                                   new))
                        (i 1 (+ i 1)))
                       ((null? args) (reverse new)))))
              (gen-java 4 "private final Ast " procname)
              (if (null? (cdr call))
                  (gen-java0 "()")
                  (begin
                   (gen-java0 "(")
                   (gen-java0 "Ast " (car args))
                   (for-each (lambda (arg)
                               (gen-java0 ", Ast " arg))
                             (cdr args))
                   (gen-java0 ")")))
              (gen-java0 " {" #\newline)
              (gen-java 8 "return actions." procname " (")
              (gen-java-list args)
              (gen-java0 ")" #\; #\newline)
              (gen-java 4 "}" #\newline))))))

; Given a nonterminal, the list of right hand sides for that nonterminal,
; the list of director sets for that nonterminal, and the list of
; names of procedures that create abstract syntax trees for those
; productions,
; generates the code for a procedure that parses that nonterminal.
; The first one is public, and the rest private, so a little trickery
; is needed.

(define (make-generate-java-for-nonterminal)
  (let ((visibility "public"))
    (define (generate-java-for-nonterminal nonterminal rhss dsets creators)
      (gen-java 4 visibility
                  " Ast "
                  (generate-parser-procname nonterminal)
                  " () {" #\newline)
      (let ((n (apply max
                      (map (lambda (rhs)
                             (length (filter symbol? rhs)))
                           rhss))))
        (if (positive? n)
            (begin (init-temp-generator)
                   (gen-java 8 "Ast ")
                   (gen-java-list (generate-temps n))
                   (gen-java0 #\; #\newline))))
      (gen-java 8 "switch (nextToken()) {" #\newline)
      (for-each (lambda (rhs dset creator)
                  (init-temp-generator)
                  (let ((kinds (map lookup-token-kind dset)))
                    (for-each (lambda (kind)
                                (gen-java 12 "case " kind ":" #\newline))
                              kinds))
                  (generate-java-for-production
                   nonterminal rhs creator))
                rhss
                dsets
                creators)
      (gen-java 12 "default:" #\newline)
      (let ((s1 "return parseError (")
            (name2 (translate-nonterminal nonterminal))
            (s2 ", ")
            (items (sorted-union
                    (map (lambda (dset)
                           (map lookup-token-kind dset))
                         dsets))))
        (gen-java 16 s1 name2 s2)
        (gen-java-set-long (+ 16 (string-length s1) 
                                 (string-length (symbol->string name2))
                                 (string-length s2))
                           items)
        (gen-java0 ")" #\; #\newline))
      
      (gen-java 8 "}" #\newline)
      (gen-java 4 "} /* end of "
                  (generate-parser-procname nonterminal)
                  " */" #\newline)
      (gen-java 0 #\newline))
    (lambda (nonterminal rhss dsets creators)
      (generate-java-for-nonterminal nonterminal rhss dsets creators)
      (set! visibility "private"))))

; Given a nonterminal, a rhs for that nonterminal
; the director set for that nonterminal,
; and the name of the procedure that creates abstract syntax trees
; for that nonterminal,
; generates code to parse that rhs and return an abstract syntax tree.
;
; If the right hand side begins with a terminal,
; then the dispatch on the lookahead token has already confirmed that
; that terminal is the lookahead token.

(define (generate-java-for-production nonterminal rhs creator)
  (if (and (not (null? rhs))
           (string? (car rhs)))
      (begin (gen-java 16 "consumeToken()" #\; #\newline)
             (generate-java-for-production1
              nonterminal (cdr rhs) creator '() 16))
      (generate-java-for-production1 nonterminal rhs creator '() 16)))

(define (generate-java-for-production1 nonterminal rhs creator trees i)
  (cond ((null? rhs)
         (gen-java i "return " (translate-procname creator) " (")
         (if (actions-take-arguments)
             (gen-java-list (reverse trees)))
         (gen-java0 ")" #\; #\newline))
        ((string? (car rhs))
         (let ((kind (lookup-token-kind (car rhs))))
           (gen-java i "if (nextToken() == " kind ") {" #\newline)
           (gen-java (+ i 4) "consumeToken()" #\; #\newline)
           (generate-java-for-production1
            nonterminal
            (cdr rhs)
            creator
            trees
            (+ i 4))
           (gen-java i "}" #\newline)
           (let* ((nonterminal (translate-nonterminal nonterminal))
                  (s1 "else return parseError(")
                  (s2 (symbol->string nonterminal))
                  (s3 ", "))
             (gen-java i s1 s2 s3)
             (gen-java-set-long (+ i (string-length s1)
                                     (string-length s2)
                                     (string-length s3))
                                (list kind))
             (gen-java0 ")" #\; #\newline))))
        (else (let ((temp (generate-temp))
                    (proc (generate-parser-procname (car rhs))))
                (gen-java i temp " = " proc "()" #\; #\newline)
                (generate-java-for-production1
                 nonterminal
                 (cdr rhs)
                 creator
                 (cons temp trees)
                 i)))))

(define (generate-java-finalize)
  (gen-java0 "}" #\newline))

; Output routine for Java code.
; The first argument indicates the number of spaces to indent.

(define (gen-java indent . args)
  (display (make-string indent #\space) output-port1)
  (for-each (lambda (x)
              (display x output-port1))
            args))

; Output routine for use in the middle of a line, when indentation
; is not called for.

(define (gen-java0 . args)
  (apply gen-java 0 args))

; Outputs a comma-separated list.

(define (gen-java-list items)
  (if (not (null? items))
      (begin (gen-java0 (car items))
             (for-each (lambda (item)
                         (gen-java0 ", " item))
                       (cdr items)))))

; Outputs a comma-separated list, using more than one line if the list
; is long enough.  The first argument is the indentation of the first
; element in the list.  Each item in the list must be a symbol.

(define (gen-java-list-long indent items)
  (let ((sizes (map (compose1 string-length symbol->string) items)))
    (define (loop n items sizes)
      (if (not (null? sizes))
          (let ((item (car items))
                (n (+ n 2 (car sizes))))
            (if (< n java-maxlinelength)
                (begin (gen-java0 ", " item)
                       (loop n (cdr items) (cdr sizes)))
                (begin (gen-java0 "," #\newline)
                       (gen-java indent item)
                       (loop (+ indent (car sizes))
                             (cdr items)
                             (cdr sizes)))))))
    (if (not (null? items))
        (begin (gen-java0 (car items))
               (loop (+ indent (car sizes)) (cdr items) (cdr sizes))))))

; Outputs an expression that constructs a value of type "tokens"
; that represents a set of the given tokens.
; These sets are constructed only when a syntax error is detected.
; The storage allocated for these lists may be reclaimed by parseError,
; but why bother?

(define (gen-java-set tokens)
  (if (null? tokens)
      (gen-java0 "null")
      (begin (gen-java0 "new Tokens(" (car tokens) ", ")
             (gen-java-set (cdr tokens))
             (gen-java0 ")"))))

; Like gen-java-set, but uses more than one line if necessary.

(define (gen-java-set-long i tokens)
  (let ((n (length tokens))
        (indentation 24)
        (s0 "null")
        (s1 "new Tokens (")
        (s2 ", "))
    (let ((length_s0 (+ (string-length s0) n))
          (length_s1s2 (+ (string-length s1)
                          (string-length s2))))
      (define (loop i tokens)
        (if (null? tokens)
            (begin (if (> (+ i length_s0)
                          java-maxlinelength)
                       (begin (gen-java0 #\newline)
                              (gen-java indentation)))
                   (gen-java0 s0)
                   (gen-java0 (make-string n (string-ref ")" 0))))
            (let* ((token (car tokens))
                   (t (symbol->string token))
                   (length_t (string-length t)))
              (if (and (> (+ i length_s1s2 length_t)
                          java-maxlinelength)
                       (> i indentation))
                  (begin (gen-java0 #\newline)
                         (gen-java indentation)
                         (loop indentation tokens))
                  (begin (gen-java0 s1 (car tokens) s2)
                         (loop (+ i length_s1s2 length_t)
                               (cdr tokens)))))))
      (loop i tokens))))
