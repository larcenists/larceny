; Code generator for Pascal.

(define (generate-pascal-parser . args)
  (set! parser-language 'pascal)
  (apply generate-parser
         generate-pascal-declarations
         generate-pascal-for-nonterminal
         generate-pascal-finalize
         args))

; Comment to indicate that code must be inserted here.

(define pascal-blank "(* whatever you like *)")

; Maximum length of a line of Pascal code.

(define pascal-maxlinelength 80)

; Set of all token strings.
; Assigned by generate-pascal-declarations for the benefit of
; generate-pascal-for-nonterminal, which needs it because the
; ISO Pascal case statement doesn't have an else clause.

(define pascal-token-strings '())

; Given a table of tokens and the simplified grammar (in which there is
; an action procedure associated with each production), writes Pascal
; declarations for tokens, nonterminals, and so forth.

(define (generate-pascal-declarations
         token-kinds calls-to-actionprocs grammar)
  (set! pascal-token-strings (map token.string token-kinds))
  (gen-pascal 0 "type" #\newline)
  (let ((s "token = ("))
    (gen-pascal 2 s)
    (gen-pascal-list-long (+ 2 (string-length s))
                          (sort1 (lambda (x y)
                                   (string<? (symbol->string x)
                                             (symbol->string y)))
                                 (map translate-token
                                      (map token.kind token-kinds))))
    (gen-pascal0 ")" #\; #\newline))
  
  (let ((s "nonterminal = ("))
    (gen-pascal 2 s)
    (gen-pascal-list-long (+ 2 (string-length s))
                          (map translate-nonterminal
                               (map entry.nonterminal grammar)))
    (gen-pascal0 ")" #\; #\newline))
  
  (gen-pascal 2 "tokens = set of token" #\; #\newline)
  (gen-pascal 2 #\newline)
  (gen-pascal 2 "(*************************************************)" #\newline)
  (gen-pascal 2 "(*                                               *)" #\newline)
  (gen-pascal 2 "(*    Each action procedure must return an ast.  *)" #\newline)
  (gen-pascal 2 "(*    An ast may be any type whatsoever.         *)" #\newline)
  (gen-pascal 2 "(*                                               *)" #\newline)
  (gen-pascal 2 "(*************************************************)" #\newline)
  (gen-pascal 2 #\newline)
  (gen-pascal 2 "ast = " pascal-blank #\; #\newline)
  (gen-pascal 2 #\newline)
  
  (generate-pascal-actionprocs calls-to-actionprocs)
  (gen-pascal 0 #\newline)
  
  (for-each (lambda (nonterminal)
              (gen-pascal 0 "function ")
              (gen-pascal0 (generate-parser-procname nonterminal))
              (gen-pascal0 ": ast" #\; " forward" #\; #\newline))
            (map entry.nonterminal grammar))
  (gen-pascal 0 #\newline))

(define (generate-pascal-actionprocs calls)
  (do ((calls calls (cdr calls)))
      ((null? calls))
      (let ((call (car calls)))
        (if (not (assq (car call) (cdr calls)))
            (begin
             (gen-pascal 0
                         "function "
                         (translate-procname (car call)))
             (if (not (null? (cdr call)))
                 (begin
                  (gen-pascal0 "(")
                  (gen-pascal-list
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
                       ((null? args) (reverse new))))
                  (gen-pascal0 ": ast)")))
             (gen-pascal0 ": ast" #\; #\newline)
             (gen-pascal 2 "begin" #\newline)
             (gen-pascal 4 pascal-blank #\newline)
             (gen-pascal 2 "end" #\; #\newline)
             (gen-pascal 0 #\newline))))))

; Given a nonterminal, the list of right hand sides for that nonterminal,
; the list of director sets for that nonterminal, and the list of
; names of procedures that create abstract syntax trees for those
; productions,
; generates the code for a procedure that parses that nonterminal.

(define (generate-pascal-for-nonterminal nonterminal rhss dsets creators)
  (gen-pascal0 "function "
               (generate-parser-procname nonterminal)
               " (* : ast *)"
               #\; #\newline)
  (let ((n (apply max
                  (map (lambda (rhs)
                         (length (filter symbol? rhs)))
                       rhss))))
    (if (positive? n)
        (begin (init-temp-generator)
               (gen-pascal 2 "var ")
               (gen-pascal-list (generate-temps n))
               (gen-pascal0 ": ast" #\; #\newline))))
  (gen-pascal 2 "begin" #\newline)
  (gen-pascal 4 "case nextToken of" #\newline)
  (for-each (lambda (rhs dset creator)
              (init-temp-generator)
              (let ((kinds (map lookup-token-kind dset)))
                (gen-pascal 6)
                (gen-pascal-list-long 6 kinds)
                (gen-pascal0 ":" #\newline))
              (generate-pascal-for-production
               nonterminal rhs creator))
            rhss
            dsets
            creators)
  (let ((error-tokens
         (map lookup-token-kind
              (difference pascal-token-strings (apply union dsets)))))
    (gen-pascal 6)
    (gen-pascal-list-long 6 error-tokens)
    (gen-pascal0 ":" #\newline))
  (let ((name1 (generate-parser-procname nonterminal))
        (s1 " := parseError (")
        (name2 (translate-nonterminal nonterminal))
        (s2 ", [")
        (items (sorted-union
                (map (lambda (dset)
                       (map lookup-token-kind dset))
                     dsets))))
    (gen-pascal 8 name1 s1 name2 s2)
    (gen-pascal-list-long (+ 8
                             (string-length (symbol->string name1))
                             (string-length s1) 
                             (string-length (symbol->string name2))
                             (string-length s2))
                          items)
    (gen-pascal0 "])" #\newline))
  
  (gen-pascal 4 "end" #\newline)
  (gen-pascal 2 "end (* "
                (generate-parser-procname nonterminal)
                " *)" #\; #\newline)
  (gen-pascal 0 #\newline))

; Given a nonterminal, a rhs for that nonterminal
; the director set for that nonterminal,
; and the name of the procedure that creates abstract syntax trees
; for that nonterminal,
; generates code to parse that rhs and return an abstract syntax tree.
;
; If the right hand side begins with a terminal,
; then the dispatch on the lookahead token has already confirmed that
; that terminal is the lookahead token.

(define (generate-pascal-for-production nonterminal rhs creator)
  (gen-pascal 8 "begin" #\newline)
  (if (and (not (null? rhs))
           (string? (car rhs)))
      (begin (gen-pascal 10 "consumeToken" #\; #\newline)
             (generate-pascal-for-production1
              nonterminal (cdr rhs) creator '() 10))
      (generate-pascal-for-production1 nonterminal rhs creator '() 10))
  (gen-pascal 8 "end" #\; #\newline))

(define (generate-pascal-for-production1 nonterminal rhs creator trees i)
  (cond ((null? rhs)
         (gen-pascal i
                     (generate-parser-procname nonterminal)
                     " := "
                     (translate-procname creator))
         (if (not (null? trees))
             (begin (gen-pascal0 "(")
                    (gen-pascal-list (reverse trees))
                    (gen-pascal0 ")")))
         (gen-pascal0 #\newline))
        ((string? (car rhs))
         (let ((kind (lookup-token-kind (car rhs))))
           (gen-pascal i "if nextToken = " kind #\newline)
           (gen-pascal (+ i 3) "then begin" #\newline)
           (gen-pascal (+ i 10) "consumeToken" #\; #\newline)
           (generate-pascal-for-production1
            nonterminal
            (cdr rhs)
            creator
            trees
            (+ i 10))
           (gen-pascal (+ i 3) "     end" #\newline)
           (gen-pascal (+ i 3)
                       "else "
                       (generate-parser-procname nonterminal)
                       " := parseError (")
           (gen-pascal0 (translate-nonterminal nonterminal)
                        ", [" kind "])" #\newline)))
        (else (let ((temp (generate-temp)))
                (gen-pascal i
                            temp
                            " := "
                            (generate-parser-procname (car rhs))
                            #\;
                            #\newline)
                (generate-pascal-for-production1
                 nonterminal
                 (cdr rhs)
                 creator
                 (cons temp trees)
                 i)))))

(define (generate-pascal-finalize) #t)

; Output routine for Pascal code.
; The first argument indicates the number of spaces to indent.

(define (gen-pascal indent . args)
  (display (make-string indent #\space) output-port1)
  (for-each (lambda (x)
              (display x output-port1))
            args))

; Output routine for use in the middle of a line, when indentation
; is not called for.

(define (gen-pascal0 . args)
  (apply gen-pascal 0 args))

; Outputs a comma-separated list.

(define (gen-pascal-list items)
  (if (not (null? items))
      (begin (gen-pascal0 (car items))
             (for-each (lambda (item)
                         (gen-pascal0 ", " item))
                       (cdr items)))))

; Outputs a comma-separated list, using more than one line if the list
; is long enough.  The first argument is the indentation of the first
; element in the list.  Each item in the list must be a symbol.

(define (gen-pascal-list-long indent items)
  (let ((sizes (map (compose1 string-length symbol->string) items)))
    (define (loop n items sizes)
      (if (not (null? sizes))
          (let ((item (car items))
                (n (+ n 2 (car sizes))))
            (if (< n pascal-maxlinelength)
                (begin (gen-pascal0 ", " item)
                       (loop n (cdr items) (cdr sizes)))
                (begin (gen-pascal0 "," #\newline)
                       (gen-pascal indent item)
                       (loop (+ indent (car sizes))
                             (cdr items)
                             (cdr sizes)))))))
    (if (not (null? items))
        (begin (gen-pascal0 (car items))
               (loop (+ indent (car sizes)) (cdr items) (cdr sizes))))))
