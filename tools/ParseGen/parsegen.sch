; Copyright 1993 William Clinger
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; Target-independent part of the LL(1) parser generator.
; Well, it's mostly target-independent.  If you write a code generator
; for a new language, you ought to confirm that the naming conventions
; used for Pascal and C are compatible with the new language.  If they
; are, then you won't have to modify this file, provided your code
; generator performs the following assignment at the beginning of
; parser generation:
;
;     (set! parser-language 'pascal)
;
; Otherwise you'll have to search this file for occurrences of the
; variable "parser-language", and make changes where necessary.

; Some of the tables are global variables for now.
; These variables are assigned at the start of parser generation.

(define parser-language 'scheme)
(define token-kinds '())
(define output-port1 (current-output-port))
(define output-port2 (current-output-port))

(define (generate-parser generate-parser-declarations
                         generate-parser-for-nonterminal
                         generate-parser-finalize
                         grammar
                         . rest)
  (call-with-current-continuation
   (lambda (k)
     (set! quit k)
     (cond ((null? rest)
            (set! output-port1 (current-output-port))
            (set! output-port2 (current-output-port)))
           ((and (null? (cdr rest))
                 (output-port? (car rest)))
            (set! output-port1 (car rest))
            (set! output-port2 (current-output-port)))
           ((and (null? (cddr rest))
                 (output-port? (car rest))
                 (output-port? (cadr rest)))
            (set! output-port1 (car rest))
            (set! output-port2 (cadr rest)))
           (else (apply generate-error "bad output port" rest)))
     (generate-progress "Checking grammar")
     (check-grammar grammar)
     (generate-progress "Computing tokens")
     (let* ((x (split-grammar grammar))
            (aliases (car x))
            (grammar (cadr x)))
       (set! token-kinds (generate-token-kinds aliases grammar))
       (generate-progress "Computing director sets")
       (let ((dsets (director-sets (unaugmented-grammar grammar)))
             (calls (calls-to-actionprocs grammar)))
         (if (not (LL1-director-sets? dsets))
             (generate-error "grammar is not LL(1)"))
         (generate-progress "Generating declarations")
         (generate-parser-declarations token-kinds calls grammar)
         (generate-progress "Generating parsing procedures")
         (for-each (lambda (nonterminal rhs dset action)
                     (let ((def (generate-parser-for-nonterminal
                                 nonterminal rhs dset action)))
                       (write-definition def output-port1)))
                   (map entry.nonterminal grammar)
                   (map (lambda (entry)
                          (map production.rhs
                               (entry.productions entry)))
                        grammar)
                   (map cdr dsets)
                   (map (lambda (entry)
                          (map production.actionproc
                               (entry.productions entry)))
                        grammar))
         (generate-parser-finalize)
         (generate-progress "Generating tables")
         (write-token-table token-kinds output-port2)
         (write-actionprocs calls output-port2))))))

; Names.
; Each language has its idiomatic way of naming procedures, variables,
; and enumerated values.  Internally this parser generator uses Scheme
; conventions, but translates Scheme names into idiomatic names when
; the code and tables are generated.  This translation is not quite
; one to one, so beware.  The translation is illustrated by examples:
;
; Class of name                 Scheme       Pascal       C
;
; token                         if           zif          zif
; nonterminal                   var          yvar         yvar
;                               <var>        yvar         yvar
; nonterminal used as variable  xvar_1       xvar_1       xvar_1
; procedure                     makeExpr     makeExpr     makeExpr
;                               make-expr    makeExpr     makeExpr
;                               parse-expr   parseExpr    parseExpr

(define (translate-token kind)
  (case parser-language
    ((scheme) kind)
    ((pascal c java)
     (string->symbol (string-append "z" (symbol->string kind))))
    (else ???)))

(define (translate-nonterminal nonterminal)
  (case parser-language
    ((scheme) nonterminal)
    ((pascal c java)
     (let ((s (symbol->string nonterminal)))
       (if (identifier-like? s)
           (string->symbol (string-append "y" s))
           (string->symbol (string-append "y" (identifierize s))))))
    (else ???)))

(define (translate-nonterminal-to-variable nonterminal . subscripts)
  (let ((s (symbol->string (translate-nonterminal nonterminal)))
        (i (if (null? subscripts)
               ""
               (apply string-append
                      "_"
                      (map number->string subscripts)))))
    (string->symbol
     (case parser-language
       ((scheme)
        (string-append "x" s i))
       ((pascal c java)
        (string-append "x" (substring s 1 (string-length s)) i))
       (else ???)))))

(define (translate-procname proc)
  (case parser-language
    ((scheme) proc)
    ((pascal c java)
     (let ((s (symbol->string proc)))
       (if (identifier-like? s)
           proc
           (string->symbol (identifierize s)))))
    (else ???)))

(define (identifierize s)
  (let ((n (string-length s)))
    (if (and (char=? (string-ref s 0) #\<)
             (char=? (string-ref s (- n 1)) #\>))
        (identifierize (substring s 1 (- n 1)))
        (let loop ((s (string->list s))
                   (new '()))
          (cond ((null? s) (list->string (reverse new)))
                ((or (char-alphabetic? (car s))
                     (char-numeric? (car s))
                     (char=? (car s) #\_))
                 (loop (cdr s) (cons (car s) new)))
                ((char=? (car s) #\space)
                 (loop (cdr s) (cons #\_ new)))
                ((null? (cdr s)) (loop (cdr s) new))
                (else (loop (cons (char-upcase (cadr s)) (cddr s))
                            new)))))))

; The next two procedures should be called only by the code generators,
; when they are generating output, and by the routines that generate
; tables.

(define (lookup-token-kind token)
  (let ((probe (assoc token token-kinds)))
    (if probe
        (translate-token (token.kind probe))
        (error "Token not found in token-kinds" token))))

; Generates "parse-expr" instead of "parse-<expr>", even for Scheme.
; I didn't like the looks of parse-<expr> in Scheme.

(define (generate-parser-procname nonterminal)
  (let* ((s (symbol->string nonterminal))
         (n (string-length s))
         (s (if (and (char=? (string-ref s 0) #\<)
                     (char=? (string-ref s (- n 1)) #\>))
                (substring s 1 (- n 1))
                s)))
    (translate-procname (string->symbol (string-append "parse-" s)))))

; Generator of symbolic names for tokens.

(define token-counter 0)

(define (init-token-generator)
  (set! temp-counter 0))

(define (generate-token)
  (set! temp-counter (+ temp-counter 1))
  (string->symbol (string-append "z" (number->string temp-counter))))

; Generator of temporary variable names for the abstract syntax trees
; that will be passed to the creator of an abstract syntax tree.

(define temp-counter 0)

(define (init-temp-generator)
  (set! temp-counter 0))

(define (generate-temp)
  (set! temp-counter (+ temp-counter 1))
  (string->symbol (string-append "ast" (number->string temp-counter))))

(define (generate-temps n)
  (if (zero? n)
      '()
      (let ((t1 (generate-temp)))
        (cons t1 (generate-temps (- n 1))))))

; Tokens.
;
; The token-kinds table, assigned by generate-parser, is an association list
; that maps the terminal strings of the grammar to identifier-like symbols.
; It also maps the symbol $, which is used internally as a terminal string
; even though it is not a Scheme string, to some identifier-like symbol.

(define token.string car)
(define token.kind cadr)

; Given a grammar, find all the terminal strings and choose symbolic names
; for them.  The names chosen by the programmer (using productions with no
; action procedure) have priority, followed by the default names for
; identifier-like tokens, followed by the default names for funny tokens.

(define (generate-token-kinds aliases g)
  (let* ((terminals
          (apply union
                 (map (lambda (entry)
                        (apply union
                               (map make-set
                                    (map (lambda (production)
                                           (filter string?
                                                   (production.rhs production)))
                                         (entry.productions entry)))))
                      g)))
         (identifier-like (filter identifier-like? terminals))
         (known-funny (filter (lambda (t) (assoc t typical-funny-tokens))
                              terminals))
         (random-funny (difference terminals
                                   (union identifier-like known-funny))))
    (init-token-generator)
    (choose-token-names
     (append aliases
             (map (lambda (id)
                    (list id (string->symbol id)))
                  identifier-like)
             (map (lambda (funny)
                    (list funny
                          (cadr (assoc funny typical-funny-tokens))))
                  known-funny)
             (map (lambda (funny)
                    (list funny (generate-token)))
                  random-funny)
             (list (list '$ 'eof))) ; used internally
     '()
     '())))

; Given a list of tentatively chosen terminal kinds,
; a table of definitively chosen terminal kinds,
; and a list of the terminal kinds that occur in the table,
; returns a table of terminal kinds in which there are
; no conflicts.

(define (choose-token-names tentative table names)
  (cond ((null? tentative) table)
        ((assoc (caar tentative) table)
         (choose-token-names (cdr tentative) table names))
        ((memq (cadr (car tentative)) names)
         (choose-token-names (cons (list (caar tentative)
                                         (generate-token))
                                   (cdr tentative))
                             table
                             names))
        ((identifier-like? (symbol->string (cadr (car tentative))))
         (choose-token-names (cdr tentative)
                             (cons (car tentative) table)
                             (cons (cadr (car tentative)) names)))
        (else
         (choose-token-names
          (cons (list (caar tentative)
                      (let ((probe (assoc (caar tentative)
                                          typical-funny-tokens)))
                        (if probe
                            (cadr probe)
                            (generate-token))))
                (cdr tentative))
          table
          names))))

(define (identifier-like? s)
  (let ((s (string->list s)))
    (and (not (null? s))
         (char-alphabetic? (car s))
         (every? (lambda (c)
                   (or (char-alphabetic? c)
                       (char-numeric? c)
                       (char=? c #\_)))
                 (cdr s)))))

(define typical-funny-tokens
  '(("~" tilde)              ; squiggle
    ("`" backquote)          ; backwhack
    ("!" exclamation)        ; bang
    ("@" atsign)
    ("#" sharpsign)          ; octathorpe
    ("$" dollar)
    ("%" percent)
    ("^" uparrow)
    ("&" ampersand)
    ("*" asterisk)
    ("(" lparen)
    (")" rparen)
    ("_" underscore)
    ("-" minus)              ; hyphen
    ("+" plus)
    ("=" equal)
    ("{" lbrace)
    ("}" rbrace)
    ("[" lbracket)
    ("]" rbracket)
    ("|" vertical)
    ("<" lt)
    (">" gt)
    ("," comma)
    ("." period)
    ("?" questionmark)
    ("/" slash)              ; whack
    (":" colon)
    (";" semicolon)
    ("\\" backslash)         ; backwhack
    ("\"" doublequote)
    ("'" singlequote)
    ("<=" le)
    (">=" ge)
    ("==" eqeq)
    (":=" assign)
    (":-" provided)
    (".." dotdot)
    ("..." dotdotdot)
    ))

; Grammars.

(define (grammar.start g)
  (entry.nonterminal (car g)))
(define entry.nonterminal car)
(define entry.productions cdr)
(define production.rhs car)
(define production.actionproc cadr)

(define noaction '*)

(define (check-grammar g)
  (if (and (list? g)
           (not (null? g)))
      (for-each
       (lambda (entry)
         (if (and (list? entry)
                  (>= (length entry) 2)
                  (symbol? (car entry)))
             (for-each
              (lambda (production)
                (if (and (list? production)
                         (= (length production) 2)
                         (symbol? (production.actionproc production)))
                    (let ((rhs (production.rhs production)))
                      (if (list? rhs)
                          (for-each
                           (lambda (x)
                             (if (or (symbol? x)
                                     (string? x))
                                 #t
                                 (generate-error
                                  "grammar symbols must be symbols or strings")))
                           rhs)
                          (generate-error "malformed right hand side" rhs))
                      (if (eq? noaction (production.actionproc production))
                          (if (and (= (length rhs) 1)
                                   (string? (car rhs))
                                   (= (length entry) 2))
                              #t
                              (generate-error
                               "action procedure is required" entry))))
                    (generate-error
                     "malformed augmented production"
                     production)))
              (entry.productions entry))
             (generate-error "malformed entry for nonterminal" entry)))
       g)
      (generate-error "malformed grammar" g))
  (do ((nonterminals (map entry.nonterminal g) (cdr nonterminals)))
      ((null? nonterminals))
      (if (memq (car nonterminals) (cdr nonterminals))
          (generate-error "duplicated nonterminal" (car nonterminals))))
  (let ((nonterminals (map entry.nonterminal g)))
    (for-each
     (lambda (entry)
       (for-each (lambda (production)
                   (for-each (lambda (s)
                               (if (and (symbol? s)
                                        (not (memq s nonterminals)))
                                   (generate-error
                                    "no production for" s)))
                             (production.rhs production)))
                 (entry.productions entry)))
     g)))

; The parser generator uses follow.sch to test the LL(1) condition and to
; compute director sets, but the code in follow.sch expects grammars to be
; represented differently (without the action procedures).
;
; Given a grammar as expected by the parser generator,
; returns a grammar as expected by follow.sch.

(define (unaugmented-grammar g)
  (map (lambda (entry)
         (cons (entry.nonterminal entry)
               (map production.rhs (entry.productions entry))))
       g))

; Productions with no action procedure serve to specify the symbolic name
; of a terminal token.  Apart from that, the nonterminal on the left hand
; side is just an alias for the right hand side.
;
; This procedure extracts the symbolic names from a grammar, and replaces
; all such aliases by the terminals they name.  It returns a list of two
; things: a token table and a new, equivalent, grammar.

(define (split-grammar g)
  (define (replace-aliases aliases g)
    (map cons
         (map entry.nonterminal g)
         (map (lambda (productions)
                (map list
                     (map (lambda (rhs)
                            (map (lambda (x)
                                   (if (and (symbol? x)
                                            (assq x aliases))
                                       (cadr (assq x aliases))
                                       x))
                                 rhs))
                          (map production.rhs productions))
                     (map production.actionproc productions)))
              (map entry.productions g))))
  (define (alias? entry)
    (eq? noaction (production.actionproc (car (entry.productions entry)))))
  (define (loop g new aliases)
    (cond ((null? g)
           (list (map reverse aliases)
                 (replace-aliases aliases (reverse new))))
          ((alias? (car g))
           (let ((entry (car g)))
             (loop (cdr g)
                   new
                   (cons (list (entry.nonterminal entry)
                               (car (production.rhs
                                     (car (entry.productions entry)))))
                         aliases))))
          (else (loop (cdr g) (cons (car g) new) aliases))))
  (loop g '() '()))

; Given a grammar, generates a list of all actionprocs and the nonterminals
; that correspond to their arguments.

(define (calls-to-actionprocs grammar)
  (let* ((productions (apply append (map entry.productions grammar)))
         (productions
          (sort1 (lambda (x y)
                   (string<? (symbol->string (production.actionproc x))
                             (symbol->string (production.actionproc y))))
                 productions)))
    (map (lambda (production)
           (cons (production.actionproc production)
                 (if (actions-take-arguments)
                     (filter symbol?
                             (production.rhs production))
                     '())))
         productions)))

(define actions-take-arguments
  (let ((flag #t))
    (lambda args
      (cond ((null? args) flag)
            (else (set! flag (car args)) flag)))))

; All output from generate-scheme-parser is written by these procedures,
; but generate-pascal-parser and generate-c-parser write directly to port1.

; Portable error procedure.
; Quit is assigned at the start of parser generation.

(define quit (lambda (v) v))

(define (generate-error msg . values)
  (display "Error during parser generation: ")
  (display msg)
  (newline)
  (for-each (lambda (v)
              (display "    ")
              (write v)
              (newline))
            values)
  (quit (string->symbol "")))

(define (generate-warning msg . values)
  (display msg)
  (newline)
  (for-each (lambda (v)
              (display "    ")
              (write v)
              (newline))
            values))

(define (generate-progress msg . values)
  (if (not (eq? (current-output-port) output-port1))
      (apply generate-warning (string-append msg "...") values)))

(define (write-token-table table port)
  (write-char #\; port)
  (display " Table of tokens and the symbolic names of their kinds." port)
  (newline port)
  (newline port)
  (display "(define token-kinds" port)
  (newline port)
  (display "  '(" port)
  (newline port)
  (for-each (lambda (x)
              (display "    " port)
              (write (list (token.string x) (token.kind x))
                     port)
              (newline port))
            (sort1 (lambda (x y)
                     (cond ((eq? (token.string x) '$) #t)
                           ((eq? (token.string y) '$) #f)
                           (else (string<? (car x) (car y)))))
                   table))
  (display "   ))" port)
  (newline port)
  (newline port)
  (newline port))

(define (write-actionprocs calls port)
  (write-char #\; port)
  (display " Table of calls to action procedures." port)
  (newline port)
  (newline port)
  (display "(define actionprocs" port)
  (newline port)
  (display "  '(" port)
  (newline port)
  (for-each (lambda (call)
              (display "    " port)
              (write call port)
              (newline port))
            calls)
  (display "   ))" port)
  (newline port)
  (newline port)
  (newline port))

; The code generators for Pascal and for C write directly to port1,
; so this procedure is useful only when generating Scheme code.

(define (write-definition def port)
  (if (eq? parser-language 'scheme)
      (begin (case *host-implementation*
               ((MacScheme ChezScheme Gambit)
                (pretty-print def port))
               (else (write def port)))
             (newline port))))

; Miscellaneous.

(define (sorted-union sets)
  (sort1 (lambda (sym1 sym2)
           (string<? (symbol->string sym1)
                     (symbol->string sym2)))
         (apply union sets)))

(define (sort1 predicate ls)
  (case *host-implementation*
    ((ChezScheme) (sort predicate ls))
    ((MacScheme)  (sort ls predicate))
    (else         ls)))

(define (filter p? ls)
  (cond ((null? ls) '())
        ((p? (car ls))
         (cons (car ls) (filter p? (cdr ls))))
        (else (filter p? (cdr ls)))))

(define (every? p? ls)
  (cond ((null? ls) #t)
        ((p? (car ls))
         (every? p? (cdr ls)))
        (else #f))) 

(define (compose f g)
  (lambda args
    (f (apply g args))))

(define (compose1 f g)
  (lambda (x)
    (f (g x))))

; To do:
;
; *  write-token-table and write-action-procs should just extract
;    the relevant information in a form that is convenient for the
;    language-specific generate-parser-declarations procedures.
; *  write-definition should be moved into the code generators.
; *  a finalization procedure should be added in case a code
;    generator needs to see all the productions before generating
;    the declarations.
