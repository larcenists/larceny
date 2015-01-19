; Computation of the LL(1) condition, LL(1) director sets,
; and FIRST and FOLLOW sets.
;
; Grammars are represented as a list of entries, where each
; entry is a list giving the productions for a nonterminal.
; The first entry in the grammar must be for the start symbol.
; The car of an entry is the nonterminal; the cdr is a list
; of productions.  Each production is a list of grammar symbols
; giving the right hand side for the production; the empty string
; is represented by the empty list.
; A nonterminal is represented as a Scheme symbol.
; A terminal is represented as a Scheme string.
;
; Example:
;
;  (define g
;    '((S ("id" ":=" E "\;")
;         ("while" E S)
;         ("do" S A "od"))
;      (A ()
;         (S A))
;      (E (T E'))
;      (E' () ("+" T E') ("-" T E'))
;      (T (F T'))
;      (T' () ("*" F T') ("/" F T'))
;      (F ("id") ("(" E ")"))))

; Given a grammar, returns #t if it is LL(1), else returns #f.

(define (LL1? g)
  (LL1-director-sets? (director-sets g)))

; Director sets are represented as lists without duplicates.

(define (LL1-director-sets? dsets)
  (define (loop dsets ok?)
    (cond ((null? dsets) ok?)
          ((disjoint? (cdr (car dsets))) (loop (cdr dsets) ok?))
          (else (display "Failure of LL(1) condition for ")
                (write (caar dsets))
                (newline)
                (display "The director sets for ")
                (write (caar dsets))
                (display " are:")
                (newline)
                (display "    ")
                (write (reverse (cdar dsets)))
                (newline)
                (loop (cdr dsets) #f))))
  (define (disjoint? sets)
    (cond ((null? sets) #t)
          ((null? (car sets)) (disjoint? (cdr sets)))
          ((member-remaining-sets? (caar sets) (cdr sets))
           #f)
          (else (disjoint? (cons (cdr (car sets)) (cdr sets))))))
  (define (member-remaining-sets? x sets)
    (cond ((null? sets) #f)
          ((member x (car sets)) #t)
          (else (member-remaining-sets? x (cdr sets)))))
  (loop dsets #t))

; Given a grammar, returns the director sets for each production.
; In a director set, the end of file token is represented as the
; Scheme symbol $.

(define (director-sets g)
  (let* ((cache (create-cache g))
         (follows (follow-sets g cache)))
    (map (lambda (p)
           (let ((lhs (car p))
                 (alternatives (cdr p)))
             (cons lhs
                   (map (lambda (rhs)
                          (let ((f (first rhs g cache '())))
                            (if (car f)
                                (union2 (lookup lhs follows)
                                        (cdr f))
                                (cdr f))))
                        alternatives))))
         g)))

; A FIRST set is represented as a list whose first element is a boolean
; and whose remaining elements are a list of terminals.  The boolean is
; #t if the empty string belongs to the first set, and #f if the empty
; string does not belong to the FIRST set.

; Given two FIRST sets, returns their union.

(define (first-union f1 f2)
  (cons (or (car f1) (car f2))
        (union2 (cdr f1) (cdr f2))))

; A cache is represented as an association list of entries of the form
;
;    (s . FIRST(s))
;
; where s is either a nonterminal symbol or a list representing a sequence
; of grammar symbols, and FIRST(s) is the FIRST set for s.

(define (lookup-in-cache s cache)
  (let ((entry (assq s cache)))
    (cond (entry
           (cdr entry))
          ((null? s) #f)
          ((and (symbol? (car s))
                (null? (cdr s)))
           (let ((entry (assq (car s) cache)))
             (if entry
                 (cdr entry)
                 #f)))
          (else #f))))

; Given a string of grammar symbols, a grammar, a cache, and a list of
; nonterminals that have appeared in the leftmost position during the
; recursive computation of FIRST(s), returns FIRST(s).
; Prints a warning message if left recursion is detected.

(define (first s g cache recursion)
  (lookup-in-cache s (first-cache s g cache recursion)))

; Given a string of grammar symbols, a grammar, a cache, and a list of
; nonterminals that have appeared in the leftmost position during the
; recursive computation of FIRST(s), returns a cache that contains FIRST(s).
; Prints a warning message if left recursion is detected.

(define (first-cache s g cache recursion)
  (cond ((lookup-in-cache s cache) cache)
        ((null? s)
         (cons '(() #t) cache))
        ((string? (car s))
         (if (null? (cdr s))
             (cons (cons s (cons #f s)) cache)
             (cons (cons s (list #f (car s))) cache)))
        ((memq (car s) recursion)
         (display "Left recursion for ")
         (write (car s))
         (newline)
         (cons (cons (car s) '(#f)) cache))
        ((and (null? (cdr s)) (symbol? (car s)))
         (let ((p (assq (car s) g))
               (newrecursion (cons (car s) recursion)))
           (cond ((not p)
                  (grammar-error "No production for " (car s)))
                 (else
                  (first-of-nonterminal p g cache newrecursion)))))
        (else (let* ((cache (first-cache (list (car s)) g cache recursion))
                     (firstset1 (lookup-in-cache (car s) cache)))
                (if (car firstset1)
                    (let* ((cache (first-cache (cdr s) g cache recursion))
                           (firstset2 (lookup-in-cache (cdr s) cache)))
                      (cons (cons s (append firstset2 (cdr firstset1)))
                            cache))
                    (cons (cons s firstset1) cache))))))

(define (first-of-nonterminal p g cache recursion)
  (let ((nonterminal (car p))
        (productions (cdr p)))
    (define (loop ss cache result)
      (if (null? ss)
          (cons (cons nonterminal result) cache)
          (let* ((cache (first-cache (car ss) g cache recursion))
                 (firstset (lookup-in-cache (car ss) cache)))
            (loop (cdr ss)
                  cache
                  (first-union firstset result)))))
    (loop productions cache '(#f))))

; Given a grammar, creates a cache containing an entry for every
; nonterminal of the grammar.

(define (create-cache g)
  (do ((nonterminals (map car g) (cdr nonterminals))
       (cache '() (first-cache (list (car nonterminals)) g cache '())))
      ((null? nonterminals) cache)))

; Given a grammar g and a cache for FIRST sets, returns FOLLOW(g)
; as a list.
; In the output, the end of file token is represented as the Scheme
; symbol $.
; Warning messages will be printed if left recursion is detected.

(define (follow-sets g cache)
  
  ; Given a nonterminal x, returns the right hand side of the
  ; FOLLOW equation for x.  A FOLLOW equation has the form
  ;
  ;     FOLLOW(x) = FIRST(s1) - {""}
  ;          \union FOLLOW(y1) if "" is in FIRST(s1)
  ;          \union ...
  ;          \union FIRST(sn) - {""}
  ;          \union FOLLOW(yn) if "" is in FIRST(sn).
  ;
  ; The right hand side of this equation is represented as
  ;    ((s1 y1) ... (sn yn)).
  
  (define (follow-equation-for x)
    (define (fef-grammar g eqn)
      (if (null? g)
          eqn
          (fef-grammar (cdr g)
                       (fef-productions (cdar g)
                                        (caar g)
                                        eqn))))
    (define (fef-productions productions lhs eqn)
      (if (null? productions)
          eqn
          (fef-productions (cdr productions)
                           lhs
                           (fef-production (car productions) lhs eqn))))
    (define (fef-production rhs lhs eqn)
      (cond ((null? rhs) eqn)
            ((eq? x (car rhs))
             (fef-production (cdr rhs)
                             lhs
                             (cons (list (cdr rhs) lhs) eqn)))
            (else
             (fef-production (cdr rhs) lhs eqn))))
    (fef-grammar g '()))
  
  (define (evaluate rhs approx1 approx2)
    (if (null? rhs)
        '()
        (let* ((s (car (car rhs)))
               (x (cadr (car rhs)))
               (firstset (first s g cache '())))
          (union2 (cdr firstset)
                  (if (car firstset)
                      (union2 (or (lookup x approx1)
                                  (lookup x approx2))
                              (evaluate (cdr rhs)
                                        approx1
                                        approx2))
                      (evaluate (cdr rhs)
                                approx1
                                approx2))))))
  
  (let ((equations (map (lambda (productions)
                          (let ((x (car productions)))
                            (cons x (follow-equation-for x))))
                        g)))
    
    (define (solve approximations)
      (define (loop equations newapprox changed?)
        (if (null? equations)
            (let ((newapprox (reverse newapprox)))
              (if changed?
                  (solve newapprox)
                  newapprox))
            (let ((f (evaluate (cdr (car equations))
                               newapprox
                               approximations)))
              (loop (cdr equations)
                    (cons (cons (car (car equations)) f)
                          newapprox)
                    (or changed?
                        (not (set-equal?
                              f
                              (lookup (car (car equations))
                                      approximations))))))))
      (loop equations '() #f))
    
    (solve (map (lambda (productions)
                  (cons (car productions) '()))
                g))))

; Tables represented as association lists using eq? for equality.

(define (lookup x t)
  (let ((entry (assq x t)))
    (if entry
        (cdr entry)
        #f)))

; Useful procedures.

; Error messages.

(define (grammar-error . args)
  (for-each (lambda (arg)
              (display arg)
              (newline))
            args)
  (reset))
