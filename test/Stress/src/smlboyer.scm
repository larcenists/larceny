;;; Translated from the SML version of the Boyer benchmark.

;;; From 1terms.sch

; terms.sch
;
; Translated from smlbench/boyer/terms.sml by William D Clinger
; Last modified: 25 October 1996

; The SML types for this, whose spirit I have attempted to preserve,
; are just awful:

; signature TERMS =
;   sig
;     type head;
;     datatype term =
;       Var of int
;     | Prop of head * term list;
;     datatype binding = Bind of int * term;      
;     val get: string -> head
;     and headname: head -> string
;     and add_lemma: term -> unit
;     and apply_subst: binding list -> term -> term
;     and rewrite: term -> term
;   end;

; In the Scheme version, a term has one of two forms:
;  * an integer
;  * a list of the form (<head> <terms>)
;    where <head> is a head and <terms> is a list of terms.

(define (Var i) i)
(define (Prop head terms) (cons head terms))

(define (Var? x) (not (pair? x)))
(define (Prop? x) (pair? x))

(define (Var.i x) x)
(define (Prop.head x) (car x))
(define (Prop.terms x) (cdr x))

(define Bind cons)
(define Bind.i car)
(define Bind.term cdr)

(define get)
(define headname)
(define add_lemma)
(define apply_subst)
(define rewrite)

;(let ()

; datatype term
;   = Var of int
;   | Prop of { name: string, props: (term * term) list ref } * term list
;
; type head = { name: string, props: (term * term) list ref }
;
; A head has the form (<name> . <props>),
;    where <name> is a string and <props> is a list of pairs of terms.
;    The <props> field can be updated destructively.

(define (head name props) (cons name props))

(define (head.name x) (car x))
(define (head.props x) (cdr x))
(define (head.props! x newprops) (set-cdr! x newprops))

(define lemmas '())

(set! headname car)

(set! get
      (lambda (name)
        (define (get_rec ls)
          (cond ((null? ls)
                 (let ((entry (head name '())))
                   (set! lemmas (cons entry lemmas))
                   entry))
                ((string=? name (head.name (car ls)))
                 (car ls))
                (else
                 (get_rec (cdr ls)))))
        (get_rec lemmas)))

(set! add_lemma
      (lambda (lemma)
        (let* ((terms (Prop.terms lemma))
               (left (car terms))
               (right (cadr terms))
               (h (Prop.head left))
               (r (head.props h)))
          (head.props! h (cons (cons left right) r)))))

; Given an int v, returns a procedure that, given a list
; of bindings, returns the value associated with v or #f.
; This won't work if #f is associated with v, but that
; won't ever happen in this benchmark.

(define (get_binding v)
  (define (get_rec bindings)
    (cond ((null? bindings)
           #f)
          ((eqv? (Bind.i (car bindings)) v)
           (Bind.term (car bindings)))
          (else
           (get_rec (cdr bindings)))))
  get_rec)

(set! apply_subst
      (lambda (alist)
        (define (as_rec term)
          (if (Var? term)
              (or ((get_binding (Var.i term)) alist)
                  term)
              (Prop (Prop.head term)
                    (map as_rec (Prop.terms term)))))
        as_rec))

; Given two terms, returns a list of bindings that unify
; them, or returns #f if they are not unifiable.

(define (unify term1 term2)
  (unify1 term1 term2 '()))

(define (unify1 term1 term2 unify_subst)
  (if (Var? term2)
      (let* ((v (Var.i term2))
             (value ((get_binding v) unify_subst)))
        (if value
            (if (equal? value term1)
                unify_subst
                #f)
            (cons (cons v term1) unify_subst)))
      (if (Var? term1)
          #f
          (if (equal? (Prop.head term1)
                      (Prop.head term2))
              (unify1_lst (Prop.terms term1)
                          (Prop.terms term2)
                          unify_subst)
              #f))))

(define (unify1_lst ls1 ls2 unify_subst)
  (cond ((and (null? ls1) (null? ls2))
         unify_subst)
        ((and (pair? ls1) (pair? ls2))
         (let ((unify_subst
                (unify1 (car ls1) (car ls2) unify_subst)))
           (if unify_subst
               (unify1_lst (cdr ls1) (cdr ls2) unify_subst)
               #f)))
        (else
         #f)))

(set! rewrite
      (lambda (term)
        (if (Var? term)
            term
            (let ((head (Prop.head term)))
              (rewrite_with_lemmas
               (Prop head
                     (map rewrite (Prop.terms term)))
               (head.props head))))))

(define (rewrite_with_lemmas term lemmas)
  (if (null? lemmas)
      term
      (let* ((lemma (car lemmas))
             (t1 (car lemma))
             (t2 (cdr lemma))
             (u (unify term t1)))
        (if u
            (rewrite ((apply_subst u) t2))
            (rewrite_with_lemmas term (cdr lemmas))))))

;;; From 1rules.sch

; rules.sch
;
; Translated from smlbench/boyer/rules.sml by William D Clinger
; Last modified: 22 October 1996

; requires terms.sch

; datatype cterm = CVar of int | CProp of string * cterm list;

(let ()

(define (CVar i)        i)
(define (CProp s terms) (list s terms))

(define (CVar? x)  (not (pair? x)))
(define (CProp? x) (pair? x))

(define (CVar.i x)      x)
(define (CProp.name x)  (car x))
(define (CProp.terms x) (cadr x))

(define (cterm_to_term x)
  (if (CVar? x)
      (Var (CVar.i x))
      (Prop (get (CProp.name x))
            (map cterm_to_term (CProp.terms x)))))

(define (add t) (add_lemma (cterm_to_term t)))
  
; The following code was obtained from rules.sml by using a text editor to
;   * delete all occurrences of "CVar"
;   * delete all occurrences of "CProp"
;   * replace all commas by spaces
;   * replace all left and right square brackets by parentheses
;   * replace all occurrences of "add (" by "(add '".


(add '
("equal" 
 ( ("compile" (5))  
  
  ("reverse" 
   ( ("codegen" ( ("optimize" (5))   ("nil" ()))))))));
(add '
("equal" 
 ( ("eqp" (23  24))  
   ("equal" ( ("fix" (23))   ("fix" (24)))))));
(add '
("equal" 
 ( ("gt" (23  24))   ("lt" (24  23)))));
(add '
("equal" 
 ( ("le" (23  24))   ("ge" (24  23)))));
(add '
("equal" 
 ( ("ge" (23  24))   ("le" (24  23)))));
(add '
("equal" 
 ( ("boolean" (23))  
  
  ("or" 
   ( ("equal" (23   ("true" ())))  
     ("equal" (23   ("false" ()))))))));
(add '
("equal" 
 ( ("iff" (23  24))  
  
  ("and" 
   ( ("implies" (23  24))  
     ("implies" (24  23)))))));
(add '
("equal" 
 ( ("even1" (23))  
  
  ("if" 
   ( ("zerop" (23))   ("true" ())  
     ("odd" ( ("sub1" (23)))))))));
(add '
("equal" 
 ( ("countps_" (11  15))  
   ("countps_loop" (11  15   ("zero" ()))))));
(add '
("equal" 
 ( ("fact_" (8))  
   ("fact_loop" (8   ("one" ()))))));
(add '
("equal" 
 ( ("reverse_" (23))  
   ("reverse_loop" (23   ("nil" ()))))));
(add '
("equal" 
 ( ("divides" (23  24))  
   ("zerop" ( ("remainder" (24  23)))))));
(add '
("equal" 
 ( ("assume_true" (21  0))  
   ("cons" ( ("cons" (21   ("true" ())))  0)))));
(add '
("equal" 
 ( ("assume_false" (21  0))  
   ("cons" ( ("cons" (21   ("false" ())))  0)))));
(add '
("equal" 
 ( ("tautology_checker" (23))  
   ("tautologyp" ( ("normalize" (23))   ("nil" ()))))));
(add '
("equal" 
 ( ("falsify" (23))  
   ("falsify1" ( ("normalize" (23))   ("nil" ()))))));
(add '
("equal" 
 ( ("prime" (23))  
  
  ("and" 
   ( ("not" ( ("zerop" (23))))  
    
    ("not" 
     ( ("equal" (23   ("add1" ( ("zero" ())))))))  
     ("prime1" (23   ("sub1" (23)))))))));
(add '
("equal" 
 ( ("and" (15  16))  
  
  ("if" 
   (15  
     ("if" (16   ("true" ())   ("false" ())))  
     ("false" ()))))));
(add '
("equal" 
 ( ("or" (15  16))  
  
  ("if" 
   (15   ("true" ())  
     ("if" (16   ("true" ())   ("false" ())))  
     ("false" ()))))));
(add '
("equal" 
 ( ("not" (15))  
   ("if" (15   ("false" ())   ("true" ()))))));
(add '
("equal" 
 ( ("implies" (15  16))  
  
  ("if" 
   (15  
     ("if" (16   ("true" ())   ("false" ())))  
     ("true" ()))))));
(add '
("equal" 
 ( ("fix" (23))  
   ("if" ( ("numberp" (23))  23   ("zero" ()))))));
(add '
("equal" 
 ( ("if" ( ("if" (0  1  2))  3  4))  
  
  ("if" 
   (0   ("if" (1  3  4))  
     ("if" (2  3  4)))))));
(add '
("equal" 
 ( ("zerop" (23))  
  
  ("or" 
   ( ("equal" (23   ("zero" ())))  
     ("not" ( ("numberp" (23)))))))));
(add '
("equal" 
 ( ("plus" ( ("plus" (23  24))  25))  
   ("plus" (23   ("plus" (24  25)))))));
(add '
("equal" 
 ( ("equal" ( ("plus" (0  1))   ("zero" ())))  
   ("and" ( ("zerop" (0))   ("zerop" (1)))))));
(add '
("equal" ( ("difference" (23  23))   ("zero" ()))));
(add '
("equal" 
 (
  ("equal" 
   ( ("plus" (0  1))   ("plus" (0  2))))  
   ("equal" ( ("fix" (1))   ("fix" (2)))))));
(add '
("equal" 
 (
  ("equal" ( ("zero" ())   ("difference" (23  24))))  
   ("not" ( ("gt" (24  23)))))));
(add '
("equal" 
 ( ("equal" (23   ("difference" (23  24))))  
  
  ("and" 
   ( ("numberp" (23))  
    
    ("or" 
     ( ("equal" (23   ("zero" ())))  
       ("zerop" (24)))))))));
(add '
("equal" 
 (
  ("meaning" 
   ( ("plus_tree" ( ("append" (23  24))))  0))  
  
  ("plus" 
   ( ("meaning" ( ("plus_tree" (23))  0))  
     ("meaning" ( ("plus_tree" (24))  0)))))));
(add '
("equal" 
 (
  ("meaning" 
   ( ("plus_tree" ( ("plus_fringe" (23))))  0))  
   ("fix" ( ("meaning" (23  0)))))));
(add '
("equal" 
 ( ("append" ( ("append" (23  24))  25))  
   ("append" (23   ("append" (24  25)))))));
(add '
("equal" 
 ( ("reverse" ( ("append" (0  1))))  
  
  ("append" ( ("reverse" (1))   ("reverse" (0)))))));
(add '
("equal" 
 ( ("times" (23   ("plus" (24  25))))  
  
  ("plus" 
   ( ("times" (23  24))  
     ("times" (23  25)))))));
(add '
("equal" 
 ( ("times" ( ("times" (23  24))  25))  
   ("times" (23   ("times" (24  25)))))));
(add '
("equal" 
 (
  ("equal" ( ("times" (23  24))   ("zero" ())))  
   ("or" ( ("zerop" (23))   ("zerop" (24)))))));
(add '
("equal" 
 ( ("exec" ( ("append" (23  24))  15  4))  
  
  ("exec" (24   ("exec" (23  15  4))  4)))));
(add '
("equal" 
 ( ("mc_flatten" (23  24))  
   ("append" ( ("flatten" (23))  24)))));
(add '
("equal" 
 ( ("member" (23   ("append" (0  1))))  
  
  ("or" 
   ( ("member" (23  0))  
     ("member" (23  1)))))));
(add '
("equal" 
 ( ("member" (23   ("reverse" (24))))  
   ("member" (23  24)))));
(add '
("equal" 
 ( ("length" ( ("reverse" (23))))  
   ("length" (23)))));
(add '
("equal" 
 ( ("member" (0   ("intersect" (1  2))))  
  
  ("and" 
   ( ("member" (0  1))   ("member" (0  2)))))));
(add '
("equal" ( ("nth" ( ("zero" ())  8))   ("zero" ()))));
(add '
("equal" 
 ( ("exp" (8   ("plus" (9  10))))  
  
  ("times" 
   ( ("exp" (8  9))   ("exp" (8  10)))))));
(add '
("equal" 
 ( ("exp" (8   ("times" (9  10))))  
   ("exp" ( ("exp" (8  9))  10)))));
(add '
("equal" 
 ( ("reverse_loop" (23  24))  
   ("append" ( ("reverse" (23))  24)))));
(add '
("equal" 
 ( ("reverse_loop" (23   ("nil" ())))  
   ("reverse" (23)))));
(add '
("equal" 
 ( ("count_list" (25   ("sort_lp" (23  24))))  
  
  ("plus" 
   ( ("count_list" (25  23))  
     ("count_list" (25  24)))))));
(add '
("equal" 
 (
  ("equal" 
   ( ("append" (0  1))   ("append" (0  2))))  
   ("equal" (1  2)))));
(add '
("equal" 
 (
  ("plus" 
   ( ("remainder" (23  24))  
     ("times" (24   ("quotient" (23  24))))))  
   ("fix" (23)))));
(add '
("equal" 
 (
  ("power_eval" ( ("big_plus" (11  8  1))  1))  
   ("plus" ( ("power_eval" (11  1))  8)))));
(add '
("equal" 
 (
  ("power_eval" 
   ( ("big_plus" (23  24  8  1))  1))  
  
  ("plus" 
   (8  
    
    ("plus" 
     ( ("power_eval" (23  1))  
       ("power_eval" (24  1)))))))));
(add '
("equal" 
 ( ("remainder" (24   ("one" ())))   ("zero" ()))));
(add '
("equal" 
 ( ("lt" ( ("remainder" (23  24))  24))  
   ("not" ( ("zerop" (24)))))));
(add '
("equal" ( ("remainder" (23  23))   ("zero" ()))));
(add '
("equal" 
 ( ("lt" ( ("quotient" (8  9))  8))  
  
  ("and" 
   ( ("not" ( ("zerop" (8))))  
    
    ("or" 
     ( ("zerop" (9))  
       ("not" ( ("equal" (9   ("one" ()))))))))))));
(add '
("equal" 
 ( ("lt" ( ("remainder" (23  24))  23))  
  
  ("and" 
   ( ("not" ( ("zerop" (24))))  
     ("not" ( ("zerop" (23))))  
     ("not" ( ("lt" (23  24)))))))));
(add '
("equal" 
 ( ("power_eval" ( ("power_rep" (8  1))  1))  
   ("fix" (8)))));
(add '
("equal" 
 (
  ("power_eval" 
   (
    ("big_plus" 
     ( ("power_rep" (8  1))  
       ("power_rep" (9  1))   ("zero" ())  
      1))  
    1))  
   ("plus" (8  9)))));
(add '
("equal" 
 ( ("gcd" (23  24))   ("gcd" (24  23)))));
(add '
("equal" 
 ( ("nth" ( ("append" (0  1))  8))  
  
  ("append" 
   ( ("nth" (0  8))  
    
    ("nth" 
     (1   ("difference" (8   ("length" (0)))))))))));
(add '
("equal" 
 ( ("difference" ( ("plus" (23  24))  23))  
   ("fix" (24)))));
(add '
("equal" 
 ( ("difference" ( ("plus" (24  23))  23))  
   ("fix" (24)))));
(add '
("equal" 
 (
  ("difference" 
   ( ("plus" (23  24))   ("plus" (23  25))))  
   ("difference" (24  25)))));
(add '
("equal" 
 ( ("times" (23   ("difference" (2  22))))  
  
  ("difference" 
   ( ("times" (2  23))  
     ("times" (22  23)))))));
(add '
("equal" 
 ( ("remainder" ( ("times" (23  25))  25))  
   ("zero" ()))));
(add '
("equal" 
 (
  ("difference" 
   ( ("plus" (1   ("plus" (0  2))))  0))  
   ("plus" (1  2)))));
(add '
("equal" 
 (
  ("difference" 
   ( ("add1" ( ("plus" (24  25))))  25))  
   ("add1" (24)))));
(add '
("equal" 
 (
  ("lt" 
   ( ("plus" (23  24))   ("plus" (23  25))))  
   ("lt" (24  25)))));
(add '
("equal" 
 (
  ("lt" 
   ( ("times" (23  25))  
     ("times" (24  25))))  
  
  ("and" 
   ( ("not" ( ("zerop" (25))))  
     ("lt" (23  24)))))));
(add '
("equal" 
 ( ("lt" (24   ("plus" (23  24))))  
   ("not" ( ("zerop" (23)))))));
(add '
("equal" 
 (
  ("gcd" 
   ( ("times" (23  25))  
     ("times" (24  25))))  
   ("times" (25   ("gcd" (23  24)))))));
(add '
("equal" 
 ( ("value" ( ("normalize" (23))  0))  
   ("value" (23  0)))));
(add '
("equal" 
 (
  ("equal" 
   ( ("flatten" (23))  
     ("cons" (24   ("nil" ())))))  
  
  ("and" 
   ( ("nlistp" (23))   ("equal" (23  24)))))));
(add '
("equal" 
 ( ("listp" ( ("gother" (23))))  
   ("listp" (23)))));
(add '
("equal" 
 ( ("samefringe" (23  24))  
  
  ("equal" ( ("flatten" (23))   ("flatten" (24)))))));
(add '
("equal" 
 (
  ("equal" 
   ( ("greatest_factor" (23  24))   ("zero" ())))  
  
  ("and" 
   (
    ("or" 
     ( ("zerop" (24))  
       ("equal" (24   ("one" ())))))  
     ("equal" (23   ("zero" ()))))))));
(add '
("equal" 
 (
  ("equal" 
   ( ("greatest_factor" (23  24))   ("one" ())))  
   ("equal" (23   ("one" ()))))));
(add '
("equal" 
 ( ("numberp" ( ("greatest_factor" (23  24))))  
  
  ("not" 
   (
    ("and" 
     (
      ("or" 
       ( ("zerop" (24))  
         ("equal" (24   ("one" ())))))  
       ("not" ( ("numberp" (23)))))))))));
(add '
("equal" 
 ( ("times_list" ( ("append" (23  24))))  
  
  ("times" 
   ( ("times_list" (23))   ("times_list" (24)))))));
(add '
("equal" 
 ( ("prime_list" ( ("append" (23  24))))  
  
  ("and" 
   ( ("prime_list" (23))   ("prime_list" (24)))))));
(add '
("equal" 
 ( ("equal" (25   ("times" (22  25))))  
  
  ("and" 
   ( ("numberp" (25))  
    
    ("or" 
     ( ("equal" (25   ("zero" ())))  
       ("equal" (22   ("one" ()))))))))));
(add '
("equal" 
 ( ("ge" (23  24))  
   ("not" ( ("lt" (23  24)))))));
(add '
("equal" 
 ( ("equal" (23   ("times" (23  24))))  
  
  ("or" 
   ( ("equal" (23   ("zero" ())))  
    
    ("and" 
     ( ("numberp" (23))  
       ("equal" (24   ("one" ()))))))))));
(add '
("equal" 
 ( ("remainder" ( ("times" (24  23))  24))  
   ("zero" ()))));
(add '
("equal" 
 ( ("equal" ( ("times" (0  1))   ("one" ())))  
  
  ("and" 
   ( ("not" ( ("equal" (0   ("zero" ())))))  
     ("not" ( ("equal" (1   ("zero" ())))))  
     ("numberp" (0))   ("numberp" (1))  
     ("equal" ( ("sub1" (0))   ("zero" ())))  
     ("equal" ( ("sub1" (1))   ("zero" ()))))))));
(add '
("equal" 
 (
  ("lt" 
   ( ("length" ( ("delete" (23  11))))  
     ("length" (11))))  
   ("member" (23  11)))));
(add '
("equal" 
 ( ("sort2" ( ("delete" (23  11))))  
   ("delete" (23   ("sort2" (11)))))));
(add ' ("equal" ( ("dsort" (23))   ("sort2" (23)))));
(add '
("equal" 
 (
  ("length" 
   (
    ("cons" 
     (0  
      
      ("cons" 
       (1  
        
        ("cons" 
         (2  
          
          ("cons" 
           (3  
             ("cons" (4   ("cons" (5  6))))))))))))))
     ("plus" ( ("six" ())   ("length" (6)))))));
(add '
("equal" 
 (
  ("difference" 
   ( ("add1" ( ("add1" (23))))   ("two" ())))  
   ("fix" (23)))));
(add '
("equal" 
 (
  ("quotient" 
   ( ("plus" (23   ("plus" (23  24))))  
     ("two" ())))  
  
  ("plus" (23   ("quotient" (24   ("two" ()))))))));
(add '
("equal" 
 ( ("sigma" ( ("zero" ())  8))  
  
  ("quotient" 
   ( ("times" (8   ("add1" (8))))   ("two" ()))))));
(add '
("equal" 
 ( ("plus" (23   ("add1" (24))))  
  
  ("if" 
   ( ("numberp" (24))  
     ("add1" ( ("plus" (23  24))))  
     ("add1" (23)))))));
(add '
("equal" 
 (
  ("equal" 
   ( ("difference" (23  24))  
     ("difference" (25  24))))  
  
  ("if" 
   ( ("lt" (23  24))  
     ("not" ( ("lt" (24  25))))  
    
    ("if" 
     ( ("lt" (25  24))  
       ("not" ( ("lt" (24  23))))  
       ("equal" ( ("fix" (23))   ("fix" (25))))))))))
);
(add '
("equal" 
 (
  ("meaning" 
   ( ("plus_tree" ( ("delete" (23  24))))  0))  
  
  ("if" 
   ( ("member" (23  24))  
    
    ("difference" 
     ( ("meaning" ( ("plus_tree" (24))  0))  
       ("meaning" (23  0))))  
     ("meaning" ( ("plus_tree" (24))  0)))))));
(add '
("equal" 
 ( ("times" (23   ("add1" (24))))  
  
  ("if" 
   ( ("numberp" (24))  
    
    ("plus" 
     (23   ("times" (23  24))  
       ("fix" (23)))))))));
(add '
("equal" 
 ( ("nth" ( ("nil" ())  8))  
  
  ("if" ( ("zerop" (8))   ("nil" ())   ("zero" ()))))));
(add '
("equal" 
 ( ("last" ( ("append" (0  1))))  
  
  ("if" 
   ( ("listp" (1))   ("last" (1))  
    
    ("if" 
     ( ("listp" (0))  
       ("cons" ( ("car" ( ("last" (0))))  1))  
      1)))))));
(add '
("equal" 
 ( ("equal" ( ("lt" (23  24))  25))  
  
  ("if" 
   ( ("lt" (23  24))  
     ("equal" ( ("true" ())  25))  
     ("equal" ( ("false" ())  25)))))));
(add '
("equal" 
 ( ("assignment" (23   ("append" (0  1))))  
  
  ("if" 
   ( ("assignedp" (23  0))  
     ("assignment" (23  0))  
     ("assignment" (23  1)))))));
(add '
("equal" 
 ( ("car" ( ("gother" (23))))  
  
  ("if" 
   ( ("listp" (23))  
     ("car" ( ("flatten" (23))))   ("zero" ()))))));
(add '
("equal" 
 ( ("flatten" ( ("cdr" ( ("gother" (23))))))  
  
  ("if" 
   ( ("listp" (23))  
     ("cdr" ( ("flatten" (23))))  
     ("cons" ( ("zero" ())   ("nil" ()))))))));
(add '
("equal" 
 ( ("quotient" ( ("times" (24  23))  24))  
  
  ("if" 
   ( ("zerop" (24))   ("zero" ())  
     ("fix" (23)))))));
(add '
("equal" 
 ( ("get" (9   ("set" (8  21  12))))  
  
  ("if" 
   ( ("eqp" (9  8))  21  
     ("get" (9  12))))))))

;;; From 1boyer.sch

; mlboyer.sch
;
; Translated from smlbench/boyer/boyer.sml by William D Clinger
; Last modified: 25 October 1996

; requires mlterms.sch

; structure Boyer: BOYER

(define tautp)

; structure Main: BMARK

(define doit)
(define testit)

;(let ()

(define (mem x z)
  (if (null? z)
      #f
      (or (equal? x (car z))
          (mem x (cdr z)))))

(define (truep x lst)
  (if (Prop? x)
      (or (string=? (headname (Prop.head x)) "true")
          (mem x lst))
      (mem x lst)))

(define (falsep x lst)
  (if (Prop? x)
      (or (string=? (headname (Prop.head x)) "false")
          (mem x lst))
      (mem x lst)))

(define (tautologyp x true_lst false_lst)
  (cond ((truep x true_lst)
         #t)
        ((falsep x false_lst)
         #f)
        ((Var? x)
         #f)
        ((string=? (headname (Prop.head x)) "if")
         (let* ((terms (Prop.terms x))
                (test (car terms))
                (yes (cadr terms))
                (no (caddr terms)))
           (cond ((truep test true_lst)
                  (tautologyp yes true_lst false_lst))
                 ((falsep test false_lst)
                  (tautologyp no true_lst false_lst))
                 (else (and (tautologyp yes
                                        (cons test true_lst)
                                        false_lst)
                            (tautologyp no
                                        true_lst
                                        (cons test false_lst)))))))
        (else #f)))

(set! tautp
      (lambda (x)
        (tautologyp (rewrite x) '() '())))

;)

(let ((subst (list

 (Bind 23
       (Prop
        (get "f")
        (list (Prop
               (get "plus")
               (list (Prop (get "plus") (list (Var 0) (Var 1)))
                     (Prop (get "plus") (list (Var 2) (Prop (get "zero") '()))))))))
 (Bind 24
       (Prop
        (get "f")
        (list (Prop
               (get "times")
               (list (Prop (get "times") (list (Var 0) (Var 1)))
                     (Prop (get "plus") (list (Var 2) (Var 3))))))))
 (Bind 25
       (Prop
        (get "f")
        (list (Prop
               (get "reverse")
               (list (Prop
                      (get "append")
                      (list (Prop (get "append") (list (Var 0) (Var 1)))
                            (Prop (get "nil") '()))))))))
 (Bind 20
       (Prop
        (get "equal")
        (list (Prop (get "plus") (list (Var 0) (Var 1)))
              (Prop (get "difference") (list (Var 23) (Var 24))))))
 (Bind 22
       (Prop
        (get "lt")
        (list (Prop (get "remainder") (list (Var 0) (Var 1)))
              (Prop (get "member")
                    (list (Var 0)
                          (Prop (get "length") (list (Var 1))))))))))
      
      (term
       (Prop
        (get "implies")
        (list (Prop
               (get "and")
               (list (Prop (get "implies") (list (Var 23) (Var 24)))
                     (Prop
                      (get "and")
                      (list (Prop (get "implies") (list (Var 24) (Var 25)))
                            (Prop
                             (get "and")
                             (list (Prop (get "implies") (list (Var 25) (Var 20)))
                                   (Prop (get "implies") (list (Var 20) (Var 22)))))))))
              (Prop (get "implies") (list (Var 23) (Var 22)))))))
  
  (set! testit
        (lambda (outstrm)
          (if (tautp ((apply_subst subst) term))
              (display "Proved!" outstrm)
              (display "Cannot prove!" outstrm))
          (newline outstrm)))
  
  (set! doit
        (lambda ()
          (tautp ((apply_subst subst) term))))
  )

(define (main . args)
  (run-benchmark
    "smlboyer"
    smlboyer-iters
    doit
    (lambda (result) result)))
