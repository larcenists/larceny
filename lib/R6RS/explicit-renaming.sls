;; Nonstandard explicit renaming library: 
;; See also examples and discussion in file examples.scm.
;;
;; Exports:
;;
;;    er-transformer     (syntax)
;;    bound-identifier=? (procedure)
;;    datum->syntax      (procedure)
;;
;; Differences with traditional explicit renaming:
;;
;; - The renaming procedure has signature <symbol> -> <identifier>,
;;   where the <identifier> type is disjoint from the <symbol> type.
;;
;; - The renaming procedure acts as a mathematical function in the sense that
;;   the identifiers obtained from any two calls with the same argument will
;;   be the same in the sense of bound-identifier=?, not eqv?
;;
;; - The output may not contain raw symbols, so implicit identifiers must
;;   be introduced using datum->syntax.
;;
;; - Breaking hygiene with datum->syntax allows more modular macro
;;   programming than traditional explicit renaming.
;;   See in particular the example of while in terms of loop below.
;;
;; - The renaming procedure is aware of the transformer environment,
;;   so that identifiers not bound at the usage site will resolve to
;;   the r6rs library-local bindings at the transformer site.
;;   More precisely, they will be resolved in the lexical environment
;;   of the er-transformer keyword.
;;
;; - Fully compatible with my r6rs syntax-case macro system.
;;
;; Portability and complexity note:
;;
;;   This library is not r6rs-portable, since it assumes that the input
;;   to a transformer is always an unwrapped syntax object, which is
;;   allowed but not required by r6rs, and is currently only true for my
;;   implementation.  The library could be ported to other implementations
;;   by inserting a step that unwrapped the input to the transformer.
;;   However, that would adversely modify the complexity class of
;;   er-transformer macros in those implementations.

(library (explicit-renaming helper)
  (export er-transformer)
  (import (only (rnrs base)
           define-syntax lambda)
          (only (rnrs syntax-case)
           syntax-case
           syntax datum->syntax free-identifier=?))
  
  (define-syntax er-transformer
    (lambda (exp)
      (syntax-case exp ()
        ((k proc)
         (syntax
          (lambda (form)
            (proc form
                  (lambda (symbol) (datum->syntax (syntax k) symbol))
                  free-identifier=?))))))))

(library (explicit-renaming)
  (export er-transformer identifier? bound-identifier=? datum->syntax)
  (import (explicit-renaming helper)
          (rnrs syntax-case)))

