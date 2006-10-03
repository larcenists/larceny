; Similar to LOCAL in SML and HIDE in BeCecil.
; Note, MzScheme uses LOCAL for a related but different syntax.
;
; (local ((counter 0))
;   (define (cur) counter)
;   (define (next) (set! counter (+ counter 1)) counter))
; =>
; (begin 
;  (define cur)
;  (define next)
;  (let ((counter 0))
;    (set! cur (lambda () counter))
;    (set! next (lambda () (set! counter (+ counter 1)) counter))
;    (unspecified)))
;
; Both DEFINE forms are allowed.
;
; Restrictions:
; - body expressions must actually be definitions, no expressions
;   that expand to definitions are allowed
; - no variable bound in the local clauses may be defined by the
;   definitions part
;
; This gives an idea for a more sophisticated macro-expander: it is
; necessary to be able to expand a macro _after_ the body has been
; expanded, or to expand it partly before the body and partly after.
; That is, what we have is something like a TRANSFORMER, but where
; the environment can be manipulated and passed on to the expander
; for the body, and then the latter returns and the final transformation
; can be done.
;
; Since RENAME and COMPARE are just functions, perhaps we can just pass
; overrides to the macro expander, like this:
;
;   (transformer (e rename compare)
;     (let ((myname (gensym ...))
;           (yourname (cadr e)))
;       ... 
;       (macro-transform e' 
;                        (lambda (x) 
;                           (if (eq? x yourname) myname (rename x)))
;                        (lambda (a b) (or (eq? a b) (compare a b))))
;       ...))
;
; Slight issue perhaps regarding compiler info?

(define-syntax local
  (syntax-rules ()
    ((local (?clause ...) ?def ...)
     (... (let-syntax ((define-names 
                        (syntax-rules (define)
                          ((define-names () (?def ...))
                           (begin ?def ...))
                          ((define-names ((define (?name ?arg ...) ?body ...)
                                          ?def ...)
                                         (?ndef ...))
                           (define-names (?def ...)
                                         (?ndef ... (define ?name))))
                          ((define-names ((define ?name ?value) ?def ...) 
                                         (?ndef ...))
                           (define-names (?def ...)
                                         (?ndef ... (define ?name))))))
                       (set-values
                        (syntax-rules (define)
                          ((set-values () (?def ...))
                           (begin ?def ...))
                          ((set-values ((define (?name ?arg ...) ?body ...)
                                        ?def ...)
                                       (?ndef ...))
                           (set-values (?def ...)
                                       (?ndef ... 
                                              (set! ?name 
                                                    (lambda (?arg ...)
                                                      ?body ...)))))
                          ((set-values ((define ?name ?value) ?def ...) 
                                       (?ndef ...))
                           (set-values (?def ...)
                                       (?ndef ... (set! ?name ?value)))))))
            (begin (define-names (?def ...) ())
                   (let (?clause ...)
                     (set-values (?def ...) ()))))))))


                        
      
            