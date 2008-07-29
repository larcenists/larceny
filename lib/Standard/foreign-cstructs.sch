(require 'std-ffi)
(require 'foreign-ctools)

;; Define getters and setters for a C struct.  Examples:
;
; Assume pair.h contains "struct pair" definition with (at least)
; the fields fst and snd.  (At the end of the examples, we provide
; an example such header file and the results it entails.)
;
;   (define-c-struct ("struct pair" #f (include "pair.h"))
;     ("fst"  (pair-fst)) ("snd"  (pair-snd)))
; provides accessors for pair struct but no constructor.
; All fields are treated as integer-valued.
; 
;   (define-c-struct ("struct pair" make-pair (include "pair.h"))
;     ("fst"  (pair-fst) (pair-fst-set!))
;     ("snd"  (pair-snd) (pair-snd-set!)))
; adds a constructor, as well as mutators for both fields.
;
;   (define-c-struct ("struct pair" make-pair (include "pair.h"))
;     ("fst"  (pair-fst 'char) (pair-fst-set! 'char))
;     ("snd"  (pair-snd) (pair-snd-set!)))
; uses the FFI attribute char when marshaling to and from the fst field.
;
;   (define-c-struct ("struct pair" make-pair (include "pair.h"))
;     ("fst"  (pair-fst 'char) (pair-fst-set! 'char))
;     ("snd"  (pair-snd (lambda (x debug-name) (number->string x))) 
;             (pair-snd-set!)))
; uses a specialized unmarshal procedure for snd that returns a string;
; the snd mutator continues to only accept integer values.
; 
; So, if the header file "pair.h" contains only the following declaration
;    struct pair { int fst; char doh; int snd; };
; (note the extra field) then one might observe the following 
; evaluation:
; > (let ()
;     (define-c-struct ("struct pair" make-pair (include "pair.h"))
;       ("fst"  (pair-fst 'char) (pair-fst-set! 'char))
;       ("snd"  (pair-snd (lambda (x n) (number->string x))) 
;               (pair-snd-set!)))
;     (let ((p (make-pair))) 
;       (pair-fst-set! p #\a) 
;       (pair-snd-set! p 12) 
;       `(fst: ,(pair-fst p) 
;         snd: ,(pair-snd p) 
;           p: ,p)))
; (fst: #\a snd: "12"
;       p: #vu8(97 0 0 0 0 0 0 0 12 0 0 0))
;
;; Here is the grammar for the define-c-struct special form.
;
; (define-c-struct (<struct-name> <ctor-name> <ctools-decl> ...) 
;   <field-clause> ...)
;
;
; <struct-name>   ::= <string-literal>
; 
; <ctor-name>     ::= <id>
;                   | #f
; 
; <ctools-decl>   ::= <decl> from foreign-ctools
;
; <field-clause>  ::= (<field-name> <getter-clause>)
;                   | (<field-name> <getter-clause> <setter-clause>
;
; <getter-clause> ::= (<id>) 
;                   | (<id> <unmarshal>)
; 
; <setter-clause  ::= (<id>)
;                   | (<id> <marshal>)
;
; <marshal>       ::= <ffi-attribute-symbol>
;                   | <marshaling procedure expression>
;                   | #f
;
; <unmarshal>     ::= <ffi-attribute-symbol>
;                   | <unmarshaling procedure expression>
;                   | #f
;
(define-syntax define-c-struct
 (syntax-rules ()
  ((define-c-struct (?name           #f ?include ...) . ?rest)
   (define-c-struct (?name ignorin-ctor ?include ...) . ?rest))
  ((define-c-struct (?name ?constructor ?include ...) ?field-clauses ...)
   (define-c-struct "create-names" ()
                  (?name ?constructor ?include ...)
                  ?field-clauses ...))

  ;; These rewrite clauses normalize the syntax into
  ;; the form:
  ;;     (define-c-struct "create-names" 
  ;;                      ((offset size (field (getter conv-get) 
  ;;                                           (setter conv-set))) ...)
  ;;                      (name ctor include ...))
  ;; where {offset, size} are freshly generated,
  ;;       {ctor, getter, setter} are freshly ignored if neccesary,
  ;;       {conv-get, conv-set} are #f if not present in original form
  ((define-c-struct "create-names" 
     ?acc 
     ?naming (?field (?getter)) . ?rest)
   (define-c-struct "create-names" 
     ((offset size (?field (?getter #f) 
                           (ignorin-setter #f))) . ?acc)
     ?naming . ?rest))
  ((define-c-struct "create-names" 
     ?acc 
     ?naming (?field (?getter ?conv-get)) . ?rest)
   (define-c-struct "create-names" 
     ((offset size (?field (?getter ?conv-get) 
                           (ignorin-setter #f))) . ?acc)
     ?naming . ?rest))
  ((define-c-struct "create-names" 
     ?acc 
     ?naming (?field (?getter) (?setter)) . ?rest)
   (define-c-struct "create-names" 
     ((offset size (?field (?getter #f) 
                           (?setter #f))) . ?acc)
     ?naming . ?rest))
  ((define-c-struct "create-names" 
     ?acc 
     ?naming (?field (?getter ?conv-get) (?setter)) . ?rest)
   (define-c-struct "create-names" 
     ((offset size (?field (?getter ?conv-get) 
                           (?setter #f))) . ?acc)
     ?naming . ?rest))
  ((define-c-struct "create-names" 
     ?acc 
     ?naming (?field (?getter ?conv-get) (?setter ?conv-set)) . ?rest)
   (define-c-struct "create-names" 
     ((offset size (?field (?getter ?conv-get) 
                           (?setter ?conv-set))) . ?acc)
     ?naming . ?rest))

  ((define-c-struct "create-names" ?acc ?naming)
   (define-c-struct "finish" ?naming . ?acc))

  ((define-c-struct "finish" (?name ?constructor ?include ...)
     (?offset ?size (?field 
                     (?getter ?conv-get)
                     (?setter ?conv-set))) ...)
   (begin
     (define ?constructor #f)
     (define ?getter #f) ...
     (define ?setter #f) ...
     (define 
      ignore-set!-results
     (let ((check-twoary
            (lambda (f)
              (let ((arity (procedure-arity f))
                    (msg "conversion routine must accept two args"))
                ;; catch what is known to be wrong...
                (cond ((and arity (number? arity) (exact? arity)
                            (not (= 2 arity)))
                       (error msg f)))))))
       (define-c-info
         ?include ...
         (sizeof whole-size ?name)
         (fields ?name (?offset ?field ?size) ...))

       (set! ?constructor
             (lambda () (make-bytevector whole-size 0)))

       (set! ?getter
             (let ((low-getter (size->%getter ?size))
                   (conv (cond 
                          ((procedure? ?conv-get)
                           ?conv-get)
                          ((symbol? ?conv-get)
                           (ffi/ret-converter ?conv-get))
                          ((not ?conv-get) 
                           (lambda (x name) x)))))
               (check-twoary conv)
               (lambda (x) (conv (low-getter x ?offset) ?field)))) 
       ...

       (set! ?setter
             (let ((low-setter (size->%setter ?size))
                   (conv (cond
                          ((procedure? ?conv-set)
                           ?conv-set)
                          ((symbol? ?conv-set)
                           (ffi/arg-converter ?conv-set))
                          ((not ?conv-set)
                           (lambda (x name) x)))))
               (check-twoary conv)
               (lambda (x n) (low-setter x ?offset (conv n ?field))))) 
       ...
       ))))))

;; Old interface to define-c-struct, provided for legacy code
;; like lib/Experimental/socket.sch
;; (the new interface automatically marshals values held in 
;;  structure fields, which does not work for strings embedded
;;  in structures.  The old interface provided the addresses 
;;  *of* each field, which allowed one to use %peek-string to 
;;  extract strings embedded in structures)
;; 
;; Example:
;
;   (define-c-struct ("struct pair" make-pair (include"pair.h"))
;     ("fst"  (pair-fst      %get-uint)
;             (pair-fst-set! %set-uint))
;     ("snd"  (pair-snd      %get-uint)
;             (pair-snd-set! %set-uint)))
;
; 
(define-syntax define-c-offset-based-struct
 (syntax-rules ()
  ((define-c-offset-based-struct (?name           #f ?include ...) . ?rest)
   (define-c-offset-based-struct (?name ignorin-ctor ?include ...) . ?rest))
  ((define-c-offset-based-struct (?name ?constructor ?include ...)
                    (?field (?getter ?low-getter)
                            (?setter ?low-setter)) ...)
   (define-c-offset-based-struct "offset-names" ()
                  (?name ?constructor ?include ...)
                  (?field (?getter ?low-getter)
                          (?setter ?low-setter)) ...))
  ((define-c-offset-based-struct (?name ?constructor ?include ...)
                    (?field (?getter ?low-getter)) ...)
   (define-c-offset-based-struct "offset-names" ()
                  (?name ?constructor ?include ...)
                  (?field (?getter ?low-getter)) ...))
  ((define-c-offset-based-struct "offset-names" ?acc ?naming ?field . ?rest)
   (define-c-offset-based-struct "offset-names" ((offset . ?field) . ?acc)
                                   ?naming . ?rest))
  ((define-c-offset-based-struct "offset-names" ?acc ?naming)
   (define-c-offset-based-struct "finish" ?naming . ?acc))

  ((define-c-offset-based-struct "finish" (?name ?constructor ?include ...)
                             (?offset ?field (?getter ?low-getter)
                                             (?setter ?low-setter)) ...)
   (begin
     (define ?constructor #f)
     (define ?getter #f) ...
     (define ?setter #f) ...
     (let ()
      (define-c-info
        ?include ...
        (sizeof size ?name)
        (fields ?name (?offset ?field) ...))

      (set! ?constructor
         (lambda () (make-bytevector size 0)))

      (set! ?getter
         (lambda (x) (?low-getter x ?offset))) ...

      (set! ?setter
         (lambda (x n) (?low-setter x ?offset n))) ...)))
  ((define-c-offset-based-struct "finish" (?name ?constructor ?include ...)
                             (?offset ?field (?getter ?low-getter)) ...)
   (begin
     (define ?constructor #f)
     (define ?getter #f) ...
     (let ()
      (define-c-info
        ?include ...
        (sizeof size ?name)
        (fields ?name (?offset ?field) ...))

      (set! ?constructor
         (lambda () (make-bytevector size 0)))

      (set! ?getter
         (lambda (x) (?low-getter x ?offset))) ...)))))

