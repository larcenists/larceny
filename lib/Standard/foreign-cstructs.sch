(require 'std-ffi)
(require 'foreign-ctools)

;; Define getters and setters for a C struct.  Example:
;
;   (define-c-struct ("struct pair" "pair.h" make-pair)
;     ("fst"  (pair-fst      %get-uint)
;             (pair-fst-set! %set-uint))
;     ("snd"  (pair-snd      %get-uint)
;             (pair-snd-set! %set-uint)))
;
(define-syntax define-c-struct
 (syntax-rules ()
  ((define-c-struct (?name ?constructor ?include ...)
                    (?field (?getter ?low-getter)
                            (?setter ?low-setter)) ...)
   (define-c-struct "offset-names" ()
                  (?name ?constructor ?include ...)
                  (?field (?getter ?low-getter)
                          (?setter ?low-setter)) ...))
  ((define-c-struct (?name ?constructor ?include ...)
                    (?field (?getter ?low-getter)) ...)
   (define-c-struct "offset-names" ()
                  (?name ?constructor ?include ...)
                  (?field (?getter ?low-getter)) ...))
  ((define-c-struct "offset-names" ?acc ?naming ?field . ?rest)
   (define-c-struct "offset-names" ((offset . ?field) . ?acc)
                                   ?naming . ?rest))
  ((define-c-struct "offset-names" ?acc ?naming)
   (define-c-struct "finish" ?naming . ?acc))

  ((define-c-struct "finish" (?name ?constructor ?include ...)
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
  ((define-c-struct "finish" (?name ?constructor ?include ...)
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

