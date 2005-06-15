
;; vector-struct defines a set of functions for dealing with struct-like
;; vectors. A symbolic tag distinguishes different struct types (and aids
;; readability). Getters and setters are defined with the given names;
;; setters may be #f, in which case none is defined, but getters must be
;; identifiers.

(define-syntax vector-struct
  (syntax-rules ()
    ((_ tag make pred (getter setter) ...)
     (vector-struct/h 'tag make pred 1 () ((getter setter) ...)))))

(define-syntax vector-struct/h
  (syntax-rules ()
    ((_ tag make pred next-index
        ((getter setter index) ...) ((getter0 setter0) (getterz setterz) ...))
     (vector-struct/h tag make pred (+ 1 next-index)
                      ((getter0 setter0 next-index) (getter setter index) ...)
                      ((getterz setterz) ...)))
    ((_ tag make pred next-index
        ((getter setter index) ...) ())
     (begin
       (define (make getter ...) (vector tag getter ...))
       (define (pred obj) (and (vector? obj)
                               (> (vector-length obj) 0)
                               (eq? tag (vector-ref obj 0))))
       (define (getter obj)
         (if (pred obj)
             (vector-ref obj index)
             (error "vector-struct (" 'getter "): expected " tag ", given " obj)))
       ...
       (vector-struct/define-setter setter (obj val)
         (if (pred obj)
             (vector-set! obj index val)
             (error "vector-struct (" 'setter "): expected " tag ", given " obj)))
       ...))))

(define-syntax vector-struct/define-setter
  (syntax-rules ()
    ((_ #f args body)
     (begin))
    ((_ setter args body)
     (define (setter . args) body))))

;;(vector-struct '$$foo make-foo foo? (foo.a foo.a!) (foo.b foo.b!))

(define (map/separated item-proc separator-proc items)
  (if (null? items)
      '()
      (cons (item-proc (car items))
            (let loop ((args (cdr items)))
              (if (null? args)
                  '()
                  (cons (separator-proc)
                        (cons (item-proc (car args))
                              (loop (cdr args)))))))))

(define (for-each/separated item-proc separator-proc items)
  (if (pair? items)
      (begin
        (item-proc (car items))
        (let loop ((args (cdr items)))
          (if (pair? args)
              (begin
                (separator-proc)
                (item-proc (car args))
                (loop (cdr args))))))))

(define (read/map f)
  (let ((next (read)))
    (if (eof-object? next)
        '()
        (cons (f next) (read/map f)))))

(define (read/for-each f)
  (let ((next (read)))
    (if (eof-object? next)
        (unspecified)
        (begin (f next) (read/map f)))))

;; Growable vector

(vector-struct $$growable-vector make-growable-vector growable-vector?
               (growable-vector.elements growable-vector.elements!)
               (growable-vector.default growable-vector.default!))

(define (make-gvector default)
  (make-growable-vector (make-vector 8 default) default))
(define (gvector-ref gv i)
  (ensure-gvector-length gv (+ 1 i))
  (vector-ref (growable-vector.elements gv) i))
(define (gvector-set! gv i value)
  (ensure-gvector-length gv (+ 1 i))
  (vector-set! (growable-vector.elements gv) i value))
(define (ensure-gvector-length gv size)
  (let ((elements (growable-vector.elements gv)))
    (let loop ((target-size (vector-length elements)))
      (if (< target-size size)
          (loop (* 2 target-size))
          (growable-vector.elements!
           gv
           (vector-copy-from-to elements
                                (make-vector target-size (growable-vector.default gv))
                                (vector-length elements)))))))
(define (vector-copy-from-to src dst len)
  (let loop ((i 0))
    (if (< i len)
        (begin (vector-set! dst i (vector-ref src i))
               (loop (+ i 1)))
        dst)))
(define (gvector-length gv)
  (vector-length (growable-vector.elements gv)))
