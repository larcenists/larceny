(require 'std-ffi)
(require 'record)

;; stdlib/malloc : PI [Rtd] . (Size -> Rtd)) 
;;   above, Rtd must have a unary constructor a la Void*-Rtd
;; stdlib/malloc : PI [Rtd (Nat -> Rtd)] . (Size -> Rtd)
;;   handles Rtd that has been extended with more fields
;; Produces a malloc function instantiated at rtd.
(define (stdlib/malloc rtd . args)
  (cond ((not (record-type-extends? rtd void*-rt))
         (error 'stdlib/malloc ": must instantiate stdlib/malloc on an instance of void*-rt.")))
  (let ((internal-malloc
         (ffi/foreign-procedure *ffi-callout-abi* "malloc" (list 'unsigned32) 'unsigned32))
        (ctor (if (null? args) 
                  (record-constructor rtd) 
                  (car args))))
    (lambda (size-t-arg)
      (ctor (internal-malloc size-t-arg)))))

;; stdlib/free : Void* -> void
(define stdlib/free
  (let ((internal-free 
         (ffi/foreign-procedure *ffi-callout-abi* "free" (list 'unsigned32) 'void))
        (void*-ptr 
         (record-accessor void*-rt 'ptr)))
    (lambda (void*)
      (let ((ptr (void*-ptr void*)))
        (internal-free ptr)))))


(define (ffi-install-void*-subtype subtype . args)
  (let ((rtd (cond 
              ((record-type-descriptor? subtype) 
               (if (not (null? args))
                   (error 'ffi-install-void*-subtype
                          ": no optional args for rtd argument"))
               subtype)
              (else
               (let* ((parent (if (null? args)
                                  void*-rt
                                  (car args)))
                      (rtd (cond 
                            ((string? subtype) 
                             (make-record-type subtype '() parent))
                            ((symbol? subtype)
                             (make-record-type (symbol->string subtype) '() parent))
                            (else
                             (error 'ffi-install-void*-subtype 
                                    ": invalid subtype " subtype)))))
                 ((record-updater (record-type-descriptor rtd) 'printer)
                  rtd
                  a-void*-printer)
                 rtd)))))
    (set! *ffi-attributes*
          (cons (list (string->symbol (record-type-name rtd))
                      'unsigned32 
                      (lambda (x name) 
                        (cond (((record-predicate rtd) x)
                               (void*->address x))
                              ((and (void*? x)
                                    (record-type-extends? rtd (record-type-descriptor x)))
                               ;; could issue a warning here; the
                               ;; above predicate ensures that our
                               ;; action *could* be legal.
                               (void*->address x))
                              (else
                               (error "Foreign-procedure " name ": " x
                                      " is not a " subtype))))
                      (lambda (v name)
                        (if (foreign-null-pointer? v)
                            #f
                            ((record-constructor rtd) v))))
                *ffi-attributes*))
    rtd))

;; A SymTree is a (cons Sym [Listof SymTree])
;; SymTree -> void; effect is to install hierarchy of void* subtypes 
;; according to tree.
(define (establish-void*-subhierarchy! tree)
  (let rec ((tree tree)
            (parent #f))
    (let ((name (car tree))
          (children (cdr tree)))
      (let ((new-rtd (if parent 
                         (ffi-install-void*-subtype (car tree) parent)
                         (ffi-install-void*-subtype (car tree)))))
        (for-each (lambda (child)
                    (rec child new-rtd))
                  children)))))

;;; string marshalling code

;; A Char* is a Void* that points to a C string.
(define char*-rt (ffi-install-void*-subtype 'char*))

;; string->char* : String -> Char*
;; result is mallocated on C-heap; must be freed by client.
(define string->char* 
  (let ((malloc (stdlib/malloc char*-rt)))
    (lambda (str)
      (let* ((len (string-length str))
             (char* (malloc (+ len 1))))
        (do ((i 0 (+ i 1)))
            ((= i len) (void*-byte-set! char* len 0) char*)
          (void*-byte-set! char* i (char->integer (string-ref str i))))))))

;; char*-strlen : Char* -> Integer
;; The length of an asciiz string.
(define (char*-strlen char*)
  (do ((i 0 (+ i 1)))
    ((= 0 (void*-byte-ref char* i)) i)))

;; char*->string : Char* x Integer -> String
;; Marshalls an ascii string of the given length
;; char*->string : Char* -> String
;; Marshalls an asciiz string.
(define char*->string
  (case-lambda
    ((char* size)
     (let ((result (make-string size)))
       (do ((i 0 (+ i 1)))
         ((= i size) result)
         (string-set! result i (integer->char (void*-byte-ref char* i))))))
    ((char*)
     (char*->string char* (char*-strlen char*)))))

;; call-with-char* : String (Char* -> T) -> T
;; (automatically allcates and frees the marshalled string; therefore func 
;;  must not retain a reference to its argument after it returns...)
(define call-with-char*
  (lambda (str func)
    (let* ((char* (string->char* str))
           (val (func char*)))
      (stdlib/free char*)
      val)))


;; mallocator->vector->words : PI [Rtd] . (T -> Void*) -> [Vectorof T] -> Rtd
;; result is mallocated on C-heap; must be freed by client.
;; (for now I'm zero terminating all generated arrays because I see no reason not to...)
(define (mallocator->vector->words rtd)
  (let ((malloc (stdlib/malloc rtd))
        (void*-ptr (record-accessor void*-rt 'ptr)))
    (lambda (t->void*)
      (lambda (vect)
        (let* ((len (vector-length vect))
               (array (malloc (* 4 (+ 1 len)))))
          (do ((i 0 (+ i 1)))
              ((= i len) (void*-word-set! array (* 4 len) 0) array)
            (let* ((void* (t->void* (vector-ref vect i)))
                   (word (void*-ptr void*)))
              (void*-word-set! array (* 4 i) word)
              )))))))

;; wordvector->words : PI [Rtd] . [Vectorof Int] -> Rtd
(define (wordvector->words rtd)
  (let ((malloc (stdlib/malloc rtd)))
    (lambda (vect)
      (let* ((len (vector-length vect))
             (array (malloc (* 4 (+ 1 len)))))
        (do ((i 0 (+ i 1)))
            ((= i len) (void*-word-set! array (* 4 len) 0) array)
          (void*-word-set! array (* 4 i) (vector-ref vect i)))))))

        

;; A Char** is a Void* that points to an array of C strings.
(define char**-rt (ffi-install-void*-subtype 'char**))

;; call-with-char** : [Vectorof String] (char** -> T) -> T
;; (automatically allcates and frees the marshalled vector; therefore func 
;;  must not retain a reference any portion of its argument after it returns...)
(define call-with-char**
  (let ((fcn->vector->array (mallocator->vector->words char**-rt)))
    (lambda (vec func)
      (let* ((objs-to-free '())
             (delayed-free! (lambda (v) (set! objs-to-free (cons v objs-to-free)) v))
             (vector->array (fcn->vector->array (lambda (str) (delayed-free! (string->char* str)))))
             (array (delayed-free! (vector->array vec)))
             (val (func array)))
        (for-each stdlib/free objs-to-free)
        val))))

;; A Int* is a Void* that points to an array of integers
(define int*-rt (ffi-install-void*-subtype 'int*))

;; call-with-int* : [Vectorof Int32] (Int* -> T) -> T
;; (automatically allcates and frees the marshalled vector; therefore func 
;;  must not retain a reference any portion of its argument after it returns...)
(define call-with-int*
  (let ((vector->array (wordvector->words int*-rt)))
    (lambda (vec func)
      (let* ((array (vector->array vec))
             (val (func array)))
        (stdlib/free array)
        val))))

;; call-with-boxed : Void* -> Rtd
;; call-with-boxed : Int -> Rtd
;; (automatically allocates and frees the box, but *not* its contents)
(define call-with-boxed
  (lambda (val func)
    (let* ((malloc (stdlib/malloc void*-rt))
           (box (malloc 4)))
      (cond ((void*? val)  (void*-void*-set! box 0 val))
            ((fixnum? val) (void*-word-set! box 0 val))
            (else (error 'call-with-boxed ": cannot box " val)))
      (let ((val (func box)))
        (stdlib/free box)
        val))))

;;; void* box (aka void**) code
             
(define (make-void**)
  (list->bytevector '(0 0 0 0)))
(define (void**-ref v**) 
  ((record-constructor void*-rt) (%get32u v** 0)))
(define (void**-set! v** v*)
  ((record-updater void*-rt 'ptr) 
   v**
   (void*-word-ref v* 0)))

(let ()
  (define (void**->pointer x name)
    (cond ((bytevector? x) 
           x)
          ((eq? x #f)
           (foreign-null-pointer))
          (else
           (error "Foreign-procedure " name ": " x
                  "is not a valid value for void** type."))))
  (ffi-add-attribute-core-entry! 'void** 'pointer void**->pointer #f))



                                   
           

