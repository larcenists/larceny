

(define sys$tag.struct-proc-typetag 0)

(define *struct-proc-proc-offset* 3)
(define *struct-proc-obj-offset* 4)

(define (make-struct-method proc struct-instance)
  (let ((sp (make-struct-method/raw proc struct-instance)))
    (typetag-set! sp sys$tag.struct-proc-typetag)
    sp))

(define (make-struct-proc proc struct-instance)
  (let ((sp (make-struct-proc/raw proc struct-instance)))
    (typetag-set! sp sys$tag.struct-proc-typetag)
    sp))

(define (struct-proc? obj)
  (and (procedure? obj)
       (= (typetag obj) sys$tag.struct-proc-typetag)))

(define (struct-proc-procedure obj)
  (if (struct-proc? obj)
      (procedure-ref obj *struct-proc-proc-offset*)
      (error "struct-proc-procedure: " obj " is not a struct-proc.")))

(define (set-struct-proc-procedure! obj proc)
  (if (struct-proc? obj)
      (procedure-set! obj *struct-proc-proc-offset* proc)
      (error "set-struct-proc-procedure!: " obj " is not an struct-proc.")))

(define (struct-proc-extra obj)
  (if (struct-proc? obj)
      (procedure-ref obj *struct-proc-obj-offset*)
      (error "struct-proc-extra: " obj " is not an struct-proc.")))

(define (set-struct-proc-extra! obj val)
  (if (struct-proc? obj)
      (procedure-set! obj *struct-proc-obj-offset* val)
      (error "set-struct-proc-extra!: " obj " is not an struct-proc.")))
