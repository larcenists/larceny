;; Functions and Variables for dumping classes to an IL file.

; Variables
(define *c-output* #f)
(define *segment-number* 0)
(define *already-compiled* '())
(define *seed* #f)
(define *live-seeds* '())
(define *entrypoints* '())
(define *loadables* '())

(define *il-top-level* '())
(define *current-class* #f)
(define *current-method* #f)
(define *current-il-stream* '())

(vector-struct $$class make-class class? 
               (class-name class-name!) 
               (class-il-namespace class-il-namespace!)
               (class-super class-super!)
               (class-options class-options!)
               (class-members class-members!))

(vector-struct $$method make-method method?
               (method-name method-name!)
               (method-type method-type!)
               (method-argtypes method-argtypes!)
               (method-options method-options!)
               (method-instrs method-instrs!))

(vector-struct $$field make-field field?
               (field-name field-name!)
               (field-type field-type!)
               (field-options field-options!))

;; class-start : string string string (listof option) -> void
(define (class-start name il-namespace super options)
  (set! *current-class* 
        (make-class name il-namespace super options '())))
;; class-finish : -> void
(define (class-finish)
  (class-add *current-class*)
  (set! *current-class* #f))
;; class-add : string string (listof option) (listof instr) -> void
(define (class-add class)
  (set! *il-top-level* (cons class *il-top-level*)))

;; method-start : string string (listof string) (listof string) -> void
(define (method-start name type argtypes options)
  (set! *current-method* (make-method name type argtypes options '())))
;; method-finish : -> void
(define (method-finish)
  (method-instrs! *current-method* (reverse *current-il-stream*))
  (class-members! *current-class* 
                  (cons *current-method*
                        (class-members *current-class*)))
  (set! *current-il-stream* '())
  (set! *current-method* #f))

;; method-add : 
;;   string string (listof string) (listof string) (listof instr) -> void
(define (method-add name type argtypes options instrs)
  (class-members! *current-class*
                  (cons (make-method name type argtypes options instrs)
                        (class-members *current-class*))))

;; global-method-add : 
;;   string string (listof string) (listof string) (listof instr) -> void
(define (global-method-add name type argtypes options instrs)
  (set! *il-top-level* (cons (make-method name type argtypes options instrs)
                             *il-top-level*)))

;; field-add : string string (listof string) -> void
(define (field-add name type options)
  (class-members! *current-class*
                  (cons (make-field name type options)
                        (class-members *current-class*))))

;; ilc (il-consumer) : value -> void
(define (ilc il)
  (set! *current-il-stream* (cons il *current-il-stream*)))

(define (topc il)
  (set! *il-top-level* (cons il *il-top-level*)))

;; il-finalize : output-port -> void
(define (il-finalize oport)
  (dump-top-level))

(define (init-variables)
  (set! *segment-number* 0)
  (set! *seed* #f)
  (set! *live-seeds* '())
  (set! *entrypoints* '())
  (set! *loadables* '())
  (set! *already-compiled* '())
  
  (set! *il-top-level* '())
  (set! *current-class* #f)
  (set! *current-method* #f)
  (set! *current-il-stream* '()))

;; =========================================================

