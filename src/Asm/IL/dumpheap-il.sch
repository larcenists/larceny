;; Functions and Variables for dumping clr-classes to an IL file.

; Variables
(define *c-output* #f)
(define *segment-number* 0)
(define *already-compiled* '())
(define *seed* #f)
(define *live-seeds* '())
(define *entrypoints* '())
(define *loadables* '())

(define *il-top-level* '())
(define *current-clr-class* #f)
(define *current-clr-method* #f)
(define *current-il-stream* '())

; (see util-struct.sch)

;; class-start : string string string (listof option) -> void
(define (class-start name il-namespace super options)
  (set! *current-clr-class*
        (make-clr-class name il-namespace super options '())))
;; class-finish : -> void
(define (class-finish)
  (class-add *current-clr-class*)
  (set! *current-clr-class* #f))
;; class-add : string string (listof option) (listof instr) -> void
(define (class-add class)
  (set! *il-top-level* (cons class *il-top-level*)))

;; method-start : string string (listof string) (listof string) -> void
(define (method-start name type argtypes options)
  (set! *current-clr-method* (make-clr-method name type argtypes options '())))
;; method-finish : -> void
(define (method-finish)
  (clr-method-instrs! *current-clr-method* (reverse *current-il-stream*))
  (clr-class-members! *current-clr-class*
                  (cons *current-clr-method*
                        (clr-class-members *current-clr-class*)))
  (set! *current-il-stream* '())
  (set! *current-clr-method* #f))

;; method-add :
;;   string string (listof string) (listof string) (listof instr) -> void
(define (method-add name type argtypes options instrs)
  (clr-class-members! *current-clr-class*
                  (cons (make-clr-method name type argtypes options instrs)
                        (clr-class-members *current-clr-class*))))

;; global-method-add : 
;;   string string (listof string) (listof string) (listof instr) -> void
(define (global-method-add name type argtypes options instrs)
  (set! *il-top-level* (cons (make-clr-method name type argtypes options instrs)
                             *il-top-level*)))

;; field-add : string string (listof string) -> void
(define (field-add name type options)
  (clr-class-members! *current-clr-class*
                  (cons (make-field name type options)
                        (clr-class-members *current-clr-class*))))

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
  (set! *current-clr-class* #f)
  (set! *current-clr-method* #f)
  (set! *current-il-stream* '()))

;; =========================================================

