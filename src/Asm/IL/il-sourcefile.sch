;; il-sourcefile.sch
;; Dumps IL code to an IL assembly file, to be compiled using ILASM, the 
;; IL assembler.

;; dump-source-text : string . (listof value) -> void
(define (dump-source-text fmt . args)
  (if *c-output*
      (begin
        (apply twobit-format *c-output* fmt args))))



;; dump-source : (oneof string IL-ref) any ... -> void
(define (dump-source fmt . args)
  (define (->formattable arg)
    (cond ((il-class? arg)  (il-class->string arg))
	  ((il-method? arg) (il-method->string arg))
	  ((il-field? arg)  (il-field->string arg))
	  ((il-type? arg)   (il-type->string arg))
	  ((il-label? arg)  (il-label->string arg))
	  ((string? arg)    arg)
	  ((number? arg)    arg)
	  ((symbol? arg)    arg)
	  (else (error '->formattable (twobit-format #f
						     "what am i? ~a" arg)))))
  (if beginning-of-line? 
      (let ((indent (make-string source-indent #\space)))
        (dump-source-text "~a" indent)))
  (set! beginning-of-line? #f)
  (apply dump-source-text (twobit-format #f "~a " (->formattable fmt))
	 (map ->formattable args)))

;; indenting control
(define (dump-newline)
  (dump-source-text "~%")
  (set! beginning-of-line? #t))
(define beginning-of-line? #t)
(define source-indent 0)
(define source-indent-string (make-string source-indent #\space))
(define (indent-up!) 
  (set! source-indent (+ 2 source-indent))
  (set! source-indent-string (make-string source-indent #\space)))
(define (indent-down!) 
  (set! source-indent (- source-indent 2))
  (set! source-indent-string (make-string source-indent #\space)))

;; dump-as-nested-block : (-> void) -> void
(define (dump-as-nested-block thunk)
  (dump-source "{")
  (indent-up!)
  (dump-newline)
  (thunk)
  (indent-down!)
  (dump-source "}")
  (dump-newline))

;; ----------------------------------

;; dump-top-level : -> void
(define (dump-top-level)
  (for-each (lambda (tli)
              (cond ((clr-class? tli) (dump-class tli))
                    ((clr-method? tli) (dump-member tli))
                    ((field? tli) (dump-member tli))
                    (else (dump-il tli))))
            (reverse *il-top-level*)))

;; dump-class : class -> void
(define (dump-class class)
  (let ((il-namespace (clr-class-il-namespace class)))
    (if il-namespace
        (begin (dump-source ".namespace ~a" il-namespace)
               (dump-as-nested-block
                (lambda ()
                  (dump-naked-class class))))
        (dump-naked-class class))))

;; dump-naked-class : class -> void
(define (dump-naked-class class)
  (let ((name (clr-class-name class))
        (super (clr-class-super class))
        (options (clr-class-options class))
        (members (clr-class-members class)))
    (dump-source ".class")
    (for-each dump-source options)
    (dump-source name)
    (dump-source "extends ~a" super)
    
    (dump-as-nested-block
     (lambda () (for-each dump-member (reverse members))))))

;; dump-member : field | method -> void
(define (dump-member member)
  (cond ((field? member)
         (dump-field member))
        ((clr-method? member)
         (dump-method member))))

;; dump-field : field -> void
(define (dump-field field)
  (let ((name (field-name field))
        (type (field-type field))
        (options (field-options field)))
    (dump-source ".field")
    (for-each dump-source options)
    (dump-source type)
    (dump-source (il-quote-name name))
    (dump-newline)))

;; dump-method : method -> void
(define (dump-method method)
  (let ((name (clr-method-name method))
        (type (clr-method-type method))
        (argtypes (clr-method-argtypes method))
        (options (clr-method-options method))
        (instrs (clr-method-instrs method)))
    (dump-source ".method")
    (for-each dump-source (filter-intersect options before-options))
    (dump-source type)
    (dump-source (il-quote-name name))
    
    (dump-source "(")
    (for-each/separated
     (lambda (item) (dump-source item))
     (lambda () (dump-source ","))
     argtypes)
    
    (dump-source ")")
    (for-each dump-source (filter-intersect options after-options))
    
    (dump-as-nested-block
     (lambda () (for-each dump-il instrs)))
    (dump-newline)))

;; filter-intersect : (listof symbol) (listof symbol) -> (listof symbol)
(define (filter-intersect a b)
  (cond ((null? a) '())
        ((memq (car a) b) (cons (car a) (filter-intersect (cdr a) b)))
        (else (filter-intersect (cdr a) b))))

(define before-options '(public private hidebysig virtual static 
                         instance specialname rtspecialname))
(define after-options '(cil managed))

;; dump-il : il -> void
(define (dump-il il)
  (if (string? il)
      (begin (dump-source "~a" il) (dump-newline))
      (let ((bytecode (il.code il))
            (args (il.args il)))
        (case bytecode
          ((comment)
           (apply dump-comment args))
          ((directive)
           (apply dump-directive args))
          ((label)
           (apply dump-label args))
          ((switch)
           (apply dump-switch args))
          ((ldc.i4)
           (apply dump-ldc-i4 bytecode args))
          ((ldloc stloc ldarg starg)
           (apply dump-short-form bytecode args))
          (else (dump-instr bytecode args))))))

;; dump-comment : string -> void
(define (dump-comment str)
  (dump-source "~a" str)
  (dump-newline))

;; dump-directive : symbol . (listof string)  -> void
(define (dump-directive directive . args)
  (case directive
    ((entrypoint)
     (dump-source ".entrypoint"))
    ((maxstack)
     (dump-source ".maxstack ~s" 80)) ;;(car args))) ; 80?
    ((module)
     (dump-source ".module '~a'" (car args)))
    ((assembly-extern)
     (dump-source ".assembly extern '~a'" (car args))
     (dump-as-nested-block (lambda () (for-each dump-il (cadr args)))))
    ((assembly)
     (dump-source ".assembly '~a'" (car args))
     (dump-as-nested-block (lambda () (for-each dump-il (cadr args)))))
    ((local)
     (for-each 
      (lambda (type)
        (dump-source ".locals init (~a)" type)
        (dump-newline))
      (car args)))
    ((line)
     (dump-source ".line ~a ~a" (car args) 
                  (if (cadr args) (twobit-format #f "'~a'" (cadr args)) "")))
    (else (error 'unimplemented "unknown directive: " directive)))
  (dump-newline))

;; dump-label : string -> void
(define (dump-label str)
  (indent-down!)
  (dump-source "~a" str)
  (dump-source ":")
  (indent-up!)
  (dump-newline))

;; dump-instr : symbol (listof string) -> void
(define (dump-instr bytecode args)
  (dump-source bytecode)
  (cond ((null? args) #t)
        ((null? (cdr args))
         (dump-source "~a" (car args)))
        ((null? (cddr args))
         (dump-source "~a, ~a" (car args) (cadr args)))
        (else (error 'unimplemented "unknown instruction format: " 
                     (cons bytecode args))))
  (dump-newline))

(define (dump-ldc-i4 bytecode datum)
  (cond ((memv datum '(0 1 2 3 4 5 6 7 8))
         (dump-instr (string-append (symbol->string bytecode) "." 
                                    (number->string datum))
                     '()))
        ((= datum -1)
         (dump-instr 'ldc.i4.m1 '()))
        ((and (>= datum 0) (< datum 128))
         (dump-instr 'ldc.i4.s (list datum)))
        (else (dump-instr bytecode (list datum)))))
 
(define (dump-short-form bytecode datum)
  (cond ((and (eqv? bytecode 'ldloc) (memv datum '(0 1 2 3)))
         (dump-instr (string-append (symbol->string bytecode) "."
                                    (number->string datum))
                     '()))
        ((and (>= datum 0) (< datum 128))
         (dump-instr (string-append (symbol->string bytecode) ".s")
                     (list datum)))
        (else (dump-instr bytecode (list datum)))))

;; dump-switch : (listof string) -> void
(define (dump-switch labels)
  (dump-source "switch (")
  (for-each/separated 
   (lambda (item) (dump-source "~a" item))
   (lambda () (dump-source ","))
   labels)
  (dump-source ")")
  (dump-newline))
