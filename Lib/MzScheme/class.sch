;;; -*-Mode: Scheme; coding: iso-8859-1 -*-
;;;
;;; Part of the RIPOFF object system.
;;; Originally part of Tiny-CLOS,
;;; Heavily hacked by Eli Barzilay: Maze is Life!  (eli@barzilay.org)
;;; as part of Swindle, then hacked and ported to Common Larceny by jrm.

;;> This module is the core object system.  It is a heavily hacked
;;> version of the original Tiny-CLOS code from Xerox, but it has been
;;> fitted to Larceny, optimized and extended.  See the source file
;;> for a lot of details about how the CLOS magic is created.

;;; Original copyright:
;;; ***************************************************************************
;;; Copyright (c) 1992 Xerox Corporation.  All Rights Reserved.
;;;
;;; Use, reproduction, and preparation of derivative works are permitted.  Any
;;; copy of this software or of any derivative work must include the above
;;; copyright notice of Xerox Corporation, this paragraph and the one after it.
;;; Any distribution of this software or derivative works must comply with all
;;; applicable United States export control laws.
;;; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
;;; WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, AND
;;; NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY LIABILITY FOR
;;; DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED,
;;; WHETHER ARISING IN CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT
;;; LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGES.
;;; ***************************************************************************

($$trace "class")

;;; Temporarily here while we bootstrap from mzscheme

(define-syntax %instance/class
  (syntax-rules ()
    ((%instance/class instance) (procedure-ref instance 4))))

(define-syntax %instance/slots
  (syntax-rules ()
    ((%instance/slots instance) (procedure-ref instance 5))))

(define-syntax %set-instance/procedure!
  (syntax-rules ()
    ((%set-instance/procedure! instance proc) (procedure-set! instance 3 proc))))

(define-syntax %instance/ref
  (syntax-rules ()
    ((%instance/ref instance offset)
     (vector-ref (%instance/slots instance) offset))))

(define-syntax %instance/set!
  (syntax-rules ()
    ((%instance/set! instance offset value)
     (vector-set! (%instance/slots instance) offset value))))

;;; end of temporarily here

(define the-slots-of-a-class
    '(
      (cpl)                             ; (class ...)
      (default-initargs)
      (direct-default-initargs :initarg :direct-default-initargs) ; ((name form thunk) ...)
      (direct-slots :initarg :direct-slots)                    ; ((name . options) ...)
      (direct-supers :initarg :direct-supers)                   ; (class ...)
      (field-initializers)              ; (proc ...)
      (getters-n-setters)               ; ((slot-name getter setter) ...)
      (initializers)                    ; (proc ...)
      (name :initarg :name :initvalue -anonymous-)             ; a symbol
      (nfields)                         ; an integer
      (serial-number)                   ; a unique integer
      (slots)                           ; ((name . options) ...)
      (valid-initargs)
      ))                ; (initarg ...) or #f

(define unspecified-initializer
  (lambda args (undefined)))

(define object? instance?)

(define class-of
  ;; Bootstrap function.  Used until the class hierarchy has an
  ;; appropriate set of primitive classes.
  (lambda (instance)
    (if (instance? instance)
        (%instance/class instance)
        <top>)))

(define make
  ;; Bootstrap version of make.
  (let ((class-slot-count (length the-slots-of-a-class)))
    (lambda (class . initargs)
      (cond ((or (eq? class <class>)
                 (eq? class <entity-class>))
             (let* ((new      (%make-instance class
                                              (make-vector class-slot-count (undefined))))
                    (dinitargs (getarg initargs ':direct-default-initargs '()))
                    (dslots    (getarg initargs ':direct-slots '()))
                    (dsupers   (getarg initargs ':direct-supers '()))
                    (name      (getarg initargs ':name '-anonymous-))
                    (cpl     (let loop ((sups dsupers) (so-far (list new)))
                               (if (pair? sups)
                                   (loop (append (cdr sups)
                                                 (%class-direct-supers (car sups)))
                                         (if (memq (car sups) so-far)
                                             so-far
                                             (cons (car sups) so-far)))
                                   (reverse! so-far))))
                    (slots
                     (append dslots (append-map %class-direct-slots (cdr cpl))))
                                        ;(all-default-initargs
                                        ; (apply append dinitargs (map %class-direct-default-initargs (cdr cpl))))
                    (nfields 0)
                    (field-initializers '())
                    ;; this is a temporary allocator version, kept as the original
                    ;; one in tiny-clos.  the permanent version below is modified.
                    (allocator
                     (lambda (init)
                       (let ((f nfields))
                         (set! nfields (+ nfields 1))
                         (set! field-initializers (cons init field-initializers))
                         (list (lambda (o)   (%instance/ref  o f))
                               (lambda (o n) (%instance/set! o f n))))))
                    (getters-n-setters
                     (map (lambda (s)
                            (cons (car s) (allocator unspecified-initializer)))
                          slots)))
               (%set-class-default-initargs!   new '()) ; no default initargs now
               (%set-class-direct-default-initargs! new dinitargs)
               (%set-class-direct-supers!      new dsupers)
               (%set-class-direct-slots!       new dslots)
               (%set-class-cpl!                new cpl)
               (%set-class-slots!              new slots)
               (%set-class-nfields!            new nfields)
               (%set-class-field-initializers! new (reverse! field-initializers))
               (%set-class-getters-n-setters!  new getters-n-setters)
               (%set-class-name!               new name)
               (%set-class-serial-number!      new (get-serial-number))
               (%set-class-initializers!       new '()) ; no class inits now
               (%set-class-valid-initargs!     new (append-map
                                                    (lambda (slot) (getargs (cdr slot) ':initarg))
                                                    (%class-slots new)))
               new))
            ((eq? class <generic>)
             (let ((new   (%make-entity class
                                        uninitialized-entity-procedure
                                        (make-vector (length (%class-slots class)) (undefined)))))
               (%set-generic-methods!     new '())
               (%set-generic-arity!       new (getarg initargs ':arity #f))
               (%set-generic-name!        new (getarg initargs ':name '-anonymous-generic-))
               (%set-generic-combination! new (getarg initargs ':combination #f))
               new))
            ((eq? class <method>)
             (let ((new (%make-entity class
                                      uninitialized-entity-procedure
                                      (make-vector (length (%class-slots class)) (undefined)))))
               (%set-method-specializers! new (getarg initargs ':specializers '()))
               (%set-method-procedure!    new (getarg initargs ':procedure #f))
               (%set-method-qualifier!    new (getarg initargs ':qualifier ':primary))
               (%set-method-name!         new (getarg initargs ':name '-anonymous-method-))
               (%set-method-arity!        new (getarg initargs ':arity
                                                      (make-arity-at-least 0)))
               (%set-instance/procedure!  new (method:compute-apply-method #f new))
               new))))))

(define-syntax lookup-slot-info
  (syntax-rules ()
    ((lookup-slot-info class slot-name selector)
     (selector (or (assq slot-name
                         ;; no need to ground slot-ref any more! -- see below
                         ;; (if (eq? class <class>)
                         ;;   ;;* This grounds out the slot-ref tower
                         ;;   getters-n-setters-for-class
                         ;;   (%class-getters-n-setters class))
                         (%class-getters-n-setters class))
                   (error "slot-ref: no slot `~e' in ~e" slot-name class))))))

(define (slot-exists? instance slot-name)
  (assq slot-name (%class-getters-n-setters (class-of instance))))

;;; These two will be replaced by generic functions
(define slot-missing
  (lambda (class instance slot-name why . more)
    (error "Slot missing" slot-name instance why)))

(define slot-unbound
  (lambda (class instance slot-name)
    (error "Slot unbound" slot-name instance why)))

(define (slot-bound? instance slot-name)
  (let* ((class (class-of instance))
         (slot (assq slot-name (%class-getters-n-setters class))))
    (if (not slot)
        (slot-missing class instance slot-name 'slot-bound?)
        (not (eq? ((cadr slot) instance) (undefined))))))

(define (slot-value instance slot-name)
  (let* ((class (class-of instance))
         (slot  (assq slot-name (%class-getters-n-setters class))))
    (if (not slot)
        (slot-missing class instance slot-name 'slot-value)
        (let ((value ((cadr slot) instance)))
          (if (eq? value (undefined))
              (slot-unbound class instance slot-name)
              value)))))

(define slot-ref slot-value)

(define (slot-set! instance slot-name new-value)
  (let* ((class (class-of instance))
         (slot  (assq slot-name (%class-getters-n-setters class))))
    (if (not slot)
        (slot-missing class instance slot-name 'setf new-value)
        ((caddr slot) instance new-value))))

(define (slot-makunbound instance slot-name)
  (let* ((class (class-of instance))
         (slot  (assq slot-name (%class-getters-n-setters class))))
    (if (not slot)
        (slot-missing class instance slot-name 'slot-makunbound)
        ((caddr slot) instance (undefined)))))

(define-syntax %slot-ref
  (syntax-rules ()
    ((%slot-ref object slot-name)
     ((lookup-slot-info (class-of object) slot-name cadr) object))))

(define-syntax %slot-set!
  (syntax-rules ()
    ((%slot-set! object slot-name new-value)
     ((lookup-slot-info (class-of object) slot-name caddr) object new-value))))

(define (make-setter-locked! g+s key error)
  (let ((setter (cadr g+s)))
    (set-car! (cdr g+s)
               (if (eq? key #t)
                   (lambda (o n)
                     (if (eq? (undefined) ((car g+s) o))
                         (setter o n)
                         (else (error))))
                   (lambda (o n)
                     (cond ((and (pair? n) (eq? key (car n)))
                            (setter o (cdr n)))
                           ((eq? (undefined) ((car g+s) o)) (setter o n))
                           (else (error))))))))

;;; The core accessors and mutators.
(define (class-cpl                c) (%slot-ref c 'cpl))
(define (class-default-initargs   c) (%slot-ref c 'default-initargs))
(define (class-direct-default-initargs c) (%slot-ref c 'direct-default-initargs))
(define (class-direct-slots       c) (%slot-ref c 'direct-slots))
(define (class-direct-supers      c) (%slot-ref c 'direct-supers))
(define (class-field-initializers c) (%slot-ref c 'field-initializers))
(define (class-getters-n-setters  c) (%slot-ref c 'getters-n-setters))
(define (class-initializers       c) (%slot-ref c 'initializers))
(define (class-name               c) (%slot-ref c 'name))
(define (class-nfields            c) (%slot-ref c 'nfields))
(define (class-serial-number      c) (%slot-ref c 'serial-number))
(define (class-slots              c) (%slot-ref c 'slots))
(define (class-valid-initargs     c) (%slot-ref c 'valid-initargs))

(define (%set-class-cpl!                     c x) (%slot-set! c 'cpl            x))
(define (%set-class-default-initargs!        c x) (%slot-set! c 'default-initargs x))
(define (%set-class-direct-default-initargs! c x) (%slot-set! c 'direct-default-initargs x))
(define (%set-class-direct-slots!            c x) (%slot-set! c 'direct-slots   x))
(define (%set-class-direct-supers!           c x) (%slot-set! c 'direct-supers  x))
(define (%set-class-field-initializers!      c x) (%slot-set! c 'field-initializers x))
(define (%set-class-getters-n-setters!       c x) (%slot-set! c 'getters-n-setters x))
(define (%set-class-initializers!            c x) (%slot-set! c 'initializers   x))
(define (%set-class-name!                    c x) (%slot-set! c 'name           x))
(define (%set-class-nfields!                 c x) (%slot-set! c 'nfields        x))
(define (%set-class-serial-number!           c x) (%slot-set! c 'serial-number  x))
(define (%set-class-slots!                   c x) (%slot-set! c 'slots          x))
(define (%set-class-valid-initargs!          c x) (%slot-set! c 'valid-initargs x))

(define (generic-arity            g) (%slot-ref g 'arity))
(define (generic-combination      g) (%slot-ref g 'combination))
(define (generic-methods          g) (%slot-ref g 'methods))
(define (generic-name             g) (%slot-ref g 'name))
(define (%generic-app-cache       g) (%slot-ref g 'app-cache))
(define (%generic-singletons-list g) (%slot-ref g 'singletons-list))

(define (%set-generic-app-cache!       g x) (%slot-set! g 'app-cache       x))
(define (%set-generic-arity!           g x) (%slot-set! g 'arity           x))
(define (%set-generic-combination!     g x) (%slot-set! g 'combination     x))
(define (%set-generic-methods!         g x) (%slot-set! g 'methods         x))
(define (%set-generic-name!            g x) (%slot-set! g 'name            x))
(define (%set-generic-singletons-list! g x) (%slot-set! g 'singletons-list x))

(define (method-arity             m) (%slot-ref m 'arity))
(define (method-name              m) (%slot-ref m 'name))
(define (method-procedure         m) (%slot-ref m 'procedure))
(define (method-qualifier         m) (%slot-ref m 'qualifier))
(define (method-specializers      m) (%slot-ref m 'specializers))

(define (%set-method-arity!         m x) (%slot-set! m 'arity          x))
(define (%set-method-name!          m x) (%slot-set! m 'name           x))
(define (%set-method-procedure!     m x) (%slot-set! m 'procedure      x))
(define (%set-method-qualifier!     m x) (%slot-set! m 'qualifier      x))
(define (%set-method-specializers!  m x) (%slot-set! m 'specializers   x))

;;; These versions will be replaced with optimized versions later.
(define %class-cpl                class-cpl)
(define %class-default-initargs   class-default-initargs)
(define %class-direct-default-initargs class-direct-default-initargs)
(define %class-direct-slots       class-direct-slots)
(define %class-direct-supers      class-direct-supers)
(define %class-field-initializers class-field-initializers)
(define %class-getters-n-setters  class-getters-n-setters)
(define %class-initializers       class-initializers)
(define %class-name               class-name)
(define %class-serial-number      class-serial-number)
(define %class-nfields            class-nfields)
(define %class-slots              class-slots)
(define %class-valid-initargs     class-valid-initargs)

(define %generic-arity            generic-arity)
(define %generic-combination      generic-combination)
(define %generic-methods          generic-methods)
(define %generic-name             generic-name)

(define %method-arity             method-arity)
(define %method-name              method-name)
(define %method-procedure         method-procedure)
(define %method-qualifier         method-qualifier)
(define %method-specializers      method-specializers)

(define (class-name-no-angles class)
  (let ((name (%class-name class)))
    (if (symbol? name)
        (let* ((namestring (symbol->string name))
               (len (string-length namestring)))
          (if (and (> len 3)
                   (char=? (string-ref namestring 0) #\<)
                   (char=? (string-ref namestring (- len 1)) #\>))
              (substring namestring 1 (- len 1))
              namestring))
        "an unnamed class")))

;;; SINGLETON

(define singleton-classes (make-hash-table 'weak))

(define (singleton x)
  (hash-table-get
   singleton-classes x
   (lambda ()
     (let ((c (list 'singleton x)))
       (hash-table-put! singleton-classes x c)
       c))))

(define (singleton? x)
  (and (pair? x)
       (eq? (car x) 'singleton)))

(define-syntax %singleton?
  (syntax-rules ()
    ((%singleton? object)
     (and (pair? object)
          (eq? (car object) 'singleton)))))

(define singleton-value cadr)

;;; Struct classes

(define struct-to-class-table (make-hash-table))

(define (struct-type->class stype)
  (hash-table-get
   struct-to-class-table stype
   (lambda ()
     (call-with-values
      (lambda () (struct-type-info stype))
      (lambda (name init-field-k auto-field-k accessor mutator
                    immutable-k-list super skipped?)
        (let* ((super (cond (super (struct-type->class super))
                            (skipped? <opaque-struct>)
                            (else <struct>)))
               (this (parameterize ((*default-object-class* #f))
                       (make <primitive-class>
                         ':name name
                         ':direct-supers (list super)
                         ':direct-default-initargs '()))))
          (hash-table-put! struct-to-class-table stype this)
          this))))))

(define (subclass? c1 c2)
  (if (%singleton? c1)
      (if (%singleton? c2)
          (eq? (singleton-value c1) (singleton-value c2))
          (let ((cc2 (if (struct-type? c2)
                         (struct-type->class c2)
                         c2)))
            (instance-of? (singleton-value c1) cc2)))
      (let ((cc1 (if (struct-type? c1)
                     (struct-type->class c1)
                     c1))
            (cc2 (if (struct-type? c2)
                     (struct-type->class c2)
                     c2)))
        (memq c2 (%class-cpl c1)))))

(define (instance-of? x c)
  (or (eq? c <top>)
      (if (%singleton? c)
          (eq? (singleton-value c) x)
          (let ((cc (if (struct-type? c)
                        (struct-type->class c)
                        c))
                (cx (class-of x)))
            (memq cc (%class-cpl (if (struct-type? cx)
                                     (struct-type->class cx)
                                     cx)))))))

;;; Return #t if each item is an instance of the corresponding class.
;;; Stops at the shortest of the two lists.
(define (instances-of? items classes)
  (if (pair? items)
      (if (pair? classes)
          (and (instance-of? (car items) (car classes))
               (instances-of? (cdr items) (cdr classes)))
          (or (null? classes)
              (error "Improper list: " classes)))
      (or (null? items)
          (error "Improper list: " items))))

;;>>...
;;> *** Basic classes

;;>> <class>
;;>   This is the "mother of all classes":  every Ripoff class is an
;;>   instance of `<class>'.
;;>   Slots:
;;>   * default-initargs: initargs
;;>   * direct-default-initargs: direct initargs
;;>   * direct-supers:  direct superclasses
;;>   * direct-slots:   direct slots, each a list of a name and options
;;>   * cpl:            class precedence list (classes list this to <top>)
;;>   * slots:          all slots (like direct slots)
;;>   * nfields:        number of fields
;;>   * field-initializers: a list of functions to initialize slots
;;>   * getters-n-setters:  an alist of slot-names, getters, and setters
;;>   * name:           class name (usually the defined identifier)
;;>   * serial-number:  a unique integer used for dispatching on the class
;;>   * initializers:   procedure list that perform additional initializing
;;>   See the `clos' documentation for available class and slot keyword
;;>   arguments and their effect.

(define <class>
  (let ((instance (%make-instance #f (make-vector (length the-slots-of-a-class) (undefined)))))
    ;; BOOTSTRAP STEP
    ;; Set class of class to itself
    (set-instance-class-to-self! instance)
    instance))

(define getters-n-setters-for-class     ; see lookup-slot-info
  (map (lambda (s)
         (let ((f (position-of (car s) (map car the-slots-of-a-class))))
           (list (car s)
                 (lambda (o)   (%instance/ref  o f))
                 (lambda (o n) (%instance/set! o f n)))))
       the-slots-of-a-class))

;; BOOTSTRAP STEP
;; Init the getters and setters
((caddr (assq 'getters-n-setters getters-n-setters-for-class)) <class> getters-n-setters-for-class)

;; BOOTSTRAP STEP
;; Put the real accessor in place
(set! %class-getters-n-setters
      ;; and (lookup-slot-info <class> 'getters-n-setters caddr) translates to:
      (cadr (assq 'getters-n-setters getters-n-setters-for-class)))

;;>> <top>
;;>   This is the "mother of all values": every value is an instance of
;;>   `<top>' (including standard Scheme values).
(define <top>
  (make <class>
    ':direct-default-initargs '()
    ':direct-supers '()
    ':direct-slots  '()
    ':name          '<top>))

;;>> <object>
;;>   This is the "mother of all objects": every Ripoff object is an
;;>   instance of `<object>'.
(define <object>
  (make <class>
    ':direct-default-initargs '()
    ':direct-supers (list <top>)
    ':direct-slots  '()
    ':name          '<object>))

;;; This cluster, together with the first cluster above that defines <class>
;;; and sets its class, have the effect of:
;;;   (define <class>
;;;     (make <class> 'direct-default-initargs '()
;;;                   'direct-supers (list <object>)
;;;                   'direct-slots  '(direct-supers ...)
;;;                   'name          '<class>))
;;; BOOTSTRAP STEP - fill in the <class> class

(%set-class-cpl!                <class> (list <class> <object> <top>))
(%set-class-default-initargs!   <class> '())
(%set-class-direct-default-initargs! <class> '())
(%set-class-direct-supers!      <class> (list <object>))
(%set-class-direct-slots!       <class> the-slots-of-a-class)
(%set-class-field-initializers! <class> (map (lambda (s)
                                               (let ((initvalue (getarg (cdr s) ':initvalue (undefined))))
                                                 (lambda args initvalue)))
                                             the-slots-of-a-class))
(%set-class-initializers!       <class> '())
(%set-class-name!               <class> '<class>)
(%set-class-nfields!            <class> (length the-slots-of-a-class))
(%set-class-serial-number!      <class> (get-serial-number))
(%set-class-slots!              <class> the-slots-of-a-class)
(%set-class-valid-initargs!     <class> (append-map
                                         (lambda (slot) (getargs (cdr slot) ':initarg))
                                         the-slots-of-a-class))

;;; At this point <top>, <class>, and <object> have been created and initialized.

;;>> <procedure-class>
;;>   The class of all procedures classes, both standard Scheme procedures
;;>   classes and entity (Ripoff procedure objects) classes.  (Note that
;;>   this is a class of *classes*).
(define <procedure-class>
  (make <class>
    ':direct-default-initargs '()
    ':direct-supers (list <class>)
    ':direct-slots  '()
    ':name          '<procedure-class>))

;;>> <entity-class>
;;>   The class of entity classes -- generic functions and methods.  An
;;>   entity is a procedural Ripoff object, something that you can apply as
;;>   a function but it is still a Ripoff object.  Note that this is the
;;>   class of entity *classes* not of entities themselves.
(define <entity-class>
  (make <class>
    ':direct-default-initargs '()
    ':direct-supers (list <procedure-class>)
    ':direct-slots  '()
    ':name          '<entity-class>))

;;>> <function>
;;>   The class of all applicable values: methods, generic functions, and
;;>   standard closures.
(define <function>
  (make <class>
    ':direct-default-initargs '()
    ':direct-supers (list <top>)
    ':direct-slots  '()
    ':name          '<function>))

;;; The two extra slots below (app-cache and singletons-list) are used to
;;; optimize generic invocations: app-cache holds an 'equal hash-table that
;;; maps a list of classes to the lambda expression that holds the method call
;;; (it used to be an l-hash-table, but 'equal is ok since we can't compare
;;; swindleobj instances recursively) .  The contents of this slot is reset
;;; whenever a method is added to the generic.  Two problems make things a
;;; little more complicated.  First, if add-method is used to modify any of the
;;; generic-invocation-generics then all of these caches should be flushed,
;;; this is achieved by setting *generic-app-cache-tag* to a new [list] object
;;; and the value of app-cache is a cons of that value and the actual hash
;;; table - if we see that the car is not eq? to the current tag, then we flush
;;; the cache.  Second, singleton values might screw things up, so we hold in
;;; singletons-list a list that has the same length as all method specializer
;;; lists, each element contains a hash table with all singleton values that
;;; appear in that place matched to #t, then when we try to see if we have a
;;; cached function for a generic application, we scan the argument list
;;; against this list, and any value that has a singleton with that value at
;;; some method, is left in place for the app-cache lookup (it is used itself
;;; rather than its class).  This whole thing is a bit complicated but lead to
;;; dramatic run-time improvement.
;;>> <generic>
;;>   The class of generic functions: objects that contain method objects
;;>   and calls the appropriate ones when applied.
;;>   Slots:
;;>   * methods:     a list of <method> objects
;;>   * arity:       the generic arity (same for all of its methods)
;;>   * name:        generic name
;;>   * combination: a method combination function or #f, see
;;>                  `make-generic-combination' below for details
(define <generic>
  (make <entity-class>
    ':direct-default-initargs '()
    ':direct-supers (list <object> <function>)
    ':direct-slots  '((methods)
                      (arity :initarg :arity
                             :initvalue #f)
                      (name  :initarg :name
                             :initvalue -anonymous-generic-)
                      (combination :initarg :combination
                                   :initvalue #f)
                      (app-cache)
                      (singletons-list))
    ':name          '<generic>))

;;>> <method>
;;>   The class of methods: objects that are similar to Scheme closures,
;;>   except that they have type specifiers attached.  Note that in contrast
;;>   to Tiny CLOS, methods are applicable objects in Ripoff -- they check
;;>   supplied argument types when applied.
;;>   Slots:
;;>   * specializers: a list of class (and singleton) specializers
;;>   * procedure:    the function (never call directly!)
;;>   * qualifier:    some qualifier tag, used when applying a generic
;;>   * name:         method name
;;>   * arity:        arity
(define <method>
  (make <entity-class>
    ':direct-default-initargs '()
    ':direct-supers (list <object> <function>)
    ':direct-slots  '((specializers :initarg :specializers)
                      (procedure    :initarg :procedure)
                      (qualifier    :initarg :qualifier
                                    :initvalue :primary)
                      (name         :initarg :name
                                    :initvalue -anonymous-method-)
                      (arity        :initarg :arity))
    ':name          '<method>))

;; Do this since compute-apply-method relies on them not changing, as well as a
;; zillion other places.  A method should be very similar to a lambda.
;; BOOTSTRAP STEP
(for-each
 (lambda (slot)
   (make-setter-locked!
    (lookup-slot-info <method> slot cdr) #t
    (lambda () (error "SLOT-SET!:  slot is locked" slot))))
 '(specializers
   procedure
   qualifier))

;;; True if left-method has the same qualifier as and compatible
;;; specializers with right-method.

(define (same-method-signature? left-method right-method)
  (and (eq? (method-qualifier left-method)
            (method-qualifier right-method))
       (let loop ((left-specs  (method-specializers left-method))
                  (right-specs (method-specializers right-method)))
         (if (pair? left-specs)
             (if (pair? right-specs)
                 (and (eq? (car left-specs) (car right-specs))
                      (loop (cdr left-specs) (cdr right-specs)))
                 (or (null? right-specs)
                     (error "Bad specializer list" (method-specializers right-method))))
             (or (null? left-specs)
                 (error "Bad specializer list" (method-specializers left-method)))))))

;; an automatic superclass for all classes -- turned off for the builtins below
;;>> *default-object-class*
;;>   This parameter contains a value which is automatically made a
;;>   superclass for all classes.  Defaults to `<object>'.
(define *default-object-class*
  (make-parameter "*default-object-class*" #f (lambda (x) (or (not x) (class? x)))))

(define (check-initargs class initargs)
  ;; sanity check - verify sensible keywords given
  (let ((valid-initargs (%class-valid-initargs class)))
    (let loop ((args initargs))
      (cond ((or (null? args) (not valid-initargs))
             #t)
            ((not (and (pair? args) (pair? (cdr args))))
             (error "MAKE: error in initargs; arg list not balanced"
                    class))
            ((not (symbol? (car args)))
             (error "MAKE: error in initargs; initarg is not a keyword"
                    class (car args)))
            ((not (memq (car args) valid-initargs))
             (error "MAKE: error in initargs for; unknown keyword"
                    class (car args)))
            (else (loop (cddr args)))))))

;;>> (no-next-method generic method [args ...])
;;>> (no-applicable-method generic [args ...])
;;>   These two generic functions are equivalents to the ones in CL.  The
;;>   first one is applied on a generic and a method in case there was no
;;>   next method and `call-next-method' was used.  The second is used when
;;>   a generic was called but no matching primary methods were found.  The
;;>   only difference is that in Ripoff methods can be applied directly,
;;>   and if `call-next-method' is used, then `no-next-method' gets `#f' for
;;>   the generic argument.
(define no-applicable-method (make <generic> ':name 'no-applicable-method))
(define no-next-method       (make <generic> ':name 'no-next-method))

(define (method:wrong-type-argument method bad-argument expected-type)
  (error
   (let ((string-output-port (string-io/open-output-string)))
     (write method       string-output-port)
     (display ": "       string-output-port)
     (write bad-argument string-output-port)
     (display " is not a(n) " string-output-port)
     (write expected-type string-output-port)
     (display "." string-output-port)
     (string-io/get-output-string string-output-port))))

(define (method:wrong-number-of-arguments method args)
  (error
   (let ((string-output-port (string-io/open-output-string)))
     (display "Wrong number of arguments to " string-output-port)
     (write method string-output-port)
     (string-io/get-output-string string-output-port))))

;;; Add possibility of generic-independent method application - this is the
;;; instance-proc of methods, which is activated when you apply the object (in
;;; the original, methods could not be applied).  This is defined using this
;;; name and arguments because it is later used directly by the generic
;;; function (cannot use the generic in the inital make since methods need to
;;; be created when the generics are constructed).

;;; NOTE:  This is NOT the code run when the method is invoked as part
;;; of a generic function.
(define (method:compute-apply-method call-next-method method)
  (let* ((specializers (%method-specializers method))
         (arity    (method-arity method))
         (exact?   (and (integer? arity) (exact? arity)))
         (required ((if exact? identity arity-at-least-value) arity)))

    (define (method-application-function self arglist)
      (define (next-method-function . new-arglist)
        (apply no-next-method #f self (if (pair? new-arglist)
                                          new-arglist
                                          arglist)))

      ;; Validate the arglist.
      (let loop ((tail     arglist)
                 (req      required)
                 (specs    specializers))
        (cond ((pair? specs) (if (pair? tail)
                                 (if (instance-of? (car tail) (car specs))
                                     (loop (cdr tail) (- req 1) (cdr specs))
                                     (method:wrong-type-argument self (car tail) (car specs)))
                                 (method:wrong-number-of-arguments self arglist)))

              ;; when we run out of specs, ensure we have sufficient
              ;; remaining arguments.
              ((null? specs) (if (if exact?
                                     (length=? tail req)
                                     (length>=? tail req))
                                 (apply (%method-procedure self) next-method-function arglist)
                                 (method:wrong-number-of-arguments self arglist)))

              (else (method:bad-specializers self specializers)))))

    (if (and exact? (length>? specializers required))
        (error "COMPUTE-APPLY-METHOD:  wrong number of specializers."
               (length specializers) (%method-name method) required)
        method-application-function)))

;;>> <primitive-class>
;;>   The class of all built-on classes.

;; Normally, these cannot be allocated.  A method will be added to
;; allocate-instance to prevent that from happening.
(define <primitive-class>
  (make <class>
    ':direct-default-initargs '()
    ':direct-supers (list <class>)
    ':direct-slots  '()
    ':name          '<primitive-class>))

;;>> <builtin>
;;>   The superclass of all built-in classes.
(define <builtin>
  (make <class>
    ':direct-default-initargs '()
    ':direct-supers (list <top>)
    ':direct-slots  '()
    ':name          '<builtin>))

;;>> <struct>
;;>> <opaque-struct>
;;>   These are also classes for built-in objects, but they are classes for
;;>   MzScheme structs -- which can be used like Ripoff classes since they
;;>   will get converted to appropriate Ripoff subclasses of `<struct>'.
(define <struct>
  (make <primitive-class>
    ':direct-default-initargs '()
    ':direct-supers (list <builtin>)
    ':direct-slots  '()
    ':name '<struct>))

;;>   `<opaque-struct>' is a class of structs that are hidden -- see the
;;>   documentation for `struct-info' and the `skipped?' result.  Note that
;;>   structs can be used as long as they can be inspected -- otherwise, we
;;>   can't even know that they are structs with `struct?' (this means that
;;>   <opaque-struct> can only appear in the cpl of a struct class that
;;>   inherits from a struct which is not under the current inspector).
(define <opaque-struct>
  (make <primitive-class>
    ':direct-default-initargs '()
    ':direct-supers (list <struct>)
    ':direct-slots  '()
    ':name '<opaque-struct>))

;;>   Predicates for instances of <builtin>, <function>, <generic>, and
;;>   <method>.
(define (builtin?  x) (instance-of? x <builtin>))
(define (class?    x) (instance-of? x <class>))
(define (function? x) (instance-of? x <function>))
(define (generic?  x) (instance-of? x <generic>))
(define (method?   x) (instance-of? x <method>))
