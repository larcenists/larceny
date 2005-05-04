;;; -*-Mode: Scheme; coding: iso-8859-1 -*-
;;;
;;; Part of the RIPOFF object system.
;;; Originally part of Tiny-CLOS,
;;; Heavily hacked by Eli Barzilay: Maze is Life!  (eli@barzilay.org)
;;; as part of Swindle, then hacked and ported Common Larceny by jrm.

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

($$trace "generic")

;;; Syntax copied here for mzscheme bootstrapping.
;;; This should go away once we are self hosting.

;;; Shamelessly abuse the macro system to get performance.
(define-syntax %set-instance/procedure!
  (syntax-rules ()
    ((%set-instance/procedure! instance proc) (procedure-set! instance 3 proc))))

(define-syntax %instance/class
  (syntax-rules ()
    ((%instance/class instance) (procedure-ref instance 4))))

(define-syntax %instance/slots
  (syntax-rules ()
    ((%instance/slots instance) (procedure-ref instance 5))))

(define-syntax %instance/ref
  (syntax-rules ()
    ((%instance/ref instance offset)
     (vector-ref (%instance/slots instance) offset))))

(define-syntax %instance/set!
  (syntax-rules ()
    ((%instance/set! instance offset value)
     (vector-set! (%instance/slots instance) offset value))))

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

;;; End of syntax for mzscheme

;;>   Determines whether `x' is a class.
(define-syntax %class?
  (syntax-rules ()
    ((%class? object) (instance-of? object <class>))))

;;>   Determines if something is a singleton specification (which is any
;;>   list with a head containing the symbol `singleton').
(define-syntax %singleton?
  (syntax-rules ()
    ((%singleton? object)
     (and (pair? object)
          (eq? (car object) 'singleton)))))

;;; This looks *much* harder than it is.  The idea is simple.  Each
;;; direct superclass has a linearization list.  We look at the heads
;;; of each of these lists.  If we find a head element that *isn't* in
;;; the tail of a different list, we use it (and pop it from all the
;;; lists that might also have that head element).  Iterate until done.

;;; NOTE:  THIS IS THE `C3' LINEARIZATION ALGORITHM
;;; IT IS DIFFERENT FROM CLOS AND SWINDLE
(define (compute-class-linearization class)

  (define (select-candidate lists)
    (let search-loop ((scan lists))
      (cond ((pair? scan)
             (let ((list (car scan))
                   (more (cdr scan)))
               (cond ((pair? list)
                      (let ((candidate (car list)))
                        (let validate-loop ((l lists))
                          (cond ((pair? l)
                                 (let ((v (car l)))
                                   (cond ((pair? v) (if (memq candidate (cdr v))
                                                        (search-loop more)
                                                        (validate-loop (cdr l))))
                                         ((null? v) (validate-loop (cdr l)))
                                         (else (error "Improper list.")))))
                                ((null? l) candidate)
                                (else (error "Improper list."))))))
                     ((null? list) (search-loop more))
                     (else (error "Improper list")))))
            ((null? scan) (error "Inconsistent class linearization."))
            (else (error "Improper list.")))))

  (define (merge-lists remaining result)
    (if (every? null? remaining)
        (reverse! result)
        (let ((candidate (select-candidate remaining)))
          (merge-lists
           (map (lambda (list)
                  (cond ((pair? list) (if (eq? candidate (car list))
                                          (cdr list)
                                          list))
                        ((null? list) list)
                        (else (error "Improper list"))))
                remaining)
           (cons candidate result)))))

  (let ((direct-superclasses (%class-direct-supers class)))
    (merge-lists
     ;; Huh?  This makes no sense at all!
     ;; (append (map %class-cpl direct-superclasses)
     ;;        (list direct-superclasses))
     (map %class-cpl direct-superclasses)
     (list class))))

;;>>... Generics in the instance initialization protocol
;;> The following generic functions are used as part of the protocol of
;;> instantiating an instance, and some are used specifically to instantiate
;;> class objects.

;;; The instance structure protocol.
;;>> (allocate-instance class)
;;>   This generic function is called to allocate an instance of a class.
;;>   It is applied on the class object, and is expected to return the new
;;>   instance object of that class.
(define allocate-instance             (make <generic> :name 'allocate-instance :arity 2))

;;>> (initialize-instance instance initargs)
;;>   This generic is called to initialize an instance.  It is applied on
;;>   the newly allocated object and the given initargs, and is not expected
;;>   to return any meaningful value -- only do some side effects on the
;;>   instance to initialize it.  When overriding this for a some class, it
;;>   is not a good idea to skip `call-next-method' since it is responsible
;;>   for initializing slot values.
(define initialize-instance                    (make <generic> :name 'initialize-instance :arity 2))

;;>> (compute-getter-and-setter class slot allocator)
;;>   This generic is used to get a getter and setter functions for a given
;;>   slot.  It is passed the class object, the slot information (a list of
;;>   a slot name and options), and an allocator function.  The allocator is
;;>   a function that gets an initializer function and returns an index
;;>   position for the new slot.  The return value should be a list of two
;;>   elements -- a getter and a setter functions.
(define compute-getter-and-setter     (make <generic> :name 'compute-getter-and-setter :arity 3))

;;; The class initialization protocol.
;;>> (compute-cpl class)
;;>   This generic is used to get the class-precedence-list for a class
;;>   object.  The standard <class> object uses the `compute-std-cpl' (see
;;>   in the code) which flattens the class ancestors using a topological
;;>   sort that resolve ambiguities left-to-right.
(define compute-cpl                   (make <generic> :name 'compute-cpl :arity 1))

;;>> (compute-slots class)
;;>   This generic is used to compute all slot information for a given
;;>   class, after its precedence list has been computed.  The standard
;;>   <class> collects information from all preceding classes.
(define compute-slots                 (make <generic> :name 'compute-slots :arity 1))

;;>> (compute-default-initargs class)
;;>   This generic is used to compute the default initargs for a given
;;>   class.
(define compute-default-initargs      (make <generic> :name 'compute-default-initargs :arity 1))

;;>> (compute-apply-method method)
;;>   This generic is used to compute the procedure that will get executed
;;>   when a method is applied directly.
(define compute-apply-method          (make <generic> :name 'compute-apply-method :arity 1))

;;>> ((compute-apply-generic generic) args ...)
;;>   This generic is used to compute the object (a closure) that is
;;>   actually applied to execute the generic call.  The standard version
;;>   uses `compute-method' and `compute-apply-methods' below, and caches
;;>   the result.
(define compute-apply-generic         (make <generic> :name 'compute-apply-generic :arity 1))

;;>> (compute-methods generic args)
;;>   Computes the methods that should be applied for this generic
;;>   invocation with args.  The standard code filters applicable methods
;;>   and sorts them according to their specificness.  The return value is
;;>   expected to depend only on the types of the arguments (and values if
;;>   there are singleton specializers).
(define compute-methods               (make <generic> :name 'compute-methods :arity 2))

;;>> ((compute-method-more-specific? generic) mthd1 mthd2 args)
;;>   Get a generic and return a function that gets two methods and a list
;;>   of arguments and decide which of the two methods is more specific.
;;>   This decision should only be based on the argument types, or values
;;>   only in case of singletons.
(define compute-method-more-specific? (make <generic> :name 'compute-method-more-specific? :arity 1))

;;>> ((compute-apply-methods generic methods) args ...)
;;>   Gets a generic and returns a function that gets the given arguments
;;>   for this call.  This function which it returns is the combination of
;;>   all given methods.  The standard one arranges them by default using
;;>   the `call-next-method' argument argument that methods have.  Ripoff
;;>   extends this with qualified methods and applies `before', `after', and
;;>   `around' methods in a similar way to CLOS: first the `around' methods
;;>   are applied (and they usually call their `call-next-method' to
;;>   continue but can return a different value), then all the `before'
;;>   methods are applied (with no `call-next-method'), then all `primary'
;;>   methods as usual (remembering the return value), and finally the
;;>   `after' methods (similar to the `before', but in reverse specificness
;;>   order).  If the generic has a `combination' slot value, then it is a
;;>   procedure that is used to combine the primary methods, but the
;;>   auxiliary ones are still applied in the same way.  This is unlike CLOS
;;>   where the standard combinations run only `around' methods, and there
;;>   is generally more control with method combinations, but in Ripoff
;;>   `compute-apply-methods' should be overridden for this.  See
;;>   `make-generic-combination' for details about method combinations.
(define compute-apply-methods         (make <generic> :name 'compute-apply-methods :arity 2))

(define generic-invocation-generics
  (list compute-apply-generic compute-methods
        compute-method-more-specific? compute-apply-methods))

;;; This is used to signal whenever all method caches are to be reset - so when
;;; a method is added to generic-invocation-generics, this is set to some value
;;; which is not eq? to the current one.
(define *generic-app-cache-tag* #t)


;;>> (add-method generic method)
;;>   This generic function is called to add a method to a generic function
;;>   object.  This is another change from the original Tiny CLOS where it
;;>   was a normal function.
(define (add-method generic method)
  ;; add singleton specializer value (if any) to the corresponding hash table
  ;; in singletons-list.
  (define (add-to-singletons-list specs tables)
    (if (pair? specs)
        (begin
          (if (%singleton? (car specs))
              (begin
                (if (not (car tables))
                    (set-car! tables (make-hash-table 'weak)))
                (hash-table-put! (car tables) (singleton-value (car specs)) #t)))
          (add-to-singletons-list (cdr specs) (cdr tables)))))

  (define (n-falses n)
    (let loop ((n n) (r '()))
      (if (zero? n) r (loop (- n 1) (cons #f r)))))

  (let ((tables    (%generic-singletons-list generic))
        (specs     (%method-specializers method))
        (qualifier (%method-qualifier method)))
    ;; make sure that tables always contain enough hash tables (or #f's)
    (cond ((eq? tables (undefined))
           (set! tables (n-falses (length specs)))
           (%set-generic-singletons-list! generic tables))
          ((shorter? tables specs)
           (set! tables (append!
                         tables
                         (n-falses (- (length specs) (length tables)))))
           (%set-generic-singletons-list! generic tables)))
    (add-to-singletons-list specs tables)
    (if (memq generic generic-invocation-generics)
        ;; reset all caches by changing the value of *generic-app-cache-tag*
        (set! *generic-app-cache-tag* (list #f))
        ;; reset this generic app-cache
        (%set-generic-app-cache! generic (undefined)))
    (%set-generic-methods!
     generic
     (cons method
           ;; When adding a method, we remove the previous definition
           ;; by filtering out methods with the same specializers and
           ;; qualifiers.
           (filter (lambda (existing)
                     (not (same-method-signature? method existing)))
                   (%generic-methods generic))))
    (%set-instance/procedure! generic (compute-apply-generic generic))))

;;; Adding a method calls COMPUTE-APPLY-GENERIC, the result of which calls the
;;; other generics in the generic invocation protocol.  Two, related, problems
;;; come up.  A chicken and egg problem and a infinite regress problem.
;;; In order to add our first method to COMPUTE-APPLY-GENERIC, we need
;;; something sitting there, so it can be called.  The first definition below
;;; does that.
;;; Then, the second definition solves both the infinite regress and the not
;;; having enough of the protocol around to build itself problem the same way:
;;; it special cases invocation of generics in the invocation protocol.

;;; BOOTSTRAP STEP
(%set-instance/procedure! compute-apply-generic
                          (lambda (instance arglist)
                            (let ((generic (car arglist)))
                              ((%method-procedure (car (%generic-methods generic))) '() generic))))

(add-method compute-apply-generic
  (make <method>
    :arity 1
    :specializers (list <generic>)
    :procedure ((lambda ()

                   ;; This function converts the list of arguments to a list of keys to look
                   ;; for in the cache - use the argument's class except when there is a
                   ;; corresponding singleton with the same value at the same position.
                   (define (get-keys args tables)
                     (let loop ((args args) (tables tables) (ks '()))
                       (if (or (null? tables) (null? args))
                           (reverse! ks)
                           (loop (cdr args) (cdr tables)
                                 (cons (if (and (car tables)
                                                (hash-table-get
                                                 (car tables) (car args) (lambda () #f)))
                                           (car args)
                                           ;; use the serial number for hashing
                                           (class-serial-number (class-of (car args))))
                                       ks)))))


                   (define (method:compute-apply-generic call-next-method ignored)
;      #| The code below is the original, then comes the optimized version below
;      ;; see the definition of the <generic> class above.
;      (lambda args
;        (if (and (memq generic generic-invocation-generics)    ;* Ground case
;                 (memq (car args) generic-invocation-generics))
;          (apply (%method-procedure (last (%generic-methods generic))) #f args)
;          ((compute-apply-methods generic)
;           (compute-methods generic args) . args)))
;      |#

                     ;; This is the main function that brings the correct value from the
                     ;; cache, or generates one and store it if there is no entry, or the
                     ;; cache was reset.  Finally, it is applied to the arguments as usual.
                     ;; NOTE: This code is delicate! Handle with extreme care!
                     (lambda (generic args)
                       (let ((app-cache (%generic-app-cache generic))
                             (arity     (%generic-arity generic))
                             (keys      (get-keys args (%generic-singletons-list generic)))
                             (ground?   (and ;* Ground case
                                         (memq generic generic-invocation-generics)
                                         (pair? args)
                                         (memq (car args) generic-invocation-generics))))
                         ;; This function creates the cached closure -- the assumption is that
                         ;; `keys' contain a specification that will identify all calls that
                         ;; will have this exact same list.
                         (define (compute-callable)
                           (let ((c (if ground?
                                        (let ((m (%method-procedure
                                                  (last (%generic-methods generic)))))
                                          (lambda args (apply m #f args)))
                                        (compute-apply-methods
                                         generic (compute-methods generic args)))))
                             (hash-table-put! (cdr app-cache) keys c)
                             c))
                         (if (cond ((not arity) #f)
                                   ((and (integer? arity) (exact? arity)) (not (length=? args arity)))
                                   (else (length<? args (arity-at-least-value arity))))
                             (let ((least (and (arity-at-least? arity)
                                               (arity-at-least-value arity))))
                               (error "APPLY:  wrong number of arguments to generic function " (%generic-name generic))))
                         (if (or (eq? app-cache (undefined))
                                 (not (eq? (car app-cache) *generic-app-cache-tag*)))
                             (begin
                               (set! app-cache (cons *generic-app-cache-tag*
                                                     (make-hash-table 'weak 'equal)))
                               (%set-generic-app-cache! generic app-cache)))
                         (apply (hash-table-get (cdr app-cache) keys compute-callable) args))))

                   method:compute-apply-generic))))

(add-method compute-methods
  (make <method>
    :arity 2
    :specializers (list <generic>)
    :procedure ((lambda ()
                   (define (sort-predicate compare arguments)
                     (lambda (left-method right-method)
                       (compare left-method right-method arguments)))

                   (define (method:compute-methods call-next-method generic args)
                     (sort (filter
                            (lambda (m)
                              (instances-of? args (%method-specializers m)))
                            (%generic-methods generic))
                           (sort-predicate (compute-method-more-specific? generic) args)))

                   method:compute-methods))))

(add-method compute-method-more-specific?
  (make <method>
    :arity 1
    :specializers (list <generic>)
    :procedure ((lambda ()
                   (define (method:compute-method-more-specific? call-next-method generic)
                     (lambda (left right args)
                       (let loop ((specls-left (%method-specializers left))
                                  (specls-right (%method-specializers right))
                                  (args    args))
                         (cond ((and (null? specls-left) (null? specls-right))
                                (if (eq? (%method-qualifier left) (%method-qualifier right))
                                    (error "COMPUTE-METHOD-MORE-SPECIFIC?:  two methods are equally specific in " generic)
                                    #f))
                               ;; some methods in this file have fewer specializers than
                               ;; others for things like args -- so remove this, leave the
                               ;; args check but treat the missing as if it's <top>
                               ;; ((or (null? specls-left) (null? specls-right))
                               ;;  (error 'generic
                               ;;         "two methods have different number of ~
                               ;;          specializers in ~e" generic))
                               ((null? args) ; shouldn't happen
                                (error "COMPUTE-METHOD-MORE-SPECIFIC?: fewer arguments than specializers for " generic))
                               ((null? specls-left) ; see above -> treat this like <top>
                                (if (eq? <top> (car specls-right))
                                    (loop specls-left (cdr specls-right) (cdr args))
                                    #f))
                               ((null? specls-right) ; see above -> treat this like <top>
                                (if (eq? <top> (car specls-left))
                                    (loop (cdr specls-left) specls-right (cdr args))
                                    #t))
                               (else (let ((c1 (car specls-left))
                                           (c2 (car specls-right)))
                                       (if (eq? c1 c2)
                                           (loop (cdr specls-left) (cdr specls-right) (cdr args))
                                           (more-specific? c1 c2 (car args)))))))))
                   method:compute-method-more-specific?))))

(add-method compute-apply-methods
  (make <method>
    :arity 2
    :specializers (list <generic>)
    :procedure ((lambda ()
                   (define (method:compute-apply-methods call-next-method generic methods)
                     (let ((primaries '()) (arounds '()) (befores '()) (afters '())
                           (combination (%generic-combination generic)))
                       ;; *** Trick: this (and in <method> above) is the only code that is
                       ;; supposed to ever apply a method procedure.  So, the closure that
                       ;; will invoke `no-next-method' is named `*no-next-method' so it is
                       ;; identifiable.  The only way to break this would be to call the
                       ;; method-procedure directly on an object with such a name.

                       (define one-step
                         (if combination
                             (combination generic)
                             (lambda (tail args)
                               (lambda newargs
                                 ;; tail is never null: (null? (cdr tail)) below, and the fact
                                 ;; that this function is applied on the primaries which are
                                 ;; never null
                                 (let ((args (if (null? newargs) args newargs)))
                                   (apply (cdar tail)
                                          (if (null? (cdr tail))
                                              ((lambda ()
                                                 (define (*no-next-method* . args)
                                                   (apply no-next-method generic (caar tail) args))
                                                 *no-next-method*))
                                              (one-step (cdr tail) args))
                                          args))))))

                       (define (apply-before/after-method args)
                         (lambda (method)
                           (apply (cdr method)
                                  ((lambda ()
                                     (define (*no-next-method* . args)
                                       (apply no-next-method generic (car method) args))
                                     *no-next-method*))
                                  args)))

                       (define (call-before-primary-after args)
                         (lambda newargs
                           ;; could supply newargs below, but change before calling befores
                           (let ((args (if (null? newargs) args newargs)))
                             (for-each (apply-before/after-method args) befores)
                             (let ((result ((one-step primaries args))))
                               (for-each (apply-before/after-method args) afters)
                               result))))

                       (define (one-around-step tail args)
                         (if (null? tail)
                             (call-before-primary-after args)
                             (lambda newargs
                               (let ((args (if (null? newargs) args newargs)))
                                 (apply (cdar tail) (one-around-step (cdr tail) args) args)))))

                       ;; first sort by qualifier and pull out method-procedures
                       (let loop ((ms methods))
                         (cond ((pair? ms)
                                (let* ((cms (car ms))
                                       (q (%method-qualifier cms)))
                                  (cond ((eq? q :primary)
                                         (set! primaries (cons (cons cms (%method-procedure cms)) primaries)))
                                        ((eq? q :around)
                                         (set! arounds (cons (cons cms (%method-procedure cms)) arounds)))
                                        ((eq? q :before)
                                         (set! befores (cons (cons cms (%method-procedure cms)) befores)))
                                        ((eq? q :after)
                                         (set! afters (cons (cons cms (%method-procedure cms)) afters)))
                                        (else (error "COMPUTE-APPLY-METHODS:  a method ~e has an unexpected qualifier "
                                                     (car methods) (%method-qualifier (car methods)))))
                                  (loop (cdr ms))))
                               ((null? ms)
                                (set! primaries (reverse! primaries))
                                (set! arounds   (reverse! arounds))
                                (set! befores   (reverse! befores)))
                               ;; no reverse! for afters)
                               (else (error "COMPUTE-APPLY-METHODS:  bad list?"))))

                       (cond ((null? primaries)
                              (lambda args (apply no-applicable-method generic args)))
                             ;; optimize common case of only primaries
                             ((and (null? befores) (null? afters) (null? arounds))
                              ;; args is initialized to () since if it is a generic of no
                              ;; arguments then it will always stay so, otherwise, the first
                              ;; call will have the real arguments anyway
                              (one-step primaries '()))
                             (else (one-around-step arounds '())))))
                   method:compute-apply-methods))))

;;>> (((make-generic-combination keys...) generic) tail args)
;;>   This function can be used to construct simple method combinations that
;;>   can be used with the `combination' slot of generic functions.  The
;;>   combination itself is a function that gets a generic and returns a
;;>   function that gets a list of method/procedure pairs (for optimization
;;>   the method-procedures are pre taken) and the arguments and performs
;;>   the call -- but this is only interesting if there's any need to
;;>   implement a method combination directly, otherwise, the
;;>   `make-generic-combination' interface should allow enough freedom.
;;>   Note that when a method combination is used, `around', `before', and
;;>   `after' are called around the primary call as usual, but the primaries
;;>   are never called with a valid `call-next-method' argument.
;;>
;;   NOTE NOTE NOTE
;;   The args are no longer keyword args but are required args.
;;>   The keyword arguments that can be taken determine the behavior of this
;;>   combination.  Overall, it is roughly like a customizable version of a
;;>   fold operation on the method calls.
;;>   * init
;;>     - The initial value for this computation.  Defaults to null.
;;>   * combine
;;>     - A function to be called on a method call result and the old value,
;;>       and produces a new value.  The default is `cons', which with an
;;>       initial null value will collect the results into a reversed list.
;;>   * process-methods
;;>     - A function that can be called on the initial list of
;;>       method/procedure pairs to change it -- for example, it can be
;;>       reversed to apply the methods from the least specific to the most
;;>       specific.  No default.
;;>   * process-result
;;>     - A function that can be called on the final resulting value to
;;>       produce the actual return value.  For example, it can reverse back
;;>       a list of accumulated values.  No default.
;;>   * control
;;>     - If this parameter is specified, then the `'combine' argument is
;;>       ignored.  The value given to `'control' should be a function of
;;>       four arguments:
;;>       1. a `loop' function that should be called on some new value and
;;>          some new tail;
;;>       2. a `val' argument that gets the current accumulated value;
;;>       3. a `this' thunk that can be called to apply the current method
;;>          and return its result;
;;>       4. a `tail' value that holds the rest of the method/procedure list
;;>          which can be sent to `loop'.
;;>       It should be clear now, that a `'control' argument can have a lot
;;>       of control on the computation, it can abort, change arbitrary
;;>       values and skip calling methods.  Note that if it won't call
;;>       `loop' with an empty list, then a `'process-result' function will
;;>       not be used as well.  See the pre-defined combinations in the
;;>       source code to see examples of using this function.
(define (make-generic-combination init process-methods process-result control)
  (lambda (generic)
    (lambda (tail dummy-args)
      (let ((tail (process-methods tail)))
        (lambda args
          (let loop ((res init) (tail tail))
            ;; see *no-next-method* trick above
            (let ((*no-next-method*
                   (lambda args (apply no-next-method generic (caar tail) args))))
              (cond ((pair? tail) (control loop res
                                           (lambda () (apply (cdar tail) *no-next-method* args))
                                           (cdr tail)))
                    ((null? tail) (process-result res))
                    (else (error "MAKE-GENERIC-COMBINATION:  Bad method list?"))))))))))

(define (generic-combination-cons loop val this tail)
  (loop (cons (this) val) tail))

(define (generic-combination-control combine)
  (lambda (loop val this tail)
    (loop (combine (this) val) tail)))

;;>> generic-+-combination
;;>> generic-list-combination
;;>> generic-min-combination
;;>> generic-max-combination
;;>> generic-append-combination
;;>> generic-append!-combination
;;>> generic-begin-combination
;;>> generic-and-combination
;;>> generic-or-combination
;;>   These are all functions that can be used as a `combination' value for
;;>   a generic function.  They work in the same way as the standard method
;;>   combinations of CL.  Most of them do the obvious thing based on some
;;>   function to combine the result.  The `begin' combination simply
;;>   executes all methods one by one and returns the last value, the `and'
;;>   and `or' combinations will call them one by one until a false or true
;;>   result is returned.  The source of these can be used as templates for
;;>   defining more combinations.
(define generic-+-combination
  (make-generic-combination   0 identity identity (generic-combination-control +)))
(define generic-list-combination
  (make-generic-combination '() identity reverse! generic-combination-cons))
(define generic-min-combination
  (make-generic-combination '() identity (lambda (r) (apply min r)) generic-combination-cons))
(define generic-max-combination
  (make-generic-combination '() identity (lambda (r) (apply max r)) generic-combination-cons))
(define generic-append-combination
  (make-generic-combination '() identity (lambda (r) (apply append (reverse! r))) generic-combination-cons))
(define generic-append!-combination
  (make-generic-combination '() identity (lambda (r) (apply append! (reverse! r))) generic-combination-cons))
(define generic-begin-combination
  (make-generic-combination #f identity identity (generic-combination-control (lambda (x y) x))))
(define generic-and-combination
  (make-generic-combination #t identity identity (lambda (loop val this tail)
                                                   (and val
                                                        (let ((it (this)))
                                                          (and it
                                                               (loop it tail)))))))
(define generic-or-combination
  (make-generic-combination #f identity identity (lambda (loop val this tail)
                                                   (or (this) (loop #f tail)))))

;;>> (specializer? x)
;;>   Determines whether `x' is a class, a singleton, or a struct-type.
(define (specializer? x)
  (or (class? x)
      (%singleton? x)
      (struct-type? x)
      (record-type-descriptor? x)))

(define-syntax %struct->class
  (syntax-rules ()
    ((%struct->class c) (cond ((record-type-descriptor? c) (record-type->class c))
                              ((struct-type? c) (struct-type->class c))
                              (else c)))))

;;>> (more-specific? class1 class2 x)
;;>   Is `class1' more specific than `class2' for the given value?
(define (more-specific? c1 c2 arg)
  (cond ((%singleton? c1) (eq? (singleton-value c1) arg))
        ((%singleton? c2) (not (eq? (singleton-value c2) arg)))
        (else (let ((cc1 (memq (%struct->class c1) (%class-cpl (class-of arg)))))
                (and cc1 (memq (%struct->class c2) (cdr cc1)))))))

;;; Install the class initializers for the standard classes.
(add-method initialize-instance
  (make <method>
    :arity 2
    :specializers (list <top>)
    :procedure ((lambda ()
                   (define (method:initialize-instance call-next-method object initargs)
                     (error "INITIALIZE-INSTANCE: can't initialize an instance of "
                            (%class-name (class-of object))))
                   method:initialize-instance))))

(add-method initialize-instance
  (make <method>
    :arity 2
    :specializers (list <object>)
    :procedure ((lambda ()
                   (define (method:initialize-instance call-next-method object initargs)
                     (let ((class (class-of object)))
                       (for-each (lambda (init) (apply init initargs))
                                 (%class-initializers class))
                       (foldl (lambda (initializer index)
                                (%instance/set! object index (apply initializer initargs))
                                (+ index 1))
                              0
                              (%class-field-initializers class))))
                   method:initialize-instance))))

(add-method initialize-instance
  (make <method>
    :arity 2
    :specializers (list <class>)
    :procedure ((lambda ()
                   (define (method:initialize-instance call-next-method class initargs)
                     (call-next-method)
                     ;; No checking on this.
                     (%set-class-direct-default-initargs!
                      class
                      (getarg initargs :direct-default-initargs #f))
                     (%set-class-direct-supers!
                      class
                      (let ((default (*default-object-class*))
                            (supers (getarg initargs :direct-supers #f)))
                        ;; check valid supers, and always have an object class
                        (cond
                         ((not default) supers) ; check disabled
                         ((or (not supers) (null? supers)) (list default))
                         ((not (list? supers)) (error "INITIALIZE-INSTANCE bad superclasses " supers))
                         (else (let ((c (find-if
                                         (lambda (c)
                                           (not (and (%class? c) (subclass? c default))))
                                         supers)))
                                 (if c
                                     (error "INITIALIZE-INSTANCE:  cannot inherit from a ~a, ~e"
                                            (if (%class? c) "non-object class" "non-class") c)
                                     supers))))))
                     (%set-class-direct-slots!
                      class
                      (let ((autoinitargs (getarg initargs :autoinitargs #f)))
                        (map (lambda (s)
                               (if (pair? s)
                                   (if (or (not autoinitargs)
                                           (getarg (cdr s) :initarg #f)
                                           (not (symbol? (car s))))
                                       s
                                       (list* (car s) :initarg (string->symbol
                                                                 (string-append ":"
                                                                                (symbol->string (car s))))
                                              (cdr s)))
                                   (list s)))
                             (getarg initargs :direct-slots '()))))
                     (%set-class-cpl!   class (compute-cpl   class))
                     (%set-class-slots! class (compute-slots class))
                     (%set-class-default-initargs! class (compute-default-initargs class))
                     (%set-class-name!  class (getarg initargs :name '-anonymous-))
                     (%set-class-serial-number! class (get-serial-number))
                     (let* ((nfields 0)
                            (field-initializers '())
                            ;; allocator: give me an initializer function, get a slot number
                            (allocator (lambda (init)
                                         (let ((f nfields))
                                           (set! nfields (+ nfields 1))
                                           (set! field-initializers
                                                 (cons init field-initializers))
                                           f)))
                            (getters-n-setters (map (lambda (slot)
                                                      (cons (car slot)
                                                            (compute-getter-and-setter
                                                             class slot allocator)))
                                                    (%class-slots class))))
                       (%set-class-nfields! class nfields)
                       (%set-class-field-initializers! class (reverse! field-initializers))
                       (%set-class-getters-n-setters! class getters-n-setters))
                     (%set-class-initializers!
                      class (reverse
                             (append-map
                              (lambda (c)
                                (if (instance-of? c <class>) (%class-initializers c) '()))
                              (cdr (%class-cpl class)))))
                     (%set-class-valid-initargs! ; for sanity checks
                      class (append-map
                             (lambda (slot) (getargs (cdr slot) :initarg))
                             (%class-slots class))))
                   method:initialize-instance))))

(add-method initialize-instance
  (make <method>
    :arity 2
    :specializers (list <generic>)
    :procedure ((lambda ()
                   (define (method:initialize-instance call-next-method generic initargs)
                     (call-next-method)
                     (%set-generic-methods!     generic '())
                     ;; Should these be necessary?!
                     (%set-generic-arity!       generic (getarg initargs :arity #f))
                     (%set-generic-name!        generic (getarg initargs :name '-anonymous-generic-))
                     (%set-generic-combination! generic (getarg initargs :combination #f))
                     (%set-instance/procedure!  generic
                                                (lambda (generic args)
                                                  (error "APPLY:  generic function has no methods "
                                                         (%generic-name generic) generic))))
                   method:initialize-instance))))

(add-method initialize-instance
  (make <method>
    :arity 2
    :specializers (list <method>)
    :procedure ((lambda ()
                   (define (method:initialize-instance call-next-method method initargs)
                     (call-next-method)
                     (%set-instance/procedure!        method (compute-apply-method method)))
                   method:initialize-instance))))

(add-method allocate-instance
  (make <method>
    :arity 2
    :specializers (list <class>)
    :procedure ((lambda ()
                   (define (method:allocate-instance call-next-method class initargs)
                     (%make-instance class
                                     (make-vector (length (%class-field-initializers class))
                                                  (undefined))))
                   method:allocate-instance))))

(add-method allocate-instance
  (make <method>
    :arity 2
    :specializers (list <entity-class>)
    :procedure ((lambda ()
                   (define (method:allocate-instance call-next-method class initargs)
                     (%make-entity class
                                   uninitialized-entity-procedure
                                   (make-vector (length (%class-field-initializers class))
                                                (undefined))))
                   method:allocate-instance))))

;; Normally, can't allocate these.
(add-method allocate-instance
  (make <method>
    :arity 2
    :specializers (list <primitive-class>)
    :procedure ((lambda ()
                   (define (method:allocate-instance call-next-method class initargs)
                     (error "ALLOCATE-INSTANCE: can't instantiate a primitive class ~e "
                            class))
                   method:allocate-instance))))

(add-method compute-cpl
  (make <method>
    :arity 1
    :specializers (list <class>)
    :procedure ((lambda ()
                   (define (method:compute-cpl call-next-method class)
                     (compute-class-linearization class))
                   method:compute-cpl))))

(add-method compute-slots
  (make <method>
    :arity 1
    :specializers (list <class>)
    :procedure ((lambda ()
                   (define (method:compute-slots call-next-method class)
                     (let ((all-slots   (map %class-direct-slots (%class-cpl class))))

                       (let collect1 ((to-process (apply append all-slots))
                                      (final-slots '()))
                         (cond ((pair? to-process) (let* ((name   (caar to-process))
                                                          (others '())
                                                          (remaining-to-process
                                                           (filter (lambda (o)
                                                                     (if (eq? (car o) name)
                                                                         (begin (set! others (cons (cdr o) others)) #f)
                                                                         #t))
                                                                   to-process)))
                                                     (collect1 remaining-to-process
                                                               (cons (cons name (apply append (reverse! others)))
                                                                     final-slots))))
                               ((null? to-process)

                                ;; Sort the slots by order of appearance in cpl, makes them stay in the
                                ;; same index, allowing optimizations for single-inheritance
                                (let collect2 ((to-process (apply append (reverse! all-slots)))
                                               (result '()))
                                  (cond ((pair? to-process) (collect2 (cdr to-process)
                                                                      (if (assq (caar to-process) result)
                                                                          result
                                                                          (cons (assq (caar to-process) final-slots)
                                                                                result))))
                                        ((null? to-process) (reverse! result))
                                        (else (error "COMPUTE-SLOTS:  bad list")))))

                               (else (error "COMPUTE-SLOTS:  bad list"))))))

                   method:compute-slots))))

(add-method compute-default-initargs
  (make <method>
    :arity 1
    :specializers (list <class>)
    :procedure ((lambda ()
                   (define (method:compute-default-initargs call-next-method class)
                     (reverse!
                      ;; Remove the duplicates from the less specific classes
                      (foldl
                       (lambda (initarg initlist)
                         (if (memf (lambda (already-seen)
                                     (eq? (car initarg) (car already-seen)))
                                   initlist)
                             initlist
                             (cons initarg initlist)))
                       '()
                       ;; Collect them into a list (if they aren't #F)
                       (foldr (lambda (initargs all-initargs)
                                (if initargs
                                    (append initargs all-initargs)
                                    all-initargs))
                              '()
                              ;; Get all the initargs
                              (map %class-direct-default-initargs (%class-cpl class))))))
                   method:compute-default-initargs))))

(add-method compute-getter-and-setter
  (make <method>
    :arity 3
    :specializers (list <class>)
    :procedure ((lambda ()
                   (define (method:compute-getter-and-setter call-next-method class slot allocator)
                     (let ((initargs    (getargs (cdr slot) :initarg))
                           (initializer (getarg (cdr slot) :initializer #f))
                           (initvalue   (getarg (cdr slot) :initvalue (undefined)))
                           (type        (getarg (cdr slot) :type #f))
                           (allocation  (getarg (cdr slot) :allocation :instance))
                           (lock        (getarg (cdr slot) :lock #f))
                           (nothing     (cons #f #f)))

                       (cond ((and type
                                   (not (specializer? type)))
                              (error "MAKE:  bad type specifier " (car slot) type))

                             ((eq? allocation :instance)
                              (let* ((f (allocator
                                         (lambda args
                                           (let* ((result (getarg* args initargs nothing))
                                                  (result1 (if (eq? result nothing)
                                                               (if (not initializer)
                                                                   initvalue
                                                                   (apply initializer args))
                                                               result)))
                                             (if (and type
                                                      (not (eq? result1 (undefined)))
                                                      (not (instance-of? result1 type)))
                                                 (error "INITIALIZE-INSTANCE:  bad initial value " (car slot) class result type)
                                                 result1)))))

                                     ;; NOTE:
                                     ;;  These internal procedures are the `real' getter and setter
                                     ;;  functions of slots in an object.
                                     (g-s (list (lambda (o) (%instance/ref o f))
                                                (if (and type (not (eq? type <top>)))
                                                    (lambda (o n)
                                                      (if (instance-of? n type)
                                                          (%instance/set! o f n)
                                                          (error "SLOT-SET!:  wrong type " (car slot) (%class-name class))))
                                                    (lambda (o n) (%instance/set! o f n))))))
                                (if lock
                                    (make-setter-locked! g-s lock
                                                         (lambda ()
                                                           (error "SLOT-SET!:  locked slot "
                                                                  (car slot) (%class-name class)))))
                                g-s))

                             ((eq? allocation :class)
                              (if (not (null? initargs))
                                  (let ((setter #f))
                                    (%set-class-initializers!
                                     class
                                     (cons (lambda args
                                             (let ((result (getarg* args initargs nothing)))
                                               ;; cache the setter
                                               (if (not setter)
                                                   (set! setter
                                                         (caddr (assq (car slot)
                                                                      (%class-getters-n-setters
                                                                       class)))))
                                               (if (not (eq? result nothing))
                                                   (setter #f result))))
                                           (%class-initializers class)))))

                              (if (and (assq (car slot) (%class-direct-slots class))
                                       (getarg (cdr (assq (car slot)
                                                          (%class-direct-slots class)))
                                               :allocation #f))
                                  ;; the slot was declared as 'class here
                                  (let* ((cell (if (not initializer)
                                                   initvalue
                                                   (initializer))) ; default value - no arguments
                                         (g+s (list (lambda (o) cell)
                                                    (lambda (o n)
                                                      (if (and type (not (instance-of? n type)))
                                                          (error "SLOT-SET!:  wrong type for shared slot ")
                                                          (begin (set! cell n) n))))))
                                    (if lock
                                        (make-setter-locked! (car slot) g+s lock
                                                             (lambda ()
                                                               (error "SLOT-SET!:  locked shared slot "
                                                                      (car slot) (%class-name class) (car slot)))))
                                    g+s)
                                  ;; the slot was inherited as 'class - fetch its getters/setters
                                  (let loop ((cpl (cdr (%class-cpl class))))
                                    (let ((probe (assq (car slot) (%class-getters-n-setters (car cpl)))))
                                      (if probe
                                          (cdr probe)
                                          (loop (cdr cpl)))))))

                             (else (error "MAKE:  allocation must be 'class or 'instance " (car slot) allocation)))))

                   method:compute-getter-and-setter))))

;;; Use the previous function when populating this generic.
(add-method compute-apply-method
  (make <method>
    :arity 1
    :specializers (list <method>)
    :procedure method:compute-apply-method))

;;; BOOTSTRAP STEP  fixup the initalizers for <method> so subclassing methods works
(let* ((class <method>)
       (nfields 0)
       (field-initializers '())
       ;; allocator: give me an initializer function, get a slot number
       (allocator (lambda (init)
                    (let ((f nfields))
                      (set! nfields (+ nfields 1))
                      (set! field-initializers
                            (cons init field-initializers))
                      f)))
       (getters-n-setters (map (lambda (slot)
                                 (cons (car slot)
                                       (compute-getter-and-setter
                                        class slot allocator)))
                               (%class-slots class))))
  (%set-class-field-initializers! class (reverse! field-initializers))
  (%set-class-getters-n-setters! class getters-n-setters))

(add-method no-next-method
  (make <method>
    :arity (make-arity-at-least 2)
    :specializers (list <generic> <method>)
    :procedure (lambda (call-next-method generic method . args)
                  (error "APPLY:  no applicable next method to call " (%method-qualifier method)
                         (%generic-name generic) generic))))

(add-method no-next-method
  (make <method>
    :arity (make-arity-at-least 2)
    :specializers (list (singleton #f) <method>)
    :procedure (lambda (call-next-method generic method . args)
                  (error "APPLY:  no applicable next method when calling a method directly "
                         (%method-name method) method))))

(add-method no-applicable-method
  (make <method>
    :arity (make-arity-at-least 1)
    :specializers (list <generic>)
    :procedure (lambda (call-next-method generic . args)
                  (error "APPLY:  no applicable primary methods for argument types ~e "
                         (%generic-name generic) (map class-of args) generic))))

;;; ---------------------------------------------------------------------------
;;; Customization variables

;;>> *default-method-class*
;;>> *default-generic-class*
;;>> *default-class-class*
;;>> *default-entityclass-class*
;;>   These parameters specify default classes for the many constructor
;;>   macros in `clos'.
(define *default-method-class*      (make-parameter "*default-method-class*" <method> class?))
(define *default-generic-class*     (make-parameter "*default-generic-class*" <generic> class?))
(define *default-class-class*       (make-parameter "*default-class-class*" <class> class?))
(define *default-entityclass-class* (make-parameter "*default-entityclass-class*" <entity-class> class?))

;;>> *make-safely*
;;>   Setting this parameter to #t will make Ripoff perform sanity checks
;;>   on given initargs for creating an object.  This will make things
;;>   easier for debugging, but also slower.  Defaults to `#f'.  Note that
;;>   the sanity checks are done in `initialize-instance'.
(define *make-safely* (make-parameter "*make-safely*" #t boolean?))

(define (extend-initargs given-initargs class-initargs)
  (let ((extended
         (foldl (lambda (initarg initlist)
                  (let ((key (car initarg)))
                    (if (getarg given-initargs key #f)
                        initlist
                        (cons key (cons ((caddr initarg)) initlist)))))
                '() class-initargs)))
    (if (null? extended)
        given-initargs
        (append given-initargs extended))))

;;; BOOTSTRAP STEP
;;; Turn MAKE into a generic function.

(let ((m (make <method>
           :arity (make-arity-at-least 1)
           :specializers (list <class>)
           :procedure ((lambda ()
                          (define (method:make call-next-method class . given-initargs)
                            (let* ((initargs (extend-initargs given-initargs (class-default-initargs class)))
                                   (instance (allocate-instance class initargs)))
                              (if (*make-safely*) (check-initargs class initargs))
                              (initialize-instance instance initargs)
                              instance))
                          method:make))))
      (g (make <generic> :name 'make :arity (make-arity-at-least 1))))
  (add-method g m)
  (set! make g))

;; The clean concept behind this is due to Joe Marshall.

;;>> (rec-make (name class arg ...) ...)
;;>   This is similar to:
;;>
;;>     (letrec ((name (make class arg ...)) ...)
;;>       (values name ...))
;;>
;;>   except that the names are first bound to allocated instances with no
;;>   initargs, and then they are initialized with all these bindings.  This
;;>   is useful for situations where creating some instances needs other
;;>   instances as values.  One sample usage is the way `defclass' makes the
;;>   class binding available for slot specifications like `'type'.  Note
;;>   that this is a special form, which invokes `allocate-instance' and
;;>   `initialize-instance' directly, so specializing `make' on some input will not
;;>   change the way `rec-make' works.

(define (rec-allocate-instance class given-initargs)
  (allocate-instance class (extend-initargs given-initargs (class-default-initargs class))))

(define (rec-initialize instance given-initargs)
  (initialize-instance instance (extend-initargs given-initargs (class-default-initargs (class-of instance)))))

;;; ---------------------------------------------------------------------------
;;; BOOTSTRAP STEP
;;; Turn `add-method' into a generic function

(let ((old-add-method add-method))
  (set! add-method (make <generic> :name 'add-method :arity 2))
  (old-add-method
   add-method
   (make <method>
     :arity 2
     :specializers (list <generic> <method>)
     :procedure
     ((lambda ()

        (define (compute-method-name specs generic-name)
          (define (spec-string spec)
            (cond ((%singleton? spec) (let ((string-output-port (string-io/open-output-string)))
                                        (write (singleton-value spec) string-output-port)
                                        (string-io/get-output-string string-output-port)))
                  ((%class? spec)     (symbol->string
                                       (%class-name (%struct->class spec))))
                  (else               "???")))

          (string->symbol
           (apply string-append
                  (symbol->string generic-name) ":"
                  (if (null? specs)
                      '("()")
                      (cons (spec-string (car specs))
                            (map (lambda (c) (string-append "," (spec-string c)))
                                 (cdr specs)))))))

        (define (method:add-method call-next-method generic method)
          ;; Ensure that the arity is compatible.
          (let ((method-arity  (method-arity method))
                (generic-arity (%generic-arity generic)))
            (cond ((not generic-arity)
                   (%set-generic-arity! generic method-arity))
                  ;; note: equal? works on arity-at-least structs
                  ((equal? generic-arity method-arity) #t)
                  ;; Ok if generic guarantees at least as much as method wants.
                  ((and (arity-at-least? method-arity)
                        (>= (if (arity-at-least? generic-arity)
                                (arity-at-least-value generic-arity)
                                generic-arity)
                            (arity-at-least-value method-arity))) #t)
                  (else
                   (error "ADD-METHOD: wrong arity for `~e', expects ~a; given a method with ~a "
                          (%generic-name generic)
                          (if (and (integer? generic-arity) (exact? generic-arity))
                              generic-arity
                              (format "at-least-~a"
                                      (arity-at-least-value generic-arity)))
                          (if (and (integer? method-arity) (exact? method-arity))
                              method-arity
                              (format "at-least-~a"
                                      (arity-at-least-value method-arity)))))))

          ;; set a name for the method if none (when attached to a generic)
          (let ((n (%method-name method)))
            (if (not (and n (not (eq? n '-anonymous-method-))))
                (%set-method-name!
                 method
                 (let* ((psym (procedure-name (%method-procedure method)))
                        (pstr (and psym (symbol->string psym))))
                   (if (or (not pstr)
                           ;; (regexp-match #rx":[0-9]*:[0-9]*$" pstr)
                           )
                       (compute-method-name (%method-specializers method)
                                            (%generic-name generic))
                       psym)))))

          ;; set the arity if none (when attached to generic)
          (let ((arity (%method-arity method)))
            (if (not arity)
                (%set-method-arity! method (%generic-arity generic))))

          ;; Add the method
          (old-add-method generic method))

        method:add-method)))))

;;; BOOTSTRAP STEP
;;; Turn slot-unbound and slot-missing into generic-functions
(set! slot-missing (make <generic> :name 'slot-missing :arity (make-arity-at-least 4)))
(add-method slot-missing
  (make <method>
    :arity (make-arity-at-least 4)
    :specializers (list <top>)
    :procedure (lambda (call-next-method class instance slot-name operation . new-value)
                  (error (string-append "slot-missing: '"(symbol->string slot-name)
                                        "' is not a slot in ") class))))

(set! slot-unbound (make <generic> :name 'slot-unbound :arity 3))
(add-method slot-unbound
  (make <method>
    :arity 3
    :specializers (list <top>)
    :procedure (lambda (call-next-method class instance slot-name)
                  (error (string-append "slot-unbound: '"(symbol->string slot-name)
                                        "' is not bound in ") instance))))

;;; BOOTSTRAP STEP
;;; Optimized frequently used accessors:
;;; This is possible because of the ordering of the slots in compute-slots,
;;; works only for single-inheritance.  Note that there is no type checking -
;;; it is unsafe (hence the percent sign), but makes things around 5-6 times faster!

;;; These are for optimizations.
(let ((%slot-getter (lambda (class slot-name) (lookup-slot-info class slot-name cadr)))
      (%slot-setter (lambda (class slot-name) (lookup-slot-info class slot-name caddr))))

  (set! %class-cpl                 (%slot-getter <class>   'cpl))
  (set! %class-default-initargs    (%slot-getter <class>   'default-initargs))
  (set! %class-direct-default-initargs (%slot-getter <class> 'direct-default-initargs))
  (set! %class-direct-slots        (%slot-getter <class>   'direct-slots))
  (set! %class-direct-supers       (%slot-getter <class>   'direct-supers))
  (set! %class-field-initializers  (%slot-getter <class>   'field-initializers))
  (set! %class-getters-n-setters   (%slot-getter <class>   'getters-n-setters))
  (set! %class-initializers        (%slot-getter <class>   'initializers))
  (set! %class-name                (%slot-getter <class>   'name))
  (set! %class-nfields             (%slot-getter <class>   'nfields))
  (set! %class-serial-number       (%slot-getter <class>   'serial-number))
  (set! %class-slots               (%slot-getter <class>   'slots))
  (set! %class-valid-initargs      (%slot-getter <class>   'valid-initargs))

  (set! %set-class-cpl!            (%slot-setter <class>   'cpl))
  (set! %set-class-default-initargs! (%slot-setter <class> 'default-initargs))
  (set! %set-class-direct-default-initargs! (%slot-setter <class> 'direct-default-initargs))
  (set! %set-class-direct-slots!   (%slot-setter <class>   'direct-slots))
  (set! %set-class-direct-supers!  (%slot-setter <class>   'direct-supers))
  (set! %set-class-field-initializers!(%slot-setter <class> 'field-initializers))
  (set! %set-class-getters-n-setters! (%slot-setter <class> 'getters-n-setters))
  (set! %set-class-initializers!   (%slot-setter <class>   'initializers))
  (set! %set-class-name!           (%slot-setter <class>   'name))
  (set! %set-class-nfields!        (%slot-setter <class>   'nfields))
  (set! %set-class-serial-number!  (%slot-setter <class>   'serial-number))
  (set! %set-class-slots!          (%slot-setter <class>   'slots))
  (set! %set-class-valid-initargs! (%slot-setter <class>   'valid-initargs)))

(let ((%slot-getter (lambda (class slot-name)
                      (let ((fast (lookup-slot-info class slot-name cadr)))
                        (lambda (instance)
                          (if (eq? (%instance/class instance) class)
                              (fast instance)
                              ((lookup-slot-info (class-of instance) slot-name cadr) instance))))))

      (%slot-setter (lambda (class slot-name)
                      (let ((fast (lookup-slot-info class slot-name caddr)))
                        (lambda (instance new-value)
                          (if (eq? (%instance/class instance) class)
                              (fast instance new-value)
                              ((lookup-slot-info (class-of instance) slot-name caddr) instance new-value)))))))

  (set! %generic-arity             (%slot-getter <generic> 'arity))
  (set! %generic-combination       (%slot-getter <generic> 'combination))
  (set! %generic-methods           (%slot-getter <generic> 'methods))
  (set! %generic-name              (%slot-getter <generic> 'name))

  (set! %method-arity              (%slot-getter <method>  'arity))
  (set! %method-name               (%slot-getter <method>  'name))
  (set! %method-procedure          (%slot-getter <method>  'procedure))
  (set! %method-qualifier          (%slot-getter <method>  'qualifier))
  (set! %method-specializers       (%slot-getter <method>  'specializers))

  (set! %set-generic-arity!        (%slot-setter <generic> 'arity))
  (set! %set-generic-combination!  (%slot-setter <generic> 'combination))
  (set! %set-generic-methods!      (%slot-setter <generic> 'methods))
  (set! %set-generic-name!         (%slot-setter <generic> 'name))

  (set! %set-method-arity!         (%slot-setter <method>  'arity))
  (set! %set-method-name!          (%slot-setter <method>  'name))
  (set! %set-method-procedure!     (%slot-setter <method>  'procedure))
  (set! %set-method-qualifier!     (%slot-setter <method>  'qualifier))
  (set! %set-method-specializers!  (%slot-setter <method>  'specializers))

  ;; Optimize these internal ones as well.
  (set! %generic-app-cache            (%slot-getter <generic> 'app-cache))
  (set! %generic-singletons-list      (%slot-getter <generic> 'singletons-list))
  (set! %set-generic-app-cache!       (%slot-setter <generic> 'app-cache))
  (set! %set-generic-singletons-list! (%slot-setter <generic> 'singletons-list)))

;;; Create the class hierarchy for builtin objects.

;;>   These classes represent built-in objects.  See the class hierarchy
;;>   below for a complete description of the relations between these
;;>   classes.
(define-syntax defprimclass
  (syntax-rules ()
    ((defprimclass primclass) (defprimclass primclass <builtin>))
    ((defprimclass primclass supers ...)
     (define primclass
       (parameterize ((*default-object-class* #f))
         ;; Quote these keywords.
         (make <primitive-class>
           ':name          'primclass
           ':direct-supers (list supers ...)))))))

;;; Concrete base types
(defprimclass <boolean>)
(defprimclass <bytevector-like>)
(defprimclass <char>)
(defprimclass <end-of-file>)
(defprimclass <vector-like>)
(defprimclass <void>)

;;; Abstract base types
(defprimclass <number>)
(defprimclass <sequence>)
(defprimclass <unknown-primitive>)

;; Larceny has a vector-like object that claims to be a `structure'.
;; It may be untagged, so you can't get much info about it.
(defprimclass <primitive-structure> <vector-like>)

(defprimclass <namespace> <primitive-structure>)
(defprimclass <record> <primitive-structure>)

;;;>> <struct>
;;;>> <opaque-struct>
;;;>   These are also classes for built-in objects, but they are classes for
;;;>   MzScheme structs -- which can be used like Ripoff classes since they
;;;>   will get converted to appropriate Ripoff subclasses of `<struct>'.
;(define <struct>
;  (make <primitive-class>
;    ':direct-supers (list <primitive-structure>)
;    ':name '<struct>))

;;;>   `<opaque-struct>' is a class of structs that are hidden -- see the
;;;>   documentation for `struct-info' and the `skipped?' result.  Note that
;;;>   structs can be used as long as they can be inspected -- otherwise, we
;;;>   can't even know that they are structs with `struct?' (this means that
;;;>   <opaque-struct> can only appear in the cpl of a struct class that
;;;>   inherits from a struct which is not under the current inspector).
;(define <opaque-struct>
;  (make <primitive-class>
;    ':direct-supers (list <struct>)
;    ':name '<opaque-struct>))


;(defprimclass <immutable>)

;(defprimclass <box>)
;(defprimclass <weak-box> <box>)

;(defprimclass <compiled-expression>)
;(defprimclass <custodian>)
;(defprimclass <exn>)
;(defprimclass <break-exn> <exn>)
;(defprimclass <non-break-exn> <exn>)

;(defprimclass <foreign-object>)
;(defprimclass <foreign-array>   <foreign-object>)
;(defprimclass <foreign-boolean> <foreign-object>)
;(defprimclass <foreign-enum>    <foreign-object>)
;(defprimclass <foreign-int32>   <foreign-object>)
;(defprimclass <foreign-null>    <foreign-object>)
;(defprimclass <foreign-string>  <foreign-object>)

;(defprimclass <inspector>)

(defprimclass <exact> <number>)
(defprimclass <inexact> <number>)
(defprimclass <complex> <number>)
(defprimclass <real> <complex>)
(defprimclass <rational> <real>)
(defprimclass <integer> <rational>)
(defprimclass <exact-complex> <complex> <exact>)
(defprimclass <inexact-complex> <complex> <inexact>)
(defprimclass <exact-real> <real> <exact-complex>)
(defprimclass <inexact-real> <real> <inexact-complex>)
(defprimclass <exact-rational> <rational> <exact-real>)
(defprimclass <inexact-rational> <rational> <inexact-real>)
(defprimclass <exact-integer> <integer> <exact-rational>)
(defprimclass <inexact-integer> <integer> <inexact-rational>)

(defprimclass <bytevector> <bytevector-like>)

(defprimclass <bignum> <exact-integer> <bytevector-like>)
(defprimclass <fixnum> <exact-integer>)
(defprimclass <flonum> <inexact-real> <bytevector-like>)
(defprimclass <ratnum> <exact-rational> <vector-like>)
(defprimclass <rectnum> <exact-complex> <vector-like>)

;(defprimclass <parameter>)

(defprimclass <port> <vector-like>)
(defprimclass <input-port> <port>)
(defprimclass <output-port> <port>)
;(defprimclass <stream-port> <port>)
;(defprimclass <input-stream-port> <input-port> <stream-port>)
;(defprimclass <output-stream-port> <output-port> <stream-port>)

;(defprimclass <promise>)
;(defprimclass <pseudo-random-generator>)
;(defprimclass <regexp>)
;(defprimclass <security-guard>)
;(defprimclass <semaphore>)

;; It is expensive to run down a list every time you want to
;; pass one to a generic function, so we don't distinguish
;; proper from improper lists.
;(defprimclass <improper-list> <sequence>)
(defprimclass <pair> <sequence>)
;(defprimclass <immutable-pair> <pair> <immutable>)
(defprimclass <list> <sequence>)
(defprimclass <nonempty-list> <pair> <list>)
;(defprimclass <immutable-nonempty-list> <nonempty-list> <immutable>)
(defprimclass <null> <list>)

(defprimclass <string> <sequence> <bytevector-like>)
;(defprimclass <immutable-string> <string> <immutable>)
(defprimclass <vector> <sequence> <vector-like>)

(defprimclass <code-object> <vector>)
(defprimclass <assignment> <code-object>)
(defprimclass <begin> <code-object>)
(defprimclass <call> <code-object>)
(defprimclass <conditional> <code-object>)
(defprimclass <constant> <code-object>)
(defprimclass <definition> <code-object>)
(defprimclass <lambda> <code-object>)
(defprimclass <variable> <code-object>)

(defprimclass <hash-table> <vector>)

(defprimclass <struct-type>)
;(defprimclass <subprocess>)
;(defprimclass <syntax>)
;(defprimclass <identifier-syntax> <syntax>)
(defprimclass <symbol> <vector-like>)
;(defprimclass <tcp-listener>)
;(defprimclass <thread>)

;(defprimclass <will-executor>)

(define <procedure>
  (parameterize ((*default-object-class* #f))
    (make <procedure-class>
      :name          '<procedure>
      :direct-supers (list <builtin> <function>))))

(define <interpreted-expression>
  (parameterize ((*default-object-class* #f))
    (make <procedure-class>
      :name          '<interpreted-expression>
      :direct-supers (list <procedure>))))

(define <interpreted-primitive>
  (parameterize ((*default-object-class* #f))
    (make <procedure-class>
      :name          '<interpreted-primitive>
      :direct-supers (list <procedure>))))

(define <interpreted-procedure>
  (parameterize ((*default-object-class* #f))
    (make <procedure-class>
      :name          '<interpreted-procedure>
      :direct-supers (list <procedure>))))

;;; BOOTSTRAP STEP
;;; replace class-of with something more intelligent
(let ((hashtable-tag-object (vector-ref (make-hashtable) 0)))

  (set! class-of
        (lambda (object)
          (cond ((procedure? object) (cond ((instance? object) (%instance/class object))
                                           ((interpreted-expression? object) <interpreted-expression>)
                                           ((interpreted-primitive? object) <interpreted-primitive>)
                                           ((interpreted-procedure? object) <interpreted-procedure>)
                                           (else <procedure>)))

                ((vector-like? object)
                 (cond ((ratnum? object) <ratnum>)
                       ((rectnum? object) <rectnum>)
                       ((symbol? object) <symbol>)
                       ((port? object) (cond ((input-port? object) <input-port>)
                                             ((output-port? object) <output-port>)
                                             (else <port>)))
                       ((structure? object) (cond ((environment? object) <namespace>)
                                        ;((struct? object) (struct-type->class object))
                                                  ((record? object)
                                                   (record-type->class (record-type-descriptor object)))
                                                  (else <primitive-structure>)))
                       ((vector? object)
                        (cond ((= (vector-length object) 0) <vector>)
                              ((eq? (vector-ref object 0) hashtable-tag-object) <hash-table>)
                              ((code-object? object)
                               (cond ((assignment? object) <assignment>)
                                     ((begin? object) <begin>)
                                     ((call? object) <call>)
                                     ((conditional? object) <conditional>)
                                     ((constant? object) <constant>)
                                     ((lambda? object) <lambda>)
                                     ((variable? object) <variable>)
                                     (else <code-object>)))
                              (else <vector>)))
                       (else <vector-like>)))

                ((bytevector-like? object)
                 (cond ((bignum? object) <bignum>)
                       ((bytevector? object) <bytevector>)
                       ((flonum? object) <flonum>)
                       ((string? object) <string>)
                       (else <bytevector-like>)))

                ((boolean? object) <boolean>)
                ((char? object)   <char>)
                ((eof-object? object) <end-of-file>)
                ((fixnum? object) <fixnum>)
                ((null? object) <null>)
                ((pair? object) <nonempty-list>)
                ((eq? object (void)) <void>)
                (else <unknown-primitive>)))))

;;; Miscellany

(define (make-class name . args)
  (apply make (getarg args :metaclass (*default-class-class*))
         :name name
         args))

(define (make-method . args)
  (apply make (*default-method-class*) args))

;;; A object that simply behaves like an aggregate will want
;;; to use these functions.

(define (generic-getter name)
  (make (*default-generic-class*) :name name :arity 1))

(define (generic-setter name)
  (make (*default-generic-class*) :name name :arity 2))

(define (getter-method class slot)
  (define (method:slot-reader call-next-method instance)
    (slot-ref instance slot))
  (make (*default-method-class*)
    :arity 1
    :specializers (list class)
    :procedure method:slot-reader))

(define (setter-method class slot)
  (define (method:slot-writer call-next-method instance new-value)
    (slot-set! instance slot new-value))
  (make (*default-method-class*)
    :arity 2
    :specializers (list class)
    :procedure method:slot-writer))

(define (initialize-generic-accessors class specs)
  (for-each
   (lambda (spec)
     (let ((slot-name (car spec))
           (getter (cadr spec))
           (setter (if (pair? (cddr spec))
                       (caddr spec)
                       #f)))

       (add-method getter (getter-method class slot-name))
       (if setter
           (add-method setter (setter-method class slot-name)))))
   specs))

;;; Print methods for objects, classes and instances.
;;; Moved to gprint.sch
