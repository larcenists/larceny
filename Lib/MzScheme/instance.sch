;;; -*-Mode: Scheme; coding: iso-8859-1 -*-
;;;
;;; Part of the RIPOFF object system.
;;; Originally part of Tiny-CLOS,
;;; Heavily hacked by Eli Barzilay: Maze is Life!  (eli@barzilay.org)
;;; as part of Swindle, then hacked and ported Common Larceny by jrm.

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

($$trace "instance")

;;; This is the basic interface to the memory.

;;; An instance is simply a structure with 3 fields

;;;   instance-class      - holds the class describing this instance
;;;   instance-procedure  - holds a procedure that is invoked if the
;;;                         instance is called
;;;   instance-slots      - holds a vector of state

;;; The instance-procedure should be a procedure of two arguments.  If
;;; the instance is invoked as a procedure, the instance-procedure
;;; will be called with the instance itself as the first argument and
;;; the supplied argument list as the second.

;;; There is some MAL code for instantiating an instance as a special
;;; procedure object.

;;; Other parts of the system expect specific structures to be present
;;; in the class field, so all user interaction should be mediated by
;;; the class system.

;;; Standard API
;;;
;;; (%make-instance <class> <vector>)                                  Procedure
;;;
;;; Create an instance.  <class> must be a class object recognized by the object
;;; system.  <vector> must be a vector of the appropriate size.  These objects
;;; are not copied, so the caller should ensure they are not exposed.  The
;;; instance-procedure will be initialized to a procedure that generates an
;;; error.
;;;
;;;
;;; (instance? <object>)                                     Predicate Procedure
;;; <object> may be any Scheme object.  Returns #t iff <object> is an instance.
;;;
;;;
;;; (instance/class <instance>)                                        Procedure
;;;
;;; <instance> must be an instance.  Returns the class object stored in the
;;; instance.  This must not be mutated.
;;;
;;;
;;; (instance/procedure <instance>)                                    Procedure
;;;
;;; <instance> must be an instance.  Returns the procedure that is invoked when
;;; instance is called as a function.
;;;
;;;
;;; (set-instance/procedure! <instance> <proc>)                        Procedure
;;;
;;; <instance> must be an instance.  <proc> must be a procedure of two
;;; arguments.  Replaces the procedure object within instance to be <proc>.
;;; When instance is invoked as a function, <proc> will be called on two values:
;;; the instance and the list of arguments supplied.  Value returned from <proc>
;;; is used as the result of invoking the instance.
;;;
;;;
;;; (instance/ref <instance> <index>)                                  Procedure
;;;
;;; <instance> must be an instance, <index> must be an exact positive integer
;;; smaller than the length of the <instance>'s state vector.  Returns the value
;;; of the <index>th field of <instance>.
;;;
;;;
;;; (instance/set! <instance> <index> <new-value>)                     Procedure
;;;
;;; <instance> must be an instance, <index> must be an exact positive integer
;;; smaller than the length of the <instance>'s state vector.  Mutates the
;;; <index>th field of <instance> to contain <new-value>.
;;;
;;; (instance/update! <instance> <index> <procedure>)                  Procedure
;;;
;;; <instance> must be an instance, <index> must be an exact positive integer
;;; smaller than the length of the <instance>'s state vector.  Reads the <index>th
;;; field of <instance>, applies <procedure> to that value, and stores the
;;; value computed by <procedure> back into the field.

;;; Performance API
;;;
;;; The following macros are provided for performance-critical
;;; applications.  These macros perform no checks and should not be
;;; used where there is the possibility that an incorrect value could
;;; be passed in.
;;;
;;; (%instance/class <instance>)                                          Syntax
;;; (%instance/procedure <instance>)                                      Syntax
;;; (%set-instance/procedure! <instance> <proc>)                          Syntax
;;;
;;; (%instance/ref <instance> <index>)                                    Syntax
;;;
;;; (%instance/set! <instance> <index> <new-value>)                       Syntax
;;;
;;; (%instance/update! <instance> <index> <procedure>)                    Syntax

;;; Special API
;;;
;;; These functions and syntax are for specific unusual circumstances and should
;;; not be used by `normal' code.
;;;
;;; (%set-instance/class! <instance> <new-class>)                         Syntax
;;; Replace the <class> associate with instance with <new-class>.  This should
;;; only be done via the object system as a part of schema upgrade.
;;;
;;; (%instance/slots <instance>)                                          Syntax
;;; (%set-instance/slots <instance> <new-value>)                          Syntax
;;; Manipulate the state vector associated with instance.  Should only be done
;;; by the object system.
;;;
;;; (instance/replace! <old-instance> <new-instance>)                  Procedure
;;; Replace the class, procedure, and slots of <old-instance> with the values
;;; present in <new-instance>.  This changes the entire behavior of the instance
;;; without changing the identity of the instance.  This is used by the object
;;; system to allow you to incrementally `redefine' classes and methods.

;;; FIXME
(define sys$tag.instance-typetag 0) ;; JRM asks:  should this be something else?

;;; Note, in an abuse of the macro system, these constants are `wired' in.
;;; Don't change them without changing class and generic.
(define instance/procedure-offset 3)
(define instance/class-offset     4)
(define instance/slots-offset     5)

;;; Shamelessly abuse the macro system to get performance.
(define-syntax %instance/procedure
  (syntax-rules ()
    ((%instance/procedure instance) (procedure-ref instance 3))))

(define-syntax %set-instance/procedure!
  (syntax-rules ()
    ((%set-instance/procedure! instance proc) (procedure-set! instance 3 proc))))

(define-syntax %instance/class
  (syntax-rules ()
    ((%instance/class instance) (procedure-ref instance 4))))

(define-syntax %set-instance/class!
  (syntax-rules ()
    ((%set-instance/class! instance class) (procedure-set! instance 4 class))))

(define-syntax %instance/slots
  (syntax-rules ()
    ((%instance/slots instance) (procedure-ref instance 5))))

(define-syntax %set-instance/slots!
  (syntax-rules ()
    ((%set-instance/slots! instance slots) (procedure-set! instance 5 slots))))

(define (instance? object)
  (and (procedure? object)
       (= (typetag object) sys$tag.instance-typetag)
       ;; MAL code will ensure that the procedure-name is `%instance'
       (eq? (procedure-name object) '%instance)
       (= (procedure-length object) 6)))

;;; End of code that knows about how instances are represented.

(define (print-instance-to-string instance)
  (let ((string-output-port (string-io/open-output-string)))
    (write instance string-output-port)
    (string-io/get-output-string string-output-port)))

;;; This procedure is placed in all instance objects by default.
;;; Error message is worded to be similar to what one would get for
;;; any other non-applicable object.
(define (default-instance-procedure instance arglist)
  (error
   (string-append "Attempt to apply "
                  (print-instance-to-string instance)
                  ", which is not a procedure.")))

(define (%make-instance class slot-vector)
  (let ((instance
         (%instance default-instance-procedure
                    class
                    slot-vector)))
    (typetag-set! instance sys$tag.instance-typetag)
    instance))

(define (%make-instance* class . initial-slot-values)
  (%make-instance class (list->vector initial-slot-values)))

(define (uninitialized-entity-procedure entity arglist)
  (error
   (string-append "Attempt to apply "
                  (print-instance-to-string entity)
                  ", which has not been initialized.")))

(define (%make-entity class entity-procedure slot-vector)
  ;; entity-procedure will be invoked if the entity is applied to an
  ;; object.
  (let ((instance (%instance
                   entity-procedure
                   class
                   slot-vector)))
    (typetag-set! instance sys$tag.instance-typetag)
    instance))

(define (%make-entity* class entity-procedure . initial-slot-values)
  (%make-entity class entity-procedure (list->vector initial-slot-values)))

(define-syntax %instance/ref
  (syntax-rules ()
    ((%instance/ref instance offset)
     (vector-ref (%instance/slots instance) offset))))

(define-syntax %instance/set!
  (syntax-rules ()
    ((%instance/set! instance offset value)
     (vector-set! (%instance/slots instance) offset value))))

(define-syntax %instance/update!
  (syntax-rules ()
    ((%instance/set! instance offset proc)
     (let ((slots (%instance/slots instance))
           (index offset))
       (vector-set! slots index (proc (vector-ref slots index)))))))

(define (instance/class instance)      (%instance/class instance))
(define (instance/procedure instance)  (%instance/procedure instance))
(define (instance/ref instance offset) (%instance/ref instance offset))

(define (instance/set! instance offset new-value)
  (%instance/set! instance offset new-value))

(define (instance/update! instance offset proc)
  (%instance/update! instance offset proc))

;; This is used to dynamically change the class of an instance.
(define (instance/replace! old-instance new-instance)
  (call-without-interrupts
   (lambda ()
     (%set-instance/class!      old-instance (%instance/class new-instance))
     (%set-instance/procedure!  old-instance (%instance/procedure new-instance))
     (%set-instance/slots!      old-instance (%instance/slots new-instance)))))

;; This is used to make the class system meta-circular.
(define (set-instance-class-to-self! instance)
  (%set-instance/class! instance instance))
