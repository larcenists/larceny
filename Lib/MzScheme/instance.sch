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
;;;   instance-procedure  - holds a function that is invoked if the instance is called
;;;   instance-slots      - holds a vector of state

;;; There is some MAL code for instantiating an instance as a special
;;; procedure object.

;;; FIXME
(define sys$tag.instance-typetag 0) ;; JRM asks:  should this be something else?

;;; Note, in an abuse of the macro system, these constants are `wired' in.
;;; Don't change them without changing class and generic.
(define instance/procedure-offset 3)
(define instance/class-offset     4)
(define instance/slots-offset     5)

(define (%instance/allocate class nslots)
  (let ((i (%instance
            (lambda args
              (error "APPLY: an instance isn't a procedure -- can't apply it"))
            class
            (make-vector nslots (undefined)))))
    (typetag-set! i sys$tag.instance-typetag)
    i))

(define (%entity/allocate class nslots)
  (let ((i (%instance
            (lambda args
              (error "APPLY:  uninitialized entity"))
            class
            (make-vector nslots (undefined)))))
    (typetag-set! i sys$tag.instance-typetag)
    i))

(define (instance? object)
  (and (procedure? object)
       (= (typetag object) sys$tag.instance-typetag)
       (eq? (procedure-name object) '%instance)
       (= (procedure-length object) 6)))

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

(define-syntax %instance/ref
  (syntax-rules ()
    ((%instance/ref instance offset)
     (vector-ref (%instance/slots instance) offset))))

(define-syntax %instance/set!
  (syntax-rules ()
    ((%instance/set! instance offset value)
     (vector-set! (%instance/slots instance) offset value))))

(define (instance/class instance)      (%instance/class instance))
(define (instance/procedure instance)  (%instance/procedure instance))
(define (instance/ref instance offset) (%instance/ref instance offset))

(define (instance/set! instance offset new-value)
  (%instance/set! instance offset new-value))

;; This is used to dynamically change the class of an instance.
(define (instance/replace! old-instance new-instance)
  (%set-instance/class!      old-instance (%instance/class new-instance))
  (%set-instance/procedure!  old-instance (%instance/procedure new-instance))
  (%set-instance/slots!      old-instance (%instance/slots new-instance)))

;; This is used to make the class system meta-circular.
(define (set-instance-class-to-self! instance)
  (%set-instance/class! instance instance))
