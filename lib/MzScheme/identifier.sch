;;; -*-Mode: Scheme; coding: iso-8859-1 -*-

;;;===============================================================================
;;;
;;; Simple Hygienic Macros and Simple Modules:
;;;
;;;   Copyright (c) 2005 André van Tonder
;;;
;;;   Permission is hereby granted, free of charge, to any person obtaining a
;;;   copy of this software and associated documentation files (the ``Software''),
;;;   to deal in the Software without restriction, including without limitation
;;;   the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;;   and/or sell copies of the Software, and to permit persons to whom the
;;;   Software is furnished to do so, subject to the following conditions:
;;;
;;;   The above copyright notice and this permission notice shall be included in
;;;   all copies or substantial portions of the Software.
;;;
;;;   THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;;   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;   DEALINGS IN THE SOFTWARE.
;;;
;;;===============================================================================

($$trace "identifier")

;;; Identifiers for use in macro expansion.


;;; This function must generate a unique name that is unique even
;;; across separate compilation.  GUIDs are also for this purpose, so
;;; we just get one.

(define (generate-globally-unique-symbol prefix)
  (string->symbol
   (string-append prefix
                  ;; Get a GUID from .NET
                  (clr/new-guid))))

;=======================================================================
;;; Colors:

;; To support separate compilation, generated colors should be
;; globally unique.

(define (generate-color)
  (generate-globally-unique-symbol "#"))

(define *no-color*     (string->symbol ""))
(define *source-color* (string->symbol "#top"))

;; Appending colors c1 and c2 must give a color disjoint from
;; the range of GENERATE-COLOR unless either is NO-COLOR.

(define (append-colors c1 c2)
  (string->symbol
   (string-append (symbol->string c1)
                  (symbol->string c2))))

;; Generates the painted names used for free toplevel or
;; module identifiers, or secret names in generated DEFINEs.
;; Painted names should be disjoint from all source symbols
;; and all previous and future gensyms.

(define (paint-name symbolic-name color)
  (string->symbol
   (string-append (symbol->string symbolic-name)
                  (symbol->string color))))


;=========================================================================

;;; The identifier type:
(define bind-procedure              (generic-getter 'bind-procedure))
(define binding-name-procedure      (generic-getter 'binding-name-procedure))
(define close-environment-procedure (generic-getter 'close-environment-procedure))
(define color                       (generic-getter 'color))
(define renamer                     (generic-getter 'renamer))
(define symbolic-name               (generic-getter 'symbolic-name))
(define unbind-procedure            (generic-getter 'unbind-procedure))

(define <identifier>
  (make-class '<identifier>
    :direct-slots
    `((bind-procedure              :initarg :bind-procedure
                                   :reader ,bind-procedure)
      (binding-name-procedure      :initarg :binding-name-procedure
                                   :reader ,binding-name-procedure)
      (close-environment-procedure :initarg :close-environment-procedure
                                   :reader ,close-environment-procedure)
      (color                       :initarg :color
                                   :reader ,color)
      (renamer                     :initarg :renamer
                                   :reader ,renamer)
      (symbolic-name               :initarg :symbolic-name
                                   :reader ,symbolic-name)
      (unbind-procedure            :initarg :unbind-procedure
                                   :reader ,unbind-procedure))))

(extend-generic print-object
  :specializers (list <identifier>)
  :procedure (named-object-printer-method symbolic-name))

(define identifier? (class-predicate <identifier>))

(define (bind!             identifier binding-name)
  ((bind-procedure identifier) binding-name))

(define (binding-name      identifier) ((binding-name-procedure identifier)))
(define (close-environment namespace identifier) ((close-environment-procedure identifier) namespace))
(define (unbind!           identifier) ((unbind-procedure identifier)))

;(define identifier-tag (list 'identifier))

;(define real-vector?
;  ;; In case this code is rerun in same environment:
;  (eval 'vector? (scheme-report-environment 5)))
;(define (vector? x) (and (real-vector? x)
;                         (not (identifier? x))))
;(define (identifier? x)
;  (and (real-vector? x)
;       (> (vector-length x) 0)
;       (eq? (vector-ref x 0) identifier-tag)))

;(define (make-identifier symbolic-name
;                         binding-name
;                         color
;                         bind!
;                         unbind!
;                         rename
;                         close-environment)
;  (vector identifier-tag
;          symbolic-name
;          binding-name
;          color
;          bind!
;          unbind!
;          rename
;          close-environment))

;(define (symbolic-name     id)              (vector-ref id 1))
;(define (binding-name      id)              ((vector-ref id 2)))
;(define (color            id)              (vector-ref id 3))
;(define (bind!             id binding-name) ((vector-ref id 4) binding-name))
;(define (unbind!           id)              ((vector-ref id 5)))
;(define (renamer           id)              (vector-ref id 6))
;(define (close-environment id)              ((vector-ref id 7)))

;==========================================================================

;;; Infrastructure for hygiene:

;; Creates a color or substitution context, in which bound-identifier=?
;; identifiers share a location, so that substitutions can be done by an
;; imperative update of an identifier (see bind! below) and we do not have
;; to do any additional work to propagate substitution environments to leaves.
;; The resulting hygiene algorithm is eager, has linear complexity, and is
;; very fast.

(define (make-renaming-procedure namespace color reflected-initial-environment capturing?)

  (let ((inserted '())
        (reflected-environment reflected-initial-environment)
        (initial-environment
         (delay (env/reify namespace reflected-initial-environment))))

    ;; The optional initial binding name will always be supplied
    ;; (avoiding a search through the initial environment) except in
    ;; the case of hygiene-breaking primitives.

    (define (rename symbolic-name . maybe-binding-name)
      (if (pair? maybe-binding-name)
          (let ((initial-binding-name (car maybe-binding-name)))
            (cond ((assq symbolic-name inserted)
                   => (lambda (entry)
                        (let ((id (cdr entry)))
                          (if (eq? (binding-name id) initial-binding-name)
                              id
                              (error "Ambiguous scope for displaced identifier:"
                                     symbolic-name)))))
                  (else
                   (insert symbolic-name initial-binding-name))))
          (cond ((assq symbolic-name inserted) => cdr)
                ((assq symbolic-name (force initial-environment))
                 => (lambda (entry)
                      (insert symbolic-name (cdr entry))))
                (else (insert symbolic-name
                              (paint-name symbolic-name color))))))

    (define (insert symbolic-name . binding-names)

      (define (binding-name)
        (car binding-names))

      (define bind!
        (if capturing?
            (lambda (binding-name) (void))
            (lambda (binding-name)
              (set! binding-names (cons binding-name binding-names))
              (mark-dirty!))))

      (define unbind!
        (if capturing?
            (lambda () (void))
            (lambda ()
              (set! binding-names (cdr binding-names))
              (mark-dirty!))))

      (let ((new (make <identifier>
                   :symbolic-name symbolic-name
                   :binding-name-procedure binding-name
                   :color color
                   :bind-procedure bind!
                   :unbind-procedure unbind!
                   :renamer rename
                   :close-environment-procedure close-environment)))
        (set! inserted (alist-cons symbolic-name new inserted))
        (mark-dirty!)
        new))

    ;; Provides a persistent snapshot of the current bindings.
    ;; Invoked when compiling SYNTAX and QUASISYNTAX expressions and when
    ;; executing hygiene-breaking operations.

    (define (close-environment current-namespace)
      (if (not reflected-environment)
          (set! reflected-environment
                (env/reflect!
                 current-namespace
                 (foldl (lambda (insertion environment)
                          (alist-cons
                           (car insertion) (binding-name (cdr insertion))
                           environment))
                        (force initial-environment)
                        inserted))))
      reflected-environment)

    (define (mark-dirty!)
      (set! reflected-environment #f))

    rename))

;; A capturing identifier in a binding form will capture
;; free-identifier=? identifiers unbound in its scope.
;; The argument context-id provides the environment for
;; determining the denotation of the identifier.

(define (make-fluid-identifier context-id symbolic-name)
  (or (and (identifier? context-id)
           (symbol? symbolic-name))
      (error "make-fluid-identifier :: identifier symbol -> identifier"
             context-id symbolic-name))
  (let ((rename (make-renaming-procedure
                 (interaction-environment)
                 (color context-id)
                 (close-environment (interaction-environment) context-id)
                 #t)))
    (rename symbolic-name)))


;=======================================================================

;; Meta-renaming procedures are used to implement SYNTAX and the quoted
;; part of QUASISYNTAX expressions, which should paint identifiers with
;; a new color without unifying previously different colors.
;; This is done by appending a new color to an identifier's
;; existing color.

(define (make-meta-renaming-procedure)
  (let ((append-color (generate-color))
        (colors->renamers '()))
    (lambda (namespace color symbolic-name binding-name reflected-environment)
      (let ((rename
             (cond ((assq color colors->renamers) => cdr)
                   (else
                    (let ((rename (make-renaming-procedure
                                   namespace
                                   (append-colors append-color color)
                                   reflected-environment
                                   #f)))
                      (set! colors->renamers
                            (alist-cons color rename colors->renamers))
                      rename)))))
        (rename symbolic-name binding-name)))))

;; The current color meta-renamer for SYNTAX expressions,
;; and stack of expressions for source-object correlation:

(define *current-meta-rename*
  (make-parameter "*current-meta-rename*" (make-meta-renaming-procedure)))

;; Substitution mechanism for lexical bindings:
;; For separate compilation, gensyms should be globally unique.
;; The special prefix "@" is used to support LEXICALLY-BOUND?
;; below.

(define (bind-lexical! id)
  (bind! id (generate-globally-unique-symbol
             (string-append "@"
                            (symbol->string (symbolic-name id))
                            "-"))))

;; This is only used in LITERAL-IDENTIFIER=?.
;; Here one may use GENSYM? instead if the host scheme supports it.

(define (lexically-bound? id)
  (char=? #\@ (string-ref (symbol->string (binding-name id)) 0)))

;; Substitution mechanism for toplevel bindings:
;; DEFINE and DEFINE-SYNTAX use as binding name the painted name
;; corresponding to the toplevel, module, or generated color context.
;; This causes names in generated DEFINEs to be "secret" and protects
;; imported module locations (including primitives) from being
;; inadvertently redefined (although they can still be SET!).

(define (bind-toplevel! id)
  (bind! id (paint-name (symbolic-name id) (color id))))

(define (import! symbolic-name imported-name context-id)
  (let ((local-id ((renamer context-id) symbolic-name)))
    (bind! local-id imported-name)))

;=====================================================================

;;; Primitives for comparing identifiers and breaking hygiene:

(define (bound-identifier=? x y)
  (and (identifier? x)
       (eq? x y)))

(define (free-identifier=? x y)
  (and (identifier? x)
       (identifier? y)
       (eq? (binding-name x)
            (binding-name y))))

;; This should be used to compare literals independently of module.

(define (literal-identifier=? x y)
  (and (identifier? x)
       (identifier? y)
       (or (eq? (binding-name x)
                (binding-name y))
           (and (not (lexically-bound? x))
                (not (lexically-bound? y))
                (eq? (symbolic-name x)
                     (symbolic-name y))))))

;;; For internal use.  Equivalent to
;;; (free-identifier=? x #`symbol)

;(define (free=? x symbol)
;  (and (identifier? x)
;       (eq? (binding-name x) symbol)))

;; For internal use.  Equivalent to
;; (literal-identifier=? x #`symbol)

;(define (literal=? x symbol)
;  (and (identifier? x)
;       (or (eq? (binding-name x) symbol)
;           (and (not (lexically-bound? x))
;                (eq? (symbolic-name x) symbol)))))

(define (datum->syntax-object context-id datum)
  (or (identifier? context-id)
      (error "datum->syntax-object: First argument must be identifier:" context-id))
  (datum->syntax-object0 (renamer context-id) datum))

(define (datum->syntax-object0 rename datum)
  (sexp-map (lambda (leaf)
              (cond ((const? leaf)  leaf)
                    ((symbol? leaf) (rename leaf))
                    (else
                     (syntax-error "datum->syntax-object: Invalid datum:" leaf))))
              datum))

(define (syntax-object->datum stx)
  (sexp-map (lambda (leaf)
              (cond ((const? leaf)      leaf)
                    ((identifier? leaf) (symbolic-name leaf))
                    (else
                     (syntax-error "syntax-object->datum: Invalid syntax object:" leaf))))
            stx))

;====================================================================================
;;; Quasi walker
(define (quasi rename x quasiquote-tag id-quoter)

  (let ((rlist         (rename 'list))
        (rcons         (rename 'cons))
        (rappend       (rename 'append))
        (rquote        (rename 'quote))
        (rlist->vector (rename 'list->vector)))

    (define (qq-expand x level)

      (cond
       ((tag-backquote? x)                `(,rlist ,(id-quoter (car x))
                                                   ,(qq-expand (cadr x) (+ level 1))))
       ((and (= level 0)
             (tag-comma? x)
             (pair? (cdr x))
             (null? (cddr x)))            (cadr x))
       ((and (= level 0)
             (pair? x)
             (tag-comma? (car x)))        `(,rappend (,rlist . ,(cdar x))
                                                     ,(qq-expand (cdr x) 0)))
       ((and (= level 0)
             (pair? x)
             (tag-comma-atsign? (car x))) `(,rappend (,rappend . ,(cdar x))
                                                     ,(qq-expand (cdr x) 0)))
       ((and (> level 0)
             (or (tag-comma? x)
                 (tag-comma-atsign? x)))  `(,rcons ,(id-quoter (car x))
                                                   ,(qq-expand (cdr x) (- level 1))))
       ((pair? x)                         `(,rcons ,(qq-expand (car x) level)
                                                   ,(qq-expand (cdr x) level)))
       ((null? x)                         `(,rquote ()))
       ((identifier? x)                   (id-quoter x))
       ((vector? x)                       `(,rlist->vector
                                            ,(qq-expand (vector->list x) level)))
       (else                              x)))

    (define (literal=? x symbol)
      (and (identifier? x)
           (or (eq? (binding-name x) symbol)
               (and (not (lexically-bound? x))
                    (eq? (symbolic-name x) symbol)))))

    (define (tag-comma? x)        (and (pair? x)
                                       (literal=? (car x) `unquote)))
    (define (tag-comma-atsign? x) (and (pair? x)
                                       (literal=? (car x) `unquote-splicing)))
    (define (tag-backquote? x)    (and (pair? x)
                                       (pair? (cdr x))
                                       (null? (cddr x))
                                       (literal=? (car x) quasiquote-tag)))

    (qq-expand x 0)))

;;; Miscellany

(define (const? t)
  (or (null?    t)
      (boolean? t)
      (number?  t)
      (string?  t)
      (char?    t)))

;; Like mapcar, but preserves dotted tail.
(define (map-in-order f ls)
  (cond ((pair? ls) (let ((first (f (car ls))))
                      (cons first
                            (map-in-order f (cdr ls)))))
        ((null? ls) '())
        (else (f ls))))

(define (sexp-map f s)
  (cond ((pair? s) (cons (sexp-map f (car s))
                         (sexp-map f (cdr s))))
        ((null? s) '())
        ((vector? s)
         (apply vector (sexp-map f (vector->list s))))
        (else (f s))))

(define (alist-cons key datum alist)
  (cons (cons key datum) alist))

(define (alist-ref key alist)
  (cond ((assq key alist) => cdr)
        (else #f)))

(define (scan-let t k)
  (or (and (pair? (cdr t))
           (list? (cadr t))
           (list? (cddr t))
           (every? (lambda (binding)
                     (and (pair? binding)
                          (identifier? (car binding))
                          (pair? (cdr binding))
                          (null? (cddr binding))))
                   (cadr t)))
      (syntax-error))
  (let ((formals (map car (cadr t)))
        (exps    (map cadr (cadr t)))
        (body    (cddr t)))
    (k formals
       exps
       body)))

(define (formals? s)
  (define (dotted-member? x ls =)
    (cond ((pair? ls) (or (= x (car ls))
                          (dotted-member? x (cdr ls) =)))
          ((null? ls) #f)
          (else (= x ls))))

  (or (and (pair? s)
           (identifier? (car s))
           (formals? (cdr s))
           (not (dotted-member? (car s)
                                (cdr s)
                                bound-identifier=?)))
      (null? s)
      (identifier? s)))

;==========================================================================

;;; Debugging facilities:

(define *source-stack*
  (make-parameter "*source-stack*" '()))

(define (syntax-debug stx)
  (sexp-map (lambda (leaf)
              (if (identifier? leaf)
                  (binding-name leaf)
                  leaf))
            stx))

(define (syntax-error . args)
  (newline)
  (display "Syntax error:")
  (newline) (newline)
  (for-each (lambda (arg)
              (display arg) (display " "))
            args)
  (newline) (newline)
  (display "In source context:")
  (newline) (newline)
  (for-each (lambda (exp)
              (display "  ")
              (display (syntax-debug exp))
              (newline) (newline))
            (*source-stack*))
  (error "Expansion stopped"))

;; Set this variable to an integer from 0 through 5 to trace execution
;; of the dotnet code.  0 is least verbose, 5 is very detailed.
(define *syntax-noise-level*
  (make-parameter "*syntax-noise-level*" #f
                  (lambda (x) (or (not x) (number? x)))))

(define (syntax-trace message-level text . objects)
  (if (and (number? (*syntax-noise-level*))
           (>= (*syntax-noise-level*) message-level))
      (begin
        (newline)
        (display "; syntax ")
        (display message-level)
        (display ": ")
        (do ((i 0 (+ i 1)))
            ((>= i message-level) (display text))
          (display " "))
        (for-each (lambda (object)
                    (display " ")
                    (display object))
                  objects)
        (flush-output-port))))
