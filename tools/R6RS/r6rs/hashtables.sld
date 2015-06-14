(define-library (r6rs hashtables)

  (export

   make-eq-hashtable
   make-eqv-hashtable
   make-hashtable
   hashtable?
   hashtable-size
   hashtable-ref
   hashtable-set!
   hashtable-delete!
   hashtable-contains?
   hashtable-update!
   hashtable-copy
   hashtable-clear!
   hashtable-keys
   hashtable-entries
   hashtable-equivalence-function
   hashtable-hash-function
   hashtable-mutable?
   equal-hash
   string-hash
   string-ci-hash
   (rename r6rs:symbol-hash symbol-hash)    ; see explanation below
   )

  (import (scheme base)
          (scheme cxr))

  ;; Hashing on inexact and complex numbers depends on whether the
  ;; (scheme inexact) and (scheme complex) libraries are available.

  (cond-expand

   ((library (rnrs hashtables)))    ; nothing to do

   ((library (scheme inexact))
    (import (scheme inexact))
    (begin
     (define (inexact-hash x)
       (cond ((finite? x)
              (hash-for-eqv (exact x)))
             ((infinite? x)
              (if (> x 0.0)
                  hash:infinity+
                  hash:infinity-))
             (else
              hash:nan)))))

   (else
    (begin
     (define (inexact-hash x) 0))))

  (cond-expand

   ((and (library (rnrs hashtables))
         (not (library (r6rs no-rnrs))))
    ;; nothing to do
    )

   ((library (scheme complex))
    (import (scheme complex))
    (begin
     (define (complex-hash z)
       (+ (hash-for-eqv (real-part z))
          (hash-for-eqv (imag-part z))))))

   (else
    (begin
     (define (complex-hash z) 0))))

  ;; If the (rnrs hashtables) library is available, import it.
  ;; Otherwise use SRFI 69 if it's available.
  ;; If SRFI 69 isn't available, use its reference implementation.
  ;;
  ;; The (r6rs hashtables) library must export symbol-hash, which
  ;; has no equivalent among the procedures specified by SRFI 69.
  ;; The SRFI 69 reference implementation does define symbol-hash,
  ;; however, which has led to the current situation in which some
  ;; implementations of (srfi 69) export symbol-hash but others
  ;; don't.  The R7RS says it's an error to import symbol-hash
  ;; more than once with different bindings, or to redefine it
  ;; if it's been imported, so this (r6rs hashtables) library
  ;; defines r6rs:symbol-hash and renames it to symbol-hash only
  ;; when it's exported.

  (cond-expand

   ((and (library (rnrs hashtables))
         (not (library (r6rs no-rnrs))))
    (import (rnrs hashtables))
    (begin (define r6rs:symbol-hash symbol-hash)))

   ((library (srfi 69 basic-hash-tables))
    (import (srfi 69 basic-hash-tables))
    (include "hashtables.atop69.scm"))

   ((library (srfi 69))
    (import (srfi 69))
    (include "hashtables.atop69.scm"))

   ((library (srfi :69 basic-hash-tables))
    (import (srfi :69 basic-hash-tables))
    (include "hashtables.atop69.scm"))

   ((library (srfi :69))
    (import (srfi :69))
    (include "hashtables.atop69.scm"))

   ((library (scheme char))
    (import (scheme char))
    (include "hashtables.body69.scm")
    (include "hashtables.atop69.scm"))

   (else
    (begin (define (string-foldcase s) s)
           (define (string-ci=? s1 s2)
             (string=? s1 s2)))
    (include "hashtables.body69.scm")
    (include "hashtables.atop69.scm")))

  )
