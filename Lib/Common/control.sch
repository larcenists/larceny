; Copyright 1991 Lightship Software
;
; $Id$
;
; Larceny library -- some control procedures.
;
; 'Call-with-current-continuation' is in malcode.mal.
; 'Exit' is OS-specific (see e.g. unix.sch)
; 'Call-without-interrupts' is in timer.sch.

($$trace "control")

; APPLY
;
; @raw-apply@ is written in MacScheme assembly language; see Lib/malcode.mal.
; It takes exactly three arguments: a procedure, a list, and the list length.
;
; (list? l) and (length l) can be computed in one pass over the list,
; if performance is a problem here.

(define apply
  (let ((raw-apply @raw-apply@))              ; see above

    (define (collect-arguments l)
      (if (null? (cdr l))
          (car l)
          (cons (car l) (collect-arguments (cdr l)))))

    (define (apply f l . rest)
      (cond ((not (procedure? f))
             (error "apply: not a procedure: " f)
             #t)
            ((not (null? rest))
             (apply f (cons l (collect-arguments rest))))
            ((null? l) (f))
            ((null? (cdr l)) (f (car l)))
            ((null? (cddr l)) (f (car l) (cadr l)))
            ((null? (cdddr l)) (f (car l) (cadr l) (caddr l)))
            ((null? (cddddr l)) (f (car l) (cadr l) (caddr l) (cadddr l)))
            ((not (list? l))
             (error "apply: not a list: " l)
             #t)
            (else (raw-apply f l (length l)))))

    apply))


; MAKE-TRAMPOLINE
;
; MAKE-TRAMPOLINE takes a procedure P and returns an environment-less
; procedure that takes any number of arguments and tail-calls P with those
; arguments.
;
; It is not possible to create a procedure with a shorter procedure
; structure than that of a trampoline.  That fact may be relied on:
; a trampoline can be patched into any other procedure.
;
; This procedure depends on the representation of procedures.

(define (make-trampoline p)
  (let* ((t trampoline-template)       ; in LIB/MALCODE.MAL
         (q (procedure-copy t)))
    (procedure-set! q 1 (vector-copy (procedure-ref q 1)))
    (let loop ((i 0) (const (procedure-ref q 1)))
      (if (eq? (vector-ref const i) 'dummy)
          (vector-set! const i p)
          (loop (+ i 1) const)))
    q))


; Used by 'delay'.

(define make-promise
  (lambda (proc)
    (let ((result-ready? #f)
          (result        #f))
      (lambda ()
        (if result-ready?
            result
            (let ((x (proc)))
              (if result-ready?
                  result
                  (begin (set! result-ready? #t)
                         (set! result x)
                         result))))))))

(define force
  (lambda (proc)
    (proc)))


; Garbage collection.

(define (collect . args)
  (cond ((null? args)
         (sys$gc 0 1))
        ((null? (cdr args))
         (sys$gc (car args) 1))
        ((null? (cddr args))
         (let ((x (cadr args)))
           (cond ((eq? x 'collect)
                  (sys$gc (car args) 1))
                 ((eq? x 'promote)
                  (sys$gc (car args) 0))
                 (else
                  (error "Second argument to COLLECT must be \"promote\""
                         " or \"collect\", not " x)))))
        (else
         (error "Too many arguments to COLLECT."))))


; Returns the current continuation structure (as chain of vectors).

(define (current-continuation-structure)
  (creg))


; From: William D Clinger <will@ccs.neu.edu>
; Newsgroups: comp.lang.scheme
; Subject: Re: call-with-values: what's all about?
; Date: Tue, 11 Nov 1997 15:29:31 -0500

; This is correct for correct programs.
;
; Returning multiple values to a continuation that does not expect them
; (an error) will not be detected.
;
; We need to replace these definitions when Larceny gets compiler
; support for them.

(define *multiple-values* (list '*multiple-values*))
(define *values0* (list *multiple-values*))

(define values
  (lambda vals
    (cond ((pair? vals) (if (null? (cdr vals))
                            (car vals)
                            (cons *multiple-values* vals)))
          ((null? vals) *values0*)
          ;; presumably impossible
          (else (error "values: Improper list " vals)
                #t))))

;; (value-list elements) = (apply values elements)
;; Avoids redundant consing, though.
(define (values-list vals)
  (cond ((pair? vals) (if (null? (cdr vals))
                          (car vals)
                          (cons *multiple-values* vals)))
        ((null? vals) *values0*)
        (else (error "values-list: Improper list " vals)
              #t)))

(define call-with-values
  (lambda (producer consumer)
    (let ((vals (producer)))
      ;; Handle most common case of up to four values
      (if (and (pair? vals)
               (eq? (car vals) *multiple-values*))
          (let ((tail0 (cdr vals)))
            (cond ((pair? tail0)
                   (let ((val0  (car tail0))
                         (tail1 (cdr tail0)))
                     (if (pair? tail1)
                         (let ((val1  (car tail1))
                               (tail2 (cdr tail1)))
                           (cond ((pair? tail2)
                                  (let ((val2  (car tail2))
                                        (tail3 (cdr tail2)))
                                    (cond ((pair? tail3)
                                           (let ((val3  (car tail3))
                                                 (tail4 (cdr tail3)))
                                             (if (null? tail4)
                                                 (consumer val0 val1 val2 val3)
                                                 (apply consumer tail0))))
                                          ((null? tail3) (consumer val0 val1 val2))
                                          (else (error
                                                 "call-with-values: bad values "
                                                 tail0)
                                                #t))))
                                 ((null? tail2) (consumer val0 val1))
                                 (else (error "call-with-values: bad values " tail0)
                                       #t)))
                         ;; single value shouldn't have values tag.
                         (error "call-with-values: bad values " tail0))))
                  ((null? tail0) (consumer))
                  (else (error "call-with-values: bad values " tail0)
                        #t)))
          (consumer vals)))))


; dynamic-wind
;
; Snarfed from Lisp Pointers, V(4), October-December 1992, p45.
; Written by Jonathan Rees.
;
; Modification:  We would like reroot! to be atomic wrt timer interrupts.
; While it is not desirable to disable interrupts while rerooting, since
; buggy user code can then hang the system, I have chosen to do for the
; time being.  Revisit when we start worrying about threads for real.

(define *here* (list #f))

(define call-with-current-continuation
  (let ((call-with-current-continuation call-with-current-continuation))
    (lambda (proc)
      (let ((here *here*))
        (call-with-current-continuation
         (lambda (cont)
           (proc (lambda results
                   (reroot! here)
                   (apply cont results)))))))))

(define (dynamic-wind before during after)
  (let ((here *here*))
    (reroot! (cons (cons before after) here))
    (call-with-values during
      (lambda results
        (reroot! here)
        (values-list results)))))

(define (reroot! there)

  (define (reroot-loop there)
    (if (not (eq? *here* there))
        (begin (reroot-loop (cdr there))
               (let ((before (caar there))
                     (after  (cdar there)))
                 (set-car! *here* (cons after before))
                 (set-cdr! *here* there)
                 (set-car! there #f)
                 (set-cdr! there '())
                 (set! *here* there)
                 (before)))))

  (let ((ticks (disable-interrupts)))
    (reroot-loop there)
    (if ticks (enable-interrupts ticks))))

; eof
