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
  (let ((raw-apply @raw-apply@))        ; see above

    (define (improper-list l)
      (error "Apply: improper list: " l)
      #t)

    ;; Since we are cdring down the list anyway, we can do the small
    ;; number of argument cases without even going through raw-apply.
    ;; Twobit is nice enough to inline all of these subfunctions.
    (define (xapply7 f a0 a1 a2 a3 a4 a5 a6 an)
      (cond ((pair? an) (raw-apply f
                                   (cons a0 (cons a1 (cons a2 (cons a3 (cons a4 (cons a5 (cons a6 an)))))))
                                   (+ (length an) 7)))
            ((null? an) (f a0 a1 a2 a3 a4 a5 a6))
            (else (improper-list an))))

    (define (xapply6 f a0 a1 a2 a3 a4 a5 an)
      (cond ((pair? an) (xapply7 f a0 a1 a2 a3 a4 a5 (car an) (cdr an)))
            ((null? an) (f a0 a1 a2 a3 a4 a5))
            (else (improper-list an))))

    (define (xapply5 f a0 a1 a2 a3 a4 an)
      (cond ((pair? an) (xapply6 f a0 a1 a2 a3 a4 (car an) (cdr an)))
            ((null? an) (f a0 a1 a2 a3 a4))
            (else (improper-list an))))

    (define (xapply4 f a0 a1 a2 a3 an)
      (cond ((pair? an) (xapply5 f a0 a1 a2 a3 (car an) (cdr an)))
            ((null? an) (f a0 a1 a2 a3))
            (else (improper-list an))))

    (define (xapply3 f a0 a1 a2 an)
      (cond ((pair? an) (xapply4 f a0 a1 a2 (car an) (cdr an)))
            ((null? an) (f a0 a1 a2))
            (else (improper-list an))))

    (define (xapply2 f a0 a1 an)
      (cond ((pair? an) (xapply3 f a0 a1 (car an) (cdr an)))
            ((null? an) (f a0 a1))
            (else (improper-list an))))

    (define (xapply1 f a0 an)
      (cond ((pair? an) (xapply2 f a0 (car an) (cdr an)))
            ((null? an) (f a0))
            (else (improper-list an))))

    (define (xapply0 f an)
      (cond ((pair? an) (xapply1 f (car an) (cdr an)))
            ((null? an) (f))
            (else (improper-list an))))

    ;; This flattens the tail of the arglist
    ;; (foo (bar) (baz quux)) => (foo (bar) baz quux)
    (define (collect-arguments a l)
      (if (pair? l)
          (cons a (collect-arguments (car l) (cdr l)))
          a))

    ;; Apply is rarely called with more than 5 arguments.

    (define (apply f a0 . rest)
      (cond ((not (procedure? f))
             (error "apply: not a procedure: " f)
             #t)
             ;; The last argument to apply is a list of arguments.
             ;; If it is the third, fourth or fifth argument, rather than cons a
             ;; list just to destructure it again, we directly invoke the appropriate routine.
            ((pair? rest)
             (let ((a1 (car rest))
                   (at1 (cdr rest)))
               (cond ((pair? at1)
                      (let ((a2  (car at1))
                            (at2 (cdr at1)))
                        (cond ((pair? at2)
                               (let ((a3 (car at2))
                                     (at3 (cdr at2)))
                                 (cond ((pair? at3)
                                        (let ((a4 (car at3))
                                              (at4 (cdr at3)))
                                          (cond ((pair? at4)
                                                 (let ((a5 (car at4))
                                                       (at5 (cdr at4)))
                                                   (cond ((pair? at5)
                                                          (xapply6 f a0 a1 a2 a3 a4 a5
                                                                   (collect-arguments (car at5) (cdr at5))))
                                                         ((null? at5) (xapply5 f a0 a1 a2 a3 a4 a5))
                                                         (else (improper-list at5)))))
                                                ((null? at4) (xapply4 f a0 a1 a2 a3 a4))
                                                (else (improper-list at4)))))
                                       ((null? at3) (xapply3 f a0 a1 a2 a3))
                                       (else (improper-list at3)))))
                              ((null? at2) (xapply2 f a0 a1 a2))
                              (else        (improper-list at2)))))
                     ((null? at1) (xapply1 f a0 a1))
                     (else        (improper-list at1)))))
            (else (xapply0 f a0))))
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
; FIXME: .creg is not a legal identifier.

(define (current-continuation-structure)
  (.creg))


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

; Note that dynamic-wind (defined below) depends on the ability to
; to pass multiple values through by simple not looking at them.

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
; (If it is not atomic, though, non-buggy user code cannot be made race-free!
; Better to make it possible to write correct code than to make it easy
; to debug code that cannot be fixed.)

(define *here* (list #f))

(define call-with-current-continuation
  (let ((call-with-current-continuation call-with-current-continuation))
    (lambda (proc)
      (let ((here *here*))
        (call-with-current-continuation
         (lambda (cont)
           (proc (lambda results
                   (reroot! here)
                   ;; Handle extremely common case.
                   (if (and (pair? results)
                            (null? (cdr results)))
                       (cont (car results))
                       (apply cont results))))))))))

(define (dynamic-wind before during after)
  (let ((here *here*))
    (reroot! (cons (cons before after) here))
    ;; Don't listify and respread the values.
    ;; (call-with-values during
    ;;  (lambda results
    ;;    (reroot! here)
    ;;    (values-list results)))
    (let ((result (during)))
      (reroot! here)
      result)))

(define (reroot! there)

  (define (reroot-loop there)
    (if (not (eq? *here* there))
        (begin (reroot-loop (cdr there))
               ;; Reusing this cell cuts a significant
               ;; amount of consing.
               (let* ((reuse-cell (car there))
                      (before (car reuse-cell))
                      (after  (cdr reuse-cell)))
                 (set-car! there #f)
                 (set-cdr! there '())
                 (set-car! reuse-cell after)
                 (set-cdr! reuse-cell before)

                 (set-car! *here* reuse-cell)
                 (set-cdr! *here* there)
                 (set! *here* there)
                 (before)))))

  (let ((ticks (disable-interrupts)))
    (reroot-loop there)
    (if ticks (enable-interrupts ticks))))

;; Continuation Marks

;; Continuation marks are pushed or added by the with-continuation-mark
;; macro or the more primitive call-with-continuation-mark procedure
;; (see malcode.mal)

;; The context changes in three main ways:
;; - normal procedure call and return:
;;   call/cm installs a cleanup frame to manage marks on procedure returns
;; - call/cc jumps
;;   The primitive continuation captured by call/cc closes over the
;;   continuation mark stack and restores it on continuation invocation.
;; - dynamic-wind
;;   Dynamic-wind is modified below to restore the appropriate continuation
;;   mark stack for the execution of the before and after thunks.


;; *cms* : (listof (list-of (cons key value)))
;; Manipulated by call-with-continuation-mark in malcode.mal
;; It is important for continuation/mutation safety that the 
;; structure of the continuation mark stack is never mutated; all
;; changes must be done by creating a new structure and mutating
;; only the variable.
(define *cms* '())

;; sys$replace-mark/call-thunk
;; Called by call-with-continuation-mark in malcode.mal
(define (sys$replace-mark/call-thunk key mark thunk)
  (define (alist-replace alist key mark)
    (cond ((pair? alist)
	   (let ((p0 (car alist)))
	     (if (eq? key (car p0))
		 (cons (cons key mark) (cdr alist))
		 (cons p0 (alist-replace (cdr alist) key mark)))))
	  (else (list (cons key mark)))))
;  (if (not (pair? *cms*))
;      (error "sys$replace-mark/call-thunk: *cms* not a pair"))
  (let ((frame0 (car *cms*)))
    (set! *cms* (cons (alist-replace frame0 key mark) (cdr *cms*)))
    ;; Tail call--call/cm frame is still at top of stack
    (thunk)))

;; dynamic-wind redefinition
;; When the before and after thunks of a dynamic-wind execute, they
;; should observe the continuation marks belonging to the continuation
;; of the dynamic wind expression.
(define dynamic-wind
  (let ((dynamic-wind dynamic-wind))
    (lambda (before during after)
      (let ((cms *cms*))
        (dynamic-wind
            (lambda () (set! *cms* cms) (before))
            during
            (lambda () (set! *cms* cms) (after)))))))

;; This would be nice, but we don't have records everywhere yet.
;(require 'record)
;
;(define cms/rtd
;  (make-record-type "continuation-mark-set" '(data) #f))
;
;((record-updater (record-type-descriptor cms/rtd) 'printer)
; cms/rtd
; (lambda (obj port)
;   (display "#<continuation-mark-set>" port)))
;
;(define cms-box (record-constructor cms/rtd))
;(define cms-unbox (record-accessor cms/rtd 'data))

;; Library procedures

; continuation-mark-set?: Any -> Boolean
(define continuation-mark-set?)

; current-continuation-marks: -> CMS
(define current-continuation-marks)

; continuation-mark-set->list: CMS x key -> (list-of mark)
(define continuation-mark-set->list)

(begin
  ;; A CMS (continuation mark set) should be opaque.  Sadly, records aren't
  ;; available everywhere, so we'll make do with structures:
  (define cms-tag (list 'cms-tag))

  (define (cms-box alists)
    (let ((cms (make-structure 2)))
      (vector-like-set! cms 0 cms-tag)
      (vector-like-set! cms 1 alists)
      cms))

  (define (cms-unbox cms)
    (vector-like-ref cms 1))

  (set! continuation-mark-set?
    (lambda (cms)
      (and (structure? cms)
           (= 2 (vector-like-length cms))
           (eq? cms-tag (vector-like-ref cms 0)))))

  (set! current-continuation-marks
    (lambda ()
      (cms-box *cms*)))

  (set! continuation-mark-set->list 
    (lambda (cms key)
      (define (process alists)
        (cond ((pair? alists)
               (let loop ((alist (car alists)))
                 (cond ((pair? alist)
                        (let ((p0 (car alist)))
                          (if (eq? key (car p0))
                            (cons (cdr p0) (process (cdr alists)))
                            (loop (cdr alist)))))
                       ((null? alist)
                        (cons '... (process (cdr alists)))))))
              ((null? alists)
               '())))
      (process (cms-unbox cms)))))

;; Print them nicely, so they can pretend to be opaque:
(let ((old-structure-printer (structure-printer)))
  (structure-printer
    (lambda (obj port quote?)
      (if (continuation-mark-set? obj)
        (display "#<continuation-mark-set>" port)
        (old-structure-printer obj port quote?)))))

; eof
