;;; -*-Mode: Scheme; coding: iso-8859-1 -*-
;;;
;;; jrm fecit anno domini mmiv
;;;
($$trace "misc")

;;; A bunch of miscellaneous functions that I can't figure out where
;;; else to put.

;;; Cheesy implementation of MzScheme-like arity-at-least.
(define (make-arity-at-least x)
  (cons 'arity-at-least x))

(define (arity-at-least? thing)
  (and (pair? thing)
       (eq? (car thing) 'arity-at-least)))

(define arity-at-least-value cdr)

;;; Given an arity or arity-at-least, return an arity or
;;; arity-at-least with N more required arguments.  Used by
;;; generic functions and methods.

(define (arity-plus arity n)
  (cond ((number? arity) (+ arity n))
        ((arity-at-least? arity)
         (make-arity-at-least (+ (arity-at-least-value arity) n)))
        (else (error "arity-plus: not an arity" arity))))

;;; ARGS should be a list of alternating keys and values.  The first
;;; (leftmost) value associated with KEYWORD is found and returned.  If
;;; KEYWORD is not in ARGS, DEFAULT is returned.
(define (getarg args keyword default)
  (define (scan tail)
    (cond ((pair? tail) (let ((key   (car tail))
                              (ttail (cdr tail)))
                          (if (pair? ttail)
                              (if (eq? key keyword)
                                  (car ttail)
                                  (scan (cdr ttail)))
                              (error "GETARG:  Unbalanced arglist" args))))
          ((null? tail) default)
          (else (error "GETARG:  Improper arglist" args))))
  (scan args))

;;; As above, but keywords is a list of keywords to look for.  First value
;;; of first matching keyword is returned.
(define getarg*
  (let ((not-found (cons #t #f)))
    (define (scan args keywords default)
      (cond ((pair? keywords) (let ((probe (getarg args (car keywords) not-found)))
                                (if (eq? probe not-found)
                                    (scan args (cdr keywords) default)
                                    probe)))
            ((null? keywords) default)
            (else (error "getarg*: improper keyword list" keywords))))
    scan))

;;; ARGS should be a list of alternating keys and values.  All values
;;; associated with KEYWORD is accumulated in a list and returned.
(define (getargs args keyword)
  (cond ((pair? args) (let ((key (car args))
                            (tail (cdr args)))
                        (if (pair? tail)
                            (if (eq? key keyword)
                                (cons (car tail) (getargs (cdr tail) keyword))
                                (getargs (cdr tail) keyword))
                            (error "GETARGS:  Unbalanced list:  " args))))
        ((null? args) '())
        (else (error "GETARGS:  Improper list:  " args))))

;;; Get a session-unique `serial number'.  This is used by the class
;;; system to improve hashing.  (Otherwise, the hash code behaves really poorly.)
(define get-serial-number
  (let ((current-serial-number 0))
    (lambda ()
      (call-without-interrupts
       (lambda ()
         (let ((sn current-serial-number))
           (set! current-serial-number (+ current-serial-number 1))
           sn))))))

;; A function that returns its argument.  Amazing how useful this is!
(define (identity x) x)

  ;; Given a procedure of at least ARITY args,
  ;; return a procedure of exacty ARITY args.
(define (%nary->fixed-arity procedure arity)
  ;; Why thirty-wonderful flavors?
  ;; The generic function arity is determined by the
  ;; arity of the enclosed methods.  If we used
  ;; a rest arg here, then we'd be creating an n-ary
  ;; generic which we don't want.

  ;; Unfortunately, we need even more of these than Twobit can handle!
  ;; For now, we'll do up to 16 arguments compiled, and let the interpreter
  ;; handle the rest.  The functions with lots of arguments are rarely used.
  (cond ((number? arity)
         (case arity
           ((0)  (lambda ()
                   (procedure)))
           ((1)  (lambda (arg0)
                   (procedure arg0)))
           ((2)  (lambda (arg0 arg1)
                   (procedure arg0 arg1)))
           ((3)  (lambda (arg0 arg1 arg2)
                   (procedure arg0 arg1 arg2)))
           ((4)  (lambda (arg0 arg1 arg2 arg3)
                   (procedure arg0 arg1 arg2 arg3)))
           ((5)  (lambda (arg0 arg1 arg2 arg3 arg4)
                   (procedure arg0 arg1 arg2 arg3 arg4)))
           ((6)  (lambda (arg0 arg1 arg2 arg3 arg4 arg5)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5)))
           ((7)  (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6)))
           ((8)  (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7)))
           ((9)  (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)))
           ((10) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)))
           ((11) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10)))
           ((12) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11)))
           ((13) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12)))
           ((14) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13)))
           ((15) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                               arg10 arg11 arg12 arg13 arg14)
                   (procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                              arg10 arg11 arg12 arg13 arg14)))
           (else (error "Need more of nary->fixed arity" arity))))
        ((arity-at-least? arity)
         (case (arity-at-least-value arity)
           ((0) procedure)
           ((1)  (lambda (arg0 . rest)
                   (apply procedure arg0 rest)))
           ((2)  (lambda (arg0 arg1 . rest)
                   (apply procedure arg0 arg1 rest)))
           ((3)  (lambda (arg0 arg1 arg2 . rest)
                   (apply procedure arg0 arg1 arg2 rest)))
           ((4)  (lambda (arg0 arg1 arg2 arg3 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 rest)))
           ((5)  (lambda (arg0 arg1 arg2 arg3 arg4 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 rest)))
           ((6)  (lambda (arg0 arg1 arg2 arg3 arg4 arg5 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 rest)))
           ((7)  (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 rest)))
           ((8)  (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 rest)))
           ((9)  (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 rest)))
           ((10) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 . rest)
                   (apply procedure arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 rest)))
           ((11) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 . rest)
                   (apply procedure
                          arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 rest)))
           ((12) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 . rest)
                   (apply procedure
                          arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 rest)))
           ((13) (lambda (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 . rest)
                   (apply procedure
                          arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 rest)))
           ((14) (lambda (arg0  arg1  arg2  arg3  arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 . rest)
                   (apply procedure
                          arg0  arg1  arg2  arg3  arg4 arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 rest)))
           ((15) (lambda (arg0  arg1  arg2  arg3  arg4  arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 . rest)
                   (apply procedure
                          arg0  arg1  arg2  arg3  arg4  arg5 arg6 arg7 arg8 arg9
                          arg10 arg11 arg12 arg13 arg14 rest)))
           (else (error "Need more of nary->fixed arity" arity))))
        (else (error "nary->fixed-arity:  not an arity" arity))))

