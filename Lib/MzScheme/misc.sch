;;; -*-Mode: Scheme; coding: iso-8859-1 -*-
;;;
;;; jrm fecit
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
