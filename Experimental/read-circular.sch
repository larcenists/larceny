; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Reader for circular and shared structures.
;
; The syntax #n=<object> makes associates mark n with <object>.
; The syntax #n# is a reference to the object associated with mark n.

(define (read-circular . rest)
  (let* ((sharp-reader (readtable-ref #\#))
         (sharp-class (car sharp-reader))
         (sharp-dispatch (cadr sharp-reader))
         (sharp-dispatch-list (caddr sharp-reader))
         (token-type (make-record-type "read-circular-token" '(x))))

    (define make-token  (record-constructor token-type))
    (define token?      (record-predicate token-type))
    (define token-value (record-accessor token-type 'x))

    (define tbl 
      (make-hashtable (lambda (x) x) assv))

    (define (read-object-id p)
      (let loop ((n 0))
        (let ((c (read-char p)))
          (if (char-numeric? c)
              (loop (+ (* n 10) (- (char->integer c) (char->integer #\0))))
              (values n c)))))

    (define circular? #f)               ; Dynamically bound at each level

    (define patchups '())               ; List of all patchups

    (define (remember p)                ; Push a patchup
      (set! patchups (cons p patchups)))

    (define (do-car x)
      (lambda ()
        (if (token? (car x))
            (set-car! x (hashtable-get tbl (token-value (car x)))))))

    (define (do-cdr x)
      (lambda ()
        (if (token? (cdr x))
            (set-cdr! x (hashtable-get tbl (token-value (cdr x)))))))

    (define (do-vector x i)
      (lambda ()
        (if (token? (vector-ref x i))
            (vector-set! x i (token-value (vector-ref x i))))))

    (define (find-places x)
      (cond ((pair? x)
             (if (token? (car x)) (remember (do-car x)))
             (if (pair? (cdr x))
                 (find-places (cdr x))
                 (if (token? (cdr x)) (remember (do-cdr x)))))
            ((vector? x)
             (do ((i 0 (+ i 1)))
                 ((= i (vector-length x)))
               (if (token? (vector-ref x i)) (remember (do-vector x i)))))))

    (define (new-sharp-dispatch c p)
      (let ((next (peek-char p)))
        (if (char-numeric? next)
            (let-values ((n trailing) (read-object-id p))
              (case trailing
                ((#\=) (fluid-let ((circular? #f))
                         (let ((item (read p)))
                           (if circular? (find-places item))
                           (hashtable-put! tbl n item)
                           item)))
                ((#\#) (let ((x (hashtable-get tbl n)))
                         (if (not x)
                             (begin (set! circular? #t)
                                    (make-token n))
                             x)))
                (else  (error "Syntax error in #n syntax: " trailing))))
            (sharp-dispatch c p))))

    (dynamic-wind
     (lambda () 
       (readtable-set! #\# (list sharp-class
                                 new-sharp-dispatch
                                 sharp-dispatch-list)))
     (lambda () 
       (let ((x (read (if (null? rest) (current-input-port) (car rest)))))
         (do ((p patchups (cdr p)))
             ((null? p) x)
           ((car p)))))
     (lambda () 
       (readtable-set! #\# sharp-reader)))))

; eof
