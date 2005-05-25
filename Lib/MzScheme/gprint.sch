;;; -*-Mode: Scheme; coding: iso-8859-1 -*-
;;;
;;; Part of the RIPOFF object system.

;;; Generic print-object

($$trace "gprint")

(define print-object (make <generic> :name 'print-object :arity 3))

;; As per Common Lisp, when the print-level gets too deep, we
;; bail and print a single # character.
(define (print-level-exceeded port)
  (write-char #\# port))

(extend-generic print-object
  :qualifier :around
  :procedure ((lambda ()
                (define (around-method:print-object call-next-method object port slashify)
                  (let ((level (print-level)))
                    (if (number? level)
                        (if (> level 0)
                            (parameterize ((print-level (- level 1)))
                              (call-next-method))
                            (print-level-exceeded port))
                        (call-next-method))))
                around-method:print-object)))

(define (print-unreadable-prefix tag port)
  (write-string "#<" port)
  (write-string tag port))

(define (print-unreadable-suffix port)
  (write-char #\> port))

;; Wrap a #<tag > around the call to the thunk.  If there is no thunk,
;; just print #<tag>.

(define (print-unreadable-object object port . thunk)
  (let ((tag (cond ((string? object) object)
                   ((symbol? object) (symbol->string object))
                   ((number? object) (number->string object))
                   (else (class-name-no-angles (class-of object))))))
    (if (pair? thunk)
        (if (null? (cdr thunk))
            (dynamic-wind (lambda ()
                            (print-unreadable-prefix tag port)
                            (write-string " " port))
                          (car thunk)
                          (lambda ()
                            (print-unreadable-suffix port)))
            (error "print-unreadable-object: too many arguments"))
        (begin
          (print-unreadable-prefix tag port)
          (print-unreadable-suffix port)))))

(define (method:print-unreadable-object call-next-method object port slashify)
  (print-unreadable-object object port))

(define (print-name name port)
  (cond ((string? name) (write-string name port))
        ((symbol? name) (write-string (symbol->string name) port))
        ((number? name) (write-string (number->string name) port))
        (else (print-object name port #f))))

(define (named-object-printer-method get-name)
  (define (method:print-object call-next-method object port slashify)
    (print-unreadable-object
     object port
     (lambda () (print-name (get-name object) port))))
  method:print-object)

;;; As per Common Lisp, when print-length is exceeded, print ...
;;; in the list.
(define (print-list object port slashify)
  (write-string "(" port)
  (let ((length (print-length)))

    (define (print-tail tail count)
      (cond ((null? tail))
            ((and (number? length)
                  (>= count length))
             (write-string " ..." port))
            ((pair? tail)
             (write-string " " port)
             (print-object (car tail) port slashify)
             (print-tail (cdr tail) (+ count 1)))
            (else
             (write-string " . " port)
             (print-object tail port slashify))))

    (if (number? length)
        (if (= length 0)
            (write-string "..." port)
            (print-object (car object) port slashify))
        (print-object (car object) port slashify))
    (print-tail (cdr object) 1))

  (write-string ")" port))

(define (print-vector object port slashify)
  (write-string "#(" port)
  (let ((length (print-length))
        (vlen (vector-length object)))

    (define (print-tail count)
      (cond ((>= count vlen))
            ((and (number? length)
                  (>= count length))
             (write-string " ..." port))
            (else
             (write-string " " port)
             (print-object (vector-ref object count) port slashify)
             (print-tail (+ count 1)))))

    (if (number? length)
        (if (= length 0)
            (write-string "..." port)
            (print-object (vector-ref object 0) port slashify))
        (print-object (vector-ref object 0) port slashify))
    (print-tail 1))

  (write-string ")" port))

(for-each
 (lambda (spec)
   (let ((class (car spec))
         (procedure (cadr spec)))
     (extend-generic print-object
       :specializers (list class)
       :procedure procedure)))
 (list
  (list <builtin> method:print-unreadable-object)

  (list <boolean> ((lambda ()
                     (define (method:print-object call-next-method object port slashify)
                       (cond ((eq? object #t) (write-string "#t" port))
                             ((eq? object #f) (write-string "#f" port))
                             (else (error "What kind of boolean is this?" object))))
                     method:print-object)))

  (list <class> ((lambda ()
                   (define (method:print-object call-next-method object port slashify)
                     (print-unreadable-object
                      object port
                      (lambda ()
                        (write-string (number->string (class-serial-number object)) port)
                        (write-string " " port)
                        (write-string (class-name-no-angles object) port))))
                   method:print-object)))

  (list <code-object> method:print-unreadable-object)

  (list <generic>   (named-object-printer-method generic-name))

  (list <hash-table> method:print-unreadable-object)

  (list <interned-symbol> ((lambda ()
                             (define (method:print-object call-next-method object port slashify)
                               ;; BUG:  should escape the string!
                               (write-string (symbol->string object) port))
                             method:print-object)))

  (list <method>    (named-object-printer-method method-name))
  (list <namespace> (named-object-printer-method environment-name))

  (list <null> ((lambda ()
                  (define (method:print-object call-next-method object port slashify)
                    (write-string "()" port))
                  method:print-object)))

  (list <number> ((lambda ()
                    (define (method:print-object call-next-method object port slashify)
                      (write-string (number->string object) port))
                    method:print-object)))

  (list <object> method:print-unreadable-object)

  (list <pair> ((lambda ()
                  (define (method:print-object call-next-method object port slashify)
                    (print-list object port slashify))
                  method:print-object)))

  (list <procedure> ((lambda ()
                       (define (method:print-object call-next-method object port slashify)
                         (let ((name (procedure-name object)))
                           (if name
                               (print-unreadable-object
                                object port
                                (lambda () (print-name name port)))
                               (let ((arity (procedure-arity object)))
                                 (if arity
                                     (print-unreadable-object
                                      object port
                                      (lambda ()
                                        (write-string "of " port)
                                        (if (exact? arity)
                                            (begin (write-string (number->string arity) port)
                                                   (write-string " argument" port)
                                                   (if (not (= arity 1))
                                                       (write-string "s" port)))
                                            (begin (write-string (number->string (inexact->exact arity)) port)
                                                   (write-string " or more arguments" port)))))
                                     (print-unreadable-object object port))))))
                       method:print-object)))

  (list <record> (let ((get-printer (record-accessor *record-type-type* 'printer)))
                   (define (method:print-object call-next-method object port slashify)
                     (let* ((descriptor (record-type-descriptor object))
                            (printer (get-printer descriptor)))
                       (if printer
                           (printer object port)
                           (print-unreadable-object (record-type-name descriptor) port))))
                   method:print-object))

  (list <uninterned-symbol> ((lambda ()
                               (define (method:print-object call-next-method object port slashify)
                                 ;; BUG:  should escape the string!
                                 (write-string "#:" port)
                                 (write-string (symbol->string object) port))
                               method:print-object)))

  (list <vector> ((lambda ()
                    (define (method:print-object call-next-method object port slashify)
                      (print-vector object port slashify))
                    method:print-object)))
  ))

;;; BOOTSTRAP STEP
(environment-printer print-object)
(hashtable-printer   print-object)
(procedure-printer   print-object)
(structure-printer   print-object)
(code-object-printer print-object)
