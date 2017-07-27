;;; Given a list of strings and a list of n descriptions of options,
;;; returns n+1 values.  The first value returned is a list of the
;;; given strings that didn't match any of the descriptions.  Each
;;; of the next n values returned is a list of lists of strings that
;;; match the corresponding description of an option.
;;;
;;; Each description is of the form
;;;
;;;     <desc>  ::=  <string>                   ; string match
;;;             ::=  (or <desc> <desc> ...)     ; non-empty alternatives
;;;             ::=  (seq <desc> <desc> ...)    ; non-empty sequence
;;;             ::=  _                          ; wild card

(define (larceny:parse-options strings descriptions)
  (define ht (make-eq-hashtable))
  (define (loop strings others)
    (if (null? strings)
        (apply values
               (reverse others)
               (map (lambda (description)
                      (reverse (hashtable-ref ht description '())))
                    descriptions))
        (call-with-values
         (lambda () (matcher strings descriptions))
         (lambda (strings matched-strings maybe-description)
           (if maybe-description
               (begin (hashtable-set! ht
                                      maybe-description
                                      (cons matched-strings
                                            (hashtable-ref ht
                                                           maybe-description
                                                           '())))
                      (loop strings others))
               (loop (cdr strings)
                     (cons (car strings) others)))))))
  (loop strings '()))

(define (matcher strings descriptions)
  (if (null? descriptions)
      (values strings '() #f)
      (let ((description (car descriptions))
            (descriptions (cdr descriptions)))
        (cond ((null? strings)
               (values strings '() #f))
              ((match1 strings description)
               =>
               (lambda (matched-strings)
                 (values (list-tail strings (length matched-strings))
                         matched-strings
                         description)))
              (else
               (matcher strings descriptions))))))

(define (match1 strings description)
  (if (null? strings)
      #f
      (let ((s1 (car strings)))
        (cond ((eq? '_ description)
               (list s1))
              ((string? description)
               (if (string=? s1 description)
                   (list s1)
                   #f))
              ((and (pair? description)
                    (pair? (cdr description))
                    (eq? 'seq (car description)))
               (let ((matched-strings
                      (match1 strings (cadr description))))
                 (cond ((not matched-strings)
                        #f)
                       ((null? (cddr description))
                        matched-strings)
                       ((match1 (list-tail strings
                                           (length matched-strings))
                                (cons 'seq (cddr description)))
                        =>
                        (lambda (matched-strings2)
                          (append matched-strings matched-strings2)))
                       (else
                        #f))))
              ((and (pair? description)
                    (pair? (cdr description))
                    (eq? 'or (car description)))
               (let ((matched-strings
                      (match1 strings (cadr description))))
                 (cond (matched-strings
                        matched-strings)
                       ((null? (cddr description))
                        #f)
                       (else
                        (match1 strings
                                (cons 'or (cddr description)))))))
              (else
               (error "bad description passed to larceny:parse-options"
                      description))))))
