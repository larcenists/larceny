; This isn't ready for release because it uses syntax-case.
; That should be easy to fix, however.
;
; $Id$

; STREAMS.SS PLB 9NOV2007

; Copyright (C) 2007 by Philip L. Bewig of Saint Louis, Missouri, USA.  All rights
; reserved.  Permission is hereby granted, free of charge, to any person obtaining a copy of
; this software and associated documentation files (the "Software"), to deal in the Software
; without restriction, including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to
; whom the Software is furnished to do so, subject to the following conditions: The above
; copyright notice and this permission notice shall be included in all copies or substantial
; portions of the Software.  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
; THE USE OR OTHER DEALINGS IN THE SOFTWARE.

; This file contains the complete source code from the Streams paper.  Since R6RS
; systems are not yet available, the code is provided in a form that can be loaded in
; any R5RS Scheme system.  Since R5RS has no module system, it is incumbent on the
; user to refrain from using private proceduress of the two modules.  Use only those
; procedures and macros described in the Streams paper.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SRFI-9 records

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
       (constructor constructor-tag ...)
       predicate
       (field-tag accessor . more) ...)
     (begin
       (define type
         (make-record-type 'type '(field-tag ...)))
       (define constructor
         (record-constructor type '(constructor-tag ...)))
       (define predicate
         (record-predicate type))
       (define-record-field type field-tag accessor . more)
       ...))))

(define-syntax define-record-field
  (syntax-rules ()
    ((define-record-field type field-tag accessor)
     (define accessor (record-accessor type 'field-tag)))
    ((define-record-field type field-tag accessor modifier)
     (begin
       (define accessor (record-accessor type 'field-tag))
       (define modifier (record-modifier type 'field-tag))))))

(define record-marker (list 'record-marker))

(define real-vector? vector?)

(define (vector? x)
  (and (real-vector? x)
       (or (= 0 (vector-length x))
	   (not (eq? (vector-ref x 0)
		record-marker)))))

(define eval
  (let ((real-eval eval))
    (lambda (exp env)
      ((real-eval `(lambda (vector?) ,exp))
       vector?))))

(define (record? x)
  (and (real-vector? x)
       (< 0 (vector-length x))
       (eq? (vector-ref x 0)
            record-marker)))

(define (make-record size)
  (let ((new (make-vector (+ size 1))))
    (vector-set! new 0 record-marker)
    new))

(define (record-ref record index)
  (vector-ref record (+ index 1)))

(define (record-set! record index value)
  (vector-set! record (+ index 1) value))

(define (record-type record)
  (record-ref record 0))

(define :record-type (make-record 3))
(record-set! :record-type 0 :record-type)
(record-set! :record-type 1 ':record-type)
(record-set! :record-type 2 '(name field-tags))

(define (make-record-type name field-tags)
  (let ((new (make-record 3)))
    (record-set! new 0 :record-type)
    (record-set! new 1 name)
    (record-set! new 2 field-tags)
    new))

(define (record-type-name record-type)
  (record-ref record-type 1))

(define (record-type-field-tags record-type)
  (record-ref record-type 2))

(define (field-index type tag)
  (let loop ((i 1) (tags (record-type-field-tags type)))
    (cond ((null? tags)
           (error 'field-index "record type has no such field" type tag))
          ((eq? tag (car tags))
           i)
          (else
           (loop (+ i 1) (cdr tags))))))

(define (record-constructor type tags)
  (let ((size (length (record-type-field-tags type)))
        (arg-count (length tags))
        (indexes (map (lambda (tag)
                        (field-index type tag))
                      tags)))
    (lambda args
      (if (= (length args)
             arg-count)
          (let ((new (make-record (+ size 1))))
            (record-set! new 0 type)
            (for-each (lambda (arg i)
			(record-set! new i arg))
                      args
                      indexes)
            new)
          (error 'record-constructor "wrong number of arguments to constructor" type args)))))

(define (record-predicate type)
  (lambda (thing)
    (and (record? thing)
         (eq? (record-type thing)
              type))))

(define (record-accessor type tag)
  (let ((index (field-index type tag)))
    (lambda (thing)
      (if (and (record? thing)
               (eq? (record-type thing)
                    type))
          (record-ref thing index)
          (error 'record-accessor "accessor applied to bad value" type tag thing)))))

(define (record-modifier type tag)
  (let ((index (field-index type tag)))
    (lambda (thing value)
      (if (and (record? thing)
               (eq? (record-type thing)
                    type))
          (record-set! thing index value)
          (error 'record-modifier "modifier applied to bad value" type tag thing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SRFI-23 errors (modified for R6RS)

(define (error reason . args)
  (display "Error: ")
  (display (symbol->string reason))
  (for-each (lambda (arg) 
            (display " ")
            (write arg))
            args)
  (newline)
  (scheme-report-environment -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (streams primitive)

  (define-record-type stream-type
    (make-stream box)
    stream?
    (box stream-promise stream-promise!))

  (define-syntax stream-lazy
    (syntax-rules ()
      ((lazy expr)
        (make-stream
          (cons 'lazy (lambda () expr))))))

  (define (stream-eager expr)
    (make-stream
      (cons 'eager expr)))

  (define-syntax stream-delay
    (syntax-rules ()
      ((stream-delay expr)
        (stream-lazy (stream-eager expr)))))

  (define (stream-force promise)
    (let ((content (stream-promise promise)))
      (case (car content)
        ((eager) (cdr content))
        ((lazy)  (let* ((promise* ((cdr content)))
                        (content  (stream-promise promise)))
                   (if (not (eqv? (car content) 'eager))
                       (begin (set-car! content (car (stream-promise promise*)))
                              (set-cdr! content (cdr (stream-promise promise*)))
                              (stream-promise! promise* content)))
                   (stream-force promise))))))

  (define stream-null (stream-delay (cons 'stream 'null)))

  (define-record-type stream-pare-type
    (make-stream-pare kar kdr)
    stream-pare?
    (kar stream-kar)
    (kdr stream-kdr))

  (define (stream-pair? obj)
    (and (stream? obj) (stream-pare? (stream-force obj))))

  (define (stream-null? obj)
    (and (stream? obj)
         (eqv? (stream-force obj)
               (stream-force stream-null))))

  (define-syntax stream-cons
    (syntax-rules ()
      ((stream-cons obj strm)
        (stream-eager (make-stream-pare (stream-delay obj) (stream-lazy strm))))))

  (define (stream-car strm)
    (cond ((not (stream? strm)) (error 'stream-car "non-stream"))
          ((stream-null? strm) (error 'stream-car "null stream"))
          (else (stream-force (stream-kar (stream-force strm))))))

  (define (stream-cdr strm)
    (cond ((not (stream? strm)) (error 'stream-cdr "non-stream"))
          ((stream-null? strm) (error 'stream-cdr "null stream"))
          (else (stream-kdr (stream-force strm)))))

  (define-syntax stream-lambda
    (syntax-rules ()
      ((stream-lambda formals body0 body1 ...)
        (lambda formals (stream-lazy (let () body0 body1 ...))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (streams derived)

  (define (exists pred? . lists)
    (and (not (null? (car lists)))
         (or (apply pred? (map car lists))
             (apply exists pred? (map cdr lists)))))

  (define-syntax define-stream
    (syntax-rules ()
      ((define-stream (name . formal) body0 body1 ...)
        (define name (stream-lambda formal body0 body1 ...)))))

  (define (list->stream objs)
    (define list->stream
      (stream-lambda (objs)
        (if (null? objs)
            stream-null
            (stream-cons (car objs) (list->stream (cdr objs))))))
    (if (not (list? objs))
        (error 'list->stream "non-list argument")
        (list->stream objs)))

  (define (port->stream . port)
    (define port->stream
      (stream-lambda (p)
        (let ((c (read-char p)))
          (if (eof-object? c)
              stream-null
              (stream-cons c (port->stream p))))))
    (let ((p (if (null? port) (current-input-port) (car port))))
      (if (not (input-port? p))
          (error 'port->stream "non-input-port argument")
          (port->stream p))))

  (define-syntax stream
    (syntax-rules ()
      ((stream) stream-null)
      ((stream x y ...) (stream-cons x (stream y ...)))))

  (define (stream->list . args)
    (let ((n (if (= 1 (length args)) #f (car args)))
          (strm (if (= 1 (length args)) (car args) (cadr args))))
      (cond ((not (stream? strm)) (error 'stream->list "non-stream argument"))
            ((and n (not (integer? n))) (error 'stream->list "non-integer count"))
            ((and n (negative? n)) (error 'stream->list "negative count"))
            (else (let loop ((n (if n n -1)) (strm strm))
                    (if (or (zero? n) (stream-null? strm))
                        '()
                        (cons (stream-car strm) (loop (- n 1) (stream-cdr strm)))))))))

  (define (stream-append . strms)
    (define stream-append
      (stream-lambda (strms)
        (cond ((null? (cdr strms)) (car strms))
              ((stream-null? (car strms)) (stream-append (cdr strms)))
              (else (stream-cons (stream-car (car strms))
                                 (stream-append (cons (stream-cdr (car strms)) (cdr strms))))))))
    (cond ((null? strms) stream-null)
          ((exists (lambda (x) (not (stream? x))) strms)
            (error 'stream-append "non-stream argument"))
          (else (stream-append strms))))
  
  (define (stream-concat strms)
    (define stream-concat
      (stream-lambda (strms)
        (cond ((stream-null? strms) stream-null)
              ((not (stream? (stream-car strms)))
                (error 'stream-concat "non-stream object in input stream"))
              ((stream-null? (stream-car strms))
                (stream-concat (stream-cdr strms)))
              (else (stream-cons
                      (stream-car (stream-car strms))
                      (stream-concat
                        (stream-cons (stream-cdr (stream-car strms)) (stream-cdr strms))))))))
    (if (not (stream? strms))
        (error 'stream-concat "non-stream argument")
        (stream-concat strms)))

  (define stream-constant
    (stream-lambda objs
      (cond ((null? objs) stream-null)
            ((null? (cdr objs)) (stream-cons (car objs) (stream-constant (car objs))))
            (else (stream-cons (car objs)
                               (apply stream-constant (append (cdr objs) (list (car objs)))))))))

  (define (stream-drop n strm)
    (define stream-drop
      (stream-lambda (n strm)
        (if (or (zero? n) (stream-null? strm))
            strm
            (stream-drop (- n 1) (stream-cdr strm)))))
    (cond ((not (integer? n)) (error 'stream-drop "non-integer argument"))
          ((negative? n) (error 'stream-drop "negative argument"))
          ((not (stream? strm)) (error 'stream-drop "non-stream argument"))
          (else (stream-drop n strm))))

  (define (stream-drop-while pred? strm)
    (define stream-drop-while
      (stream-lambda (strm)
        (if (and (stream-pair? strm) (pred? (stream-car strm)))
            (stream-drop-while (stream-cdr strm))
            strm)))
    (cond ((not (procedure? pred?)) (error 'stream-drop-while "non-procedural argument"))
          ((not (stream? strm)) (error 'stream-drop-while "non-stream argument"))
          (else (stream-drop-while strm))))

  (define (stream-filter pred? strm)
    (define stream-filter
      (stream-lambda (strm)
        (cond ((stream-null? strm) stream-null)
              ((pred? (stream-car strm))
                (stream-cons (stream-car strm) (stream-filter (stream-cdr strm))))
              (else (stream-filter (stream-cdr strm))))))
    (cond ((not (procedure? pred?)) (error 'stream-filter "non-procedural argument"))
          ((not (stream? strm)) (error 'stream-filter "non-stream argument"))
          (else (stream-filter strm))))

  (define (stream-fold proc base strm)
    (cond ((not (procedure? proc)) (error 'stream-fold "non-procedural argument"))
          ((not (stream? strm)) (error 'stream-fold "non-stream argument"))
          (else (let loop ((base base) (strm strm))
                  (if (stream-null? strm)
                      base
                      (loop (proc base (stream-car strm)) (stream-cdr strm)))))))

  (define (stream-for-each proc . strms)
    (define (stream-for-each strms)
      (if (not (exists stream-null? strms))
          (begin (apply proc (map stream-car strms))
                 (stream-for-each (map stream-cdr strms)))))
    (cond ((not (procedure? proc)) (error 'stream-for-each "non-procedural argument"))
          ((null? strms) (error 'stream-for-each "no stream arguments"))
          ((exists (lambda (x) (not (stream? x))) strms)
            (error 'stream-for-each "non-stream argument"))
          (else (stream-for-each strms))))

  (define (stream-from first . step)
    (define stream-from
      (stream-lambda (first delta)
        (stream-cons first (stream-from (+ first delta) delta))))
    (let ((delta (if (null? step) 1 (car step))))
      (cond ((not (number? first)) (error 'stream-from "non-numeric starting number"))
            ((not (number? delta)) (error 'stream-from "non-numeric step size"))
            (else (stream-from first delta)))))

  (define (stream-iterate proc base)
    (define stream-iterate
      (stream-lambda (base)
        (stream-cons base (stream-iterate (proc base)))))
    (if (not (procedure? proc))
        (error 'stream-iterate "non-procedural argument")
        (stream-iterate base)))

  (define (stream-length strm)
    (if (not (stream? strm))
        (error 'stream-length "non-stream argument")
        (let loop ((len 0) (strm strm))
          (if (stream-null? strm)
              len
              (loop (+ len 1) (stream-cdr strm))))))

  (define-syntax stream-let
    (syntax-rules ()
      ((stream-let tag ((name val) ...) body1 body2 ...)
       ((letrec ((tag (stream-lambda (name ...) body1 body2 ...))) tag) val ...))))

  (define (stream-map proc . strms)
    (define stream-map
      (stream-lambda (strms)
        (if (exists stream-null? strms)
            stream-null
            (stream-cons (apply proc (map stream-car strms))
                         (stream-map (map stream-cdr strms))))))
    (cond ((not (procedure? proc)) (error 'stream-map "non-procedural argument"))
          ((null? strms) (error 'stream-map "no stream arguments"))
          ((exists (lambda (x) (not (stream? x))) strms)
            (error 'stream-map "non-stream argument"))
          (else (stream-map strms))))
  
  (define-syntax stream-match
    (syntax-rules ()
      ((stream-match strm-expr clause ...)
        (let ((strm strm-expr))
          (cond
            ((not (stream? strm)) (error 'stream-match "non-stream argument"))
            ((stream-match-test strm clause) => car) ...
            (else (error 'stream-match "pattern failure")))))))
 
  (define-syntax stream-match-test
    (syntax-rules ()
      ((stream-match-test strm (pattern fender expr))
        (stream-match-pattern strm pattern () (and fender (list expr))))
      ((stream-match-test strm (pattern expr))
        (stream-match-pattern strm pattern () (list expr)))))

  (define-syntax stream-match-pattern
    (lambda (x)
      (define (wildcard? x)
        (and (identifier? x)
             (free-identifier=? x (syntax _))))
      (syntax-case x () 
        ((stream-match-pattern strm () (binding ...) body)
          (syntax (and (stream-null? strm) (let (binding ...) body))))
        ((stream-match-pattern strm (w? . rest) (binding ...) body)
          (wildcard? #'w?) 
          (syntax (and (stream-pair? strm)
                       (let ((strm (stream-cdr strm)))
                         (stream-match-pattern strm rest (binding ...) body)))))
        ((stream-match-pattern strm (var . rest) (binding ...) body)
          (syntax (and (stream-pair? strm)
                       (let ((temp (stream-car strm)) (strm (stream-cdr strm))) 
                         (stream-match-pattern strm rest ((var temp) binding ...) body)))))
        ((stream-match-pattern strm w? (binding ...) body)
          (wildcard? #'w?)
          (syntax (let (binding ...) body)))
        ((stream-match-pattern strm var (binding ...) body) 
          (syntax (let ((var strm) binding ...) body))))))
  
  (define-syntax stream-of
    (syntax-rules ()
      ((_ expr rest ...)
        (stream-of-aux expr stream-null rest ...))))

  (define-syntax stream-of-aux
    (syntax-rules (in is)
      ((stream-of-aux expr base)
        (stream-cons expr base))
      ((stream-of-aux expr base (var in stream) rest ...)
        (stream-let loop ((strm stream))
          (if (stream-null? strm)
              base
              (let ((var (stream-car strm)))
                (stream-of-aux expr (loop (stream-cdr strm)) rest ...)))))
      ((stream-of-aux expr base (var is exp) rest ...)
        (let ((var exp)) (stream-of-aux expr base rest ...)))
      ((stream-of-aux expr base pred? rest ...)
        (if pred? (stream-of-aux expr base rest ...) base))))

  (define (stream-range first past . step)
    (define stream-range
      (stream-lambda (first past delta lt?)
        (if (lt? first past)
            (stream-cons first (stream-range (+ first delta) past delta lt?))
            stream-null)))
    (cond ((not (number? first)) (error 'stream-range "non-numeric starting number"))
          ((not (number? past)) (error 'stream-range "non-numeric ending number"))
          (else (let ((delta (cond ((pair? step) (car step)) ((< first past) 1) (else -1))))
                  (if (not (number? delta))
                      (error 'stream-range "non-numeric step size")
                      (let ((lt? (if (< 0 delta) < >)))
                        (stream-range first past delta lt?)))))))

  (define (stream-ref strm n)
    (cond ((not (stream? strm)) (error 'stream-ref "non-stream argument"))
          ((not (integer? n)) (error 'stream-ref "non-integer argument"))
          ((negative? n) (error 'stream-ref "negative argument"))
          (else (let loop ((strm strm) (n n))
                  (cond ((stream-null? strm) (error 'stream-ref "beyond end of stream"))
                        ((zero? n) (stream-car strm))
                        (else (loop (stream-cdr strm) (- n 1))))))))

  (define (stream-reverse strm)
    (define stream-reverse
      (stream-lambda (strm rev)
        (if (stream-null? strm)
            rev
            (stream-reverse (stream-cdr strm) (stream-cons (stream-car strm) rev)))))
    (if (not (stream? strm))
        (error 'stream-reverse "non-stream argument")
        (stream-reverse strm stream-null)))

  (define (stream-scan proc base strm)
    (define stream-scan
      (stream-lambda (base strm)
        (if (stream-null? strm)
            (stream base)
            (stream-cons base (stream-scan (proc base (stream-car strm)) (stream-cdr strm))))))
    (cond ((not (procedure? proc)) (error 'stream-scan "non-procedural argument"))
          ((not (stream? strm)) (error 'stream-scan "non-stream argument"))
          (else (stream-scan base strm))))

  (define (stream-take n strm)
    (define stream-take
      (stream-lambda (n strm)
        (if (or (stream-null? strm) (zero? n))
            stream-null
            (stream-cons (stream-car strm) (stream-take (- n 1) (stream-cdr strm))))))
    (cond ((not (stream? strm)) (error 'stream-take "non-stream argument"))
          ((not (integer? n)) (error 'stream-take "non-integer argument"))
          ((negative? n) (error 'stream-take "negative argument"))
          (else (stream-take n strm))))

  (define (stream-take-while pred? strm)
    (define stream-take-while
      (stream-lambda (strm)
        (cond ((stream-null? strm) stream-null)
              ((pred? (stream-car strm))
                (stream-cons (stream-car strm) (stream-take-while (stream-cdr strm))))
              (else stream-null))))
    (cond ((not (stream? strm)) (error 'stream-take-while "non-stream argument"))
          ((not (procedure? pred?)) (error 'stream-take-while "non-procedural argument"))
          (else (stream-take-while strm))))

  (define (stream-unfold mapper pred? generator base)
    (define stream-unfold
      (stream-lambda (base)
        (if (pred? base)
            (stream-cons (mapper base) (stream-unfold (generator base)))
            stream-null)))
    (cond ((not (procedure? mapper)) (error 'stream-unfold "non-procedural mapper"))
          ((not (procedure? pred?)) (error 'stream-unfold "non-procedural pred?"))
          ((not (procedure? generator)) (error 'stream-unfold "non-procedural generator"))
          (else (stream-unfold base))))

  (define (stream-unfolds gen seed)
    (define (len-values gen seed)
      (call-with-values
        (lambda () (gen seed))
        (lambda vs (- (length vs) 1))))
    (define unfold-result-stream
      (stream-lambda (gen seed)
        (call-with-values
          (lambda () (gen seed))
          (lambda (next . results)
            (stream-cons results (unfold-result-stream gen next))))))
    (define result-stream->output-stream
      (stream-lambda (result-stream i)
        (let ((result (list-ref (stream-car result-stream) (- i 1))))
          (cond ((pair? result)
                  (stream-cons
                    (car result)
                    (result-stream->output-stream (stream-cdr result-stream) i)))
                ((not result)
                  (result-stream->output-stream (stream-cdr result-stream) i))
                ((null? result) stream-null)
                (else (error 'stream-unfolds "can't happen"))))))
    (define (result-stream->output-streams result-stream)
      (let loop ((i (len-values gen seed)) (outputs '()))
        (if (zero? i)
            (apply values outputs)
            (loop (- i 1) (cons (result-stream->output-stream result-stream i) outputs)))))
    (if (not (procedure? gen))
        (error 'stream-unfolds "non-procedural argument")
        (result-stream->output-streams (unfold-result-stream gen seed))))

  (define (stream-zip . strms)
    (define stream-zip
      (stream-lambda (strms)
        (if (exists stream-null? strms)
            stream-null
            (stream-cons (map stream-car strms) (stream-zip (map stream-cdr strms))))))
    (cond ((null? strms) (error 'stream-zip "no stream arguments"))
          ((exists (lambda (x) (not (stream? x))) strms)
            (error 'stream-zip "non-stream argument"))
          (else (stream-zip strms))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; utilities

(define (identity obj) obj)

(define (const obj) (lambda x obj))

(define (negate pred?) (lambda (x) (not (pred? x))))

(define (lsec proc . args) (lambda x (apply proc (append args x))))

(define (rsec proc . args) (lambda x (apply proc (reverse (append (reverse args) (reverse x))))))

(define (compose . fns)
  (let comp ((fns fns))
    (cond
      ((null? fns) 'error)
      ((null? (cdr fns)) (car fns))
      (else
        (lambda args
          (call-with-values
            (lambda ()
              (apply
                (comp (cdr fns))
                args))
            (car fns)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; examples

(define-stream (file->stream filename)
  (let ((p (open-input-file filename)))
    (stream-let loop ((c (read-char p)))
      (if (eof-object? c)
          (begin (close-input-port p)
                 stream-null)
          (stream-cons c
            (loop (read-char p)))))))

(define-stream (qsort lt? strm)
  (if (stream-null? strm)
      stream-null
      (let ((x (stream-car strm))
            (xs (stream-cdr strm)))
        (stream-append
          (qsort lt?
            (stream-filter
              (lambda (u) (lt? u x))
              xs))
          (stream x)
          (qsort lt?
            (stream-filter
              (lambda (u) (not (lt? u x)))
              xs))))))

(define-stream (interleave x yy)
  (stream-match yy
    (() (stream (stream x)))
    ((y . ys)
      (stream-append
        (stream (stream-cons x yy))
        (stream-map
          (lambda (z) (stream-cons y z))
          (interleave x ys))))))

(define-stream (perms xs)
  (if (stream-null? xs)
      (stream (stream))
      (stream-concat
        (stream-map
          (lsec interleave (stream-car xs))
          (perms (stream-cdr xs))))))

(define (stream-split n strm)
  (values (stream-take n strm)
          (stream-drop n strm)))

(define-stream (stream-unique eql? strm)
  (if (stream-null? strm)
      stream-null
      (stream-cons (stream-car strm)
        (stream-unique eql?
          (stream-drop-while
            (lambda (x)
              (eql? (stream-car strm) x))
            strm)))))

(define (stream-maximum lt? strm)
  (stream-fold
    (lambda (x y) (if (lt? x y) y x))
    (stream-car strm)
    (stream-cdr strm))) 

(define (stream-fold-one proc strm)
  (stream-fold proc
    (stream-car strm)
    (stream-cdr strm)))

(define (stream-minimum lt? strm)
  (stream-fold-one
    (lambda (x y) (if (lt? x y) x y))
    strm))

(define-stream (isort lt? strm)
    (define-stream (insert strm x)
      (stream-match strm
        (() (stream x))
        ((y . ys)
          (if (lt? y x)
              (stream-cons y (insert ys x))
              (stream-cons x strm)))))
    (stream-fold insert stream-null strm))

(define (display-file filename)
  (stream-for-each display
    (file->stream filename)))

(define-stream (stream-member eql? obj strm)
  (stream-let loop ((strm strm))
    (cond ((stream-null? strm) #f)
          ((eql? obj (stream-car strm)) strm)
          (else (loop (stream-cdr strm))))))

(define (sigma f m n)
  (st?eam-fold + 0
    (stream-map f (stream-range m (+ n 1)))))

(define-stream (stream-merge lt? . strms)
  (define-stream (merge xx yy)
    (stream-match xx (() yy) ((x . xs)
      (stream-match yy (() xx) ((y . ys)
        (if (lt? y x)
            (stream-cons y (merge xx ys))
            (stream-cons x (merge xs yy))))))))
  (stream-let loop ((strms strms))
    (cond ((null? strms) stream-null)
          ((null? (cdr strms)) (car strms))
          (else (merge (car strms)
                       (apply stream-merge lt?
                         (cdr strms)))))))

(define (fact n)
  (stream-ref
    (stream-scan * 1 (stream-from 1)))
    n)

(define-stream (msort lt? strm)
  (let* ((n (quotient (stream-length strm) 2))
         (ts (stream-take n strm))
         (ds (stream-drop n strm)))
    (if (zero? n)
        strm
        (stream-merge lt?
          (msort < ts) (msort < ds)))))

(define (stream-partition pred? strm)
  (stream-unfolds
    (lambda (s)
      (if (stream-null? s)
          (values s '() '())
          (let ((a (stream-car s))
                (d (stream-cdr s)))
            (if (pred? a)
                (values d (list a) #f)
                (values d #f (list a))))))
    strm)) 

(define-stream (stream-finds eql? obj strm)
  (stream-of (car x)
    (x in (stream-zip (stream-from 0) strm))
    (eql? obj (cadr x))))

(define (stream-find eql? obj strm)
  (stream-car
    (stream-append
      (stream-finds eql? obj strm)
      (stream #f))))

(define-stream (stream-remove pred? strm)
  (stream-filter (negate pred?) strm))

(define stream-sum (lsec stream-fold + 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; infinite streams

(define nats
  (stream-cons 0
    (stream-map add1 nats)))

(define fibs
  (stream-cons 1
    (stream-cons 1
      (stream-map +
        fibs
        (stream-cdr fibs)))))

(define hamming
  (stream-unique =
    (stream-cons 1
      (stream-merge <
        (stream-map (lsec * 2) hamming)
        (stream-merge <
          (stream-map (lsec * 3) hamming)
          (stream-map (lsec * 5) hamming))))))

(define power-table
  (stream-of
    (stream-of (expt m n)
      (m in (stream-from 1)))
      (n in (stream-from 2))))

(define primes (let ()
  (define-stream (next base mult strm)
    (let ((first (stream-car strm))
          (rest (stream-cdr strm)))
      (cond ((< first mult)
              (stream-cons first
                (next base mult rest)))
            ((< mult first)
              (next base (+ base mult) strm))
            (else (next base
                    (+ base mult) rest)))))
  (define-stream (sift base strm)
    (next base (+ base base) strm))
  (define-stream (sieve strm)
    (let ((first (stream-car strm))
          (rest (stream-cdr strm)))
      (stream-cons first
        (sieve (sift first rest)))))
  (sieve (stream-from 2))))


(define rats
  (stream-iterate
    (lambda (x)
      (let* ((n (floor x))
             (y (- x n)))
        (/ (- n -1 y))))
    1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; backtracking via the stream of successes

(define (check? i j m n)
  (or (= j n)
      (= (+ i j) (+ m n))
      (= (- i j) (- m n))))

(define (stream-and strm)
  (let loop ((strm strm))
    (cond ((stream-null? strm) #t)
          ((not (stream-car strm)) #f)
          (else (loop (stream-cdr strm))))))

(define (safe? p n)
  (let* ((len (stream-length p))
         (m (+ len 1)))
    (stream-and
      (stream-of
        (not (check? (car ij) (cadr ij) m n))
          (ij in (stream-zip
                   (stream-range 1 m)
                   p))))))

(define (queens m)
  (if (zero? m)
      (stream (stream))
      (stream-of (stream-append p (stream n))
        (p in (queens (- m 1)))
        (n in (stream-range 1 9))
        (safe? p n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; generators and co-routines

(define-stream (flatten tree)
  (cond ((null? tree) stream-nu?l)
        ((pair? (car tree))
          (stream-append
            (flatten (car tree))
            (flatten (cdr tree))))
        (else (stream-cons
                (car tree)
                (flatten (cdr tree))))))

(define (same-fringe? eql? tree1 tree2)
  (let loop ((t1 (flatten tree1))
             (t2 (flatten tree2)))
    (cond ((and (stream-null? t1)
                (stream-null? t2)) #t)
          ((or  (stream-null? t1)
                (stream-null? t2)) #f)
          ((not (eql? (stream-car t1)
                      (stream-car t2))) #f)
          (else (loop (stream-cdr t1)
                      (stream-cdr t2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; a pipeline of procedures

; Gettysburg Address -- Abraham Lincoln -- November 19, 1863
(define getty (list->stream (string->list (string-append
"Four score and seven years ago our fathers brought forth\n"
"on this continent a new nation, conceived in Liberty, and\n"
"dedicated to the proposition that all men are created equal.\n" 
"\n"
"Now we are engaged in a great civil war, testing whether\n"
"that nation, or any nation, so conceived and so dedicated,\n"
"can long endure.  We are met on a great battlefield of that\n"
"war.  We have come to dedicate a portion of that field, as\n"
"a final resting place for those who here gave their lives\n"
"that that nation might live.  It is altogether fitting and\n"
"proper that we should do this.\n"
"\n"
"But, in a larger sense, we can not dedicate -? we can not\n"
"consecrate -? we can not hallow ?- this ground.  The brave\n"
"men, living and dead, who struggled here, have consecrated\n"
"it, far above our poor power to add or detract.  The world\n"
"will little note, nor long remember what we say here, but\n"
"it can never forget what they did here.  It is for us the\n"
"living, rather, to be dedicated here to the unfinished work\n"
"which they who fought here have thus far so nobly advanced.\n"
"It is rather for us to be here dedicated to the great task\n"
"remaining before us ?- that from these honored dead we take\n"
"increased devotion to that cause for which they gave the\n"
"last full measure of devotion ?- that we here highly resolve\n"
"that these dead shall not have died in vain ?- that this\n"
"nation, under God, shall have a new birth of freedom ?- and\n"
"that government of the people, by the people, for the people,\n"
"shall not perish from the earth.\n"))))

(define (stream-fold-right f base strm) 
  (if (stream-null? strm)
      base
      (f (stream-car strm)
         (stream-fold-right f base
           (stream-cdr strm)))))

(define (stream-fold-right-one f strm)
  (stream-match strm
  ((x) x)
  ((x . xs)
    (f x (stream-fold-right-one f xs)))))

(define (breakon a)
  (stream-lambda (x xss)
    (if (equal? a x)
        (stream-append (stream (stream)) xss)
        (stream-append
          (stream (stream-append
              (stream x) (stream-car xss)))
          (stream-cdr xss)))))

(define-stream (lines strm) 
  (stream-fold-right
    (breakon #\newline)
    (stream (stream))
    strm))

(define-stream (words strm)
  (stream-filter stream-pair?
    (stream-fold-right
      (breakon #\space)
      (stream (stream))
      strm)))

(define-stream (paras strm)
  (stream-filter stream-pair?
    (stream-fold-right
      (breakon stream-null)
      (stream (stream))
      strm)))

(define (insert a)
  (stream-lambda (xs ys)
    (stream-append xs (stream a) ys)))

(define unlines
  (lsec stream-fold-right-one
    (insert #\newline)))

(define unwords
  (lsec stream-fold-right-one
    (insert #\space)))

(define unparas
  (lsec stream-fold-right-one
    (insert stream-null)))

(define countlines
  (compose stream-length lines))

(define countwords
  (compose stream-length
           stream-concat
           (lsec stream-map words)
           lines))

(define countparas
  (compose stream-length paras lines))

(define parse
  (compose (lsec stream-map
             (lsec stream-map words))
           paras
           lines))

(define unparse
  (compose unlines
           unparas
           (lsec stream-map
             (lsec stream-map unwords))))

(define normalize (compose unparse parse))

(define (greedy m ws)
  (- (stream-length
       (stream-take-while (rsec <= m)
         (stream-scan
           (lambda (n word)
             (+ n (stream-length word) 1))
           -1
           ws))) 1))

(define-stream (fill m ws)
  (if (stream-null? ws)
      stream-null
      (let* ((n (greedy m ws))
             (fstline (stream-take n ws))
             (rstwrds (stream-drop n ws)))
        (stream-append
          (stream fstline)
          (fill m rstwrds)))))

(define linewords
  (compose stream-concat
           (lsec stream-map words)))

(define textparas
  (compose (lsec stream-map linewords)
           paras
           lines))

(define (filltext m strm)
  (unparse (stream-map (lsec fill m) (textparas strm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; persistent queues

(define queue-null
  (cons stream-null stream-null))

(define (queue-null? x)
  (and (pair? x) (stream-null (car x))))

(define (queue-check f r)
  (if (< (stream-length r) (stream-length f))
      (cons f r)
      (cons (stream-append f (stream-reverse r))
            stream-null)))

(define (queue-snoc q x)
  (queue-check (car q) (stream-cons x (cdr q))))

(define (queue-head q)
  (if (stream-null? (car q))
      (error "empty queue: head")
      (stream-car (car q))))

(define (queue-tail q)
  (if (stream-null? (car q))
      (error "empty queue: tail")
      (queue-check (stream-cdr (car q))
                   (cdr q))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; reducing two passes to one

(define requests
  (stream
    '(get 3)
    '(put 1 "a")    ; use follows definition
    '(put 3 "c")    ; use precedes definition
    '(get 1)
    '(get 2)
    '(put 2 "b")    ; use precedes definition
    '(put 4 "d")))  ; unused definition

(define (get? obj) (eq? (car obj) 'get))

(define-stream (gets strm)
  (stream-map cadr (stream-filter get? strm)))

(define-stream (puts strm)
  (stream-map cdr  (stream-remove get? strm)))

(define-stream (run-dict requests)
  (let ((dict (build-dict (puts requests))))
    (stream-map (rsec stream-assoc dict)
      (gets requests))))

(define (stream-assoc key dict)
    (cond ((stream-null? dict) #f)
          ((equal? key (car (stream-car dict)))
            (stream-car dict))
          (else (stream-assoc key
                  (stream-cdr dict)))))

(define-stream (build-dict puts)
  (if (stream-null? puts)
      stream-null
      (stream-cons
        (stream-car puts)
        (build-dict (stream-cdr puts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pitfalls

(define (stream-equal? eql? xs ys)
  (cond ((and (stream-null? xs)
              (stream-null? ys)) #t)
        ((or (stream-null? xs)
             (stream-null? ys)) #f)
        ((not (eql? (stream-car xs)
                    (stream-car ys))) #f)
        (else (stream-equal? eql?
                (stream-cdr xs)
                (stream-cdr ys)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; unit tests

(define-syntax assert
  (syntax-rules ()
    ((assert expr result)
      (if (not (equal? expr result))
          (begin
            (newline) (display "failed assertion:") (newline)
            (display 'expr) (newline)
            (display "expected: ") (display result) (newline)
            (display "returned: ") (display expr) (newline))))
    ((assert descrip expr result)
      (if (not (equal? expr result))
          (begin
            (newline) (display "failed assertion: ") (display descrip) (newline)
            (display 'expr) (newline)
            (display "expected: ") (display result) (newline)
            (display "returned: ") (display ex?r) (newline))))))

(define strm123 (stream 1 2 3))

; uncomment next line only for testing
; (define (error s x) (string-append (symbol->string s) ": " x))

; executing (unit-test) should produce no output
(define (unit-test)
  
  ; stream-null
  (assert (stream? stream-null) #t)
  (assert (stream-null? stream-null) #t)
  (assert (stream-pair? stream-null) #f)
  
  ; stream-cons
  (assert (stream? (stream-cons 1 stream-null)) #t)
  (assert (stream-null? (stream-cons 1 stream-null)) #f)
  (assert (stream-pair? (stream-cons 1 stream-null)) #t)
  
  ; stream?
  (assert (stream? stream-null) #t)
  (assert (stream? (stream-cons 1 stream-null)) #t)
  (assert (stream? "four") #f)
  
  ; stream-null?
  (assert (stream-null? stream-null) #t)
  (assert (stream-null? (stream-cons 1 stream-null)) #f)
  (assert (stream-null? "four") #f)
  
  ; stream-pair?
  (assert (stream-pair? stream-null) #f)
  (assert (stream-pair? (stream-cons 1 stream-null)) #t)
  (assert (stream-pair? "four") #f)
  
  ; stream-car
  (assert (stream-car "four") "stream-car: non-stream")
  (assert (stream-car stream-null) "stream-car: null stream")
  (assert (stream-car strm123) 1)
  
  ; stream-cdr
  (assert (stream-cdr "four") "stream-cdr: non-stream")
  (assert (stream-cdr stream-null) "stream-cdr: null stream")
  (assert (stream-car (stream-cdr strm123)) 2)
  
  ; stream-lambda
  (assert
    (stream->list
      (letrec ((double
        (stream-lambda (strm)
          (if (stream-null? strm)
              stream-null
              (stream-cons
                (* 2 (stream-car strm))
                (double (stream-cdr strm)))))))
        (double strm123)))
    '(2 4 6))
  
  ; define-stream
  (assert
    (stream->list
      (let ()
        (define-stream (double strm)
          (if (stream-null? strm)
              stream-null
              (stream-cons
                (* 2 (stream-car strm))
                (double (stream-cdr strm)))))
        (double strm123)))
    '(2 4 6))
  
  ; list->stream
  (assert (list->stream "four") "list->stream: non-list argument")
  (assert (stream->list (list->stream '())) '())
  (assert (stream->list (list->stream '(1 2 3))) '(1 2 3))
  
  ; port->stream
  (let* ((p (open-input-file "streams.ss"))
         (s (port->stream p)))
    (assert (port->stream "four") "port->stream: non-input-port argument")
    (assert (list->string (stream->list 12 s)) "; STREAMS.SS")
    (close-input-port p))
  
  ; stream
  (assert (stream->list (stream)) '())
  (assert (stream->list (stream 1)) '(1))
  (assert (stream->list (stream 1 2 3)) '(1 2 3))
  
  ; stream->list
  (assert (stream->list '()) "stream->list: non-stream argument")
  (assert (stream->list "four" strm123) "stream->list: non-integer count")
  (assert (stream->list -1 strm123) "stream->list: negative count")
  (assert (stream->list (stream)) '())
  (assert (stream->list strm123) '(1 2 3))
  (assert (stream->list 5 strm123) '(1 2 3))
  (assert (stream->list 3 (stream-from 1)) '(1 2 3))
  
  ; stream-append
  (assert (stream-append "four") "stream-append: non-stream argument")
  (assert (stream->list (stream-append strm123)) '(1 2 3))
  (assert (stream->list (stream-append strm123 strm123)) '(1 2 3 1 2 3))
  (assert (stream->list (stream-append strm123 strm123 strm123)) '(1 2 3 1 2 3 1 2 3))
  (assert (stream->list (stream-append strm123 stream-null)) '(1 2 3))
  (assert (stream->list (stream-append stream-null strm123)) '(1 2 3))
  
  ; stream-concat
  (assert (stream-concat "four") "stream-concat: non-stream argument")
  (assert (stream->list (stream-concat (stream strm123))) '(1 2 3))
  (assert (stream->list (stream-concat (stream strm123 strm123))) '(1 2 3 1 2 3))
  
  ; stream-constant
  (assert (stream-ref (stream-constant 1) 100) 1)
  (assert (stream-ref (stream-constant 1 2) 100) 1)
  (assert (stream-ref (stream-constant 1 2 3) 3) 1)
  
  ; stream-drop
  (assert (stream-drop "four" strm123) "stream-drop: non-integer argument")
  (assert (stream-drop -1 strm123) "stream-drop: negative argument")
  (assert (stream-drop 2 "four") "stream-drop: non-stream argument")
  (assert (stream->list (stream-drop 0 stream-null)) '())
  (assert (stream->list (stream-drop 0 strm123)) '(1 2 3))
  (assert (stream->list (stream-drop 1 strm123)) '(2 3))
  (assert (stream->list (stream-drop 5 strm123)) '())
  
  ; stream-drop-while
  (assert (stream-drop-while "four" strm123) "stream-drop-while: non-procedural argument")
  (assert (stream-drop-while odd? "four") "stream-drop-while: non-stream argument")
  (assert (stream->list (stream-drop-while odd? stream-null)) '())
  (assert (stream->list (stream-drop-while odd? strm123)) '(2 3))
  (assert (stream->list (stream-drop-while even? strm123)) '(1 2 3))
  (assert (stream->list (stream-drop-while positive? strm123)) '())
  (assert (stream->list (stream-drop-while negative? strm123)) '(1 2 3))
  
  ; stream-filter
  (assert (stream-filter "four" strm123) "stream-filter: non-procedural argument")
  (assert (stream-filter odd? '()) "stream-filter: non-stream argument")
  (assert (stream-null? (stream-filter odd? (stream))) #t)
  (assert (stream->list (stream-filter odd? strm123)) '(1 3))
  (assert (stream->list (stream-filter even? strm123)) '(2))
  (assert (stream->list (stream-filter positive? strm123)) '(1 2 3))
  (assert (stream->list (stream-filter negative? strm123)) '())
  (let loop ((n 10))
    (assert (odd? (stream-ref (stream-filter odd? (stream-from 0)) n)) #t)
    (if (positive? n) (loop (- n 1))))
  (let loop ((n 10))
    (assert (even? (stream-ref (stream-filter odd? (stream-from 0)) n)) #f)
    (if (positive? n) (loop (- n 1))))
  
  ; stream-fold
  (assert (stream-fold "four" 0 strm123) "stream-fold: non-procedural argument")
  (assert (stream-fold + 0 '()) "stream-fold: non-stream argument")
  (assert (stream-fold + 0 strm123) 6)
  
  ; stream-for-each
  (assert (stream-for-each "four" strm123) "stream-for-each: non-procedural argument")
  (assert (stream-for-each display) "stream-for-each: no stream arguments")
  (assert (stream-for-each display "four") "stream-for-each: non-stream argument")
  (assert (let ((sum 0)) (stream-for-each (lambda (x) (set! sum (+ sum x))) strm123) sum) 6)

  ; stream-from
  (assert (stream-from "four") "stream-from: non-numeric starting number")
  (assert (stream-from 1 "four") "stream-from: non-numeric step size")
  (assert (stream-ref (stream-from 0) 100) 100)
  (assert (stream-ref (stream-from 1 2) 100) 201)
  (assert (stream-ref (stream-from 0 -1) 100) -100)
  
  ; stream-iterate
  (assert (stream-iterate "four" 0) "stream-iterate: non-procedural argument")
  (assert (stream->list 3 (stream-iterate (lsec + 1) 1)) '(1 2 3))
  
  ; stream-length
  (assert (stream-length "four") "stream-length: non-stream argument")
  (assert (stream-length (stream)) 0)
  (assert (stream-length strm123) 3)
  
  ; stream-let
  (assert (stream->list
            (stream-let loop ((strm strm123))
              (if (stream-null? strm)
                  stream-null
                  (stream-cons
                    (* 2 (stream-car strm))
                    (loop (stream-cdr strm))))))
          '(2 4 6))
  
  ; stream-map
  (assert (stream-map "four" strm123) "stream-map: non-procedural argument")
  (assert (stream-map odd?) "stream-map: no stream arguments")
  (assert (stream-map odd? "four") "stream-map: non-stream argument")
  (assert (stream->list (stream-map - strm123)) '(-1 -2 -3))
  (assert (stream->list (stream-map + strm123 strm123)) '(2 4 6))
  (assert (stream->list (stream-map + strm123 (stream-from 1))) '(2 4 6))
  (assert (stream->list (stream-map + (stream-from 1) strm123)) '(2 4 6))
  (assert (stream->list (stream-map + strm123 strm123 strm123)) '(3 6 9))
  
  ; stream-match
  (assert (stream-match '(1 2 3) (_ 'ok)) "stream-match: non-stream argument")
  (assert (stream-match strm123 (() 42)) "stream-match: pattern failure")
  (assert (stream-match stream-null (() 'ok)) 'ok)
  (assert (stream-match strm123 (() 'no) (else 'ok)) 'ok)
  (assert (stream-match (stream 1) (() 'no) ((a) a)) 1)
  (assert (stream-match (stream 1) (() 'no) ((_) 'ok)) 'ok)
  (assert (stream-match strm123 ((a b c) (list a b c))) '(1 2 3))
  (assert (stream-match strm123 ((a . _) a)) 1)
  (assert (stream-match strm123 ((a b . _) (list a b))) '(1 2))
  (assert (stream-match strm123 ((a b . c) (list a b (stream-car c)))) '(1 2 3))
  (assert (stream-match strm123 (s (stream->list s))) '(1 2 3))
  (assert (stream-match strm123 ((a . _) (= a 1) 'ok)) 'ok)
  (assert (stream-match strm123 ((a . _) (= a 2) 'yes) (_ 'no)) 'no)
  (assert (stream-match strm123 ((a b c) (= a b) 'yes) (_ 'no)) 'no)
  (assert (stream-match (stream 1 1 2) ((a b c) (= a b) 'yes) (_ 'no)) 'yes)
  
  ; stream-of
  (assert (stream->list
            (stream-of (+ y 6)
              (x in (stream-range 1 6))
              (odd? x)
              (y is (* x x)))) '(7 15 31))
  (assert (stream->list
            (stream-of (* x y)
              (x in (stream-range 1 4))
              (y in (stream-range 1 5))))
          '(1 2 3 4 2 4 6 8 3 6 9 12))
  (assert (stream-car (stream-of 1)) 1)

  ; stream-range
  (assert (stream-range "four" 0) "stream-range: non-numeric starting number")
  (assert (stream-range 0 "four") "stream-range: non-numeric ending number")
  (assert (stream-range 1 2 "three") "stream-range: non-numeric step size")
  (assert (stream->list (stream-range 0 5)) '(0 1 2 3 4))
  (assert (stream->list (stream-range 5 0)) '(5 4 3 2 1))
  (assert (stream->list (stream-range 0 5 2)) '(0 2 4))
  (assert (stream->list (stream-range 5 0 -2)) '(5 3 1))
  (assert (stream->list (stream-range 0 1 -1)) '())
  
  ; stream-ref
  (assert (stream-ref '() 4) "stream-ref: non-stream argument")
  (assert (stream-ref nats 3.5) "stream-ref: non-integer argument")
  (assert (stream-ref nats -3) "stream-ref: negative argument")
  (assert (stream-ref strm123 5) "stream-ref: beyond end of stream")
  (assert (stream-ref strm123 0) 1)
  (assert (stream-ref strm123 1) 2)
  (assert (stream-ref strm123 2) 3)
  
  ; stream-reverse
  (assert (stream-reverse '()) "stream-reverse: non-stream argument")
  (assert (stream->list (stream-reverse (stream))) '())
  (assert (stream->list (stream-reverse strm123)) '(3 2 1))
  
  ; stream-scan
  (assert (stream-scan "four" 0 strm123) "stream-scan: non-procedural argument")
  (assert (stream-scan + 0 '()) "stream-scan: non-stream argument")
  (assert (stream->list (stream-scan + 0 strm123)) '(0 1 3 6))
  
  ; stream-take
  (assert (stream-take 5 "four") "stream-take: non-stream argument")
  (assert (stream-take "four" strm123) "stream-take: non-integer argument")
  (assert (stream-take -4 strm123) "stream-take: negative argument")
  (assert (stream->list (stream-take 5 stream-null)) '())
  (assert (stream->list (stream-take 0 stream-null)) '())
  (assert (stream->list (stream-take 0 strm123)) '())
  (assert (stream->list (stream-take 2 strm123)) '(1 2))
  (assert (stream->list (stream-take 3 strm123)) '(1 2 3))
  (assert (stream->list (stream-take 5 strm123)) '(1 2 3))
  
  ; stream-take-while
  (assert (stream-take-while odd? "four") "stream-take-while: non-stream argument")
  (assert (stream-take-while "four" strm123) "stream-take-while: non-procedural argument")
  (assert (stream->list (stream-take-while odd? strm123)) '(1))
  (assert (stream->list (stream-take-while even? strm123)) '())
  (assert (stream->list (stream-take-while positive? strm123)) '(1 2 3))
  (assert (stream->list (stream-take-while negative? strm123)) '())
  
  ; stream-unfold
  (assert (stream-unfold "four" odd? + 0) "stream-unfold: non-procedural mapper")
  (assert (stream-unfold + "four" + 0) "stream-unfold: non-procedural pred?")
  (assert (stream-unfold + odd? "four" 0) "stream-unfold: non-procedural generator")
  (assert (stream->list (stream-unfold (rsec expt 2) (rsec < 10) (rsec + 1) 0))
          '(0 1 4 9 16 25 36 49 64 81))
  
  ; stream-unfolds
  (assert
    (stream->list
      (stream-unfolds
        (lambda (x)
          (let ((n (car x)) (s (cdr x)))
            (if (zero? n)
                (values 'dummy '())
                (values
                  (cons (- n 1) (stream-cdr s))
                  (list (stream-car s))))))
        (cons 5 (stream-from 0))))
      '(0 1 2 3 4))
  
  ; stream-zip
  (assert (stream-zip) "stream-zip: no stream arguments")
  (assert (stream-zip "four") "stream-zip: non-stream argument")
  (assert (stream-zip strm123 "four") "stream-zip: non-stream argument")
  (assert (stream->list (stream-zip strm123 stream-null)) '())
  (assert (stream->list (stream-zip strm123)) '((1) (2) (3)))
  (assert (stream->list (stream-zip strm123 strm123)) '((1 1) (2 2) (3 3)))
  (assert (stream->list (stream-zip strm123 (stream-from 1))) '((1 1) (2 2) (3 3)))
  (assert (stream->list (stream-zip strm123 strm123 strm123)) '((1 1 1) (2 2 2) (3 3 3)))
  
  ; other tests
  (assert
    (stream-car
      (stream-reverse
        (stream-take-while
          (rsec < 1000)
          primes)))
    997)
  
  (assert
    (equal?
      (stream->list (qsort < (stream 3 1 5 2 4)))
      (stream->list (isort < (stream 2 5 1 4 3))))
    #t)
  
  (assert
    (equal?
      (stream->list (msort < (stream 3 1 5 2 4)))
      (stream->list (isort < (stream 2 5 1 4 3))))
    #t)
  
  ; http://www.research.att.com/~njas/sequences/A051037
  (assert (stream-ref hamming 999) 51200000)
  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; leak tests

; These tests can't be automated with portable code, so they need to be run by hand.
; Thus, they have been commented out.  To run the tests, uncomment them one by one,
; load them into a running Scheme system, and monitor space consumption using some
; os-specific tool outside the Scheme system.  All should run in bounded space.

; traversing a stream should take bounded space ...
; (define-stream (traverse s) (traverse (stream-cdr s)))
; (stream-ref (traverse (stream-from 0)) 10000000)

; ... even if something holds the head of the stream
; (define s (traverse (stream-from 0)))
; (stream-ref s 10000000)

; the infamous times3 test from SRFI-40
; (define (times3 n)
;   (stream-ref
;     (stream-filter
;       (lambda (x)
;         (zero? (modulo x n)))
;       (stream-from 0))
;     3))
; (times3 10000000)
