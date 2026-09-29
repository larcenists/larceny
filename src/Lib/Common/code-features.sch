; Copyright 2026 Felix S Klock II
;
; Code features: what compiled code needs from the run-time system and
; what it promises to it.
;
; Compiled files begin with a marker that records the features of the
; code in them:
;
;     #lap((assumes <feature> ...) (guarantees <feature> ...))
;     #lop((assumes <feature> ...) (guarantees <feature> ...))
;
; at the head of .lap and .lop files, and #lop(...) after #!fasl in
; .fasl files.  A feature is a symbol or a list whose car is a symbol,
; such as (word-size 64).
;
; - The code *assumes* features of the run-time system: a target and
;   calling convention, a word size, millicode entry points, and so on.
;   The code can be loaded into any system that provides a superset of
;   them.  A system may provide several targets, say.
;
; - The code *guarantees* properties the run-time system may rely on,
;   such as a write barrier or the SATB pre-barrier the regional
;   collector needs.  The code can be loaded into any system whose
;   current requirements (which may depend on the collector in use) are
;   a subset of them.
;
; Other clauses are kept but have no meaning yet.

($$trace "code-features")

(define code-features:rtd
  (make-record-type-descriptor 'code-features #f #f #t #f
   '#((immutable kind)          ; lap or lop
      (immutable clauses))))    ; list of (name <feature> ...)

(define code-features:make (record-constructor code-features:rtd))
(define code-features? (record-predicate code-features:rtd))
(define code-features-kind (record-accessor code-features:rtd 0))
(define code-features-clauses (record-accessor code-features:rtd 1))

; Kind is lap or lop, clauses is the list of data between the
; parentheses of the marker.

(define (list->code-features kind clauses)
  (define (feature? x)
    (or (symbol? x)
        (and (pair? x) (symbol? (car x)) (list? x))))
  (if (not (memq kind '(lap lop)))
      (assertion-violation 'list->code-features "bad kind" kind))
  (for-each (lambda (clause)
              (if (not (and (pair? clause)
                            (symbol? (car clause))
                            (list? clause)
                            (every? feature? (cdr clause))))
                  (assertion-violation 'list->code-features
                                       "bad clause" clause)))
            clauses)
  (code-features:make kind clauses))

(define (make-code-features kind assumes guarantees)
  (list->code-features kind
                       (list (cons 'assumes assumes)
                             (cons 'guarantees guarantees))))

(define (code-features-clause cf name)
  (let ((probe (assq name (code-features-clauses cf))))
    (if probe (cdr probe) '())))

(define (code-features-assumes cf)
  (code-features-clause cf 'assumes))

(define (code-features-guarantees cf)
  (code-features-clause cf 'guarantees))

(define (write-code-features cf port)
  (write-string (if (eq? (code-features-kind cf) 'lap) "#lap(" "#lop(") port)
  (let loop ((clauses (code-features-clauses cf)) (first? #t))
    (if (pair? clauses)
        (begin (if (not first?) (write-char #\space port))
               (write (car clauses) port)
               (loop (cdr clauses) #f))))
  (write-char #\) port))

(rtd-printer-set! code-features:rtd write-code-features)

; What the running system provides and requires.
;
; The system provides what the code in its heap assumes: the build
; records the features of the heap's code in larceny:heap-code-features
; (see heap-code-features.sch, which the build generates).
;
; What the system requires of code depends on the collector: the
; generational collector needs the write barrier, and the regional
; collector also needs the SATB pre-barrier for its concurrent marking.

(define (system-code-features)
  (list->code-features 'lop larceny:heap-code-features))

; Not cached: a heap may be dumped and started with another collector.

(define (system-code-requirements)
  (case (cdr (assq 'gc-technology (system-features)))
    ((regional) '(write-barrier satb-barrier))
    ((generational) '(write-barrier))
    (else '())))

; Code compiled before the markers existed: it has the write barrier but
; not the SATB pre-barrier, and its assumptions are unknown.

(define (legacy-code-features)
  (make-code-features 'lop '() '(write-barrier)))

; Returns a list of problems that keep code with the features cf from
; running in this system: (assumes <feature>) for each feature it
; assumes that the system does not provide, and (lacks <feature>) for
; each feature the system requires that it does not guarantee.
; Legacy code's assumptions are not checked.

(define (code-features-problems cf)
  (let ((provided (code-features-assumes (system-code-features)))
        (guaranteed (code-features-guarantees cf)))
    (append
     (let loop ((fs (code-features-assumes cf)) (problems '()))
       (cond ((null? fs) (reverse problems))
             ((member (car fs) provided) (loop (cdr fs) problems))
             (else (loop (cdr fs) (cons (list 'assumes (car fs)) problems)))))
     (let loop ((fs (system-code-requirements)) (problems '()))
       (cond ((null? fs) (reverse problems))
             ((memq (car fs) guaranteed) (loop (cdr fs) problems))
             (else (loop (cdr fs) (cons (list 'lacks (car fs)) problems))))))))

; Signals an error if code with the features cf, or legacy code if cf is
; #f, cannot run in this system.  Where is a file name or #f.

(define (check-code-features! cf where)
  (let ((problems (code-features-problems (or cf (legacy-code-features)))))
    (if (not (null? problems))
        (error 'load
               (if cf
                   "compiled code does not fit this system; recompile it"
                   "compiled code predates feature markers; recompile it")
               where
               problems))))

; eof
