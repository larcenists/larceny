; Soft Scheme -- Copyright (C) 1993, 1994 Andrew K. Wright
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; Packaged as a single file for Larceny by Lars T Hansen.
; Modified 2000-02-15 by lth.
;
; Compilation notes.
; 
; The macro definitions for MATCH in this file depend on the presence of 
; certain helper functions in the compilation environment, eg. match:andmap.
; (That is not a problem when loading this file, but it is an issue when
; compiling it.)  The easiest way to provide the helper functions during
; compilation is to load match.sch into the compilation environment before
; compiling.
;
; Once compiled, this program is self-contained.

; The SoftScheme benchmark performs soft typing on a program and prints
; a diagnostic report.  All screen output is captured in an output
; string port, which is subsequently discarded.  (There is a moderate
; amount of output).  No file I/O occurs while the program is running.

(define (softscheme-benchmark)
  (let ((expr `(begin ,@(readfile "ss-input.scm")))
        (out  (open-output-string)))
    (run-benchmark "softscheme"
                   (lambda () 
                     (with-output-to-port out
                       (lambda ()
                         (soft-def expr #f)))))
    (newline)
    (display (string-length (get-output-string out)))
    (display " characters of output written.")
    (newline)))

;;; Define defmacro, macro?, and macroexpand-1.

(define *macros* '())

(define-syntax
  defmacro
  (transformer
    (lambda (exp rename compare)
      (define (arglist? x)
        (or (symbol? x)
            (null? x)
            (and (pair? x)
                 (symbol? (car x))
                 (arglist? (cdr x)))))
      (if (not (and (list? exp)
                    (>= (length exp) 4)
                    (symbol? (cadr exp))
                    (arglist? (caddr exp))))
        (error "Bad macro definition: " exp))
      (let ((name (cadr exp))
            (args (caddr exp))
            (body (cdddr exp)))
        `(begin
           (define-syntax
             ,name
             (transformer
              (lambda (_defmacro_exp
                       _defmacro_rename
                       _defmacro_compare)
                (apply (lambda ,args ,@body) (cdr _defmacro_exp)))))
           (set! *macros*
                 (cons (cons ',name
                             (lambda (_exp)
                               (apply (lambda ,args ,@body) (cdr _exp))))
                       *macros*))
           )))))

(define (macroexpand-1 exp)
  (cond ((pair? exp)
         (let ((probe (assq (car exp) *macros*)))
           (if probe ((cdr probe) exp) exp)))
        (else exp)))

(define (macro? keyword)
  (and (symbol? keyword) (assq keyword *macros*)))

;;; Other compatibility hacks

(define slib:error error)

(define force-output flush-output-port)

(define format
  (let ((format format))
    (lambda (port . rest)
      (if (not port)
        (let ((s (open-output-string)))
          (apply format s rest)
          (get-output-string s))
        (apply format port rest)))))

(define gentemp
  (let ((gensym gensym)) (lambda () (gensym "G"))))

(define getenv
  (let ((getenv getenv))
    (lambda (x)
      (or (getenv x)
          (if (string=? x "HOME")
            "Ertevann:Desktop folder:"
            #f)))))

;;; The rest of the file should be more or less portable.

(define match-file #f)
(define installation-directory #f)
(define customization-file #f)
(define fastlibrary-file #f)
(define st:version
  "Larceny Version 0.18, April 21, 1995")
(define match:version
  "Version 1.18, July 17, 1995")
(define match:error
  (lambda (val . args)
    (for-each pretty-print args)
    (slib:error "no matching clause for " val)))
(define match:andmap
  (lambda (f l)
    (if (null? l)
      (and)
      (and (f (car l)) (match:andmap f (cdr l))))))
(define match:syntax-err
  (lambda (obj msg) (slib:error msg obj)))
(define match:disjoint-structure-tags '())
(define match:make-structure-tag
  (lambda (name)
    (if (or (eq? match:structure-control 'disjoint)
            match:runtime-structures)
      (let ((tag (gentemp)))
        (set! match:disjoint-structure-tags
          (cons tag match:disjoint-structure-tags))
        tag)
      (string->symbol
        (string-append "<" (symbol->string name) ">")))))
(define match:structure?
  (lambda (tag)
    (memq tag match:disjoint-structure-tags)))
(define match:structure-control 'vector)
(define match:set-structure-control
  (lambda (v) (set! match:structure-control v)))
(define match:set-error
  (lambda (v) (set! match:error v)))
(define match:error-control 'error)
(define match:set-error-control
  (lambda (v) (set! match:error-control v)))
(define match:disjoint-predicates
  (cons 'null
        '(pair? symbol?
                boolean?
                number?
                string?
                char?
                procedure?
                vector?)))
(define match:vector-structures '())
(define match:expanders
  (letrec ((genmatch
             (lambda (x clauses match-expr)
               (let* ((length>= (gentemp))
                      (eb-errf (error-maker match-expr))
                      (blist (car eb-errf))
                      (plist (map (lambda (c)
                                    (let* ((x (bound (validate-pattern
                                                       (car c))))
                                           (p (car x))
                                           (bv (cadr x))
                                           (bindings (caddr x))
                                           (code (gentemp))
                                           (fail (and (pair? (cdr c))
                                                      (pair? (cadr c))
                                                      (eq? (caadr c) '=>)
                                                      (symbol? (cadadr c))
                                                      (pair? (cdadr c))
                                                      (null? (cddadr c))
                                                      (pair? (cddr c))
                                                      (cadadr c)))
                                           (bv2 (if fail (cons fail bv) bv))
                                           (body (if fail (cddr c) (cdr c))))
                                      (set! blist
                                        (cons `(,code (lambda ,bv2 ,@body))
                                              (append bindings blist)))
                                      (list p
                                            code
                                            bv
                                            (and fail (gentemp))
                                            #f)))
                                  clauses))
                      (code (gen x
                                 '()
                                 plist
                                 (cdr eb-errf)
                                 length>=
                                 (gentemp))))
                 (unreachable plist match-expr)
                 (inline-let
                   `(let ((,length>=
                           (lambda (n) (lambda (l) (>= (length l) n))))
                          ,@blist)
                      ,code)))))
           (genletrec
             (lambda (pat exp body match-expr)
               (let* ((length>= (gentemp))
                      (eb-errf (error-maker match-expr))
                      (x (bound (validate-pattern pat)))
                      (p (car x))
                      (bv (cadr x))
                      (bindings (caddr x))
                      (code (gentemp))
                      (plist (list (list p code bv #f #f)))
                      (x (gentemp))
                      (m (gen x
                              '()
                              plist
                              (cdr eb-errf)
                              length>=
                              (gentemp)))
                      (gs (map (lambda (_) (gentemp)) bv)))
                 (unreachable plist match-expr)
                 `(letrec ((,length>=
                            (lambda (n) (lambda (l) (>= (length l) n))))
                           ,@(map (lambda (v) `(,v #f)) bv)
                           (,x ,exp)
                           (,code
                            (lambda ,gs
                              ,@(map (lambda (v g) `(set! ,v ,g)) bv gs)
                              ,@body))
                           ,@bindings
                           ,@(car eb-errf))
                    ,m))))
           (gendefine
             (lambda (pat exp match-expr)
               (let* ((length>= (gentemp))
                      (eb-errf (error-maker match-expr))
                      (x (bound (validate-pattern pat)))
                      (p (car x))
                      (bv (cadr x))
                      (bindings (caddr x))
                      (code (gentemp))
                      (plist (list (list p code bv #f #f)))
                      (x (gentemp))
                      (m (gen x
                              '()
                              plist
                              (cdr eb-errf)
                              length>=
                              (gentemp)))
                      (gs (map (lambda (_) (gentemp)) bv)))
                 (unreachable plist match-expr)
                 `(begin
                    ,@(map (lambda (v) `(define ,v #f)) bv)
                    ,(inline-let
                       `(let ((,length>=
                               (lambda (n) (lambda (l) (>= (length l) n))))
                              (,x ,exp)
                              (,code
                               (lambda ,gs
                                 ,@(map (lambda (v g) `(set! ,v ,g)) bv gs)
                                 (cond (#f #f))))
                              ,@bindings
                              ,@(car eb-errf))
                          ,m))))))
           (pattern-var?
             (lambda (x)
               (and (symbol? x)
                    (not (dot-dot-k? x))
                    (not (memq x
                               '(quasiquote
                                  quote
                                  unquote
                                  unquote-splicing
                                  ?
                                  _
                                  $
                                  =
                                  and
                                  or
                                  not
                                  set!
                                  get!
                                  ...
                                  ___))))))
           (dot-dot-k?
             (lambda (s)
               (and (symbol? s)
                    (if (memq s '(... ___))
                      0
                      (let* ((s (symbol->string s)) (n (string-length s)))
                        (and (<= 3 n)
                             (memq (string-ref s 0) '(#\. #\_))
                             (memq (string-ref s 1) '(#\. #\_))
                             (match:andmap
                               char-numeric?
                               (string->list (substring s 2 n)))
                             (string->number (substring s 2 n))))))))
           (error-maker
             (lambda (match-expr)
               (cond ((eq? match:error-control 'unspecified)
                      (cons '() (lambda (x) `(cond (#f #f)))))
                     ((memq match:error-control '(error fail))
                      (cons '() (lambda (x) `(match:error ,x))))
                     ((eq? match:error-control 'match)
                      (let ((errf (gentemp)) (arg (gentemp)))
                        (cons `((,errf
                                 (lambda (,arg)
                                   (match:error ,arg ',match-expr))))
                              (lambda (x) `(,errf ,x)))))
                     (else
                      (match:syntax-err
                        '(unspecified error fail match)
                        "invalid value for match:error-control, legal values are")))))
           (unreachable
             (lambda (plist match-expr)
               (for-each
                 (lambda (x)
                   (if (not (car (cddddr x)))
                     (begin
                       (display "Warning: unreachable pattern ")
                       (display (car x))
                       (display " in ")
                       (display match-expr)
                       (newline))))
                 plist)))
           (validate-pattern
             (lambda (pattern)
               (letrec ((simple?
                          (lambda (x)
                            (or (string? x)
                                (boolean? x)
                                (char? x)
                                (number? x)
                                (null? x))))
                        (ordinary
                          (lambda (p)
                            (let ((g88 (lambda (x y)
                                         (cons (ordinary x) (ordinary y)))))
                              (if (simple? p)
                                ((lambda (p) p) p)
                                (if (equal? p '_)
                                  ((lambda () '_))
                                  (if (pattern-var? p)
                                    ((lambda (p) p) p)
                                    (if (pair? p)
                                      (if (equal? (car p) 'quasiquote)
                                        (if (and (pair? (cdr p))
                                                 (null? (cddr p)))
                                          ((lambda (p) (quasi p)) (cadr p))
                                          (g88 (car p) (cdr p)))
                                        (if (equal? (car p) 'quote)
                                          (if (and (pair? (cdr p))
                                                   (null? (cddr p)))
                                            ((lambda (p) p) p)
                                            (g88 (car p) (cdr p)))
                                          (if (equal? (car p) '?)
                                            (if (and (pair? (cdr p))
                                                     (list? (cddr p)))
                                              ((lambda (pred ps)
                                                 `(? ,pred
                                                     ,@(map ordinary ps)))
                                               (cadr p)
                                               (cddr p))
                                              (g88 (car p) (cdr p)))
                                            (if (equal? (car p) '=)
                                              (if (and (pair? (cdr p))
                                                       (pair? (cddr p))
                                                       (null? (cdddr p)))
                                                ((lambda (sel p)
                                                   `(= ,sel ,(ordinary p)))
                                                 (cadr p)
                                                 (caddr p))
                                                (g88 (car p) (cdr p)))
                                              (if (equal? (car p) 'and)
                                                (if (and (list? (cdr p))
                                                         (pair? (cdr p)))
                                                  ((lambda (ps)
                                                     `(and ,@(map ordinary
                                                                  ps)))
                                                   (cdr p))
                                                  (g88 (car p) (cdr p)))
                                                (if (equal? (car p) 'or)
                                                  (if (and (list? (cdr p))
                                                           (pair? (cdr p)))
                                                    ((lambda (ps)
                                                       `(or ,@(map ordinary
                                                                   ps)))
                                                     (cdr p))
                                                    (g88 (car p) (cdr p)))
                                                  (if (equal? (car p) 'not)
                                                    (if (and (list? (cdr p))
                                                             (pair? (cdr p)))
                                                      ((lambda (ps)
                                                         `(not ,@(map ordinary
                                                                      ps)))
                                                       (cdr p))
                                                      (g88 (car p) (cdr p)))
                                                    (if (equal? (car p) '$)
                                                      (if (and (pair? (cdr p))
                                                               (symbol?
                                                                 (cadr p))
                                                               (list? (cddr p)))
                                                        ((lambda (r ps)
                                                           `($ ,r
                                                               ,@(map ordinary
                                                                      ps)))
                                                         (cadr p)
                                                         (cddr p))
                                                        (g88 (car p) (cdr p)))
                                                      (if (equal?
                                                            (car p)
                                                            'set!)
                                                        (if (and (pair? (cdr p))
                                                                 (pattern-var?
                                                                   (cadr p))
                                                                 (null? (cddr p)))
                                                          ((lambda (p) p) p)
                                                          (g88 (car p)
                                                               (cdr p)))
                                                        (if (equal?
                                                              (car p)
                                                              'get!)
                                                          (if (and (pair? (cdr p))
                                                                   (pattern-var?
                                                                     (cadr p))
                                                                   (null? (cddr p)))
                                                            ((lambda (p) p) p)
                                                            (g88 (car p)
                                                                 (cdr p)))
                                                          (if (equal?
                                                                (car p)
                                                                'unquote)
                                                            (g88 (car p)
                                                                 (cdr p))
                                                            (if (equal?
                                                                  (car p)
                                                                  'unquote-splicing)
                                                              (g88 (car p)
                                                                   (cdr p))
                                                              (if (and (pair? (cdr p))
                                                                       (dot-dot-k?
                                                                         (cadr p))
                                                                       (null? (cddr p)))
                                                                ((lambda (p
                                                                          ddk)
                                                                   `(,(ordinary
                                                                        p)
                                                                     ,ddk))
                                                                 (car p)
                                                                 (cadr p))
                                                                (g88 (car p)
                                                                     (cdr p)))))))))))))))
                                      (if (vector? p)
                                        ((lambda (p)
                                           (let* ((pl (vector->list p))
                                                  (rpl (reverse pl)))
                                             (apply vector
                                                    (if (and (not (null? rpl))
                                                             (dot-dot-k?
                                                               (car rpl)))
                                                      (reverse
                                                        (cons (car rpl)
                                                              (map ordinary
                                                                   (cdr rpl))))
                                                      (map ordinary pl)))))
                                         p)
                                        ((lambda ()
                                           (match:syntax-err
                                             pattern
                                             "syntax error in pattern")))))))))))
                        (quasi (lambda (p)
                                 (let ((g109 (lambda (x y)
                                               (cons (quasi x) (quasi y)))))
                                   (if (simple? p)
                                     ((lambda (p) p) p)
                                     (if (symbol? p)
                                       ((lambda (p) `',p) p)
                                       (if (pair? p)
                                         (if (equal? (car p) 'unquote)
                                           (if (and (pair? (cdr p))
                                                    (null? (cddr p)))
                                             ((lambda (p) (ordinary p))
                                              (cadr p))
                                             (g109 (car p) (cdr p)))
                                           (if (and (pair? (car p))
                                                    (equal?
                                                      (caar p)
                                                      'unquote-splicing)
                                                    (pair? (cdar p))
                                                    (null? (cddar p)))
                                             (if (null? (cdr p))
                                               ((lambda (p) (ordinary p))
                                                (cadar p))
                                               ((lambda (p y)
                                                  (append
                                                    (ordlist p)
                                                    (quasi y)))
                                                (cadar p)
                                                (cdr p)))
                                             (if (and (pair? (cdr p))
                                                      (dot-dot-k? (cadr p))
                                                      (null? (cddr p)))
                                               ((lambda (p ddk)
                                                  `(,(quasi p) ,ddk))
                                                (car p)
                                                (cadr p))
                                               (g109 (car p) (cdr p)))))
                                         (if (vector? p)
                                           ((lambda (p)
                                              (let* ((pl (vector->list p))
                                                     (rpl (reverse pl)))
                                                (apply vector
                                                       (if (dot-dot-k?
                                                             (car rpl))
                                                         (reverse
                                                           (cons (car rpl)
                                                                 (map quasi
                                                                      (cdr rpl))))
                                                         (map ordinary pl)))))
                                            p)
                                           ((lambda ()
                                              (match:syntax-err
                                                pattern
                                                "syntax error in pattern"))))))))))
                        (ordlist
                          (lambda (p)
                            (cond ((null? p) '())
                                  ((pair? p)
                                   (cons (ordinary (car p)) (ordlist (cdr p))))
                                  (else
                                   (match:syntax-err
                                     pattern
                                     "invalid use of unquote-splicing in pattern"))))))
                 (ordinary pattern))))
           (bound (lambda (pattern)
                    (letrec ((pred-bodies '())
                             (bound (lambda (p a k)
                                      (cond ((eq? '_ p) (k p a))
                                            ((symbol? p)
                                             (if (memq p a)
                                               (match:syntax-err
                                                 pattern
                                                 "duplicate variable in pattern"))
                                             (k p (cons p a)))
                                            ((and (pair? p)
                                                  (eq? 'quote (car p)))
                                             (k p a))
                                            ((and (pair? p) (eq? '? (car p)))
                                             (cond ((not (null? (cddr p)))
                                                    (bound `(and (? ,(cadr p))
                                                                 ,@(cddr p))
                                                           a
                                                           k))
                                                   ((or (not (symbol?
                                                               (cadr p)))
                                                        (memq (cadr p) a))
                                                    (let ((g (gentemp)))
                                                      (set! pred-bodies
                                                        (cons `(,g ,(cadr p))
                                                              pred-bodies))
                                                      (k `(? ,g) a)))
                                                   (else (k p a))))
                                            ((and (pair? p) (eq? '= (car p)))
                                             (cond ((or (not (symbol?
                                                               (cadr p)))
                                                        (memq (cadr p) a))
                                                    (let ((g (gentemp)))
                                                      (set! pred-bodies
                                                        (cons `(,g ,(cadr p))
                                                              pred-bodies))
                                                      (bound `(= ,g ,(caddr p))
                                                             a
                                                             k)))
                                                   (else
                                                    (bound (caddr p)
                                                           a
                                                           (lambda (p2 a)
                                                             (k `(= ,(cadr p)
                                                                    ,p2)
                                                                a))))))
                                            ((and (pair? p) (eq? 'and (car p)))
                                             (bound*
                                               (cdr p)
                                               a
                                               (lambda (p a)
                                                 (k `(and ,@p) a))))
                                            ((and (pair? p) (eq? 'or (car p)))
                                             (bound (cadr p)
                                                    a
                                                    (lambda (first-p first-a)
                                                      (let or* ((plist (cddr p))
                                                                (k (lambda (plist)
                                                                     (k `(or ,first-p
                                                                             ,@plist)
                                                                        first-a))))
                                                        (if (null? plist)
                                                          (k plist)
                                                          (bound (car plist)
                                                                 a
                                                                 (lambda (car-p
                                                                          car-a)
                                                                   (if (not (permutation
                                                                              car-a
                                                                              first-a))
                                                                     (match:syntax-err
                                                                       pattern
                                                                       "variables of or-pattern differ in"))
                                                                   (or* (cdr plist)
                                                                        (lambda (cdr-p)
                                                                          (k (cons car-p
                                                                                   cdr-p)))))))))))
                                            ((and (pair? p) (eq? 'not (car p)))
                                             (cond ((not (null? (cddr p)))
                                                    (bound `(not (or ,@(cdr p)))
                                                           a
                                                           k))
                                                   (else
                                                    (bound (cadr p)
                                                           a
                                                           (lambda (p2 a2)
                                                             (if (not (permutation
                                                                        a
                                                                        a2))
                                                               (match:syntax-err
                                                                 p
                                                                 "no variables allowed in"))
                                                             (k `(not ,p2)
                                                                a))))))
                                            ((and (pair? p)
                                                  (pair? (cdr p))
                                                  (dot-dot-k? (cadr p)))
                                             (bound (car p)
                                                    a
                                                    (lambda (q b)
                                                      (let ((bvars (find-prefix
                                                                     b
                                                                     a)))
                                                        (k `(,q
                                                             ,(cadr p)
                                                             ,bvars
                                                             ,(gentemp)
                                                             ,(gentemp)
                                                             ,(map (lambda (_)
                                                                     (gentemp))
                                                                   bvars))
                                                           b)))))
                                            ((and (pair? p) (eq? '$ (car p)))
                                             (bound*
                                               (cddr p)
                                               a
                                               (lambda (p1 a)
                                                 (k `($ ,(cadr p) ,@p1) a))))
                                            ((and (pair? p)
                                                  (eq? 'set! (car p)))
                                             (if (memq (cadr p) a)
                                               (k p a)
                                               (k p (cons (cadr p) a))))
                                            ((and (pair? p)
                                                  (eq? 'get! (car p)))
                                             (if (memq (cadr p) a)
                                               (k p a)
                                               (k p (cons (cadr p) a))))
                                            ((pair? p)
                                             (bound (car p)
                                                    a
                                                    (lambda (car-p a)
                                                      (bound (cdr p)
                                                             a
                                                             (lambda (cdr-p a)
                                                               (k (cons car-p
                                                                        cdr-p)
                                                                  a))))))
                                            ((vector? p)
                                             (boundv
                                               (vector->list p)
                                               a
                                               (lambda (pl a)
                                                 (k (list->vector pl) a))))
                                            (else (k p a)))))
                             (boundv
                               (lambda (plist a k)
                                 (let ((g115 (lambda () (k plist a))))
                                   (if (pair? plist)
                                     (if (and (pair? (cdr plist))
                                              (dot-dot-k? (cadr plist))
                                              (null? (cddr plist)))
                                       ((lambda () (bound plist a k)))
                                       (if (null? plist)
                                         (g115)
                                         ((lambda (x y)
                                            (bound x
                                                   a
                                                   (lambda (car-p a)
                                                     (boundv
                                                       y
                                                       a
                                                       (lambda (cdr-p a)
                                                         (k (cons car-p cdr-p)
                                                            a))))))
                                          (car plist)
                                          (cdr plist))))
                                     (if (null? plist)
                                       (g115)
                                       (match:error plist))))))
                             (bound*
                               (lambda (plist a k)
                                 (if (null? plist)
                                   (k plist a)
                                   (bound (car plist)
                                          a
                                          (lambda (car-p a)
                                            (bound*
                                              (cdr plist)
                                              a
                                              (lambda (cdr-p a)
                                                (k (cons car-p cdr-p) a))))))))
                             (find-prefix
                               (lambda (b a)
                                 (if (eq? b a)
                                   '()
                                   (cons (car b) (find-prefix (cdr b) a)))))
                             (permutation
                               (lambda (p1 p2)
                                 (and (= (length p1) (length p2))
                                      (match:andmap
                                        (lambda (x1) (memq x1 p2))
                                        p1)))))
                      (bound pattern
                             '()
                             (lambda (p a)
                               (list p (reverse a) pred-bodies))))))
           (inline-let
             (lambda (let-exp)
               (letrec ((occ (lambda (x e)
                               (let loop ((e e))
                                 (cond ((pair? e)
                                        (+ (loop (car e)) (loop (cdr e))))
                                       ((eq? x e) 1)
                                       (else 0)))))
                        (subst (lambda (e old new)
                                 (let loop ((e e))
                                   (cond ((pair? e)
                                          (cons (loop (car e)) (loop (cdr e))))
                                         ((eq? old e) new)
                                         (else e)))))
                        (const?
                          (lambda (sexp)
                            (or (symbol? sexp)
                                (boolean? sexp)
                                (string? sexp)
                                (char? sexp)
                                (number? sexp)
                                (null? sexp)
                                (and (pair? sexp)
                                     (eq? (car sexp) 'quote)
                                     (pair? (cdr sexp))
                                     (symbol? (cadr sexp))
                                     (null? (cddr sexp))))))
                        (isval?
                          (lambda (sexp)
                            (or (const? sexp)
                                (and (pair? sexp)
                                     (memq (car sexp)
                                           '(lambda quote
                                              match-lambda
                                              match-lambda*))))))
                        (small?
                          (lambda (sexp)
                            (or (const? sexp)
                                (and (pair? sexp)
                                     (eq? (car sexp) 'lambda)
                                     (pair? (cdr sexp))
                                     (pair? (cddr sexp))
                                     (const? (caddr sexp))
                                     (null? (cdddr sexp)))))))
                 (let loop ((b (cadr let-exp))
                            (new-b '())
                            (e (caddr let-exp)))
                   (cond ((null? b)
                          (if (null? new-b) e `(let ,(reverse new-b) ,e)))
                         ((isval? (cadr (car b)))
                          (let* ((x (caar b)) (n (occ x e)))
                            (cond ((= 0 n) (loop (cdr b) new-b e))
                                  ((or (= 1 n) (small? (cadr (car b))))
                                   (loop (cdr b)
                                         new-b
                                         (subst e x (cadr (car b)))))
                                  (else
                                   (loop (cdr b) (cons (car b) new-b) e)))))
                         (else (loop (cdr b) (cons (car b) new-b) e)))))))
           (gen (lambda (x sf plist erract length>= eta)
                  (if (null? plist)
                    (erract x)
                    (let* ((v '())
                           (val (lambda (x) (cdr (assq x v))))
                           (fail (lambda (sf)
                                   (gen x sf (cdr plist) erract length>= eta)))
                           (success
                             (lambda (sf)
                               (set-car! (cddddr (car plist)) #t)
                               (let* ((code (cadr (car plist)))
                                      (bv (caddr (car plist)))
                                      (fail-sym (cadddr (car plist))))
                                 (if fail-sym
                                   (let ((ap `(,code
                                               ,fail-sym
                                               ,@(map val bv))))
                                     `(call-with-current-continuation
                                        (lambda (,fail-sym)
                                          (let ((,fail-sym
                                                 (lambda ()
                                                   (,fail-sym ,(fail sf)))))
                                            ,ap))))
                                   `(,code ,@(map val bv)))))))
                      (let next ((p (caar plist))
                                 (e x)
                                 (sf sf)
                                 (kf fail)
                                 (ks success))
                        (cond ((eq? '_ p) (ks sf))
                              ((symbol? p)
                               (set! v (cons (cons p e) v))
                               (ks sf))
                              ((null? p) (emit `(null? ,e) sf kf ks))
                              ((equal? p ''()) (emit `(null? ,e) sf kf ks))
                              ((string? p) (emit `(equal? ,e ,p) sf kf ks))
                              ((boolean? p) (emit `(equal? ,e ,p) sf kf ks))
                              ((char? p) (emit `(equal? ,e ,p) sf kf ks))
                              ((number? p) (emit `(equal? ,e ,p) sf kf ks))
                              ((and (pair? p) (eq? 'quote (car p)))
                               (emit `(equal? ,e ,p) sf kf ks))
                              ((and (pair? p) (eq? '? (car p)))
                               (let ((tst `(,(cadr p) ,e)))
                                 (emit tst sf kf ks)))
                              ((and (pair? p) (eq? '= (car p)))
                               (next (caddr p) `(,(cadr p) ,e) sf kf ks))
                              ((and (pair? p) (eq? 'and (car p)))
                               (let loop ((p (cdr p)) (sf sf))
                                 (if (null? p)
                                   (ks sf)
                                   (next (car p)
                                         e
                                         sf
                                         kf
                                         (lambda (sf) (loop (cdr p) sf))))))
                              ((and (pair? p) (eq? 'or (car p)))
                               (let ((or-v v))
                                 (let loop ((p (cdr p)) (sf sf))
                                   (if (null? p)
                                     (kf sf)
                                     (begin
                                       (set! v or-v)
                                       (next (car p)
                                             e
                                             sf
                                             (lambda (sf) (loop (cdr p) sf))
                                             ks))))))
                              ((and (pair? p) (eq? 'not (car p)))
                               (next (cadr p) e sf ks kf))
                              ((and (pair? p) (eq? '$ (car p)))
                               (let* ((tag (cadr p))
                                      (fields (cdr p))
                                      (rlen (length fields))
                                      (tst `(,(symbol-append tag '?) ,e)))
                                 (emit tst
                                       sf
                                       kf
                                       (let rloop ((n 1))
                                         (lambda (sf)
                                           (if (= n rlen)
                                             (ks sf)
                                             (next (list-ref fields n)
                                                   `(,(symbol-append tag '- n)
                                                     ,e)
                                                   sf
                                                   kf
                                                   (rloop (+ 1 n)))))))))
                              ((and (pair? p) (eq? 'set! (car p)))
                               (set! v (cons (cons (cadr p) (setter e p)) v))
                               (ks sf))
                              ((and (pair? p) (eq? 'get! (car p)))
                               (set! v (cons (cons (cadr p) (getter e p)) v))
                               (ks sf))
                              ((and (pair? p)
                                    (pair? (cdr p))
                                    (dot-dot-k? (cadr p)))
                               (emit `(list? ,e)
                                     sf
                                     kf
                                     (lambda (sf)
                                       (let* ((k (dot-dot-k? (cadr p)))
                                              (ks (lambda (sf)
                                                    (let ((bound (list-ref
                                                                   p
                                                                   2)))
                                                      (cond ((eq? (car p) '_)
                                                             (ks sf))
                                                            ((null? bound)
                                                             (let* ((ptst (next (car p)
                                                                                eta
                                                                                sf
                                                                                (lambda (sf)
                                                                                  #f)
                                                                                (lambda (sf)
                                                                                  #t)))
                                                                    (tst (if (and (pair? ptst)
                                                                                  (symbol?
                                                                                    (car ptst))
                                                                                  (pair? (cdr ptst))
                                                                                  (eq? eta
                                                                                       (cadr ptst))
                                                                                  (null? (cddr ptst)))
                                                                           (car ptst)
                                                                           `(lambda (,eta)
                                                                              ,ptst))))
                                                               (assm `(match:andmap
                                                                        ,tst
                                                                        ,e)
                                                                     (kf sf)
                                                                     (ks sf))))
                                                            ((and (symbol?
                                                                    (car p))
                                                                  (equal?
                                                                    (list (car p))
                                                                    bound))
                                                             (next (car p)
                                                                   e
                                                                   sf
                                                                   kf
                                                                   ks))
                                                            (else
                                                             (let* ((gloop (list-ref
                                                                             p
                                                                             3))
                                                                    (ge (list-ref
                                                                          p
                                                                          4))
                                                                    (fresh (list-ref
                                                                             p
                                                                             5))
                                                                    (p1 (next (car p)
                                                                              `(car ,ge)
                                                                              sf
                                                                              kf
                                                                              (lambda (sf)
                                                                                `(,gloop
                                                                                  (cdr ,ge)
                                                                                  ,@(map (lambda (b
                                                                                                  f)
                                                                                           `(cons ,(val b)
                                                                                                  ,f))
                                                                                         bound
                                                                                         fresh))))))
                                                               (set! v
                                                                 (append
                                                                   (map cons
                                                                        bound
                                                                        (map (lambda (x)
                                                                               `(reverse
                                                                                  ,x))
                                                                             fresh))
                                                                   v))
                                                               `(let ,gloop
                                                                  ((,ge ,e)
                                                                   ,@(map (lambda (x)
                                                                            `(,x
                                                                              '()))
                                                                          fresh))
                                                                  (if (null? ,ge)
                                                                    ,(ks sf)
                                                                    ,p1)))))))))
                                         (case k
                                           ((0) (ks sf))
                                           ((1) (emit `(pair? ,e) sf kf ks))
                                           (else
                                            (emit `((,length>= ,k) ,e)
                                                  sf
                                                  kf
                                                  ks)))))))
                              ((pair? p)
                               (emit `(pair? ,e)
                                     sf
                                     kf
                                     (lambda (sf)
                                       (next (car p)
                                             (add-a e)
                                             sf
                                             kf
                                             (lambda (sf)
                                               (next (cdr p)
                                                     (add-d e)
                                                     sf
                                                     kf
                                                     ks))))))
                              ((and (vector? p)
                                    (>= (vector-length p) 6)
                                    (dot-dot-k?
                                      (vector-ref p (- (vector-length p) 5))))
                               (let* ((vlen (- (vector-length p) 6))
                                      (k (dot-dot-k?
                                           (vector-ref p (+ vlen 1))))
                                      (minlen (+ vlen k))
                                      (bound (vector-ref p (+ vlen 2))))
                                 (emit `(vector? ,e)
                                       sf
                                       kf
                                       (lambda (sf)
                                         (assm `(>= (vector-length ,e) ,minlen)
                                               (kf sf)
                                               ((let vloop ((n 0))
                                                  (lambda (sf)
                                                    (cond ((not (= n vlen))
                                                           (next (vector-ref
                                                                   p
                                                                   n)
                                                                 `(vector-ref
                                                                    ,e
                                                                    ,n)
                                                                 sf
                                                                 kf
                                                                 (vloop (+ 1
                                                                           n))))
                                                          ((eq? (vector-ref
                                                                  p
                                                                  vlen)
                                                                '_)
                                                           (ks sf))
                                                          (else
                                                           (let* ((gloop (vector-ref
                                                                           p
                                                                           (+ vlen
                                                                              3)))
                                                                  (ind (vector-ref
                                                                         p
                                                                         (+ vlen
                                                                            4)))
                                                                  (fresh (vector-ref
                                                                           p
                                                                           (+ vlen
                                                                              5)))
                                                                  (p1 (next (vector-ref
                                                                              p
                                                                              vlen)
                                                                            `(vector-ref
                                                                               ,e
                                                                               ,ind)
                                                                            sf
                                                                            kf
                                                                            (lambda (sf)
                                                                              `(,gloop
                                                                                (- ,ind
                                                                                   1)
                                                                                ,@(map (lambda (b
                                                                                                f)
                                                                                         `(cons ,(val b)
                                                                                                ,f))
                                                                                       bound
                                                                                       fresh))))))
                                                             (set! v
                                                               (append
                                                                 (map cons
                                                                      bound
                                                                      fresh)
                                                                 v))
                                                             `(let ,gloop
                                                                ((,ind
                                                                  (- (vector-length
                                                                       ,e)
                                                                     1))
                                                                 ,@(map (lambda (x)
                                                                          `(,x
                                                                            '()))
                                                                        fresh))
                                                                (if (> ,minlen
                                                                       ,ind)
                                                                  ,(ks sf)
                                                                  ,p1)))))))
                                                sf))))))
                              ((vector? p)
                               (let ((vlen (vector-length p)))
                                 (emit `(vector? ,e)
                                       sf
                                       kf
                                       (lambda (sf)
                                         (emit `(equal?
                                                  (vector-length ,e)
                                                  ,vlen)
                                               sf
                                               kf
                                               (let vloop ((n 0))
                                                 (lambda (sf)
                                                   (if (= n vlen)
                                                     (ks sf)
                                                     (next (vector-ref p n)
                                                           `(vector-ref ,e ,n)
                                                           sf
                                                           kf
                                                           (vloop (+ 1
                                                                     n)))))))))))
                              (else
                               (display "FATAL ERROR IN PATTERN MATCHER")
                               (newline)
                               (error #f "THIS NEVER HAPPENS"))))))))
           (emit (lambda (tst sf kf ks)
                   (cond ((in tst sf) (ks sf))
                         ((in `(not ,tst) sf) (kf sf))
                         (else
                          (let* ((e (cadr tst))
                                 (implied
                                   (cond ((eq? (car tst) 'equal?)
                                          (let ((p (caddr tst)))
                                            (cond ((string? p) `((string? ,e)))
                                                  ((boolean? p)
                                                   `((boolean? ,e)))
                                                  ((char? p) `((char? ,e)))
                                                  ((number? p) `((number? ,e)))
                                                  ((and (pair? p)
                                                        (eq? 'quote (car p)))
                                                   `((symbol? ,e)))
                                                  (else '()))))
                                         ((eq? (car tst) 'null?) `((list? ,e)))
                                         ((vec-structure? tst) `((vector? ,e)))
                                         (else '())))
                                 (not-imp
                                   (case (car tst)
                                     ((list?) `((not (null? ,e))))
                                     (else '())))
                                 (s (ks (cons tst (append implied sf))))
                                 (k (kf (cons `(not ,tst)
                                              (append not-imp sf)))))
                            (assm tst k s))))))
           (assm (lambda (tst f s)
                   (cond ((equal? s f) s)
                         ((and (eq? s #t) (eq? f #f)) tst)
                         ((and (eq? (car tst) 'pair?)
                               (memq match:error-control '(unspecified fail))
                               (memq (car f) '(cond match:error))
                               (guarantees s (cadr tst)))
                          s)
                         ((and (pair? s)
                               (eq? (car s) 'if)
                               (equal? (cadddr s) f))
                          (if (eq? (car (cadr s)) 'and)
                            `(if (and ,tst ,@(cdr (cadr s))) ,(caddr s) ,f)
                            `(if (and ,tst ,(cadr s)) ,(caddr s) ,f)))
                         ((and (pair? s)
                               (equal? (car s) 'call-with-current-continuation)
                               (pair? (cdr s))
                               (pair? (cadr s))
                               (equal? (caadr s) 'lambda)
                               (pair? (cdadr s))
                               (pair? (cadadr s))
                               (null? (cdr (cadadr s)))
                               (pair? (cddadr s))
                               (pair? (car (cddadr s)))
                               (equal? (caar (cddadr s)) 'let)
                               (pair? (cdar (cddadr s)))
                               (pair? (cadar (cddadr s)))
                               (pair? (caadar (cddadr s)))
                               (pair? (cdr (caadar (cddadr s))))
                               (pair? (cadr (caadar (cddadr s))))
                               (equal? (caadr (caadar (cddadr s))) 'lambda)
                               (pair? (cdadr (caadar (cddadr s))))
                               (null? (cadadr (caadar (cddadr s))))
                               (pair? (cddadr (caadar (cddadr s))))
                               (pair? (car (cddadr (caadar (cddadr s)))))
                               (pair? (cdar (cddadr (caadar (cddadr s)))))
                               (null? (cddar (cddadr (caadar (cddadr s)))))
                               (null? (cdr (cddadr (caadar (cddadr s)))))
                               (null? (cddr (caadar (cddadr s))))
                               (null? (cdadar (cddadr s)))
                               (pair? (cddar (cddadr s)))
                               (null? (cdddar (cddadr s)))
                               (null? (cdr (cddadr s)))
                               (null? (cddr s))
                               (equal? f (cadar (cddadr (caadar (cddadr s))))))
                          (let ((k (car (cadadr s)))
                                (fail (car (caadar (cddadr s))))
                                (s2 (caddar (cddadr s))))
                            `(call-with-current-continuation
                               (lambda (,k)
                                 (let ((,fail (lambda () (,k ,f))))
                                   ,(assm tst `(,fail) s2))))))
                         ((and #f
                               (pair? s)
                               (equal? (car s) 'let)
                               (pair? (cdr s))
                               (pair? (cadr s))
                               (pair? (caadr s))
                               (pair? (cdaadr s))
                               (pair? (car (cdaadr s)))
                               (equal? (caar (cdaadr s)) 'lambda)
                               (pair? (cdar (cdaadr s)))
                               (null? (cadar (cdaadr s)))
                               (pair? (cddar (cdaadr s)))
                               (null? (cdddar (cdaadr s)))
                               (null? (cdr (cdaadr s)))
                               (null? (cdadr s))
                               (pair? (cddr s))
                               (null? (cdddr s))
                               (equal? (caddar (cdaadr s)) f))
                          (let ((fail (caaadr s)) (s2 (caddr s)))
                            `(let ((,fail (lambda () ,f)))
                               ,(assm tst `(,fail) s2))))
                         (else `(if ,tst ,s ,f)))))
           (guarantees
             (lambda (code x)
               (let ((a (add-a x)) (d (add-d x)))
                 (let loop ((code code))
                   (cond ((not (pair? code)) #f)
                         ((memq (car code) '(cond match:error)) #t)
                         ((or (equal? code a) (equal? code d)) #t)
                         ((eq? (car code) 'if)
                          (or (loop (cadr code))
                              (and (loop (caddr code)) (loop (cadddr code)))))
                         ((eq? (car code) 'lambda) #f)
                         ((and (eq? (car code) 'let) (symbol? (cadr code)))
                          #f)
                         (else (or (loop (car code)) (loop (cdr code)))))))))
           (in (lambda (e l)
                 (or (member e l)
                     (and (eq? (car e) 'list?)
                          (or (member `(null? ,(cadr e)) l)
                              (member `(pair? ,(cadr e)) l)))
                     (and (eq? (car e) 'not)
                          (let* ((srch (cadr e))
                                 (const-class (equal-test? srch)))
                            (cond (const-class
                                   (let mem ((l l))
                                     (if (null? l)
                                       #f
                                       (let ((x (car l)))
                                         (or (and (equal? (cadr x) (cadr srch))
                                                  (disjoint? x)
                                                  (not (equal?
                                                         const-class
                                                         (car x))))
                                             (equal?
                                               x
                                               `(not (,const-class
                                                      ,(cadr srch))))
                                             (and (equal? (cadr x) (cadr srch))
                                                  (equal-test? x)
                                                  (not (equal?
                                                         (caddr srch)
                                                         (caddr x))))
                                             (mem (cdr l)))))))
                                  ((disjoint? srch)
                                   (let mem ((l l))
                                     (if (null? l)
                                       #f
                                       (let ((x (car l)))
                                         (or (and (equal? (cadr x) (cadr srch))
                                                  (disjoint? x)
                                                  (not (equal?
                                                         (car x)
                                                         (car srch))))
                                             (mem (cdr l)))))))
                                  ((eq? (car srch) 'list?)
                                   (let mem ((l l))
                                     (if (null? l)
                                       #f
                                       (let ((x (car l)))
                                         (or (and (equal? (cadr x) (cadr srch))
                                                  (disjoint? x)
                                                  (not (memq (car x)
                                                             '(list? pair?
                                                                     null?))))
                                             (mem (cdr l)))))))
                                  ((vec-structure? srch)
                                   (let mem ((l l))
                                     (if (null? l)
                                       #f
                                       (let ((x (car l)))
                                         (or (and (equal? (cadr x) (cadr srch))
                                                  (or (disjoint? x)
                                                      (vec-structure? x))
                                                  (not (equal?
                                                         (car x)
                                                         'vector?))
                                                  (not (equal?
                                                         (car x)
                                                         (car srch))))
                                             (equal?
                                               x
                                               `(not (vector? ,(cadr srch))))
                                             (mem (cdr l)))))))
                                  (else #f)))))))
           (equal-test?
             (lambda (tst)
               (and (eq? (car tst) 'equal?)
                    (let ((p (caddr tst)))
                      (cond ((string? p) 'string?)
                            ((boolean? p) 'boolean?)
                            ((char? p) 'char?)
                            ((number? p) 'number?)
                            ((and (pair? p)
                                  (pair? (cdr p))
                                  (null? (cddr p))
                                  (eq? 'quote (car p))
                                  (symbol? (cadr p)))
                             'symbol?)
                            (else #f))))))
           (disjoint?
             (lambda (tst)
               (memq (car tst) match:disjoint-predicates)))
           (vec-structure?
             (lambda (tst)
               (memq (car tst) match:vector-structures)))
           (add-a (lambda (a)
                    (let ((new (and (pair? a) (assq (car a) c---rs))))
                      (if new (cons (cadr new) (cdr a)) `(car ,a)))))
           (add-d (lambda (a)
                    (let ((new (and (pair? a) (assq (car a) c---rs))))
                      (if new (cons (cddr new) (cdr a)) `(cdr ,a)))))
           (c---rs
             '((car caar . cdar)
               (cdr cadr . cddr)
               (caar caaar . cdaar)
               (cadr caadr . cdadr)
               (cdar cadar . cddar)
               (cddr caddr . cdddr)
               (caaar caaaar . cdaaar)
               (caadr caaadr . cdaadr)
               (cadar caadar . cdadar)
               (caddr caaddr . cdaddr)
               (cdaar cadaar . cddaar)
               (cdadr cadadr . cddadr)
               (cddar caddar . cdddar)
               (cdddr cadddr . cddddr)))
           (setter
             (lambda (e p)
               (let ((mk-setter
                       (lambda (s) (symbol-append 'set- s '!))))
                 (cond ((not (pair? e))
                        (match:syntax-err p "unnested set! pattern"))
                       ((eq? (car e) 'vector-ref)
                        `(let ((x ,(cadr e)))
                           (lambda (y) (vector-set! x ,(caddr e) y))))
                       ((eq? (car e) 'unbox)
                        `(let ((x ,(cadr e))) (lambda (y) (set-box! x y))))
                       ((eq? (car e) 'car)
                        `(let ((x ,(cadr e))) (lambda (y) (set-car! x y))))
                       ((eq? (car e) 'cdr)
                        `(let ((x ,(cadr e))) (lambda (y) (set-cdr! x y))))
                       ((let ((a (assq (car e) get-c---rs)))
                          (and a
                               `(let ((x (,(cadr a) ,(cadr e))))
                                  (lambda (y) (,(mk-setter (cddr a)) x y))))))
                       (else
                        `(let ((x ,(cadr e)))
                           (lambda (y) (,(mk-setter (car e)) x y))))))))
           (getter
             (lambda (e p)
               (cond ((not (pair? e))
                      (match:syntax-err p "unnested get! pattern"))
                     ((eq? (car e) 'vector-ref)
                      `(let ((x ,(cadr e)))
                         (lambda () (vector-ref x ,(caddr e)))))
                     ((eq? (car e) 'unbox)
                      `(let ((x ,(cadr e))) (lambda () (unbox x))))
                     ((eq? (car e) 'car)
                      `(let ((x ,(cadr e))) (lambda () (car x))))
                     ((eq? (car e) 'cdr)
                      `(let ((x ,(cadr e))) (lambda () (cdr x))))
                     ((let ((a (assq (car e) get-c---rs)))
                        (and a
                             `(let ((x (,(cadr a) ,(cadr e))))
                                (lambda () (,(cddr a) x))))))
                     (else
                      `(let ((x ,(cadr e))) (lambda () (,(car e) x)))))))
           (get-c---rs
             '((caar car . car)
               (cadr cdr . car)
               (cdar car . cdr)
               (cddr cdr . cdr)
               (caaar caar . car)
               (caadr cadr . car)
               (cadar cdar . car)
               (caddr cddr . car)
               (cdaar caar . cdr)
               (cdadr cadr . cdr)
               (cddar cdar . cdr)
               (cdddr cddr . cdr)
               (caaaar caaar . car)
               (caaadr caadr . car)
               (caadar cadar . car)
               (caaddr caddr . car)
               (cadaar cdaar . car)
               (cadadr cdadr . car)
               (caddar cddar . car)
               (cadddr cdddr . car)
               (cdaaar caaar . cdr)
               (cdaadr caadr . cdr)
               (cdadar cadar . cdr)
               (cdaddr caddr . cdr)
               (cddaar cdaar . cdr)
               (cddadr cdadr . cdr)
               (cdddar cddar . cdr)
               (cddddr cdddr . cdr)))
           (symbol-append
             (lambda l
               (string->symbol
                 (apply string-append
                        (map (lambda (x)
                               (cond ((symbol? x) (symbol->string x))
                                     ((number? x) (number->string x))
                                     (else x)))
                             l)))))
           (rac (lambda (l)
                  (if (null? (cdr l)) (car l) (rac (cdr l)))))
           (rdc (lambda (l)
                  (if (null? (cdr l))
                    '()
                    (cons (car l) (rdc (cdr l)))))))
    (list genmatch genletrec gendefine pattern-var?)))
(defmacro
  match
  args
  (cond ((and (list? args)
              (<= 1 (length args))
              (match:andmap
                (lambda (y) (and (list? y) (<= 2 (length y))))
                (cdr args)))
         (let* ((exp (car args))
                (clauses (cdr args))
                (e (if (symbol? exp) exp (gentemp))))
           (if (symbol? exp)
             ((car match:expanders) e clauses `(match ,@args))
             `(let ((,e ,exp))
                ,((car match:expanders) e clauses `(match ,@args))))))
        (else
         (match:syntax-err
           `(match ,@args)
           "syntax error in"))))
(defmacro
  match-lambda
  args
  (if (and (list? args)
           (match:andmap
             (lambda (g126)
               (if (and (pair? g126) (list? (cdr g126)))
                 (pair? (cdr g126))
                 #f))
             args))
    ((lambda ()
       (let ((e (gentemp)))
         `(lambda (,e) (match ,e ,@args)))))
    ((lambda ()
       (match:syntax-err
         `(match-lambda ,@args)
         "syntax error in")))))
(defmacro
  match-lambda*
  args
  (if (and (list? args)
           (match:andmap
             (lambda (g134)
               (if (and (pair? g134) (list? (cdr g134)))
                 (pair? (cdr g134))
                 #f))
             args))
    ((lambda ()
       (let ((e (gentemp)))
         `(lambda ,e (match ,e ,@args)))))
    ((lambda ()
       (match:syntax-err
         `(match-lambda* ,@args)
         "syntax error in")))))
(defmacro
  match-let
  args
  (let ((g158 (lambda (pat exp body)
                `(match ,exp (,pat ,@body))))
        (g154 (lambda (pat exp body)
                (let ((g (map (lambda (x) (gentemp)) pat))
                      (vpattern (list->vector pat)))
                  `(let ,(map list g exp)
                     (match (vector ,@g) (,vpattern ,@body))))))
        (g146 (lambda ()
                (match:syntax-err
                  `(match-let ,@args)
                  "syntax error in")))
        (g145 (lambda (p1 e1 p2 e2 body)
                (let ((g1 (gentemp)) (g2 (gentemp)))
                  `(let ((,g1 ,e1) (,g2 ,e2))
                     (match (cons ,g1 ,g2) ((,p1 unquote p2) ,@body))))))
        (g136 (cadddr match:expanders)))
    (if (pair? args)
      (if (symbol? (car args))
        (if (and (pair? (cdr args)) (list? (cadr args)))
          (let g161 ((g162 (cadr args)) (g160 '()) (g159 '()))
            (if (null? g162)
              (if (and (list? (cddr args)) (pair? (cddr args)))
                ((lambda (name pat exp body)
                   (if (match:andmap (cadddr match:expanders) pat)
                     `(let ,@args)
                     `(letrec ((,name (match-lambda* (,pat ,@body))))
                        (,name ,@exp))))
                 (car args)
                 (reverse g159)
                 (reverse g160)
                 (cddr args))
                (g146))
              (if (and (pair? (car g162))
                       (pair? (cdar g162))
                       (null? (cddar g162)))
                (g161 (cdr g162)
                      (cons (cadar g162) g160)
                      (cons (caar g162) g159))
                (g146))))
          (g146))
        (if (list? (car args))
          (if (match:andmap
                (lambda (g167)
                  (if (and (pair? g167)
                           (g136 (car g167))
                           (pair? (cdr g167)))
                    (null? (cddr g167))
                    #f))
                (car args))
            (if (and (list? (cdr args)) (pair? (cdr args)))
              ((lambda () `(let ,@args)))
              (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                (if (null? g150)
                  (g146)
                  (if (and (pair? (car g150))
                           (pair? (cdar g150))
                           (null? (cddar g150)))
                    (g149 (cdr g150)
                          (cons (cadar g150) g148)
                          (cons (caar g150) g147))
                    (g146)))))
            (if (and (pair? (car args))
                     (pair? (caar args))
                     (pair? (cdaar args))
                     (null? (cddaar args)))
              (if (null? (cdar args))
                (if (and (list? (cdr args)) (pair? (cdr args)))
                  (g158 (caaar args) (cadaar args) (cdr args))
                  (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                    (if (null? g150)
                      (g146)
                      (if (and (pair? (car g150))
                               (pair? (cdar g150))
                               (null? (cddar g150)))
                        (g149 (cdr g150)
                              (cons (cadar g150) g148)
                              (cons (caar g150) g147))
                        (g146)))))
                (if (and (pair? (cdar args))
                         (pair? (cadar args))
                         (pair? (cdadar args))
                         (null? (cdr (cdadar args)))
                         (null? (cddar args)))
                  (if (and (list? (cdr args)) (pair? (cdr args)))
                    (g145 (caaar args)
                          (cadaar args)
                          (caadar args)
                          (car (cdadar args))
                          (cdr args))
                    (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                      (if (null? g150)
                        (g146)
                        (if (and (pair? (car g150))
                                 (pair? (cdar g150))
                                 (null? (cddar g150)))
                          (g149 (cdr g150)
                                (cons (cadar g150) g148)
                                (cons (caar g150) g147))
                          (g146)))))
                  (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                    (if (null? g150)
                      (if (and (list? (cdr args)) (pair? (cdr args)))
                        (g154 (reverse g147) (reverse g148) (cdr args))
                        (g146))
                      (if (and (pair? (car g150))
                               (pair? (cdar g150))
                               (null? (cddar g150)))
                        (g149 (cdr g150)
                              (cons (cadar g150) g148)
                              (cons (caar g150) g147))
                        (g146))))))
              (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                (if (null? g150)
                  (if (and (list? (cdr args)) (pair? (cdr args)))
                    (g154 (reverse g147) (reverse g148) (cdr args))
                    (g146))
                  (if (and (pair? (car g150))
                           (pair? (cdar g150))
                           (null? (cddar g150)))
                    (g149 (cdr g150)
                          (cons (cadar g150) g148)
                          (cons (caar g150) g147))
                    (g146))))))
          (if (pair? (car args))
            (if (and (pair? (caar args))
                     (pair? (cdaar args))
                     (null? (cddaar args)))
              (if (null? (cdar args))
                (if (and (list? (cdr args)) (pair? (cdr args)))
                  (g158 (caaar args) (cadaar args) (cdr args))
                  (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                    (if (null? g150)
                      (g146)
                      (if (and (pair? (car g150))
                               (pair? (cdar g150))
                               (null? (cddar g150)))
                        (g149 (cdr g150)
                              (cons (cadar g150) g148)
                              (cons (caar g150) g147))
                        (g146)))))
                (if (and (pair? (cdar args))
                         (pair? (cadar args))
                         (pair? (cdadar args))
                         (null? (cdr (cdadar args)))
                         (null? (cddar args)))
                  (if (and (list? (cdr args)) (pair? (cdr args)))
                    (g145 (caaar args)
                          (cadaar args)
                          (caadar args)
                          (car (cdadar args))
                          (cdr args))
                    (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                      (if (null? g150)
                        (g146)
                        (if (and (pair? (car g150))
                                 (pair? (cdar g150))
                                 (null? (cddar g150)))
                          (g149 (cdr g150)
                                (cons (cadar g150) g148)
                                (cons (caar g150) g147))
                          (g146)))))
                  (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                    (if (null? g150)
                      (if (and (list? (cdr args)) (pair? (cdr args)))
                        (g154 (reverse g147) (reverse g148) (cdr args))
                        (g146))
                      (if (and (pair? (car g150))
                               (pair? (cdar g150))
                               (null? (cddar g150)))
                        (g149 (cdr g150)
                              (cons (cadar g150) g148)
                              (cons (caar g150) g147))
                        (g146))))))
              (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                (if (null? g150)
                  (if (and (list? (cdr args)) (pair? (cdr args)))
                    (g154 (reverse g147) (reverse g148) (cdr args))
                    (g146))
                  (if (and (pair? (car g150))
                           (pair? (cdar g150))
                           (null? (cddar g150)))
                    (g149 (cdr g150)
                          (cons (cadar g150) g148)
                          (cons (caar g150) g147))
                    (g146)))))
            (g146))))
      (g146))))
(defmacro
  match-let*
  args
  (let ((g176 (lambda ()
                (match:syntax-err
                  `(match-let* ,@args)
                  "syntax error in"))))
    (if (pair? args)
      (if (null? (car args))
        (if (and (list? (cdr args)) (pair? (cdr args)))
          ((lambda (body) `(let* ,@args)) (cdr args))
          (g176))
        (if (and (pair? (car args))
                 (pair? (caar args))
                 (pair? (cdaar args))
                 (null? (cddaar args))
                 (list? (cdar args))
                 (list? (cdr args))
                 (pair? (cdr args)))
          ((lambda (pat exp rest body)
             (if ((cadddr match:expanders) pat)
               `(let ((,pat ,exp)) (match-let* ,rest ,@body))
               `(match ,exp (,pat (match-let* ,rest ,@body)))))
           (caaar args)
           (cadaar args)
           (cdar args)
           (cdr args))
          (g176)))
      (g176))))
(defmacro
  match-letrec
  args
  (let ((g200 (cadddr match:expanders))
        (g199 (lambda (p1 e1 p2 e2 body)
                `(match-letrec
                   (((,p1 unquote p2) (cons ,e1 ,e2)))
                   ,@body)))
        (g195 (lambda ()
                (match:syntax-err
                  `(match-letrec ,@args)
                  "syntax error in")))
        (g194 (lambda (pat exp body)
                `(match-letrec
                   ((,(list->vector pat) (vector ,@exp)))
                   ,@body)))
        (g186 (lambda (pat exp body)
                ((cadr match:expanders)
                 pat
                 exp
                 body
                 `(match-letrec ((,pat ,exp)) ,@body)))))
    (if (pair? args)
      (if (list? (car args))
        (if (match:andmap
              (lambda (g206)
                (if (and (pair? g206)
                         (g200 (car g206))
                         (pair? (cdr g206)))
                  (null? (cddr g206))
                  #f))
              (car args))
          (if (and (list? (cdr args)) (pair? (cdr args)))
            ((lambda () `(letrec ,@args)))
            (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
              (if (null? g190)
                (g195)
                (if (and (pair? (car g190))
                         (pair? (cdar g190))
                         (null? (cddar g190)))
                  (g189 (cdr g190)
                        (cons (cadar g190) g188)
                        (cons (caar g190) g187))
                  (g195)))))
          (if (and (pair? (car args))
                   (pair? (caar args))
                   (pair? (cdaar args))
                   (null? (cddaar args)))
            (if (null? (cdar args))
              (if (and (list? (cdr args)) (pair? (cdr args)))
                (g186 (caaar args) (cadaar args) (cdr args))
                (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                  (if (null? g190)
                    (g195)
                    (if (and (pair? (car g190))
                             (pair? (cdar g190))
                             (null? (cddar g190)))
                      (g189 (cdr g190)
                            (cons (cadar g190) g188)
                            (cons (caar g190) g187))
                      (g195)))))
              (if (and (pair? (cdar args))
                       (pair? (cadar args))
                       (pair? (cdadar args))
                       (null? (cdr (cdadar args)))
                       (null? (cddar args)))
                (if (and (list? (cdr args)) (pair? (cdr args)))
                  (g199 (caaar args)
                        (cadaar args)
                        (caadar args)
                        (car (cdadar args))
                        (cdr args))
                  (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                    (if (null? g190)
                      (g195)
                      (if (and (pair? (car g190))
                               (pair? (cdar g190))
                               (null? (cddar g190)))
                        (g189 (cdr g190)
                              (cons (cadar g190) g188)
                              (cons (caar g190) g187))
                        (g195)))))
                (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                  (if (null? g190)
                    (if (and (list? (cdr args)) (pair? (cdr args)))
                      (g194 (reverse g187) (reverse g188) (cdr args))
                      (g195))
                    (if (and (pair? (car g190))
                             (pair? (cdar g190))
                             (null? (cddar g190)))
                      (g189 (cdr g190)
                            (cons (cadar g190) g188)
                            (cons (caar g190) g187))
                      (g195))))))
            (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
              (if (null? g190)
                (if (and (list? (cdr args)) (pair? (cdr args)))
                  (g194 (reverse g187) (reverse g188) (cdr args))
                  (g195))
                (if (and (pair? (car g190))
                         (pair? (cdar g190))
                         (null? (cddar g190)))
                  (g189 (cdr g190)
                        (cons (cadar g190) g188)
                        (cons (caar g190) g187))
                  (g195))))))
        (if (pair? (car args))
          (if (and (pair? (caar args))
                   (pair? (cdaar args))
                   (null? (cddaar args)))
            (if (null? (cdar args))
              (if (and (list? (cdr args)) (pair? (cdr args)))
                (g186 (caaar args) (cadaar args) (cdr args))
                (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                  (if (null? g190)
                    (g195)
                    (if (and (pair? (car g190))
                             (pair? (cdar g190))
                             (null? (cddar g190)))
                      (g189 (cdr g190)
                            (cons (cadar g190) g188)
                            (cons (caar g190) g187))
                      (g195)))))
              (if (and (pair? (cdar args))
                       (pair? (cadar args))
                       (pair? (cdadar args))
                       (null? (cdr (cdadar args)))
                       (null? (cddar args)))
                (if (and (list? (cdr args)) (pair? (cdr args)))
                  (g199 (caaar args)
                        (cadaar args)
                        (caadar args)
                        (car (cdadar args))
                        (cdr args))
                  (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                    (if (null? g190)
                      (g195)
                      (if (and (pair? (car g190))
                               (pair? (cdar g190))
                               (null? (cddar g190)))
                        (g189 (cdr g190)
                              (cons (cadar g190) g188)
                              (cons (caar g190) g187))
                        (g195)))))
                (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                  (if (null? g190)
                    (if (and (list? (cdr args)) (pair? (cdr args)))
                      (g194 (reverse g187) (reverse g188) (cdr args))
                      (g195))
                    (if (and (pair? (car g190))
                             (pair? (cdar g190))
                             (null? (cddar g190)))
                      (g189 (cdr g190)
                            (cons (cadar g190) g188)
                            (cons (caar g190) g187))
                      (g195))))))
            (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
              (if (null? g190)
                (if (and (list? (cdr args)) (pair? (cdr args)))
                  (g194 (reverse g187) (reverse g188) (cdr args))
                  (g195))
                (if (and (pair? (car g190))
                         (pair? (cdar g190))
                         (null? (cddar g190)))
                  (g189 (cdr g190)
                        (cons (cadar g190) g188)
                        (cons (caar g190) g187))
                  (g195)))))
          (g195)))
      (g195))))
(defmacro
  match-define
  args
  (let ((g210 (cadddr match:expanders))
        (g209 (lambda ()
                (match:syntax-err
                  `(match-define ,@args)
                  "syntax error in"))))
    (if (pair? args)
      (if (g210 (car args))
        (if (and (pair? (cdr args)) (null? (cddr args)))
          ((lambda () `(begin (define ,@args))))
          (g209))
        (if (and (pair? (cdr args)) (null? (cddr args)))
          ((lambda (pat exp)
             ((caddr match:expanders)
              pat
              exp
              `(match-define ,@args)))
           (car args)
           (cadr args))
          (g209)))
      (g209))))
(define match:runtime-structures #f)
(define match:set-runtime-structures
  (lambda (v) (set! match:runtime-structures v)))
(define match:primitive-vector? vector?)
(defmacro
  defstruct
  args
  (let ((field?
          (lambda (x)
            (if (symbol? x)
              ((lambda () #t))
              (if (and (pair? x)
                       (symbol? (car x))
                       (pair? (cdr x))
                       (symbol? (cadr x))
                       (null? (cddr x)))
                ((lambda () #t))
                ((lambda () #f))))))
        (selector-name
          (lambda (x)
            (if (symbol? x)
              ((lambda () x))
              (if (and (pair? x)
                       (symbol? (car x))
                       (pair? (cdr x))
                       (null? (cddr x)))
                ((lambda (s) s) (car x))
                (match:error x)))))
        (mutator-name
          (lambda (x)
            (if (symbol? x)
              ((lambda () #f))
              (if (and (pair? x)
                       (pair? (cdr x))
                       (symbol? (cadr x))
                       (null? (cddr x)))
                ((lambda (s) s) (cadr x))
                (match:error x)))))
        (filter-map-with-index
          (lambda (f l)
            (letrec ((mapi (lambda (l i)
                             (cond ((null? l) '())
                                   ((f (car l) i)
                                    =>
                                    (lambda (x)
                                      (cons x (mapi (cdr l) (+ 1 i)))))
                                   (else (mapi (cdr l) (+ 1 i)))))))
              (mapi l 1)))))
    (let ((g227 (lambda ()
                  (match:syntax-err
                    `(defstruct ,@args)
                    "syntax error in"))))
      (if (and (pair? args)
               (symbol? (car args))
               (pair? (cdr args))
               (symbol? (cadr args))
               (pair? (cddr args))
               (symbol? (caddr args))
               (list? (cdddr args)))
        (let g229 ((g230 (cdddr args)) (g228 '()))
          (if (null? g230)
            ((lambda (name constructor predicate fields)
               (let* ((selectors (map selector-name fields))
                      (mutators (map mutator-name fields))
                      (tag (if match:runtime-structures
                             (gentemp)
                             `',(match:make-structure-tag name)))
                      (vectorp
                        (cond ((eq? match:structure-control 'disjoint)
                               'match:primitive-vector?)
                              ((eq? match:structure-control 'vector)
                               'vector?))))
                 (cond ((eq? match:structure-control 'disjoint)
                        (if (eq? vector? match:primitive-vector?)
                          (set! vector?
                            (lambda (v)
                              (and (match:primitive-vector? v)
                                   (or (zero? (vector-length v))
                                       (not (symbol? (vector-ref v 0)))
                                       (not (match:structure?
                                              (vector-ref v 0))))))))
                        (if (not (memq predicate match:disjoint-predicates))
                          (set! match:disjoint-predicates
                            (cons predicate match:disjoint-predicates))))
                       ((eq? match:structure-control 'vector)
                        (if (not (memq predicate match:vector-structures))
                          (set! match:vector-structures
                            (cons predicate match:vector-structures))))
                       (else
                        (match:syntax-err
                          '(vector disjoint)
                          "invalid value for match:structure-control, legal values are")))
                 `(begin
                    ,@(if match:runtime-structures
                        `((define ,tag (match:make-structure-tag ',name)))
                        '())
                    (define ,constructor
                      (lambda ,selectors (vector ,tag ,@selectors)))
                    (define ,predicate
                      (lambda (obj)
                        (and (,vectorp obj)
                             (= (vector-length obj) ,(+ 1 (length selectors)))
                             (eq? (vector-ref obj 0) ,tag))))
                    ,@(filter-map-with-index
                        (lambda (n i)
                          `(define ,n (lambda (obj) (vector-ref obj ,i))))
                        selectors)
                    ,@(filter-map-with-index
                        (lambda (n i)
                          (and n
                               `(define ,n
                                  (lambda (obj newval)
                                    (vector-set! obj ,i newval)))))
                        mutators))))
             (car args)
             (cadr args)
             (caddr args)
             (reverse g228))
            (if (field? (car g230))
              (g229 (cdr g230) (cons (car g230) g228))
              (g227))))
        (g227)))))
(defmacro
  define-structure
  args
  (let ((g242 (lambda ()
                (match:syntax-err
                  `(define-structure ,@args)
                  "syntax error in"))))
    (if (and (pair? args)
             (pair? (car args))
             (list? (cdar args)))
      (if (null? (cdr args))
        ((lambda (name id1)
           `(define-structure (,name ,@id1) ()))
         (caar args)
         (cdar args))
        (if (and (pair? (cdr args)) (list? (cadr args)))
          (let g239 ((g240 (cadr args)) (g238 '()) (g237 '()))
            (if (null? g240)
              (if (null? (cddr args))
                ((lambda (name id1 id2 val)
                   (let ((mk-id (lambda (id)
                                  (if (and (pair? id)
                                           (equal? (car id) '@)
                                           (pair? (cdr id))
                                           (symbol? (cadr id))
                                           (null? (cddr id)))
                                    ((lambda (x) x) (cadr id))
                                    ((lambda () `(! ,id)))))))
                     `(define-const-structure
                        (,name ,@(map mk-id id1))
                        ,(map (lambda (id v) `(,(mk-id id) ,v)) id2 val))))
                 (caar args)
                 (cdar args)
                 (reverse g237)
                 (reverse g238))
                (g242))
              (if (and (pair? (car g240))
                       (pair? (cdar g240))
                       (null? (cddar g240)))
                (g239 (cdr g240)
                      (cons (cadar g240) g238)
                      (cons (caar g240) g237))
                (g242))))
          (g242)))
      (g242))))
(defmacro
  define-const-structure
  args
  (let ((field?
          (lambda (id)
            (if (symbol? id)
              ((lambda () #t))
              (if (and (pair? id)
                       (equal? (car id) '!)
                       (pair? (cdr id))
                       (symbol? (cadr id))
                       (null? (cddr id)))
                ((lambda () #t))
                ((lambda () #f))))))
        (field-name
          (lambda (x) (if (symbol? x) x (cadr x))))
        (has-mutator? (lambda (x) (not (symbol? x))))
        (filter-map-with-index
          (lambda (f l)
            (letrec ((mapi (lambda (l i)
                             (cond ((null? l) '())
                                   ((f (car l) i)
                                    =>
                                    (lambda (x)
                                      (cons x (mapi (cdr l) (+ 1 i)))))
                                   (else (mapi (cdr l) (+ 1 i)))))))
              (mapi l 1))))
        (symbol-append
          (lambda l
            (string->symbol
              (apply string-append
                     (map (lambda (x)
                            (cond ((symbol? x) (symbol->string x))
                                  ((number? x) (number->string x))
                                  (else x)))
                          l))))))
    (let ((g266 (lambda ()
                  (match:syntax-err
                    `(define-const-structure ,@args)
                    "syntax error in"))))
      (if (and (pair? args)
               (pair? (car args))
               (list? (cdar args)))
        (if (null? (cdr args))
          ((lambda (name id1)
             `(define-const-structure (,name ,@id1) ()))
           (caar args)
           (cdar args))
          (if (symbol? (caar args))
            (let g259 ((g260 (cdar args)) (g258 '()))
              (if (null? g260)
                (if (and (pair? (cdr args)) (list? (cadr args)))
                  (let g263 ((g264 (cadr args)) (g262 '()) (g261 '()))
                    (if (null? g264)
                      (if (null? (cddr args))
                        ((lambda (name id1 id2 val)
                           (let* ((id1id2 (append id1 id2))
                                  (raw-constructor
                                    (symbol-append 'make-raw- name))
                                  (constructor (symbol-append 'make- name))
                                  (predicate (symbol-append name '?)))
                             `(begin
                                (defstruct
                                  ,name
                                  ,raw-constructor
                                  ,predicate
                                  ,@(filter-map-with-index
                                      (lambda (arg i)
                                        (if (has-mutator? arg)
                                          `(,(symbol-append name '- i)
                                            ,(symbol-append
                                               'set-
                                               name
                                               '-
                                               i
                                               '!))
                                          (symbol-append name '- i)))
                                      id1id2))
                                ,(let* ((make-fresh
                                          (lambda (x)
                                            (if (eq? '_ x) (gentemp) x)))
                                        (names1
                                          (map make-fresh
                                               (map field-name id1)))
                                        (names2
                                          (map make-fresh
                                               (map field-name id2))))
                                   `(define ,constructor
                                      (lambda ,names1
                                        (let* ,(map list names2 val)
                                          (,raw-constructor
                                           ,@names1
                                           ,@names2)))))
                                ,@(filter-map-with-index
                                    (lambda (field i)
                                      (if (eq? (field-name field) '_)
                                        #f
                                        `(define (unquote
                                                  (symbol-append
                                                    name
                                                    '-
                                                    (field-name field)))
                                           ,(symbol-append name '- i))))
                                    id1id2)
                                ,@(filter-map-with-index
                                    (lambda (field i)
                                      (if (or (eq? (field-name field) '_)
                                              (not (has-mutator? field)))
                                        #f
                                        `(define (unquote
                                                  (symbol-append
                                                    'set-
                                                    name
                                                    '-
                                                    (field-name field)
                                                    '!))
                                           ,(symbol-append
                                              'set-
                                              name
                                              '-
                                              i
                                              '!))))
                                    id1id2))))
                         (caar args)
                         (reverse g258)
                         (reverse g261)
                         (reverse g262))
                        (g266))
                      (if (and (pair? (car g264))
                               (field? (caar g264))
                               (pair? (cdar g264))
                               (null? (cddar g264)))
                        (g263 (cdr g264)
                              (cons (cadar g264) g262)
                              (cons (caar g264) g261))
                        (g266))))
                  (g266))
                (if (field? (car g260))
                  (g259 (cdr g260) (cons (car g260) g258))
                  (g266))))
            (g266)))
        (g266)))))
(define home-directory
  (or (getenv "HOME")
      (error "environment variable HOME is not defined")))
(defmacro recur args `(let ,@args))
(defmacro
  rec
  args
  (match args
         (((? symbol? x) v) `(letrec ((,x ,v)) ,x))))
(defmacro
  parameterize
  args
  (match args ((bindings exp ...) `(begin ,@exp))))
(define gensym gentemp)
(define expand-once macroexpand-1)
(defmacro check-increment-counter args #f)
(define symbol-append
  (lambda l
    (string->symbol
      (apply string-append
             (map (lambda (x) (format #f "~a" x)) l)))))
(define gensym gentemp)
(define andmap
  (lambda (f . lists)
    (cond ((null? (car lists)) (and))
          ((null? (cdr (car lists)))
           (apply f (map car lists)))
          (else
           (and (apply f (map car lists))
                (apply andmap f (map cdr lists)))))))
(define true-object? (lambda (x) (eq? #t x)))
(define false-object? (lambda (x) (eq? #f x)))
(define void (lambda () (cond (#f #f))))
(defmacro
  when
  args
  (match args
         ((tst body __1)
          `(if ,tst (begin ,@body (void)) (void)))))
(defmacro
  unless
  args
  (match args
         ((tst body __1)
          `(if ,tst (void) (begin ,@body (void))))))
(define should-never-reach
  (lambda (form)
    (slib:error "fell off end of " form)))
(define make-cvector make-vector)
(define cvector vector)
(define cvector-length vector-length)
(define cvector-ref vector-ref)
(define cvector->list vector->list)
(define list->cvector list->vector)
(define-const-structure (record _))
(defmacro
  record
  args
  (match args
         ((((? symbol? id) exp) ...)
          `(make-record
             (list ,@(map (lambda (i x) `(cons ',i ,x)) id exp))))
         (_ (slib:error "syntax error at " `(record ,@args)))))
(defmacro
  field
  args
  (match args
         (((? symbol? id) exp)
          `(match ,exp
                  (($ record x)
                   (match (assq ',id x)
                          (#f
                           (slib:error
                             "no field "
                             ,id
                             'in
                             (cons 'record (map car x))))
                          ((_ . x) x)))
                  (_ (slib:error "not a record: " '(field ,id _)))))
         (_ (slib:error "syntax error at " `(field ,@args)))))
(define-const-structure (module _))
(defmacro
  module
  args
  (match args
         (((i ...) defs ...)
          `(let ()
             ,@defs
             (make-module
               (record ,@(map (lambda (x) (list x x)) i)))))
         (_ (slib:error "syntax error at " `(module ,@args)))))
(defmacro
  import
  args
  (match args
         ((((mod defs ...) ...) body __1)
          (let* ((m (map (lambda (_) (gentemp)) mod))
                 (newdefs
                   (let loop ((mod-names m) (l-defs defs))
                     (if (null? mod-names)
                       '()
                       (append
                         (let ((m (car mod-names)))
                           (map (match-lambda
                                  ((? symbol? x) `(,x (field ,x ,m)))
                                  (((? symbol? i) (? symbol? e))
                                   `(,i (field ,e ,m)))
                                  (x (slib:error "ill-formed definition: " x)))
                                (car l-defs)))
                         (loop (cdr mod-names) (cdr l-defs)))))))
            `(let (unquote
                   (map (lambda (m mod)
                          `(,m (match ,mod (($ module x) x))))
                        m
                        mod))
               (let ,newdefs body ...))))))
(define raise
  (lambda vals
    (slib:error "Unhandled exception " vals)))
(defmacro
  fluid-let
  args
  (match args
         ((((x val) ...) body __1)
          (let ((old-x (map (lambda (_) (gentemp)) x))
                (swap-x (map (lambda (_) (gentemp)) x))
                (swap (gentemp)))
            `(let ,(map list old-x val)
               (let ((,swap
                      (lambda ()
                        (let ,(map list swap-x old-x)
                          ,@(map (lambda (old x) `(set! ,old ,x)) old-x x)
                          ,@(map (lambda (x swap) `(set! ,x ,swap))
                                 x
                                 swap-x)))))
                 (dynamic-wind ,swap (lambda () ,@body) ,swap)))))
         (_ (slib:error
              "syntax error at "
              `(fluid-let ,@args)))))
(defmacro
  handle
  args
  (match args
         ((e h)
          (let ((k (gentemp)) (exn (gentemp)))
            `((call-with-current-continuation
                (lambda (k)
                  (fluid-let
                    ((raise (lambda ,exn (k (lambda () (apply ,h ,exn))))))
                    (let ((v ,e)) (lambda () v))))))))
         (_ (slib:error "syntax error in " `(handle ,@args)))))
(defmacro
  :
  args
  (match args ((typeexp exp) exp)))
(defmacro
  module:
  args
  (match args
         ((((i type) ...) defs ...)
          `(let ()
             ,@defs
             (make-module
               (record
                 ,@(map (lambda (i type) `(,i (: ,type ,i))) i type)))))))
(defmacro
  define:
  args
  (match args
         ((name type exp) `(define ,name (: ,type ,exp)))))
(define st:failure
  (lambda (chk fmt . args)
    (slib:error
      (apply format
             #f
             (string-append "~a : " fmt)
             chk
             args))))
(defmacro
  check-bound
  args
  (match args
         ((var) var)
         (x (st:failure `(check-bound ,@x) "syntax-error"))))
(defmacro
  clash
  args
  (match args
         ((name info ...) name)
         (x (st:failure `(clash ,@x) "syntax error"))))
(defmacro
  check-lambda
  args
  (match args
         (((id info ...) (? symbol? args) body __1)
          `(lambda ,args
             (check-increment-counter ,id)
             ,@body))
         (((id info ...) args body __1)
          (let* ((n 0)
                 (chk (let loop ((a args) (nargs 0))
                        (cond ((pair? a) (loop (cdr a) (+ 1 nargs)))
                              ((null? a)
                               (set! n nargs)
                               `(= ,nargs (length args)))
                              (else
                               (set! n nargs)
                               `(<= ,nargs (length args))))))
                 (incr (if (number? id)
                         `(check-increment-counter ,id)
                         #f)))
            `(let ((lam (lambda ,args ,@body)))
               (lambda args
                 ,incr
                 (if ,chk
                   (apply lam args)
                   ,(if (eq? '= (car chk))
                      `(st:failure
                         '(check-lambda ,id ,@info)
                         "requires ~a arguments, passed: ~a"
                         ,n
                         args)
                      `(st:failure
                         '(check-lambda ,id ,@info)
                         "requires >= ~a arguments, passed: ~a"
                         ,n
                         args)))))))
         (x (st:failure `(check-lambda ,@x) "syntax error"))))
(defmacro
  check-ap
  args
  (match args
         (((id info ...) (? symbol? f) args ...)
          `(begin
             (check-increment-counter ,id)
             (if (procedure? ,f)
               (,f ,@args)
               (st:failure
                 '(check-ap ,id ,@info)
                 "not a procedure: ~a"
                 ,f))))
         (((id info ...) f args ...)
          `((lambda (proc . args)
              (check-increment-counter ,id)
              (if (procedure? proc)
                (apply proc args)
                (st:failure
                  '(check-ap ,id ,@info)
                  "not a procedure: ~a"
                  proc)))
            ,f
            ,@args))
         (x (st:failure `(check-ap ,@x) "syntax error"))))
(defmacro
  check-field
  args
  (match args
         (((id info ...) (? symbol? f) exp)
          `(match ,exp
                  (($ record x)
                   (match (assq ',f x)
                          (#f
                           (st:failure
                             '(check-field ,id ,@info)
                             "no ~a field in (record ~a)"
                             ',f
                             (map car x)))
                          ((_ . x) x)))
                  (v (st:failure
                       '(check-field ,id ,@info)
                       "not a record: ~a"
                       v))))
         (x (st:failure `(check-field ,@x) "syntax error"))))
(defmacro
  check-match
  args
  (match args
         (((id info ...) exp (and clause (pat _ __1)) ...)
          (letrec ((last (lambda (pl)
                           (if (null? (cdr pl)) (car pl) (last (cdr pl))))))
            (if (match (last pat)
                       ((? symbol?) #t)
                       (('and subp ...) (andmap symbol? subp))
                       (_ #f))
              `(begin
                 (check-increment-counter ,id)
                 (match ,exp ,@clause))
              `(begin
                 (check-increment-counter ,id)
                 (match ,exp
                        ,@clause
                        (x (st:failure
                             '(check-match ,id ,@info)
                             "no matching clause for ~a"
                             x)))))))
         (x (st:failure `(check-match ,@x) "syntax error"))))
(defmacro
  check-:
  args
  (match args
         (((id info ...) typeexp exp)
          `(st:failure
             '(check-: ,id ,@info)
             "static type annotation reached"))
         (x (st:failure `(check-: ,@x) "syntax error"))))
(defmacro
  make-check-typed
  args
  (match args
         ((prim)
          (let ((chkprim (symbol-append 'check- prim)))
            (list 'defmacro
                  chkprim
                  'id
                  (list 'quasiquote
                        `(lambda a
                           (check-increment-counter (,'unquote (car id)))
                           (if (null? a)
                             (,prim)
                             (st:failure
                               (cons ',chkprim '(,'unquote id))
                               "invalid arguments: ~a"
                               a)))))))
         ((prim '_)
          (let ((chkprim (symbol-append 'check- prim)))
            (list 'defmacro
                  chkprim
                  'id
                  (list 'quasiquote
                        `(lambda a
                           (check-increment-counter (,'unquote (car id)))
                           (if (= 1 (length a))
                             (,prim (car a))
                             (st:failure
                               (cons ',chkprim '(,'unquote id))
                               "invalid arguments: ~a"
                               a)))))))
         ((prim type1)
          (let ((chkprim (symbol-append 'check- prim)))
            (list 'defmacro
                  chkprim
                  'id
                  (list 'quasiquote
                        `(lambda a
                           (check-increment-counter (,'unquote (car id)))
                           (if (and (= 1 (length a)) (,type1 (car a)))
                             (,prim (car a))
                             (st:failure
                               (cons ',chkprim '(,'unquote id))
                               "invalid arguments: ~a"
                               a)))))))
         ((prim '_ '_)
          (let ((chkprim (symbol-append 'check- prim)))
            (list 'defmacro
                  chkprim
                  'id
                  (list 'quasiquote
                        `(lambda a
                           (check-increment-counter (,'unquote (car id)))
                           (if (= 2 (length a))
                             (,prim (car a) (cadr a))
                             (st:failure
                               (cons ',chkprim '(,'unquote id))
                               "invalid arguments: ~a"
                               a)))))))
         ((prim '_ type2)
          (let ((chkprim (symbol-append 'check- prim)))
            (list 'defmacro
                  chkprim
                  'id
                  (list 'quasiquote
                        `(lambda a
                           (check-increment-counter (,'unquote (car id)))
                           (if (and (= 2 (length a)) (,type2 (cadr a)))
                             (,prim (car a) (cadr a))
                             (st:failure
                               (cons ',chkprim '(,'unquote id))
                               "invalid arguments: ~a"
                               a)))))))
         ((prim type1 '_)
          (let ((chkprim (symbol-append 'check- prim)))
            (list 'defmacro
                  chkprim
                  'id
                  (list 'quasiquote
                        `(lambda a
                           (check-increment-counter (,'unquote (car id)))
                           (if (and (= 2 (length a)) (,type1 (car a)))
                             (,prim (car a) (cadr a))
                             (st:failure
                               (cons ',chkprim '(,'unquote id))
                               "invalid arguments: ~a"
                               a)))))))
         ((prim type1 type2)
          (let ((chkprim (symbol-append 'check- prim)))
            (list 'defmacro
                  chkprim
                  'id
                  (list 'quasiquote
                        `(lambda a
                           (check-increment-counter (,'unquote (car id)))
                           (if (and (= 2 (length a))
                                    (,type1 (car a))
                                    (,type2 (cadr a)))
                             (,prim (car a) (cadr a))
                             (st:failure
                               (cons ',chkprim '(,'unquote id))
                               "invalid arguments: ~a"
                               a)))))))
         ((prim types ...)
          (let ((nargs (length types))
                (chkprim (symbol-append 'check- prim))
                (types (map (match-lambda ('_ '(lambda (_) #t)) (x x))
                            types)))
            (list 'defmacro
                  chkprim
                  'id
                  (list 'quasiquote
                        `(lambda a
                           (check-increment-counter (,'unquote (car id)))
                           (if (and (= ,nargs (length a))
                                    (andmap
                                      (lambda (f a) (f a))
                                      (list ,@types)
                                      a))
                             (apply ,prim a)
                             (st:failure
                               (cons ',chkprim '(,'unquote id))
                               "invalid arguments: ~a"
                               a)))))))))
(defmacro
  make-check-selector
  args
  (match args
         ((prim pat)
          (let ((chkprim (symbol-append 'check- prim)))
            (list 'defmacro
                  chkprim
                  'id
                  (list 'quasiquote
                        `(lambda a
                           (check-increment-counter (,'unquote (car id)))
                           (match a
                                  ((,pat) x)
                                  (_ (st:failure
                                       (cons ',chkprim '(,'unquote id))
                                       "invalid arguments: ~a"
                                       a))))))))))
(make-check-typed number? _)
(make-check-typed null? _)
(make-check-typed char? _)
(make-check-typed symbol? _)
(make-check-typed string? _)
(make-check-typed vector? _)
(make-check-typed box? _)
(make-check-typed pair? _)
(make-check-typed procedure? _)
(make-check-typed eof-object? _)
(make-check-typed input-port? _)
(make-check-typed output-port? _)
(make-check-typed true-object? _)
(make-check-typed false-object? _)
(make-check-typed boolean? _)
(make-check-typed list? _)
(make-check-typed not _)
(make-check-typed eqv? _ _)
(make-check-typed eq? _ _)
(make-check-typed equal? _ _)
(make-check-typed cons _ _)
(make-check-selector car (x . _))
(make-check-selector cdr (_ . x))
(make-check-selector caar ((x . _) . _))
(make-check-selector cadr (_ x . _))
(make-check-selector cdar ((_ . x) . _))
(make-check-selector cddr (_ _ . x))
(make-check-selector caaar (((x . _) . _) . _))
(make-check-selector caadr (_ (x . _) . _))
(make-check-selector cadar ((_ x . _) . _))
(make-check-selector caddr (_ _ x . _))
(make-check-selector cdaar (((_ . x) . _) . _))
(make-check-selector cdadr (_ (_ . x) . _))
(make-check-selector cddar ((_ _ . x) . _))
(make-check-selector cdddr (_ _ _ . x))
(make-check-selector
  caaaar
  ((((x . _) . _) . _) . _))
(make-check-selector
  caaadr
  (_ ((x . _) . _) . _))
(make-check-selector
  caadar
  ((_ (x . _) . _) . _))
(make-check-selector caaddr (_ _ (x . _) . _))
(make-check-selector
  cadaar
  (((_ x . _) . _) . _))
(make-check-selector cadadr (_ (_ x . _) . _))
(make-check-selector caddar ((_ _ x . _) . _))
(make-check-selector cadddr (_ _ _ x . _))
(make-check-selector
  cdaaar
  ((((_ . x) . _) . _) . _))
(make-check-selector
  cdaadr
  (_ ((_ . x) . _) . _))
(make-check-selector
  cdadar
  ((_ (_ . x) . _) . _))
(make-check-selector cdaddr (_ _ (_ . x) . _))
(make-check-selector
  cddaar
  (((_ _ . x) . _) . _))
(make-check-selector cddadr (_ (_ _ . x) . _))
(make-check-selector cdddar ((_ _ _ . x) . _))
(make-check-selector cddddr (_ _ _ _ . x))
(make-check-typed set-car! pair? _)
(make-check-typed set-cdr! pair? _)
(defmacro
  check-list
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (apply list a)))
(make-check-typed length list?)
(defmacro
  check-append
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (let loop ((b a))
       (match b
              (() #t)
              ((l) #t)
              (((? list?) . y) (loop y))
              (_ (st:failure
                   (cons 'check-append ',id)
                   "invalid arguments: ~a"
                   a))))
     (apply append a)))
(make-check-typed reverse list?)
(make-check-typed list-tail list? number?)
(make-check-typed list-ref list? number?)
(make-check-typed memq _ list?)
(make-check-typed memv _ list?)
(make-check-typed member _ list?)
(defmacro
  check-assq
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (= 2 (length a))
              (list? (cadr a))
              (andmap pair? (cadr a)))
       (assq (car a) (cadr a))
       (st:failure
         (cons 'check-assq ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-assv
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (= 2 (length a))
              (list? (cadr a))
              (andmap pair? (cadr a)))
       (assv (car a) (cadr a))
       (st:failure
         (cons 'check-assv ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-assoc
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (= 2 (length a))
              (list? (cadr a))
              (andmap pair? (cadr a)))
       (assoc (car a) (cadr a))
       (st:failure
         (cons 'check-assoc ',id)
         "invalid arguments: ~a"
         a))))
(make-check-typed symbol->string symbol?)
(make-check-typed string->symbol string?)
(make-check-typed complex? _)
(make-check-typed real? _)
(make-check-typed rational? _)
(make-check-typed integer? _)
(make-check-typed exact? number?)
(make-check-typed inexact? number?)
(defmacro
  check-=
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (<= 2 (length a)) (andmap number? a))
       (apply = a)
       (st:failure
         (cons 'check-= ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-<
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (<= 2 (length a)) (andmap number? a))
       (apply < a)
       (st:failure
         (cons 'check-< ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check->
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (<= 2 (length a)) (andmap number? a))
       (apply > a)
       (st:failure
         (cons 'check-> ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-<=
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (<= 2 (length a)) (andmap number? a))
       (apply <= a)
       (st:failure
         (cons 'check-<= ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check->=
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (<= 2 (length a)) (andmap number? a))
       (apply >= a)
       (st:failure
         (cons 'check->= ',id)
         "invalid arguments: ~a"
         a))))
(make-check-typed zero? number?)
(make-check-typed positive? number?)
(make-check-typed negative? number?)
(make-check-typed odd? number?)
(make-check-typed even? number?)
(defmacro
  check-max
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (<= 1 (length a)) (andmap number? a))
       (apply max a)
       (st:failure
         (cons 'check-max ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-min
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (<= 1 (length a)) (andmap number? a))
       (apply min a)
       (st:failure
         (cons 'check-min ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-+
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (andmap number? a)
       (apply + a)
       (st:failure
         (cons 'check-+ ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-*
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (andmap number? a)
       (apply * a)
       (st:failure
         (cons 'check-* ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check--
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (<= 1 (length a)) (andmap number? a))
       (apply - a)
       (st:failure
         (cons 'check-- ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-/
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (<= 1 (length a)) (andmap number? a))
       (apply / a)
       (st:failure
         (cons 'check-/ ',id)
         "invalid arguments: ~a"
         a))))
(make-check-typed abs number?)
(make-check-typed quotient number? number?)
(make-check-typed remainder number? number?)
(make-check-typed modulo number? number?)
(defmacro
  check-gcd
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (andmap number? a)
       (apply gcd a)
       (st:failure
         (cons 'check-gcd ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-lcm
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (andmap number? a)
       (apply lcm a)
       (st:failure
         (cons 'check-lcm ',id)
         "invalid arguments: ~a"
         a))))
(make-check-typed numerator number?)
(make-check-typed denominator number?)
(make-check-typed floor number?)
(make-check-typed ceiling number?)
(make-check-typed truncate number?)
(make-check-typed round number?)
(make-check-typed rationalize number? number?)
(make-check-typed exp number?)
(make-check-typed log number?)
(make-check-typed sin number?)
(make-check-typed cos number?)
(make-check-typed tan number?)
(make-check-typed asin number?)
(make-check-typed acos number?)
(defmacro
  check-atan
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (andmap number? a)
              (pair? a)
              (>= 2 (length a)))
       (apply atan a)
       (st:failure
         (cons 'check-atan ',id)
         "invalid arguments: ~a"
         a))))
(make-check-typed sqrt number?)
(make-check-typed expt number? number?)
(make-check-typed
  make-rectangular
  number?
  number?)
(make-check-typed make-polar number? number?)
(make-check-typed real-part number?)
(make-check-typed imag-part number?)
(make-check-typed magnitude number?)
(make-check-typed angle number?)
(make-check-typed exact->inexact number?)
(make-check-typed inexact->exact number?)
(defmacro
  check-number->string
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (andmap number? a)
              (pair? a)
              (>= 2 (length a)))
       (apply number->string a)
       (st:failure
         (cons 'check-number->string ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-string->number
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (pair? a)
              (string? (car a))
              (>= 2 (length a))
              (or (null? (cdr a)) (number? (cadr a))))
       (apply string->number a)
       (st:failure
         (cons 'check-string->number ',id)
         "invalid arguments: ~a"
         a))))
(make-check-typed char=? char? char?)
(make-check-typed char<? char? char?)
(make-check-typed char>? char? char?)
(make-check-typed char<=? char? char?)
(make-check-typed char>=? char? char?)
(make-check-typed char-ci=? char? char?)
(make-check-typed char-ci<? char? char?)
(make-check-typed char-ci>? char? char?)
(make-check-typed char-ci<=? char? char?)
(make-check-typed char-ci>=? char? char?)
(make-check-typed char-alphabetic? char?)
(make-check-typed char-numeric? char?)
(make-check-typed char-whitespace? char?)
(make-check-typed char-upper-case? char?)
(make-check-typed char-lower-case? char?)
(make-check-typed char->integer char?)
(make-check-typed integer->char number?)
(make-check-typed char-upcase char?)
(make-check-typed char-downcase char?)
(defmacro
  check-make-string
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (pair? a)
              (number? (car a))
              (>= 2 (length a))
              (or (null? (cdr a)) (char? (cadr a))))
       (apply make-string a)
       (st:failure
         (cons 'check-make-string ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-string
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (andmap char? a)
       (apply string a)
       (st:failure
         (cons 'check-string ',id)
         "invalid arguments: ~a"
         a))))
(make-check-typed string-length string?)
(make-check-typed string-ref string? number?)
(make-check-typed
  string-set!
  string?
  number?
  char?)
(make-check-typed string=? string? string?)
(make-check-typed string<? string? string?)
(make-check-typed string>? string? string?)
(make-check-typed string<=? string? string?)
(make-check-typed string>=? string? string?)
(make-check-typed string-ci=? string? string?)
(make-check-typed string-ci<? string? string?)
(make-check-typed string-ci>? string? string?)
(make-check-typed string-ci<=? string? string?)
(make-check-typed string-ci>=? string? string?)
(make-check-typed
  substring
  string?
  number?
  number?)
(defmacro
  check-string-append
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (andmap string? a)
       (apply string-append a)
       (st:failure
         (cons 'check-string-append ',id)
         "invalid arguments: ~a"
         a))))
(make-check-typed string->list string?)
(defmacro
  check-list->string
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (= 1 (length a))
              (list? (car a))
              (andmap char? (car a)))
       (list->string (car a))
       (st:failure
         (cons 'check-list->string ',id)
         "invalid arguments: ~a"
         a))))
(make-check-typed string-copy string?)
(make-check-typed string-fill! string? char?)
(make-check-typed make-vector number? _)
(defmacro
  check-vector
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (apply vector a)))
(make-check-typed vector-length vector?)
(make-check-typed vector-ref vector? number?)
(make-check-typed vector-set! vector? number? _)
(make-check-typed vector->list vector?)
(make-check-typed list->vector list?)
(make-check-typed vector-fill! vector? _)
(defmacro
  check-apply
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (pair? a)
       (let loop ((arg (cdr a)))
         (match arg
                (((? list?)) (apply apply a))
                ((_ . y) (loop y))
                (_ (st:failure
                     (cons 'check-apply ',id)
                     "invalid arguments: ~a"
                     a))))
       (st:failure
         `(check-apply ,@id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-map
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (<= 2 (length a))
              (procedure? (car a))
              (andmap list? (cdr a)))
       (apply map a)
       (st:failure
         (cons 'check-map ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-for-each
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (<= 2 (length a))
              (procedure? (car a))
              (andmap list? (cdr a)))
       (apply for-each a)
       (st:failure
         (cons 'check-for-each ',id)
         "invalid arguments: ~a"
         a))))
(make-check-typed force procedure?)
(defmacro
  check-call-with-current-continuation
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (= 1 (length a)) (procedure? (car a)))
       (call-with-current-continuation
         (lambda (k)
           ((car a) (check-lambda (continuation) (x) (k x)))))
       (st:failure
         (cons 'check-call-with-current-continuation ',id)
         "invalid arguments: ~a"
         a))))
(make-check-typed
  call-with-input-file
  string?
  procedure?)
(make-check-typed
  call-with-output-file
  string?
  procedure?)
(make-check-typed input-port? _)
(make-check-typed output-port? _)
(make-check-typed current-input-port)
(make-check-typed current-output-port)
(make-check-typed
  with-input-from-file
  string?
  procedure?)
(make-check-typed
  with-output-to-file
  string?
  procedure?)
(make-check-typed open-input-file string?)
(make-check-typed open-output-file string?)
(make-check-typed close-input-port input-port?)
(make-check-typed close-output-port output-port?)
(defmacro
  check-read
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (or (null? a)
             (and (= 1 (length a)) (input-port? (car a))))
       (apply read a)
       (st:failure
         (cons 'check-read ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-read-char
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (or (null? a)
             (and (= 1 (length a)) (input-port? (car a))))
       (apply read-char a)
       (st:failure
         (cons 'check-read-char ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-peek-char
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (or (null? a)
             (and (= 1 (length a)) (input-port? (car a))))
       (apply peek-char a)
       (st:failure
         (cons 'check-peek-char ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-char-ready?
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (or (null? a)
             (and (= 1 (length a)) (input-port? (car a))))
       (apply char-ready? a)
       (st:failure
         (cons 'check-char-ready? ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-write
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (pair? a)
              (or (null? (cdr a)) (output-port? (cadr a))))
       (apply write a)
       (st:failure
         (cons 'check-write ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-display
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (pair? a)
              (or (null? (cdr a)) (output-port? (cadr a))))
       (apply display a)
       (st:failure
         (cons 'check-display ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-newline
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (or (null? a) (output-port? (car a)))
       (apply newline a)
       (st:failure
         (cons 'check-newline ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-write-char
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (pair? a)
              (char? (car a))
              (or (null? (cdr a)) (output-port? (cadr a))))
       (apply write-char a)
       (st:failure
         (cons 'check-write-char ',id)
         "invalid arguments: ~a"
         a))))
(make-check-typed load string?)
(make-check-typed transcript-on string?)
(make-check-typed transcript-off)
(defmacro
  check-symbol-append
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (apply symbol-append a)))
(make-check-typed box _)
(make-check-typed unbox box?)
(make-check-typed set-box! box? _)
(make-check-typed void)
(make-check-typed make-module _)
(defmacro
  check-match:error
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (pair? a)
       (apply match:error a)
       (st:failure
         (cons 'check-match:error ',id)
         "invalid arguments: ~a"
         a))))
(make-check-typed should-never-reach symbol?)
(defmacro
  check-make-cvector
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (if (and (pair? a)
              (number? (car a))
              (= 2 (length a)))
       (apply make-cvector a)
       (st:failure
         (cons 'check-make-cvector ',id)
         "invalid arguments: ~a"
         a))))
(defmacro
  check-cvector
  id
  `(lambda a
     (check-increment-counter ,(car id))
     (apply cvector a)))
(make-check-typed cvector-length cvector?)
(make-check-typed cvector-ref cvector? number?)
(make-check-typed cvector->list cvector?)
(make-check-typed list->cvector list?)
(defmacro
  check-define-const-structure
  args
  (let ((field?
          (lambda (x)
            (or (symbol? x)
                (and (pair? x)
                     (equal? (car x) '!)
                     (pair? (cdr x))
                     (symbol? (cadr x))
                     (null? (cddr x))))))
        (arg-name
          (lambda (x) (if (symbol? x) x (cadr x))))
        (with-mutator? (lambda (x) (not (symbol? x)))))
    (match args
           ((((? symbol? name) (? field? id1) ...))
            (let ((constructor (symbol-append 'make- name))
                  (check-constructor
                    (symbol-append 'check-make- name))
                  (predicate (symbol-append name '?))
                  (access
                    (let loop ((l id1))
                      (cond ((null? l) '())
                            ((eq? '_ (arg-name (car l))) (loop (cdr l)))
                            (else
                             (cons (symbol-append name '- (arg-name (car l)))
                                   (loop (cdr l)))))))
                  (assign
                    (let loop ((l id1))
                      (cond ((null? l) '())
                            ((eq? '_ (arg-name (car l))) (loop (cdr l)))
                            ((not (with-mutator? (car l))) (loop (cdr l)))
                            (else
                             (cons (symbol-append
                                     'set-
                                     name
                                     '-
                                     (arg-name (car l))
                                     '!)
                                   (loop (cdr l)))))))
                  (nargs (length id1)))
              `(begin
                 (define-const-structure (,name ,@id1) ())
                 (defmacro
                   ,check-constructor
                   id
                   (lambda a
                     (check-increment-counter (,'unquote (car id)))
                     (if (= ,nargs (length a))
                       (apply ,constructor a)
                       (st:failure
                         (cons ',check-constructor '(,'unquote id))
                         "invalid arguments: ~a"
                         a))))
                 (make-check-typed ,predicate _)
                 ,@(map (lambda (a) `(make-check-typed ,a ,predicate))
                        access)
                 ,@(map (lambda (a) `(make-check-typed ,a ,predicate _))
                        assign))))
           (x (st:failure
                `(check-define-const-structure ,@x)
                "syntax error")))))
(if (equal? '(match 1) (macroexpand-1 '(match 1)))
  (load "/home/wright/scheme/match/match-slib.scm"))
(define sprintf
  (lambda args (apply format #f args)))
(define printf
  (lambda args (apply format #t args)))
(define disaster
  (lambda (context fmt . args)
    (slib:error
      (apply sprintf
             (string-append "in ~a: " fmt)
             context
             args))))
(define use-error
  (lambda (fmt . args)
    (slib:error (apply sprintf fmt args))))
(define syntax-err
  (lambda (context fmt . args)
    (newline)
    (if context (pretty-print context))
    (slib:error
      (apply sprintf
             (string-append "in syntax: " fmt)
             args))))
(define flush-output force-output)
(define print-context
  (lambda (obj depth)
    (pretty-print
      (recur loop
             ((obj obj) (n 0))
             (if (pair? obj)
               (if (< n depth)
                 (cons (loop (car obj) (+ 1 n))
                       (loop (cdr obj) n))
                 '(...))
               obj)))))
(define *box-tag* (gensym))
(define box (lambda (a) (cons *box-tag* a)))
(define box?
  (lambda (b)
    (and (pair? b) (eq? (car b) *box-tag*))))
(define unbox cdr)
(define box-1 cdr)
(define set-box! set-cdr!)
(define sort-list sort)
(define expand-once-if-macro
  (lambda (e)
    (and (macro? (car e)) (macroexpand-1 e))))
(define ormap
  (lambda (f . lists)
    (if (null? (car lists))
      (or)
      (or (apply f (map car lists))
          (apply ormap f (map cdr lists))))))
(define call/cc call-with-current-continuation)
(define (cpu-time) 0)
(define (pretty-print x) (display x) (newline))
(define clock-granularity 1.0e-3)
(define set-vector! vector-set!)
(define set-string! string-set!)
(define maplr
  (lambda (f l)
    (match l
           (() '())
           ((x . y) (let ((v (f x))) (cons v (maplr f y)))))))
(define maprl
  (lambda (f l)
    (match l
           (() '())
           ((x . y) (let ((v (maprl f y))) (cons (f x) v))))))
(define foldl
  (lambda (f i l)
    (recur loop
           ((l l) (acc i))
           (match l (() acc) ((x . y) (loop y (f x acc)))))))
(define foldr
  (lambda (f i l)
    (recur loop
           ((l l))
           (match l (() i) ((x . y) (f x (loop y)))))))
(define filter
  (lambda (p l)
    (match l
           (() '())
           ((x . y)
            (if (p x) (cons x (filter p y)) (filter p y))))))
(define filter-map
  (lambda (p l)
    (match l
           (() '())
           ((x . y)
            (match (p x)
                   (#f (filter-map p y))
                   (x (cons x (filter-map p y))))))))
(define rac
  (lambda (l)
    (match l ((last) last) ((_ . rest) (rac rest)))))
(define rdc
  (lambda (l)
    (match l
           ((_) '())
           ((x . rest) (cons x (rdc rest))))))
(define map-with-n
  (lambda (f l)
    (recur loop
           ((l l) (n 0))
           (match l
                  (() '())
                  ((x . y)
                   (let ((v (f x n))) (cons v (loop y (+ 1 n)))))))))
(define readfile
  (lambda (f)
    (with-input-from-file
      f
      (letrec ((rf (lambda ()
                     (match (read)
                            ((? eof-object?) '())
                            (sexp (cons sexp (rf)))))))
        rf))))
(define map2
  (lambda (f a b)
    (match (cons a b)
           ((()) '())
           (((ax . ay) bx . by)
            (let ((v (f ax bx))) (cons v (map2 f ay by))))
           (else (error 'map2 "lists differ in length")))))
(define for-each2
  (lambda (f a b)
    (match (cons a b)
           ((()) (void))
           (((ax . ay) bx . by)
            (f ax bx)
            (for-each2 f ay by))
           (else (error 'for-each2 "lists differ in length")))))
(define andmap2
  (lambda (f a b)
    (match (cons a b)
           ((()) (and))
           (((ax) bx) (f ax bx))
           (((ax . ay) bx . by)
            (and (f ax bx) (andmap2 f ay by)))
           (else (error 'andmap2 "lists differ in length")))))
(define ormap2
  (lambda (f a b)
    (match (cons a b)
           ((()) (or))
           (((ax) bx) (f ax bx))
           (((ax . ay) bx . by)
            (or (f ax bx) (ormap2 f ay by)))
           (else (error 'ormap2 "lists differ in length")))))
(define empty-set '())
(define empty-set? null?)
(define set (lambda l (list->set l)))
(define list->set
  (match-lambda
    (() '())
    ((x . y)
     (if (memq x y)
       (list->set y)
       (cons x (list->set y))))))
(define element-of?
  (lambda (x set) (and (memq x set) #t)))
(define cardinality length)
(define set<=
  (lambda (a b)
    (foldr (lambda (a-elt acc) (and acc (memq a-elt b) #t))
           (and)
           a)))
(define set-eq?
  (lambda (a b)
    (and (= (cardinality a) (cardinality b))
         (set<= a b))))
(define union2
  (lambda (a b)
    (if (null? b)
      a
      (foldr (lambda (x b) (if (memq x b) b (cons x b)))
             b
             a))))
(define union (lambda l (foldr union2 '() l)))
(define setdiff2
  (lambda (a b)
    (if (null? b)
      a
      (foldr (lambda (x c) (if (memq x b) c (cons x c)))
             '()
             a))))
(define setdiff
  (lambda l
    (if (null? l)
      '()
      (setdiff2 (car l) (foldr union2 '() (cdr l))))))
(define intersect2
  (lambda (a b)
    (if (null? b)
      a
      (foldr (lambda (x c) (if (memq x b) (cons x c) c))
             '()
             a))))
(define intersect
  (lambda l
    (if (null? l) '() (foldl intersect2 (car l) l))))
(define-const-structure (some _))
(define-const-structure (none))
(define none (make-none))
(define some make-some)
(define-const-structure (and exps))
(define-const-structure (app exp exps))
(define-const-structure (begin exps))
(define-const-structure (const val pred))
(define-const-structure (if exp1 exp2 exp3))
(define-const-structure (lam names body))
(define-const-structure (let binds body))
(define-const-structure (let* binds body))
(define-const-structure (letr binds body))
(define-const-structure (or exps))
(define-const-structure (prim name))
(define-const-structure (delay exp))
(define-const-structure (set! (! name) exp))
(define-const-structure (var (! name)))
(define-const-structure (vlam names name body))
(define-const-structure (match exp mclauses))
(define-const-structure (record binds))
(define-const-structure (field name exp))
(define-const-structure (cast type exp))
(define-const-structure (body defs exps))
(define-const-structure (bind name exp))
(define-const-structure (mclause pat body fail))
(define-const-structure (pvar name))
(define-const-structure (pany))
(define-const-structure (pelse))
(define-const-structure (pconst name pred))
(define-const-structure (pobj name pats))
(define-const-structure (ppred name))
(define-const-structure (pand pats))
(define-const-structure (pnot pat))
(define-const-structure (define name (! exp)))
(define-const-structure
  (defstruct
    tag
    args
    make
    pred
    get
    set
    getn
    setn
    mutable))
(define-const-structure (datatype _))
(define-const-structure
  (variant con pred arg-types))
(define-structure
  (name name
        ty
        timestamp
        occ
        mutated
        gdef
        primitive
        struct
        pure
        predicate
        variant
        selector))
(define-structure (type ty exp))
(define-const-structure (shape _ _))
(define-const-structure (check _ _))
(define parse-def
  (lambda (def)
    (let ((parse-name
            (match-lambda
              ((? symbol? s)
               (if (keyword? s)
                 (syntax-err def "invalid use of keyword ~a" s)
                 s))
              (n (syntax-err def "invalid variable at ~a" n)))))
      (match def
             (('extend-syntax ((? symbol? name) . _) . _)
              (printf
                "Note: installing but _not_ checking (extend-syntax (~a) ...)~%"
                name)
              (eval def)
              '())
             (('extend-syntax . _)
              (syntax-err def "invalid syntax"))
             (('defmacro (? symbol? name) . _)
              (printf
                "Note: installing but _not_ checking (defmacro ~a ...)~%"
                name)
              (eval def)
              '())
             (('defmacro . _)
              (syntax-err def "invalid syntax"))
             (('define (? symbol? n) e)
              (list (make-define (parse-name n) (parse-exp e))))
             (('define (n . args) . body)
              (list (make-define
                      (parse-name n)
                      (parse-exp `(lambda ,args ,@body)))))
             (('define . _) (syntax-err def "at define"))
             (('begin . defs)
              (foldr append '() (smap parse-def defs)))
             (('define-structure (n . args))
              (parse-def `(define-structure (,n ,@args) ())))
             (('define-structure (n . args) inits)
              (let ((m-args (smap (lambda (x) `(! ,x)) args))
                    (m-inits
                      (smap (match-lambda
                              ((x e) `((! ,x) ,e))
                              (_ (syntax-err
                                   def
                                   "invalid structure initializer")))
                            inits)))
                (parse-def
                  `(define-const-structure (,n ,@m-args) ,m-inits))))
             (('define-const-structure ((? symbol? n) . args))
              (parse-def
                `(define-const-structure (,n ,@args) ())))
             (('define-const-structure
               ((? symbol? n) . args)
               ())
              (letrec ((smap-with-n
                         (lambda (f l)
                           (recur loop
                                  ((l l) (n 0))
                                  (match l
                                         (() '())
                                         ((x . y)
                                          (let ((v (f x n)))
                                            (cons v (loop y (+ 1 n)))))
                                         (_ (syntax-err l "invalid list"))))))
                       (parse-arg
                         (lambda (a index)
                           (match a
                                  (('! '_)
                                   (list none
                                         none
                                         (some (symbol-append
                                                 n
                                                 '-
                                                 (+ index 1)))
                                         (some (symbol-append
                                                 'set-
                                                 n
                                                 '-
                                                 (+ index 1)
                                                 '!))
                                         #t))
                                  (('! a)
                                   (let ((a (parse-name a)))
                                     (list (some (symbol-append n '- a))
                                           (some (symbol-append
                                                   'set-
                                                   n
                                                   '-
                                                   a
                                                   '!))
                                           (some (symbol-append
                                                   n
                                                   '-
                                                   (+ index 1)))
                                           (some (symbol-append
                                                   'set-
                                                   n
                                                   '-
                                                   (+ index 1)
                                                   '!))
                                           #t)))
                                  ('_
                                   (list none
                                         none
                                         (some (symbol-append
                                                 n
                                                 '-
                                                 (+ index 1)))
                                         none
                                         #f))
                                  (a (let ((a (parse-name a)))
                                       (list (some (symbol-append n '- a))
                                             none
                                             (some (symbol-append
                                                     n
                                                     '-
                                                     (+ index 1)))
                                             none
                                             #f)))))))
                (let* ((arg-info (smap-with-n parse-arg args))
                       (get (map car arg-info))
                       (set (map cadr arg-info))
                       (getn (map caddr arg-info))
                       (setn (map cadddr arg-info))
                       (mutable
                         (map (lambda (x) (car (cddddr x))) arg-info)))
                  (list (make-defstruct
                          n
                          (cons n args)
                          (symbol-append 'make- n)
                          (symbol-append n '?)
                          get
                          set
                          getn
                          setn
                          mutable)))))
             (('define-const-structure
               ((? symbol? n) . args)
               inits)
              (syntax-err
                def
                "sorry, structure initializers are not supported"))
             (('datatype . d)
              (let* ((parse-variant
                       (match-lambda
                         (((? symbol? con) ? list? args)
                          (let ((n (parse-name con)))
                            (make-variant
                              (symbol-append 'make- n)
                              (symbol-append n '?)
                              (cons con args))))
                         (_ (syntax-err def "invalid datatype syntax"))))
                     (parse-dt
                       (match-lambda
                         (((? symbol? type) . variants)
                          (cons (list (parse-name type))
                                (smap parse-variant variants)))
                         ((((? symbol? type) ? list? targs) . variants)
                          (cons (cons (parse-name type)
                                      (smap parse-name targs))
                                (smap parse-variant variants)))
                         (_ (syntax-err def "invalid datatype syntax")))))
                (list (make-datatype (smap parse-dt d)))))
             (((? symbol? k) . _)
              (cond ((and (not (keyword? k))
                          (expand-once-if-macro def))
                     =>
                     parse-def)
                    (else (list (make-define #f (parse-exp def))))))
             (_ (list (make-define #f (parse-exp def))))))))
(define keep-match #t)
(define parse-exp
  (lambda (expression)
    (letrec ((n-primitive (string->symbol "#primitive"))
             (parse-exp
               (match-lambda
                 (('quote (? symbol? s)) (make-const s 'symbol?))
                 ((and m ('quote _)) (parse-exp (quote-tf m)))
                 ((and m ('quasiquote _))
                  (parse-exp (quasiquote-tf m)))
                 ((and m (? box?)) (parse-exp (quote-tf m)))
                 ((and m (? vector?)) (parse-exp (quote-tf m)))
                 ((and m ('cond . _)) (parse-exp (cond-tf m)))
                 ((and m ('case . _)) (parse-exp (case-tf m)))
                 ((and m ('do . _)) (parse-exp (do-tf m)))
                 ((? symbol? s) (make-var (parse-name s)))
                 (#t (make-const #t 'true-object?))
                 (#f (make-const #f 'false-object?))
                 ((? null? c) (make-const c 'null?))
                 ((? number? c) (make-const c 'number?))
                 ((? char? c) (make-const c 'char?))
                 ((? string? c) (make-const c 'string?))
                 ((': ty e1) (make-cast ty (parse-exp e1)))
                 ((and exp ('record . bind))
                  (let ((bindings (smap parse-bind bind)))
                    (no-repeats (map bind-name bindings) exp)
                    (make-record bindings)))
                 ((and exp ('field name e1))
                  (make-field (parse-name name) (parse-exp e1)))
                 ((and exp ('match e clause0 . clauses))
                  (=> fail)
                  (if keep-match
                    (let* ((e2 (parse-exp e))
                           (parse-clause
                             (match-lambda
                               ((p ('=> (? symbol? failsym)) . body)
                                (make-mclause
                                  (parse-pat p expression)
                                  (parse-body
                                    `((let ((,failsym (lambda () (,failsym))))
                                        ,@body)))
                                  failsym))
                               ((p . body)
                                (make-mclause
                                  (parse-pat p expression)
                                  (parse-body body)
                                  #f))
                               (_ (syntax-err exp "invalid match clause")))))
                      (make-match
                        e2
                        (smap parse-clause (cons clause0 clauses))))
                    (fail)))
                 ((and exp ('lambda bind . body))
                  (recur loop
                         ((b bind) (names '()))
                         (match b
                                ((? symbol? n)
                                 (let ((rest (parse-name n)))
                                   (no-repeats (cons rest names) exp)
                                   (make-vlam
                                     (reverse names)
                                     rest
                                     (parse-body body))))
                                (()
                                 (no-repeats names exp)
                                 (make-lam (reverse names) (parse-body body)))
                                ((n . x) (loop x (cons (parse-name n) names)))
                                (_ (syntax-err
                                     exp
                                     "invalid lambda expression")))))
                 (('if e1 e2 e3)
                  (make-if
                    (parse-exp e1)
                    (parse-exp e2)
                    (parse-exp e3)))
                 ((and if-expr ('if e1 e2))
                  (printf "Note: one-armed if: ")
                  (print-context if-expr 2)
                  (make-if
                    (parse-exp e1)
                    (parse-exp e2)
                    (parse-exp '(void))))
                 (('delay e) (make-delay (parse-exp e)))
                 (('set! n e)
                  (make-set! (parse-name n) (parse-exp e)))
                 (('and . args) (make-and (smap parse-exp args)))
                 (('or . args) (make-or (smap parse-exp args)))
                 ((and exp ('let (? symbol? n) bind . body))
                  (let* ((nb (parse-name n))
                         (bindings (smap parse-bind bind)))
                    (no-repeats (map bind-name bindings) exp)
                    (make-app
                      (make-letr
                        (list (make-bind
                                nb
                                (make-lam
                                  (map bind-name bindings)
                                  (parse-body body))))
                        (make-body '() (list (make-var nb))))
                      (map bind-exp bindings))))
                 ((and exp ('let bind . body))
                  (let ((bindings (smap parse-bind bind)))
                    (no-repeats (map bind-name bindings) exp)
                    (make-let bindings (parse-body body))))
                 (('let* bind . body)
                  (make-let*
                    (smap parse-bind bind)
                    (parse-body body)))
                 ((and exp ('letrec bind . body))
                  (let ((bindings (smap parse-bind bind)))
                    (no-repeats (map bind-name bindings) exp)
                    (make-letr bindings (parse-body body))))
                 (('begin e1 . rest)
                  (make-begin (smap parse-exp (cons e1 rest))))
                 (('define . _)
                  (syntax-err
                    expression
                    "invalid context for internal define"))
                 (('define-structure . _)
                  (syntax-err
                    expression
                    "invalid context for internal define-structure"))
                 (('define-const-structure . _)
                  (syntax-err
                    expression
                    "invalid context for internal define-const-structure"))
                 ((and m (f . args))
                  (cond ((and (eq? f n-primitive)
                              (match args
                                     (((? symbol? p)) (make-prim p))
                                     (_ #f))))
                        ((and (symbol? f)
                              (not (keyword? f))
                              (expand-once-if-macro m))
                         =>
                         parse-exp)
                        (else
                         (make-app (parse-exp f) (smap parse-exp args)))))
                 (x (syntax-err
                      expression
                      "invalid expression at ~a"
                      x))))
             (parse-name
               (match-lambda
                 ((? symbol? s)
                  (when (keyword? s)
                        (syntax-err
                          expression
                          "invalid use of keyword ~a"
                          s))
                  s)
                 (n (syntax-err
                      expression
                      "invalid variable at ~a"
                      n))))
             (parse-bind
               (match-lambda
                 ((x e) (make-bind (parse-name x) (parse-exp e)))
                 (b (syntax-err expression "invalid binding at ~a" b))))
             (parse-body
               (lambda (body)
                 (recur loop
                        ((b body) (defs '()))
                        (match b
                               (((and d ('define . _)) . rest)
                                (loop rest (append defs (parse-def d))))
                               (((and d ('define-structure . _)) . rest)
                                (loop rest (append defs (parse-def d))))
                               (((and d ('define-const-structure . _)) . rest)
                                (loop rest (append defs (parse-def d))))
                               ((('begin) . rest) (loop rest defs))
                               (((and beg ('begin ('define . _) . _)) . rest)
                                (loop rest (append defs (parse-def beg))))
                               (((and beg ('begin ('define-structure . _) . _))
                                 .
                                 rest)
                                (loop rest (append defs (parse-def beg))))
                               (((and beg
                                      ('begin
                                       ('define-const-structure . _)
                                       .
                                       _))
                                 .
                                 rest)
                                (loop rest (append defs (parse-def beg))))
                               ((_ . _) (make-body defs (smap parse-exp b)))
                               (_ (syntax-err
                                    expression
                                    "invalid body at ~a"
                                    b))))))
             (no-repeats
               (lambda (l exp)
                 (match l
                        (() #f)
                        ((_) #f)
                        ((x . l)
                         (if (memq x l)
                           (syntax-err exp "name ~a repeated" x)
                           (no-repeats l exp)))))))
      (parse-exp expression))))
(define parse-pat
  (lambda (pat expression)
    (letrec ((parse-pat
               (match-lambda
                 (#f (make-ppred 'false-object?))
                 (#t (make-ppred 'true-object?))
                 (() (make-ppred 'null?))
                 ((? number? c) (make-pconst c 'number?))
                 ((? char? c) (make-pconst c 'char?))
                 ((? string? c) (make-pconst c 'string?))
                 (('quote x) (parse-quote x))
                 ('_ (make-pany))
                 ('else (make-pelse))
                 ((? symbol? n) (make-pvar (parse-pname n)))
                 (('not . pats)
                  (syntax-err
                    expression
                    "not patterns are not supported"))
                 (('or . pats)
                  (syntax-err
                    expression
                    "or patterns are not supported"))
                 (('get! . pats)
                  (syntax-err
                    expression
                    "get! patterns are not supported"))
                 (('set! . pats)
                  (syntax-err
                    expression
                    "set! patterns are not supported"))
                 (('and . pats)
                  (let* ((pats (smap parse-pat pats))
                         (p (make-flat-pand pats))
                         (non-var?
                           (match-lambda
                             ((? pvar?) #f)
                             ((? pany?) #f)
                             (_ #t))))
                    (match p
                           (($ pand pats)
                            (when (< 1 (length (filter non-var? pats)))
                                  (syntax-err
                                    expression
                                    "~a has conflicting subpatterns"
                                    (ppat p))))
                           (_ #f))
                    p))
                 (('? (? symbol? pred) p)
                  (parse-pat `(and (? ,pred) ,p)))
                 (('? (? symbol? pred))
                  (if (keyword? pred)
                    (syntax-err
                      expression
                      "invalid use of keyword ~a"
                      pred)
                    (make-ppred pred)))
                 (('$ (? symbol? c) . args)
                  (if (memq c '(? _ $))
                    (syntax-err
                      expression
                      "invalid use of pattern keyword ~a"
                      c)
                    (make-pobj
                      (symbol-append c '?)
                      (smap parse-pat args))))
                 ((? box? cb)
                  (make-pobj 'box? (list (parse-pat (unbox cb)))))
                 ((x . y)
                  (make-pobj
                    'pair?
                    (list (parse-pat x) (parse-pat y))))
                 ((? vector? v)
                  (make-pobj
                    'vector?
                    (map parse-pat (vector->list v))))
                 (m (syntax-err expression "invalid pattern at ~a" m))))
             (parse-quote
               (match-lambda
                 (#f (make-pobj 'false-object? '()))
                 (#t (make-pobj 'true-object? '()))
                 (() (make-pobj 'null? '()))
                 ((? number? c) (make-pconst c 'number?))
                 ((? char? c) (make-pconst c 'char?))
                 ((? string? c) (make-pconst c 'string?))
                 ((? symbol? s) (make-pconst s 'symbol?))
                 ((? box? cb)
                  (make-pobj 'box? (list (parse-quote (unbox cb)))))
                 ((x . y)
                  (make-pobj
                    'pair?
                    (list (parse-quote x) (parse-quote y))))
                 ((? vector? v)
                  (make-pobj
                    'vector?
                    (map parse-quote (vector->list v))))
                 (m (syntax-err expression "invalid pattern at ~a" m))))
             (parse-pname
               (match-lambda
                 ((? symbol? s)
                  (cond ((keyword? s)
                         (syntax-err
                           expression
                           "invalid use of keyword ~a"
                           s))
                        ((memq s '(? _ else $ and or not set! get! ...))
                         (syntax-err
                           expression
                           "invalid use of pattern keyword ~a"
                           s))
                        (else s)))
                 (n (syntax-err
                      expression
                      "invalid pattern variable at ~a"
                      n)))))
      (parse-pat pat))))
(define smap
  (lambda (f l)
    (match l
           (() '())
           ((x . r) (let ((v (f x))) (cons v (smap f r))))
           (_ (syntax-err l "invalid list")))))
(define primitive
  (lambda (p)
    (list (string->symbol "#primitive") p)))
(define keyword?
  (lambda (s)
    (or (memq s
              '(=> and
                   begin
                   case
                   cond
                   do
                   define
                   delay
                   if
                   lambda
                   let
                   let*
                   letrec
                   or
                   quasiquote
                   quote
                   set!
                   unquote
                   unquote-splicing
                   define-structure
                   define-const-structure
                   record
                   field
                   :
                   datatype))
        (and keep-match (eq? s 'match)))))
(define make-flat-pand
  (lambda (pats)
    (let* ((l (foldr (lambda (p plist)
                       (match p
                              (($ pand pats) (append pats plist))
                              (_ (cons p plist))))
                     '()
                     pats))
           (concrete?
             (match-lambda
               ((? pconst?) #t)
               ((? pobj?) #t)
               ((? ppred?) #t)
               (_ #f)))
           (sorted
             (append
               (filter concrete? l)
               (filter (lambda (x) (not (concrete? x))) l))))
      (match sorted ((p) p) (_ (make-pand sorted))))))
(define never-counter 0)
(define reinit-macros!
  (lambda () (set! never-counter 0)))
(define cond-tf
  (lambda (cond-expr)
    (recur loop
           ((e (cdr cond-expr)))
           (match e
                  (()
                   (begin
                     (set! never-counter (+ 1 never-counter))
                     `(,(primitive 'should-never-reach)
                       '(cond ,never-counter))))
                  ((('else b1 . body)) `(begin ,b1 ,@body))
                  ((('else . _) . _)
                   (syntax-err cond-expr "invalid cond expression"))
                  (((test '=> proc) . rest)
                   (let ((g (gensym)))
                     `(let ((,g ,test))
                        (if ,g (,proc ,g) ,(loop rest)))))
                  (((#t b1 . body)) `(begin ,b1 ,@body))
                  (((test) . rest) `(or ,test ,(loop rest)))
                  (((test . body) . rest)
                   `(if ,test (begin ,@body) ,(loop rest)))
                  (_ (syntax-err cond-expr "invalid cond expression"))))))
(define scheme-cond-tf
  (lambda (cond-expr)
    (recur loop
           ((e (cdr cond-expr)))
           (match e
                  (() `(,(primitive 'void)))
                  ((('else b1 . body)) `(begin ,b1 ,@body))
                  ((('else . _) . _)
                   (syntax-err cond-expr "invalid cond expression"))
                  (((test '=> proc) . rest)
                   (let ((g (gensym)))
                     `(let ((,g ,test))
                        (if ,g (,proc ,g) ,(loop rest)))))
                  (((#t b1 . body)) `(begin ,b1 ,@body))
                  (((test) . rest) `(or ,test ,(loop rest)))
                  (((test . body) . rest)
                   `(if ,test (begin ,@body) ,(loop rest)))
                  (_ (syntax-err cond-expr "invalid cond expression"))))))
(define case-tf
  (lambda (case-expr)
    (recur loop
           ((e (cdr case-expr)))
           (match e
                  ((exp) `(begin ,exp (,(primitive 'void))))
                  ((exp ('else b1 . body)) `(begin ,b1 ,@body))
                  ((exp ('else . _) . _)
                   (syntax-err case-expr "invalid case expression"))
                  (((? symbol? exp)
                    ((? list? test) b1 . body)
                    .
                    rest)
                   `(if (,(primitive 'memv) ,exp ',test)
                      (begin ,b1 ,@body)
                      ,(loop (cons exp rest))))
                  (((? symbol? exp) (test b1 . body) . rest)
                   `(if (,(primitive 'memv) ,exp '(,test))
                      (begin ,b1 ,@body)
                      ,(loop (cons exp rest))))
                  ((exp . rest)
                   (if (not (symbol? exp))
                     (let ((g (gensym)))
                       `(let ((,g ,exp)) ,(loop (cons g rest))))
                     (syntax-err case-expr "invalid case expression")))
                  (_ (syntax-err case-expr "invalid case expression"))))))
(define conslimit 8)
(define quote-tf
  (lambda (exp)
    (letrec ((qloop (match-lambda
                      ((? box? q)
                       `(,(primitive qbox) ,(qloop (unbox q))))
                      ((? symbol? q) `',q)
                      ((? null? q) q)
                      ((? list? q)
                       (if (< (length q) conslimit)
                         `(,(primitive qcons)
                           ,(qloop (car q))
                           ,(qloop (cdr q)))
                         `(,(primitive qlist) ,@(map qloop q))))
                      ((x . y)
                       `(,(primitive qcons) ,(qloop x) ,(qloop y)))
                      ((? vector? q)
                       `(,(primitive qvector)
                         ,@(map qloop (vector->list q))))
                      ((? boolean? q) q)
                      ((? number? q) q)
                      ((? char? q) q)
                      ((? string? q) q)
                      (q (syntax-err
                           exp
                           "invalid quote expression at ~a"
                           q)))))
      (match exp
             (('quote q) (qloop q))
             ((? vector? q) (qloop q))
             ((? box? q) (qloop q))))))
(define quasiquote-tf
  (lambda (exp)
    (letrec ((make-cons
               (lambda (x y)
                 (cond ((null? y) `(,(primitive 'list) ,x))
                       ((and (pair? y)
                             (equal? (car y) (primitive 'list)))
                        (cons (car y) (cons x (cdr y))))
                       (else `(,(primitive 'cons) ,x ,y)))))
             (qloop (lambda (e n)
                      (match e
                             (('quasiquote e)
                              (make-cons 'quasiquote (qloop `(,e) (+ 1 n))))
                             (('unquote e)
                              (if (zero? n)
                                e
                                (make-cons 'unquote (qloop `(,e) (- n 1)))))
                             (('unquote-splicing e)
                              (if (zero? n)
                                e
                                (make-cons
                                  'unquote-splicing
                                  (qloop `(,e) (- n 1)))))
                             ((('unquote-splicing e) . y)
                              (=> fail)
                              (if (zero? n)
                                (if (null? y)
                                  e
                                  `(,(primitive 'append) ,e ,(qloop y n)))
                                (fail)))
                             ((? box? q)
                              `(,(primitive 'box) ,(qloop (unbox q) n)))
                             ((? symbol? q)
                              (if (memq q
                                        '(quasiquote unquote unquote-splicing))
                                (syntax-err
                                  exp
                                  "invalid use of ~a inside quasiquote"
                                  q)
                                `',q))
                             ((? null? q) q)
                             ((x . y) (make-cons (qloop x n) (qloop y n)))
                             ((? vector? q)
                              `(,(primitive 'vector)
                                ,@(map (lambda (z) (qloop z n))
                                       (vector->list q))))
                             ((? boolean? q) q)
                             ((? number? q) q)
                             ((? char? q) q)
                             ((? string? q) q)
                             (q (syntax-err
                                  exp
                                  "invalid quasiquote expression at ~a"
                                  q))))))
      (match exp (('quasiquote q) (qloop q 0))))))
(define do-tf
  (lambda (do-expr)
    (recur loop
           ((e (cdr do-expr)))
           (match e
                  (((? list? vis) (e0 ? list? e1) ? list? c)
                   (if (andmap (match-lambda ((_ _ . _) #t) (_ #f)) vis)
                     (let* ((var (map car vis))
                            (init (map cadr vis))
                            (step (map cddr vis))
                            (step (map (lambda (v s)
                                         (match s
                                                (() v)
                                                ((e) e)
                                                (_ (syntax-err
                                                     do-expr
                                                     "invalid do expression"))))
                                       var
                                       step)))
                       (let ((doloop (gensym)))
                         (match e1
                                (()
                                 `(let ,doloop
                                    ,(map list var init)
                                    (if (not ,e0)
                                      (begin ,@c (,doloop ,@step) (void))
                                      (void))))
                                ((body0 ? list? body)
                                 `(let ,doloop
                                    ,(map list var init)
                                    (if ,e0
                                      (begin ,body0 ,@body)
                                      (begin ,@c (,doloop ,@step)))))
                                (_ (syntax-err
                                     do-expr
                                     "invalid do expression")))))
                     (syntax-err do-expr "invalid do expression")))
                  (_ (syntax-err do-expr "invalid do expression"))))))
(define empty-env '())
(define lookup
  (lambda (env x)
    (match (assq x env)
           (#f (disaster 'lookup "no binding for ~a" x))
           ((_ . b) b))))
(define lookup?
  (lambda (env x)
    (match (assq x env) (#f #f) ((_ . b) b))))
(define bound?
  (lambda (env x)
    (match (assq x env) (#f #f) (_ #t))))
(define extend-env
  (lambda (env x v) (cons (cons x v) env)))
(define extend-env*
  (lambda (env xs vs)
    (append (map2 cons xs vs) env)))
(define join-env
  (lambda (env newenv) (append newenv env)))
(define populated #t)
(define pseudo #f)
(define global-error #f)
(define share #f)
(define matchst #f)
(define fullsharing #t)
(define dump-depths #f)
(define flags #t)
(define-structure
  (c depth kind fsym pres args next))
(define-structure
  (v depth kind name vis split inst))
(define-structure (ts type n-gen))
(define-structure (k name order args))
(define top (box 'top))
(define bot (box 'bot))
(define generic? (lambda (d) (< d 0)))
(define new-type
  (lambda (s d)
    (let ((t (box s)))
      (vector-set!
        types
        d
        (cons t (vector-ref types d)))
      t)))
(define generate-counter
  (lambda ()
    (let ((n 0)) (lambda () (set! n (+ 1 n)) n))))
(define var-counter (generate-counter))
(define make-raw-tvar
  (lambda (d k) (make-v d k var-counter #t #f #f)))
(define make-tvar
  (lambda (d k) (new-type (make-raw-tvar d k) d)))
(define ord? (lambda (k) (eq? 'ord k)))
(define abs? (lambda (k) (eq? 'abs k)))
(define pre? (lambda (k) (eq? 'pre k)))
(define ord-depth 2)
(define depth ord-depth)
(define types (make-vector 16 '()))
(define reset-types!
  (lambda ()
    (set! depth ord-depth)
    (set! types (make-vector 16 '()))))
(define push-level
  (lambda ()
    (set! depth (+ depth 1))
    (when (< (vector-length types) (+ 1 depth))
          (set! types
            (let ((l (vector->list types)))
              (list->vector
                (append l (map (lambda (_) '()) l))))))))
(define pop-level
  (lambda ()
    (vector-set! types depth '())
    (set! depth (- depth 1))))
(define v-ord (lambda () (make-tvar depth 'ord)))
(define v-abs (lambda () (make-tvar depth 'abs)))
(define v-pre (lambda () (make-tvar depth 'pre)))
(define tvar v-ord)
(define out1tvar
  (lambda () (make-tvar (- depth 1) 'ord)))
(define monotvar
  (lambda () (make-tvar ord-depth 'ord)))
(define pvar
  (match-lambda
    (($ box (and x ($ v d k _ vis _ _)))
     (unless
       (number? (v-name x))
       (set-v-name! x ((v-name x))))
     (string->symbol
       (sprintf
         "~a~a~a"
         (match k
                ('ord
                 (if (generic? d)
                   (if vis "X" "x")
                   (if vis "Z" "z")))
                ('abs (if vis "A" "a"))
                ('pre (if vis "P" "p")))
         (v-name x)
         (if dump-depths (sprintf ".~a" d) ""))))))
(define make-tvar-like
  (match-lambda
    (($ box ($ v d k _ _ _ _)) (make-tvar d k))))
(define ind*
  (lambda (t)
    (match (unbox t)
           ((? box? u)
            (let ((v (ind* u))) (set-box! t v) v))
           (_ t))))
(define type-check?
  (match-lambda
    ((abs def inexhaust once _)
     (cond (((if once check-abs1? check-abs?) abs)
            (if (and def (definite? def)) 'def #t))
           (inexhaust 'inexhaust)
           (else #f)))))
(define type-check1?
  (match-lambda
    ((abs def inexhaust _ _)
     (cond ((check-abs1? abs)
            (if (and def (definite? def)) 'def #t))
           (inexhaust 'inexhaust)
           (else #f)))))
(define check-abs?
  (lambda (vlist)
    (letrec ((seen '())
             (labs? (lambda (t)
                      (match t
                             (($ box ($ v _ _ _ _ _ inst))
                              (and inst
                                   (not (memq t seen))
                                   (begin
                                     (set! seen (cons t seen))
                                     (ormap (match-lambda ((t . _) (labs? t)))
                                            inst))))
                             (($ box ($ c _ _ _ p _ n))
                              (or (labs? p) (labs? n)))
                             (($ box (? symbol?)) #t)
                             (($ box i) (labs? i))))))
      (ormap labs? vlist))))
(define check-abs1?
  (lambda (vlist)
    (letrec ((labs1?
               (lambda (t)
                 (match t
                        (($ box (? v?)) #f)
                        (($ box ($ c _ _ _ p _ n))
                         (or (labs1? p) (labs1? n)))
                        (($ box (? symbol?)) #t)
                        (($ box i) (labs1? i))))))
      (ormap labs1? vlist))))
(define check-sources
  (lambda (info)
    (letrec ((seen '())
             (lsrcs (lambda (t source)
                      (match t
                             (($ box ($ v _ k _ _ _ inst))
                              (union (if (and inst (not (memq t seen)))
                                       (begin
                                         (set! seen (cons t seen))
                                         (foldr union
                                                empty-set
                                                (map (match-lambda
                                                       ((t . s) (lsrcs t s)))
                                                     inst)))
                                       empty-set)))
                             (($ box ($ c _ _ _ p _ n))
                              (union (lsrcs p source) (lsrcs n source)))
                             (($ box (? symbol?))
                              (if source (set source) empty-set))
                             (($ box i) (lsrcs i source))))))
      (match-let
        (((abs _ _ _ _) info))
        (if (eq? #t abs)
          empty-set
          (foldr union
                 empty-set
                 (map (lambda (t) (lsrcs t #f)) abs)))))))
(define check-local-sources
  (match-lambda ((_ _ _ _ component) component)))
(define mk-definite-prim
  (match-lambda
    (($ box ($ c _ _ x p a n))
     (if (eq? (k-name x) '?->)
       (let ((seen '()))
         (recur lprim
                ((t (car a)))
                (match t
                       (($ box ($ c _ _ x p a n))
                        (if (memq t seen)
                          '()
                          (begin
                            (set! seen (cons t seen))
                            (match (k-name x)
                                   ('noarg (cons p (lprim n)))
                                   ('arg
                                    (let ((args (recur argloop
                                                       ((a (car a)))
                                                       (match a
                                                              (($ box
                                                                  ($ c
                                                                     _
                                                                     _
                                                                     _
                                                                     p
                                                                     _
                                                                     n))
                                                               (cons p
                                                                     (argloop
                                                                       n)))
                                                              (($ box
                                                                  ($ v
                                                                     _
                                                                     k
                                                                     _
                                                                     _
                                                                     _
                                                                     _))
                                                               (if (ord? k)
                                                                 (list a)
                                                                 '()))
                                                              (($ box
                                                                  (? symbol?))
                                                               '())
                                                              (($ box i)
                                                               (argloop i))))))
                                      (cons (list p args (lprim (cadr a)))
                                            (lprim n))))))))
                       (($ box ($ v _ k _ _ _ _))
                        (if (ord? k) (list t) '()))
                       (($ box (? symbol?)) '())
                       (($ box i) (lprim i)))))
       (mk-definite-prim n)))
    (($ box (? v?)) '())
    (($ box (? symbol?)) '())
    (($ box i) (mk-definite-prim i))))
(define mk-definite-app
  (match-lambda
    (($ box ($ c _ _ _ p _ _)) (list p))))
(define mk-definite-lam
  (match-lambda
    (($ box ($ c _ _ x p a n))
     (if (eq? (k-name x) '?->)
       (let ((seen '()))
         (recur llam
                ((t (car a)))
                (match t
                       (($ box ($ c _ _ x p a n))
                        (if (memq t seen)
                          '()
                          (begin
                            (set! seen (cons t seen))
                            (match (k-name x)
                                   ('noarg (cons p (llam n)))
                                   ('arg
                                    (let ((args (list top)))
                                      (cons (list p args (llam (cadr a)))
                                            (llam n))))))))
                       (($ box ($ v _ k _ _ _ _))
                        (if (ord? k) (list t) '()))
                       (($ box (? symbol?)) '())
                       (($ box i) (llam i)))))
       (mk-definite-lam n)))
    (($ box (? v?)) '())
    (($ box (? symbol?)) '())
    (($ box i) (mk-definite-lam i))))
(define definite?
  (lambda (def-info)
    (letrec ((non-empty?
               (lambda (t)
                 (let ((seen '()))
                   (recur ldef
                          ((t t))
                          (match t
                                 (($ box ($ c _ _ _ p _ n))
                                  (or (ldef p) (ldef n)))
                                 (($ box ($ v d k _ _ _ inst))
                                  (if (or global-error (abs? k))
                                    (and inst
                                         (generic? d)
                                         (not (memq t seen))
                                         (begin
                                           (set! seen (cons t seen))
                                           (ormap (match-lambda
                                                    ((t . _) (ldef t)))
                                                  inst)))
                                    (generic? d)))
                                 (($ box 'top) #t)
                                 (($ box 'bot) #f)
                                 (($ box i) (ldef i)))))))
             (ok (lambda (l)
                   (ormap (match-lambda
                            ((? box? t) (non-empty? t))
                            ((p arg rest)
                             (and (non-empty? p)
                                  (ormap non-empty? arg)
                                  (ok rest))))
                          l))))
      (not (ok def-info)))))
(define close
  (lambda (t-list) (close-type t-list #f)))
(define closeall
  (lambda (t) (car (close-type (list t) #t))))
(define for
  (lambda (from to f)
    (cond ((= from to) (f from))
          ((< from to)
           (begin (f from) (for (+ from 1) to f)))
          (else #f))))
(define close-type
  (lambda (t-list all?)
    (let* ((sorted (make-vector (+ depth 1) '()))
           (sort (lambda (t)
                   (match t
                          (($ box ($ c d _ _ _ _ _))
                           (vector-set!
                             sorted
                             d
                             (cons t (vector-ref sorted d))))
                          (($ box ($ v d _ _ _ _ _))
                           (vector-set!
                             sorted
                             d
                             (cons t (vector-ref sorted d))))
                          (_ #f))))
           (prop-d
             (lambda (down)
               (letrec ((pr (match-lambda
                              (($ box (and x ($ v d _ _ _ _ _)))
                               (when (< down d) (set-v-depth! x down)))
                              (($ box (and x ($ c d _ _ p a n)))
                               (when (< down d)
                                     (set-c-depth! x down)
                                     (pr p)
                                     (for-each pr a)
                                     (pr n)))
                              (($ box (? symbol?)) #f)
                              (z (pr (ind* z))))))
                 (match-lambda
                   (($ box (and x ($ c d _ _ p a n)))
                    (when (<= down d) (pr p) (for-each pr a) (pr n)))
                   (_ #f)))))
           (prop-k
             (lambda (t)
               (let ((pk (lambda (kind)
                           (rec pr
                                (match-lambda
                                  (($ box (and x ($ v _ k _ _ _ _)))
                                   (when (kind< kind k) (set-v-kind! x kind)))
                                  (($ box (and x ($ c _ k _ p a n)))
                                   (when (kind< kind k)
                                         (set-c-kind! x kind)
                                         (pr p)
                                         (unless populated (for-each pr a))
                                         (pr n)))
                                  (($ box (? symbol?)) #f)
                                  (z (pr (ind* z))))))))
                 (match t
                        (($ box (and x ($ c _ k _ p a n)))
                         (when (not (ord? k))
                               (let ((prop (pk k)))
                                 (prop p)
                                 (unless populated (for-each prop a))
                                 (prop n))))
                        (_ #f)))))
           (might-be-generalized?
             (match-lambda
               (($ box ($ v d k _ _ _ _))
                (and (<= depth d) (or populated (ord? k) all?)))
               (($ box ($ c d k _ _ _ _))
                (and (<= depth d) (or populated (ord? k) all?)))
               (($ box (? symbol?)) #f)))
           (leaves '())
           (depth-of
             (match-lambda
               (($ box ($ v d _ _ _ _ _)) d)
               (($ box ($ c d _ _ _ _ _)) d)))
           (vector-grow
             (lambda (v)
               (let* ((n (vector-length v))
                      (v2 (make-vector (* n 2) '())))
                 (recur loop
                        ((i 0))
                        (when (< i n)
                              (vector-set! v2 i (vector-ref v i))
                              (loop (+ 1 i))))
                 v2)))
           (parents (make-vector 64 '()))
           (parent-index 0)
           (parents-of
             (lambda (t)
               (let ((d (depth-of t)))
                 (if (< depth d)
                   (vector-ref parents (- (- d depth) 1))
                   '()))))
           (xtnd-parents!
             (lambda (t parent)
               (match t
                      (($ box (and x ($ v d _ _ _ _ _)))
                       (when (= d depth)
                             (set! parent-index (+ 1 parent-index))
                             (set-v-depth! x (+ depth parent-index))
                             (when (< (vector-length parents) parent-index)
                                   (set! parents (vector-grow parents)))
                             (set! d (+ depth parent-index)))
                       (vector-set!
                         parents
                         (- (- d depth) 1)
                         (cons parent
                               (vector-ref parents (- (- d depth) 1)))))
                      (($ box (and x ($ c d _ _ _ _ _)))
                       (when (= d depth)
                             (set! parent-index (+ 1 parent-index))
                             (set-c-depth! x (+ depth parent-index))
                             (when (< (vector-length parents) parent-index)
                                   (set! parents (vector-grow parents)))
                             (set! d (+ depth parent-index)))
                       (vector-set!
                         parents
                         (- (- d depth) 1)
                         (cons parent
                               (vector-ref parents (- (- d depth) 1))))))))
           (needs-cleanup '())
           (revtype
             (rec revtype
                  (lambda (parent t)
                    (let ((t (ind* t)))
                      (cond ((not (might-be-generalized? t)) #f)
                            ((null? (parents-of t))
                             (xtnd-parents! t parent)
                             (set! needs-cleanup (cons t needs-cleanup))
                             (match t
                                    (($ box (? v?))
                                     (set! leaves (cons t leaves)))
                                    (($ box ($ c _ _ _ p a n))
                                     (let ((rev (lambda (q) (revtype t q))))
                                       (rev p)
                                       (for-each rev a)
                                       (rev n)))))
                            ((not (memq parent (parents-of t)))
                             (xtnd-parents! t parent))
                            (else #f))))))
           (generic-index 0)
           (gen (rec gen
                     (lambda (t)
                       (let ((t (ind* t)))
                         (when (might-be-generalized? t)
                               (set! generic-index (- generic-index 1))
                               (let ((parents (parents-of t)))
                                 (match t
                                        (($ box (and x ($ v _ k _ _ _ _)))
                                         (set-v-depth! x generic-index)
                                         (when (and populated
                                                    (or global-error
                                                        (abs? k)
                                                        (pre? k))
                                                    (not all?))
                                               (set-v-inst! x '())))
                                        (($ box (? c? x))
                                         (set-c-depth! x generic-index)))
                                 (for-each gen parents)))))))
           (cleanup
             (match-lambda
               (($ box (and x ($ v d _ _ _ _ _)))
                (unless (< d 0) (set-v-depth! x (- depth 1))))
               (($ box (and x ($ c d _ _ _ _ _)))
                (unless (< d 0) (set-c-depth! x (- depth 1))))))
           (gen2 (rec gen
                      (lambda (t)
                        (let ((t (ind* t)))
                          (when (might-be-generalized? t)
                                (set! generic-index (- generic-index 1))
                                (match t
                                       (($ box (and x ($ v _ k _ _ _ _)))
                                        (set-v-depth! x generic-index)
                                        (when (and populated
                                                   (or global-error
                                                       (abs? k)
                                                       (pre? k))
                                                   (not all?))
                                              (set-v-inst! x '())))
                                       (($ box (and x ($ c _ _ _ p a n)))
                                        (set-c-depth! x generic-index)
                                        (gen p)
                                        (for-each gen a)
                                        (gen n))))))))
           (upd (lambda (t)
                  (let ((d (depth-of t)))
                    (when (< 0 d)
                          (vector-set!
                            types
                            d
                            (cons t (vector-ref types d))))))))
      (for-each sort (vector-ref types depth))
      (for 0
           (- depth 1)
           (lambda (i)
             (for-each (prop-d i) (vector-ref sorted i))))
      (for-each prop-k (vector-ref types depth))
      (vector-set! types depth '())
      (if fullsharing
        (begin
          (for-each (lambda (t) (revtype t t)) t-list)
          (for-each gen leaves)
          (for-each cleanup needs-cleanup))
        (for-each gen2 t-list))
      (for 0
           depth
           (lambda (i) (for-each upd (vector-ref sorted i))))
      (if (null? t-list)
        '()
        (match-let*
          ((n-gen (- generic-index))
           ((t-list n-gen)
            (if (and pseudo flags (not all?))
              (pseudo t-list n-gen)
              (list t-list n-gen))))
          (visible t-list n-gen)
          (map (lambda (t) (make-ts t n-gen)) t-list))))))
(define visible-time 0)
(define visible
  (lambda (t-list n-gen)
    (let* ((before (cpu-time))
           (valences (make-vector n-gen '()))
           (namer (generate-counter))
           (lvis (rec lvis
                      (lambda (t pos rcd)
                        (match t
                               (($ box ($ c d _ x p a n))
                                (when (and (generic? d)
                                           (not (element-of?
                                                  pos
                                                  (vector-ref
                                                    valences
                                                    (- (- d) 1)))))
                                      (let ((u (union (vector-ref
                                                        valences
                                                        (- (- d) 1))
                                                      (set pos))))
                                        (vector-set! valences (- (- d) 1) u))
                                      (lvis p pos rcd)
                                      (match (k-name x)
                                             ('?->
                                              (lvis (car a) (not pos) #f)
                                              (lvis (cadr a) pos #f))
                                             ('record (lvis (car a) pos #t))
                                             (_ (for-each
                                                  (lambda (x) (lvis x pos #f))
                                                  a)))
                                      (lvis n pos rcd)))
                               (($ box (and x ($ v d k _ _ _ _)))
                                (when (and (generic? d)
                                           (not (element-of?
                                                  pos
                                                  (vector-ref
                                                    valences
                                                    (- (- d) 1)))))
                                      (let ((u (union (vector-ref
                                                        valences
                                                        (- (- d) 1))
                                                      (set pos))))
                                        (vector-set! valences (- (- d) 1) u)
                                        (set-v-name! x namer)
                                        (cond ((abs? k) #f)
                                              ((= 2 (cardinality u))
                                               (set-v-split! x #t)
                                               (set-v-vis! x #t))
                                              ((eq? pos rcd) (set-v-vis! x #t))
                                              (else (set-v-vis! x #f))))))
                               (($ box (? symbol?)) #f)
                               (($ box i) (lvis i pos rcd)))))))
      (for-each (lambda (t) (lvis t #t #f)) t-list)
      (set! visible-time
        (+ visible-time (- (cpu-time) before))))))
(define visible?
  (match-lambda
    (($ box ($ v _ k _ vis _ _))
     (or (pre? k) (and vis (not (abs? k)))))
    (($ box 'top) #t)
    (($ box 'bot) #f)
    (($ box i) (visible? i))))
(define instantiate
  (lambda (ts syntax)
    (match ts
           (($ ts t n-gen)
            (let* ((absv '())
                   (seen (make-vector n-gen #f))
                   (t2 (recur linst
                              ((t t))
                              (match t
                                     (($ box (and y ($ v d k _ _ _ inst)))
                                      (cond ((not (generic? d)) t)
                                            ((vector-ref seen (- (- d) 1)))
                                            (else
                                             (let ((u (make-tvar depth k)))
                                               (vector-set! seen (- (- d) 1) u)
                                               (when inst
                                                     (set-v-inst!
                                                       y
                                                       (cons (cons u syntax)
                                                             inst)))
                                               (when (or (abs? k) (pre? k))
                                                     (set! absv (cons u absv)))
                                               u))))
                                     (($ box ($ c d _ x p a n))
                                      (cond ((not (generic? d)) t)
                                            ((vector-ref seen (- (- d) 1)))
                                            (else
                                             (let ((u (new-type
                                                        '**fix**
                                                        depth)))
                                               (vector-set! seen (- (- d) 1) u)
                                               (set-box!
                                                 u
                                                 (make-c
                                                   depth
                                                   'ord
                                                   x
                                                   (if flags (linst p) top)
                                                   (map linst a)
                                                   (linst n)))
                                               u))))
                                     (($ box (? symbol?)) t)
                                     (($ box i) (linst i))))))
              (list t2 absv))))))
(define pseudo-subtype
  (lambda (t-list n-gen)
    (let* ((valences (make-vector n-gen '()))
           (valence-of
             (lambda (d) (vector-ref valences (- (- d) 1))))
           (set-valence
             (lambda (d v)
               (vector-set! valences (- (- d) 1) v)))
           (find (rec find
                      (lambda (t pos mutable)
                        (match t
                               (($ box ($ v d _ _ _ _ _))
                                (when (generic? d)
                                      (cond (mutable
                                             (set-valence d (set #t #f)))
                                            ((not (element-of?
                                                    pos
                                                    (valence-of d)))
                                             (set-valence
                                               d
                                               (union (valence-of d)
                                                      (set pos))))
                                            (else #f))))
                               (($ box ($ c d _ x p a n))
                                (when (generic? d)
                                      (cond ((= 2 (cardinality (valence-of d)))
                                             #f)
                                            (mutable
                                             (set-valence d (set #t #f))
                                             (for-each2
                                               (lambda (t m)
                                                 (find t pos mutable))
                                               a
                                               (k-args x))
                                             (find n pos mutable))
                                            ((not (element-of?
                                                    pos
                                                    (valence-of d)))
                                             (set-valence
                                               d
                                               (union (valence-of d)
                                                      (set pos)))
                                             (if (eq? '?-> (k-name x))
                                               (begin
                                                 (find (car a)
                                                       (not pos)
                                                       mutable)
                                                 (find (cadr a) pos mutable))
                                               (for-each2
                                                 (lambda (t m)
                                                   (find t pos (or m mutable)))
                                                 a
                                                 (k-args x)))
                                             (find n pos mutable))
                                            (else #f))))
                               (($ box (? symbol?)) #f)
                               (($ box i) (find i pos mutable))))))
           (seen (make-vector n-gen #f))
           (new-generic-var
             (lambda ()
               (set! n-gen (+ 1 n-gen))
               (box (make-raw-tvar (- n-gen) 'ord))))
           (copy (rec copy
                      (lambda (t)
                        (match t
                               (($ box ($ v d k _ _ _ _))
                                (if (generic? d)
                                  (or (vector-ref seen (- (- d) 1))
                                      (let ((u (if (and (abs? k)
                                                        (equal?
                                                          (valence-of d)
                                                          '(#t)))
                                                 (new-generic-var)
                                                 t)))
                                        (vector-set! seen (- (- d) 1) u)
                                        u))
                                  t))
                               (($ box ($ c d k x p a n))
                                (if (generic? d)
                                  (or (vector-ref seen (- (- d) 1))
                                      (let* ((u (box '**fix**))
                                             (_ (vector-set!
                                                  seen
                                                  (- (- d) 1)
                                                  u))
                                             (new-p (if (and (eq? (ind* p) top)
                                                             (equal?
                                                               (valence-of d)
                                                               '(#f)))
                                                      (new-generic-var)
                                                      (copy p)))
                                             (new-a (map copy a))
                                             (new-n (copy n)))
                                        (set-box!
                                          u
                                          (make-c d 'ord x new-p new-a new-n))
                                        u))
                                  t))
                               (($ box (? symbol?)) t)
                               (($ box i) (copy i))))))
           (t-list
             (map (lambda (t) (find t #t #f) (copy t)) t-list)))
      (list t-list n-gen))))
(set! pseudo pseudo-subtype)
(define unify
  (letrec ((uni (lambda (u v)
                  (unless
                    (eq? u v)
                    (match (cons u v)
                           ((($ box (and us ($ c ud uk ux up ua un)))
                             $
                             box
                             (and vs ($ c vd vk vx vp va vn)))
                            (if (eq? ux vx)
                              (begin
                                (if (< ud vd)
                                  (begin
                                    (set-box! v u)
                                    (when (kind< vk uk) (set-c-kind! us vk)))
                                  (begin
                                    (set-box! u v)
                                    (when (kind< uk vk) (set-c-kind! vs uk))))
                                (uni un vn)
                                (for-each2 uni ua va)
                                (uni up vp))
                              (let* ((next (tvar))
                                     (k (if (kind< uk vk) uk vk)))
                                (if (< ud vd)
                                  (begin
                                    (when (< vd ud) (set-c-depth! us vd))
                                    (when (kind< vk uk) (set-c-kind! us vk))
                                    (set-box! v u))
                                  (begin
                                    (when (< ud vd) (set-c-depth! vs ud))
                                    (when (kind< uk vk) (set-c-kind! vs uk))
                                    (set-box! u v)))
                                (uni (new-type
                                       (make-c depth k ux up ua next)
                                       depth)
                                     vn)
                                (uni un
                                     (new-type
                                       (make-c depth k vx vp va next)
                                       depth)))))
                           ((($ box (and x ($ v ud uk _ _ _ _)))
                             $
                             box
                             ($ v vd vk _ _ _ _))
                            (set-v-depth! x (min ud vd))
                            (set-v-kind! x (if (kind< uk vk) uk vk))
                            (set-box! v u))
                           ((($ box ($ v ud uk _ _ _ _))
                             $
                             box
                             (and x ($ c vd vk _ _ _ _)))
                            (when (< ud vd) (set-c-depth! x ud))
                            (when (kind< uk vk) (set-c-kind! x uk))
                            (set-box! u v))
                           ((($ box (and x ($ c ud uk _ _ _ _)))
                             $
                             box
                             ($ v vd vk _ _ _ _))
                            (when (< vd ud) (set-c-depth! x vd))
                            (when (kind< vk uk) (set-c-kind! x vk))
                            (set-box! v u))
                           ((($ box ($ v _ _ _ _ _ _)) $ box (? symbol?))
                            (set-box! u v))
                           ((($ box (? symbol?)) $ box ($ v _ _ _ _ _ _))
                            (set-box! v u))
                           ((($ box 'bot) $ box ($ c _ _ _ p _ n))
                            (set-box! v u)
                            (uni u p)
                            (uni u n))
                           ((($ box ($ c _ _ _ p _ n)) $ box 'bot)
                            (set-box! u v)
                            (uni v p)
                            (uni v n))
                           (_ (uni (ind* u) (ind* v))))))))
    uni))
(define kind<
  (lambda (k1 k2) (and (ord? k2) (not (ord? k1)))))
(define r+-
  (lambda (flag+ flag- tail+- absent- pos env type)
    (letrec ((absent+ v-ord)
             (tvars '())
             (fvars '())
             (absv '())
             (make-flag
               (lambda (pos)
                 (cond ((not flags) top)
                       (pos (flag+))
                       (else (flag-)))))
             (typevar?
               (lambda (v)
                 (and (symbol? v)
                      (not (bound? env v))
                      (not (memq v
                                 '(_ bool
                                     mu
                                     list
                                     &list
                                     &optional
                                     &rest
                                     arglist
                                     +
                                     not
                                     rec
                                     *tidy))))))
             (parse-type
               (lambda (t pos)
                 (match t
                        (('mu a t)
                         (unless
                           (typevar? a)
                           (raise 'type "invalid type syntax at ~a" t))
                         (when (assq a tvars)
                               (raise 'type "~a is defined more than once" a))
                         (let* ((fix (new-type '**fix** depth))
                                (_ (set! tvars (cons (list a fix '()) tvars)))
                                (t (parse-type t pos)))
                           (when (eq? t fix)
                                 (raise 'type
                                        "recursive type is not contractive"))
                           (set-box! fix t)
                           (ind* t)))
                        (('rec (? list? bind) t2)
                         (for-each
                           (match-lambda
                             ((a _)
                              (unless
                                (typevar? a)
                                (raise 'type "invalid type syntax at ~a" t))
                              (when (assq a tvars)
                                    (raise 'type
                                           "~a is defined more than once"
                                           a))
                              (set! tvars
                                (cons (list a (new-type '**fix** depth) '())
                                      tvars)))
                             (_ (raise 'type "invalid type syntax at ~a" t)))
                           bind)
                         (for-each
                           (match-lambda
                             ((a t)
                              (match (assq a tvars)
                                     ((_ fix _)
                                      (let ((t (parse-type t '?)))
                                        (when (eq? t fix)
                                              (raise 'type
                                                     "type is not contractive"))
                                        (set-box! fix t))))))
                           bind)
                         (parse-type t2 pos))
                        ('bool (parse-type '(+ false true) pos))
                        ('s-exp
                         (let ((v (gensym)))
                           (parse-type
                             `(mu ,v
                                  (+ num
                                     nil
                                     false
                                     true
                                     char
                                     sym
                                     str
                                     (vec ,v)
                                     (box ,v)
                                     (cons ,v ,v)))
                             pos)))
                        (('list t)
                         (let ((u (gensym)))
                           (parse-type `(mu ,u (+ nil (cons ,t ,u))) pos)))
                        (('arglist t)
                         (let ((u (gensym)))
                           (parse-type `(mu ,u (+ noarg (arg ,t ,u))) pos)))
                        (('+ ? list? union) (parse-union union pos))
                        (t (parse-union (list t) pos)))))
             (parse-union
               (lambda (t pos)
                 (letrec ((sort-cs
                            (lambda (cs)
                              (sort-list
                                cs
                                (lambda (x y) (k< (c-fsym x) (c-fsym y))))))
                          (link (lambda (c t)
                                  (set-c-next! c t)
                                  (new-type c depth))))
                   (recur loop
                          ((t t) (cs '()))
                          (match t
                                 (()
                                  (foldr link
                                         (if pos
                                           (absent+)
                                           (let ((v (absent-)))
                                             (set! absv (cons v absv))
                                             v))
                                         (sort-cs cs)))
                                 (((? box? t)) (foldr link t (sort-cs cs)))
                                 (('_) (foldr link (tail+-) (sort-cs cs)))
                                 (((? symbol? a))
                                  (=> fail)
                                  (unless (typevar? a) (fail))
                                  (let* ((cs (sort-cs cs))
                                         (ks (map c-fsym cs)))
                                    (foldr link
                                           (match (assq a tvars)
                                                  ((_ f aks)
                                                   (unless
                                                     (equal? ks aks)
                                                     (raise 'type
                                                            "variable ~a is not tidy"
                                                            a))
                                                   f)
                                                  (#f
                                                   (let ((v (tail+-)))
                                                     (set! tvars
                                                       (cons (list a v ks)
                                                             tvars))
                                                     v)))
                                           cs)))
                                 ((k . rest)
                                  (loop rest (cons (parse-k k pos) cs))))))))
             (parse-k
               (lambda (k pos)
                 (cond ((and (list? k)
                             (let ((n (length k)))
                               (and (<= 2 n) (eq? '-> (list-ref k (- n 2))))))
                        (let* ((rk (reverse k))
                               (arg (reverse (cddr rk)))
                               (res (car rk)))
                          (letrec ((mkargs
                                     (match-lambda
                                       (() 'noarg)
                                       ((('&rest x)) x)
                                       ((('&list x))
                                        (let ((u (gensym)))
                                          `(mu ,u (+ noarg (arg ,x ,u)))))
                                       ((('&optional x))
                                        `(+ noarg (arg ,x noarg)))
                                       ((x . y) `(arg ,x ,(mkargs y)))
                                       (_ (raise 'type
                                                 "invalid type syntax")))))
                            (make-c
                              depth
                              'ord
                              (lookup env '?->)
                              (make-flag pos)
                              (let ((a (parse-type (mkargs arg) (flip pos)))
                                    (r (parse-type res pos)))
                                (list a r))
                              '**fix**))))
                       (else
                        (match k
                               ((arg '?-> res)
                                (make-c
                                  depth
                                  'ord
                                  (lookup env '?->)
                                  (make-flag pos)
                                  (let ((a (parse-type arg (flip pos)))
                                        (r (parse-type res pos)))
                                    (list a r))
                                  '**fix**))
                               (('record ? list? fields)
                                (make-c
                                  depth
                                  'ord
                                  (lookup env 'record)
                                  (make-flag pos)
                                  (list (recur loop
                                               ((fields fields))
                                               (match fields
                                                      (() (if pos bot (v-ord)))
                                                      ((((? symbol? f) ftype)
                                                        .
                                                        rest)
                                                       (new-type
                                                         (make-c
                                                           depth
                                                           'ord
                                                           (new-field! f)
                                                           (if pos
                                                             (v-ord)
                                                             (let ((v (v-pre)))
                                                               (set! absv
                                                                 (cons v absv))
                                                               v))
                                                           (list (parse-type
                                                                   ftype
                                                                   pos))
                                                           (loop rest))
                                                         depth)))))
                                  '**fix**))
                               (('not (? k? k))
                                (make-c
                                  depth
                                  'ord
                                  k
                                  (if pos
                                    (absent+)
                                    (let ((v (absent-)))
                                      (set! absv (cons v absv))
                                      v))
                                  (map (lambda (x) (tail+-)) (k-args k))
                                  '**fix**))
                               (('not c)
                                (unless
                                  (bound? env c)
                                  (raise 'type "invalid type syntax at ~a" k))
                                (let ((k (lookup env c)))
                                  (make-c
                                    depth
                                    'ord
                                    k
                                    (if pos
                                      (absent+)
                                      (let ((v (absent-)))
                                        (set! absv (cons v absv))
                                        v))
                                    (map (lambda (x) (tail+-)) (k-args k))
                                    '**fix**)))
                               (('*tidy c (? symbol? f))
                                (unless
                                  (bound? env c)
                                  (raise 'type "invalid type syntax at ~a" k))
                                (let ((k (lookup env c)))
                                  (make-c
                                    depth
                                    'ord
                                    k
                                    (match (assq f fvars)
                                           ((_ . f) f)
                                           (#f
                                            (let ((v (tail+-)))
                                              (set! fvars
                                                (cons (cons f v) fvars))
                                              v)))
                                    (map (lambda (x) (parse-type '(+) pos))
                                         (k-args k))
                                    '**fix**)))
                               (((? k? k) ? list? arg)
                                (unless
                                  (= (length arg) (length (k-args k)))
                                  (raise 'type
                                         "~a requires ~a arguments"
                                         (k-name k)
                                         (length (k-args k))))
                                (make-c
                                  depth
                                  'ord
                                  k
                                  (make-flag pos)
                                  (smap (lambda (x) (parse-type x pos)) arg)
                                  '**fix**))
                               ((c ? list? arg)
                                (unless
                                  (bound? env c)
                                  (raise 'type "invalid type syntax at ~a" k))
                                (let ((k (lookup env c)))
                                  (unless
                                    (= (length arg) (length (k-args k)))
                                    (raise 'type
                                           "~a requires ~a arguments"
                                           c
                                           (length (k-args k))))
                                  (make-c
                                    depth
                                    'ord
                                    k
                                    (make-flag pos)
                                    (smap (lambda (x) (parse-type x pos)) arg)
                                    '**fix**)))
                               (c (unless
                                    (bound? env c)
                                    (raise 'type
                                           "invalid type syntax at ~a"
                                           k))
                                  (let ((k (lookup env c)))
                                    (unless
                                      (= 0 (length (k-args k)))
                                      (raise 'type
                                             "~a requires ~a arguments"
                                             c
                                             (length (k-args k))))
                                    (make-c
                                      depth
                                      'ord
                                      k
                                      (make-flag pos)
                                      '()
                                      '**fix**))))))))
             (flip (match-lambda ('? '?) (#t #f) (#f #t))))
      (let ((t (parse-type type pos))) (list t absv)))))
(define v-top (lambda () top))
(define r+
  (lambda (env t)
    (car (r+- v-top v-ord v-ord v-abs #t env t))))
(define r-
  (lambda (env t)
    (car (r+- v-top v-ord v-ord v-abs #f env t))))
(define r++
  (lambda (env t)
    (car (r+- v-top v-ord v-ord v-ord #t env t))))
(define r+collect
  (lambda (env t)
    (r+- v-top v-ord v-ord v-abs #t env t)))
(define r-collect
  (lambda (env t)
    (r+- v-top v-ord v-ord v-abs #f env t)))
(define r (lambda (t) (r+ initial-type-env t)))
(define r-match
  (lambda (t)
    (close '())
    '(pretty-print `(fixing ,(ptype t)))
    (fix-pat-abs! t)
    (list t (collect-abs t))))
(define collect-abs
  (lambda (t)
    (let ((seen '()))
      (recur loop
             ((t t))
             (match t
                    (($ box ($ v _ k _ _ _ _))
                     (if (abs? k) (set t) empty-set))
                    (($ box ($ c _ _ _ p a n))
                     (if (memq t seen)
                       empty-set
                       (begin
                         (set! seen (cons t seen))
                         (foldr union
                                (union (loop p) (loop n))
                                (map loop a)))))
                    (($ box (? symbol?)) empty-set)
                    (($ box i) (loop i)))))))
(define fix-pat-abs!
  (lambda (t)
    (let ((seen '()))
      (recur loop
             ((t t))
             (match t
                    (($ box (and x ($ v d _ _ _ _ _)))
                     (when (= d depth) (set-v-kind! x 'abs)))
                    (($ box (and c ($ c _ _ _ p a n)))
                     (unless
                       (memq t seen)
                       (set! seen (cons t seen))
                       (loop p)
                       (when (and matchst flags (eq? (ind* p) top))
                             (set-c-pres! c (v-ord)))
                       (for-each loop a)
                       (loop n)))
                    (($ box (? symbol?)) t)
                    (($ box i) (loop i)))))))
(define pat-var-bind
  (lambda (t)
    (let ((seen '()))
      (recur loop
             ((t t))
             (match t
                    (($ box ($ v d _ _ _ _ _))
                     (if (< d depth)
                       t
                       (match (assq t seen)
                              ((_ . new) new)
                              (#f
                               (let* ((new (v-ord)))
                                 (set! seen (cons (cons t new) seen))
                                 new)))))
                    (($ box ($ c d k x p a n))
                     (match (assq t seen)
                            ((_ . new) new)
                            (#f
                             (let* ((fix (new-type '**fix** depth))
                                    (fixbox (box fix))
                                    (_ (set! seen (cons (cons t fixbox) seen)))
                                    (new-p (if flags (loop p) top))
                                    (new-a (map2 (lambda (mutable a)
                                                   (if mutable a (loop a)))
                                                 (k-args x)
                                                 a))
                                    (new-n (loop n)))
                               (if (and (eq? new-p p)
                                        (eq? new-n n)
                                        (andmap eq? new-a a))
                                 (begin (set-box! fixbox t) t)
                                 (begin
                                   (set-box!
                                     fix
                                     (make-c d k x new-p new-a new-n))
                                   fix))))))
                    (($ box (? symbol?)) t)
                    (($ box i) (loop i)))))))
(define fields '())
(define new-field!
  (lambda (x)
    (match (assq x fields)
           (#f
            (let ((k (make-k x (+ 1 (length fields)) '(#f))))
              (set! fields (cons (cons x k) fields))
              k))
           ((_ . k) k))))
(define k<
  (lambda (x y) (< (k-order x) (k-order y))))
(define k-counter 0)
(define bind-tycon
  (lambda (x args covers fail-thunk)
    (when (memq x
                '(_ bool
                    mu
                    list
                    &list
                    &optional
                    &rest
                    arglist
                    +
                    not
                    rec
                    *tidy))
          (fail-thunk "invalid type constructor ~a" x))
    (set! k-counter (+ 1 k-counter))
    (make-k
      (if covers
        (symbol-append x "." (- k-counter 100))
        x)
      k-counter
      args)))
(define initial-type-env '())
(define init-types!
  (lambda ()
    (set! k-counter 0)
    (set! var-counter (generate-counter))
    (set! initial-type-env
      (foldl (lambda (l env)
               (extend-env
                 env
                 (car l)
                 (bind-tycon
                   (car l)
                   (cdr l)
                   #f
                   (lambda x (apply disaster 'init x)))))
             empty-env
             initial-type-info))
    (set! k-counter 100)
    (reset-types!)))
(define reinit-types!
  (lambda ()
    (set! var-counter (generate-counter))
    (set! k-counter 100)
    (set! fields '())
    (set-cons-mutability! #t)
    (reset-types!)))
(define deftype
  (lambda (tag mutability)
    (set! initial-type-env
      (extend-env
        initial-type-env
        tag
        (make-k
          tag
          (+ 1 (length initial-type-env))
          mutability)))))
(define initial-type-info
  '((?-> #f #f)
    (arg #f #f)
    (noarg)
    (num)
    (nil)
    (false)
    (true)
    (char)
    (sym)
    (str)
    (void)
    (iport)
    (oport)
    (eof)
    (vec #t)
    (box #t)
    (cons #t #t)
    (cvec #f)
    (promise #t)
    (record #f)
    (module #f)))
(define cons-is-mutable #f)
(define set-cons-mutability!
  (lambda (m)
    (set! cons-is-mutable m)
    (set-k-args!
      (lookup initial-type-env 'cons)
      (list m m))))
(define tidy?
  (lambda (t)
    (let ((seen '()))
      (recur loop
             ((t t) (label '()))
             (match t
                    (($ box (? v?))
                     (match (assq t seen)
                            (#f (set! seen (cons (cons t label) seen)) #t)
                            ((_ . l2) (equal? label l2))))
                    (($ box ($ c _ _ x _ a n))
                     (match (assq t seen)
                            ((_ . l2) (equal? label l2))
                            (#f
                             (set! seen (cons (cons t label) seen))
                             (and (loop n (sort-list (cons x label) k<))
                                  (andmap (lambda (t) (loop t '())) a)))))
                    (($ box (? symbol?)) #t)
                    (($ box i) (loop i label)))))))
(define tidy
  (match-lambda
    (($ ts t _)
     (tidy-print t print-union assemble-union #f))
    (t (tidy-print t print-union assemble-union #f))))
(define ptype
  (match-lambda
    (($ ts t _)
     (tidy-print
       t
       print-raw-union
       assemble-raw-union
       #t))
    (t (tidy-print
         t
         print-raw-union
         assemble-raw-union
         #t))))
(define tidy-print
  (lambda (t print assemble top)
    (let* ((share (shared-unions t top))
           (bindings
             (map-with-n
               (lambda (t n)
                 (list t
                       (box #f)
                       (box #f)
                       (symbol-append "Y" (+ 1 n))))
               share))
           (body (print t (print-binding bindings)))
           (let-bindings
             (filter-map
               (match-lambda
                 ((_ _ ($ box #f) _) #f)
                 ((_ ($ box t) ($ box x) _) (list x t)))
               bindings)))
      (assemble let-bindings body))))
(define print-binding
  (lambda (bindings)
    (lambda (ty share-wrapper var-wrapper render)
      (match (assq ty bindings)
             (#f (render))
             ((_ box-tprint box-name nprint)
              (var-wrapper
                (or (unbox box-name)
                    (begin
                      (set-box! box-name nprint)
                      (set-box! box-tprint (share-wrapper (render)))
                      nprint))))))))
(define shared-unions
  (lambda (t all)
    (let ((seen '()))
      (recur loop
             ((t t) (top #t))
             (match t
                    (($ box (? v?)) #f)
                    (($ box ($ c _ _ _ _ a n))
                     (match (and top (assq t seen))
                            (#f
                             (set! seen (cons (cons t (box 1)) seen))
                             (for-each (lambda (x) (loop x #t)) a)
                             (loop n all))
                            ((_ . b) (set-box! b (+ 1 (unbox b))))))
                    (($ box (? symbol?)) #f)
                    (($ box i) (loop i top))))
      (reverse
        (filter-map
          (match-lambda ((_ $ box 1) #f) ((t . _) t))
          seen)))))
(define print-raw-union
  (lambda (t print-share)
    (recur loop
           ((t t))
           (match t
                  (($ box ($ v _ _ _ _ split _))
                   (if (and share split)
                     (string->symbol (sprintf "~a#" (pvar t)))
                     (pvar t)))
                  (($ box ($ c d k x p a n))
                   (print-share
                     t
                     (lambda (x) x)
                     (lambda (x) x)
                     (lambda ()
                       (let* ((name (if (abs? k)
                                      (symbol-append '~ (k-name x))
                                      (k-name x)))
                              (name (if dump-depths
                                      (symbol-append d '! name)
                                      name))
                              (pr-x `(,name ,@(maplr loop (cons p a)))))
                         (cons pr-x (loop n))))))
                  (($ box 'top) '+)
                  (($ box 'bot) '-)
                  (($ box i) (loop i))))))
(define assemble-raw-union
  (lambda (bindings body)
    (if (null? bindings) body `(rec ,bindings ,body))))
(define print-union
  (lambda (t print-share)
    (add-+ (recur loop
                  ((t t) (tailvis (visible? (tailvar t))))
                  (match t
                         (($ box (? v?))
                          (if (visible? t) (list (pvar t)) '()))
                         (($ box ($ c _ _ x p a n))
                          (print-share
                            t
                            add-+
                            list
                            (lambda ()
                              (cond ((visible? p)
                                     (let* ((split-flag
                                              (and share
                                                   (match (ind* p)
                                                          (($ box
                                                              ($ v
                                                                 _
                                                                 _
                                                                 _
                                                                 _
                                                                 split
                                                                 _))
                                                           split)
                                                          (_ #f))))
                                            (kname (if split-flag
                                                     (string->symbol
                                                       (sprintf
                                                         "~a#~a"
                                                         (k-name x)
                                                         (pvar p)))
                                                     (k-name x))))
                                       (cons (cond ((null? a) kname)
                                                   ((eq? '?-> (k-name x))
                                                    (let ((arg (add-+ (loop (car a)
                                                                            (visible?
                                                                              (tailvar
                                                                                (car a))))))
                                                          (res (add-+ (loop (cadr a)
                                                                            (visible?
                                                                              (tailvar
                                                                                (cadr a)))))))
                                                      (decode-arrow
                                                        kname
                                                        (lambda ()
                                                          (if split-flag
                                                            (string->symbol
                                                              (sprintf
                                                                "->#~a"
                                                                (pvar p)))
                                                            '->))
                                                        arg
                                                        res)))
                                                   ((eq? 'record (k-name x))
                                                    `(,kname
                                                      ,@(loop (car a) #f)))
                                                   (else
                                                    `(,kname
                                                      ,@(maplr (lambda (x)
                                                                 (add-+ (loop x
                                                                              (visible?
                                                                                (tailvar
                                                                                  x)))))
                                                               a))))
                                             (loop n tailvis))))
                                    ((not tailvis) (loop n tailvis))
                                    (else
                                     (cons `(not ,(k-name x))
                                           (loop n tailvis)))))))
                         (($ box 'bot) '())
                         (($ box i) (loop i tailvis)))))))
(define assemble-union
  (lambda (bindings body)
    (subst-small-type
      (map clean-binding bindings)
      body)))
(define add-+
  (match-lambda
    (() 'empty)
    ((t) t)
    (x (cons '+ x))))
(define tailvar
  (lambda (t)
    (match t
           (($ box (? v?)) t)
           (($ box ($ c _ _ _ _ _ n)) (tailvar n))
           (($ box 'bot) t)
           (($ box i) (tailvar i)))))
(define decode-arrow
  (lambda (kname thunk-> arg res)
    (let ((args (recur loop
                       ((l arg))
                       (match l
                              ('noarg '())
                              (('arg a b) `(,a ,@(loop b)))
                              (('+ ('arg a b) 'noarg . _)
                               `((&optional ,a) ,@(loop b)))
                              (('+ 'noarg ('arg a b) . _)
                               `((&optional ,a) ,@(loop b)))
                              ((? symbol? z)
                               (if (rectypevar? z) `(,z) `((&rest ,z))))
                              (('+ 'noarg z) (loop z))
                              (('+ ('arg a b) z)
                               (loop `(+ (arg ,a ,b) noarg ,z)))))))
      `(,@args ,(thunk->) ,res))))
(define rectypevar?
  (lambda (s)
    (memq (string-ref (symbol->string s) 0) '(#\Y))))
(define typevar?
  (lambda (s)
    (memq (string-ref (symbol->string s) 0)
          '(#\X #\Z))))
(define clean-binding
  (lambda (binding)
    (match binding
           ((u ('+ 'nil ('cons a v)))
            (if (and (equal? u v) (not (memq* u a)))
              (list u `(list ,a))
              binding))
           ((u ('+ ('cons a v) 'nil))
            (if (and (equal? u v) (not (memq* u a)))
              (list u `(list ,a))
              binding))
           ((u ('+ 'nil ('cons a v) (? symbol? z)))
            (if (and (equal? u v) (not (memq* u a)) (typevar? z))
              (list u `(list* ,a ,z))
              binding))
           ((u ('+ ('cons a v) 'nil (? symbol? z)))
            (if (and (equal? u v) (not (memq* u a)) (typevar? z))
              (list u `(list* ,a ,z))
              binding))
           ((u ('+ 'noarg ('arg a v)))
            (if (and (equal? u v) (not (memq* u a)))
              (list u `(&list ,a))
              binding))
           ((u ('+ ('arg a v) 'noarg))
            (if (and (equal? u v) (not (memq* u a)))
              (list u `(&list ,a))
              binding))
           (x x))))
(define memq*
  (lambda (v t)
    (recur loop
           ((t t))
           (match t
                  ((x . y) (or (loop x) (loop y)))
                  (_ (eq? v t))))))
(define subst-type
  (lambda (new old t)
    (match new
           (('list elem) (subst-list elem old t))
           (_ (subst* new old t)))))
(define subst-list
  (lambda (elem old t)
    (match t
           ((? symbol?) (if (eq? old t) `(list ,elem) t))
           (('+ 'nil ('cons a (? symbol? b)))
            (if (and (eq? b old) (equal? elem a))
              `(list ,elem)
              `(+ nil (cons ,(subst-list elem old a) ,b))))
           (('+ ('cons a (? symbol? b)) 'nil)
            (if (and (eq? b old) (equal? elem a))
              `(list ,elem)
              `(+ nil (cons ,(subst-list elem old a) ,b))))
           ((a . b)
            (cons (subst-list elem old a)
                  (subst-list elem old b)))
           (z z))))
(define subst*
  (lambda (new old t)
    (cond ((eq? old t) new)
          ((pair? t)
           (cons (subst* new old (car t))
                 (subst* new old (cdr t))))
          (else t))))
(define subst-small-type
  (lambda (bindings body)
    (recur loop
           ((bindings bindings) (newb '()) (body body))
           (match bindings
                  (()
                   (let ((newb (filter
                                 (match-lambda
                                   ((name type) (not (equal? name type))))
                                 newb)))
                     (if (null? newb)
                       body
                       `(rec ,(reverse newb) ,body))))
                  (((and b (name type)) . rest)
                   (if (and (not (memq* name type)) (small-type? type))
                     (loop (subst-type type name rest)
                           (subst-type type name newb)
                           (subst-type type name body))
                     (loop rest (cons b newb) body)))))))
(define small-type?
  (lambda (t)
    (>= 8
        (recur loop
               ((t t))
               (match t
                      ('+ 0)
                      ((? symbol? s) 1)
                      ((? number? n) 0)
                      ((x . y) (+ (loop x) (loop y)))
                      (() 0))))))
(define qop
  (lambda (s)
    (string->symbol (string-append "# " s))))
(define qcons (qop "cons"))
(define qbox (qop "box"))
(define qlist (qop "list"))
(define qvector (qop "vector"))
(define initial-info
  `((not (a -> bool))
    (eqv? (a a -> bool))
    (eq? (a a -> bool))
    (equal? (a a -> bool))
    (cons (a b -> (cons a b)) (ic))
    (car ((cons a b) -> a) (s (x . _)))
    (cdr ((cons b a) -> a) (s (_ . x)))
    (caar ((cons (cons a b) c) -> a)
          (s ((x . _) . _)))
    (cadr ((cons c (cons a b)) -> a) (s (_ x . _)))
    (cdar ((cons (cons b a) c) -> a)
          (s ((_ . x) . _)))
    (cddr ((cons c (cons b a)) -> a) (s (_ _ . x)))
    (caaar ((cons (cons (cons a b) c) d) -> a)
           (s (((x . _) . _) . _)))
    (caadr ((cons d (cons (cons a b) c)) -> a)
           (s (_ (x . _) . _)))
    (cadar ((cons (cons c (cons a b)) d) -> a)
           (s ((_ x . _) . _)))
    (caddr ((cons d (cons c (cons a b))) -> a)
           (s (_ _ x . _)))
    (cdaar ((cons (cons (cons b a) c) d) -> a)
           (s (((_ . x) . _) . _)))
    (cdadr ((cons d (cons (cons b a) c)) -> a)
           (s (_ (_ . x) . _)))
    (cddar ((cons (cons c (cons b a)) d) -> a)
           (s ((_ _ . x) . _)))
    (cdddr ((cons d (cons c (cons b a))) -> a)
           (s (_ _ _ . x)))
    (caaaar
      ((cons (cons (cons (cons a b) c) d) e) -> a)
      (s ((((x . _) . _) . _) . _)))
    (caaadr
      ((cons e (cons (cons (cons a b) c) d)) -> a)
      (s (_ ((x . _) . _) . _)))
    (caadar
      ((cons (cons d (cons (cons a b) c)) e) -> a)
      (s ((_ (x . _) . _) . _)))
    (caaddr
      ((cons e (cons d (cons (cons a b) c))) -> a)
      (s (_ _ (x . _) . _)))
    (cadaar
      ((cons (cons (cons c (cons a b)) d) e) -> a)
      (s (((_ x . _) . _) . _)))
    (cadadr
      ((cons e (cons (cons c (cons a b)) d)) -> a)
      (s (_ (_ x . _) . _)))
    (caddar
      ((cons (cons d (cons c (cons a b))) e) -> a)
      (s ((_ _ x . _) . _)))
    (cadddr
      ((cons e (cons d (cons c (cons a b)))) -> a)
      (s (_ _ _ x . _)))
    (cdaaar
      ((cons (cons (cons (cons b a) c) d) e) -> a)
      (s ((((_ . x) . _) . _) . _)))
    (cdaadr
      ((cons e (cons (cons (cons b a) c) d)) -> a)
      (s (_ ((_ . x) . _) . _)))
    (cdadar
      ((cons (cons d (cons (cons b a) c)) e) -> a)
      (s ((_ (_ . x) . _) . _)))
    (cdaddr
      ((cons e (cons d (cons (cons b a) c))) -> a)
      (s (_ _ (_ . x) . _)))
    (cddaar
      ((cons (cons (cons c (cons b a)) d) e) -> a)
      (s (((_ _ . x) . _) . _)))
    (cddadr
      ((cons e (cons (cons c (cons b a)) d)) -> a)
      (s (_ (_ _ . x) . _)))
    (cdddar
      ((cons (cons d (cons c (cons b a))) e) -> a)
      (s ((_ _ _ . x) . _)))
    (cddddr
      ((cons e (cons d (cons c (cons b a)))) -> a)
      (s (_ _ _ _ . x)))
    (set-car! ((cons a b) a -> void))
    (set-cdr! ((cons a b) b -> void))
    (list ((&list a) -> (list a)) (ic))
    (length ((list a) -> num))
    (append ((&list (list a)) -> (list a)) (ic) (d))
    (reverse ((list a) -> (list a)) (ic))
    (list-tail ((list a) num -> (list a)) (c))
    (list-ref ((list a) num -> a) (c))
    (memq (a (list a) -> (+ false (cons a (list a)))))
    (memv (a (list a) -> (+ false (cons a (list a)))))
    (member
      (a (list a) -> (+ false (cons a (list a)))))
    (assq (a (list (cons a c)) -> (+ false (cons a c))))
    (assv (a (list (cons a c)) -> (+ false (cons a c))))
    (assoc (a (list (cons a c)) -> (+ false (cons a c))))
    (symbol->string (sym -> str))
    (string->symbol (str -> sym))
    (complex? (a -> bool))
    (real? (a -> bool))
    (rational? (a -> bool))
    (integer? (a -> bool))
    (exact? (num -> bool))
    (inexact? (num -> bool))
    (= (num num (&list num) -> bool))
    (< (num num (&list num) -> bool))
    (> (num num (&list num) -> bool))
    (<= (num num (&list num) -> bool))
    (>= (num num (&list num) -> bool))
    (zero? (num -> bool))
    (positive? (num -> bool))
    (negative? (num -> bool))
    (odd? (num -> bool))
    (even? (num -> bool))
    (max (num (&list num) -> num))
    (min (num (&list num) -> num))
    (+ ((&list num) -> num))
    (* ((&list num) -> num))
    (- (num (&list num) -> num))
    (/ (num (&list num) -> num))
    (abs (num -> num))
    (quotient (num num -> num))
    (remainder (num num -> num))
    (modulo (num num -> num))
    (gcd ((&list num) -> num))
    (lcm ((&list num) -> num))
    (numerator (num -> num))
    (denominator (num -> num))
    (floor (num -> num))
    (ceiling (num -> num))
    (truncate (num -> num))
    (round (num -> num))
    (rationalize (num num -> num))
    (exp (num -> num))
    (log (num -> num))
    (sin (num -> num))
    (cos (num -> num))
    (tan (num -> num))
    (asin (num -> num))
    (acos (num -> num))
    (atan (num (&optional num) -> num))
    (sqrt (num -> num))
    (expt (num num -> num))
    (make-rectangular (num num -> num))
    (make-polar (num num -> num))
    (real-part (num -> num))
    (imag-part (num -> num))
    (magnitude (num -> num))
    (angle (num -> num))
    (exact->inexact (num -> num))
    (inexact->exact (num -> num))
    (number->string (num (&optional num) -> str))
    (string->number (str (&optional num) -> num))
    (char=? (char char -> bool))
    (char<? (char char -> bool))
    (char>? (char char -> bool))
    (char<=? (char char -> bool))
    (char>=? (char char -> bool))
    (char-ci=? (char char -> bool))
    (char-ci<? (char char -> bool))
    (char-ci>? (char char -> bool))
    (char-ci<=? (char char -> bool))
    (char-ci>=? (char char -> bool))
    (char-alphabetic? (char -> bool))
    (char-numeric? (char -> bool))
    (char-whitespace? (char -> bool))
    (char-upper-case? (char -> bool))
    (char-lower-case? (char -> bool))
    (char->integer (char -> num))
    (integer->char (num -> char))
    (char-upcase (char -> char))
    (char-downcase (char -> char))
    (make-string (num (&optional char) -> str))
    (string ((&list char) -> str))
    (string-length (str -> num))
    (string-ref (str num -> char))
    (string-set! (str num char -> void))
    (string=? (str str -> bool))
    (string<? (str str -> bool))
    (string>? (str str -> bool))
    (string<=? (str str -> bool))
    (string>=? (str str -> bool))
    (string-ci=? (str str -> bool))
    (string-ci<? (str str -> bool))
    (string-ci>? (str str -> bool))
    (string-ci<=? (str str -> bool))
    (string-ci>=? (str str -> bool))
    (substring (str num num -> str))
    (string-append ((&list str) -> str))
    (string->list (str -> (list char)) (ic))
    (list->string ((list char) -> str))
    (string-copy (str -> str))
    (string-fill! (str char -> void))
    (make-vector (num a -> (vec a)) (i))
    (vector ((&list a) -> (vec a)) (i))
    (vector-length ((vec a) -> num))
    (vector-ref ((vec a) num -> a))
    (vector-set! ((vec a) num a -> void))
    (vector->list ((vec a) -> (list a)) (ic))
    (list->vector ((list a) -> (vec a)) (i))
    (vector-fill! ((vec a) a -> void))
    (apply (((&list a) -> b) (list a) -> b) (i) (d))
    (map ((a -> b) (list a) -> (list b)) (i) (d))
    (for-each ((a -> b) (list a) -> void) (i) (d))
    (force ((promise a) -> a) (i))
    (call-with-current-continuation
      (((a -> b) -> a) -> a)
      (i))
    (call-with-input-file
      (str (iport -> a) -> a)
      (i))
    (call-with-output-file
      (str (oport -> a) -> a)
      (i))
    (input-port? (a -> bool))
    (output-port? (a -> bool))
    (current-input-port (-> iport))
    (current-output-port (-> oport))
    (with-input-from-file (str (-> a) -> a) (i))
    (with-output-to-file (str (-> a) -> a) (i))
    (open-input-file (str -> iport))
    (open-output-file (str -> oport))
    (close-input-port (iport -> void))
    (close-output-port (oport -> void))
    (read ((&optional iport)
           ->
           (+ eof
              num
              nil
              false
              true
              char
              sym
              str
              (box (mu sexp
                       (+ num
                          nil
                          false
                          true
                          char
                          sym
                          str
                          (vec sexp)
                          (cons sexp sexp)
                          (box sexp))))
              (cons sexp sexp)
              (vec sexp)))
          (i))
    (read-char
      ((&optional iport) -> (+ char eof))
      (i))
    (peek-char
      ((&optional iport) -> (+ char eof))
      (i))
    (char-ready? ((&optional iport) -> bool) (i))
    (write (a (&optional oport) -> void) (i))
    (display (a (&optional oport) -> void) (i))
    (newline ((&optional oport) -> void) (i))
    (write-char (char (&optional oport) -> void) (i))
    (load (str -> void))
    (transcript-on (str -> void))
    (transcript-off (-> void))
    (symbol-append ((&rest a) -> sym))
    (box (a -> (box a)) (i))
    (unbox ((box a) -> a) (s boxx))
    (set-box! ((box a) a -> void))
    (void (-> void))
    (make-module (a -> (module a)))
    (raise ((&rest a) -> b))
    (match:error (a (&rest b) -> c))
    (should-never-reach (a -> b))
    (make-cvector (num a -> (cvec a)))
    (cvector ((&list a) -> (cvec a)))
    (cvector-length ((cvec a) -> num))
    (cvector-ref ((cvec a) num -> a))
    (cvector->list ((cvec a) -> (list a)) (ic))
    (list->cvector ((list a) -> (cvec a)))
    (,qcons (a b -> (cons a b)) (ic) (n))
    (,qvector ((&list a) -> (vec a)) (i) (n))
    (,qbox (a -> (box a)) (i) (n))
    (,qlist ((&list a) -> (list a)) (ic) (n))
    (number? ((+ num x) -> bool) (p (num)))
    (null? ((+ nil x) -> bool) (p (nil)))
    (char? ((+ char x) -> bool) (p (char)))
    (symbol? ((+ sym x) -> bool) (p (sym)))
    (string? ((+ str x) -> bool) (p (str)))
    (vector? ((+ (vec a) x) -> bool) (p (vec a)))
    (cvector? ((+ (cvec a) x) -> bool) (p (cvec a)))
    (box? ((+ (box a) x) -> bool) (p (box a)))
    (pair? ((+ (cons a b) x) -> bool) (p (cons a b)))
    (procedure?
      ((+ ((&rest a) -> b) x) -> bool)
      (p (?-> a b)))
    (eof-object? ((+ eof x) -> bool) (p (eof)))
    (input-port? ((+ iport x) -> bool) (p (iport)))
    (output-port? ((+ oport x) -> bool) (p (oport)))
    (true-object? ((+ true x) -> bool) (p (true)))
    (false-object? ((+ false x) -> bool) (p (false)))
    (module?
      ((+ (module a) x) -> bool)
      (p (module a)))
    (boolean? ((+ true false x) -> bool) (p #t))
    (list? ((mu u (+ nil (cons y u) x)) -> bool)
           (p #t))))
(define initial-env '())
(define init-env!
  (lambda ()
    (set! initial-env
      (foldr init-prim empty-env initial-info))))
(define init-prim
  (lambda (l env)
    (letrec ((build-selector
               (match-lambda
                 ('x (lambda (x) x))
                 ('_ (lambda (x) (make-pany)))
                 ('boxx
                  (let ((c (lookup env 'box?)))
                    (lambda (x) (make-pobj c (list x)))))
                 ((x . y)
                  (let ((c (lookup env 'pair?))
                        (lx (build-selector x))
                        (ly (build-selector y)))
                    (lambda (x) (make-pobj c (list (lx x) (ly x)))))))))
      (match l
             ((name type . attr)
              (let* ((pure (cond ((assq 'i attr) #f)
                                 ((assq 'ic attr) 'cons)
                                 (else #t)))
                     (def (assq 'd attr))
                     (check (assq 'c attr))
                     (nocheck (assq 'n attr))
                     (pred (match (assq 'p attr)
                                  (#f #f)
                                  ((_ #t) #t)
                                  ((_ (tag . args))
                                   (cons (lookup initial-type-env tag) args))))
                     (sel (match (assq 's attr)
                                 (#f #f)
                                 ((_ s) (build-selector s))))
                     (env1 (extend-env
                             env
                             name
                             (make-name
                               name
                               (closeall (r+ initial-type-env type))
                               #f
                               0
                               #f
                               #f
                               (cond (nocheck 'nocheck)
                                     (check 'check)
                                     (def 'imprecise)
                                     (else #t))
                               #f
                               pure
                               pred
                               #f
                               sel)))
                     (env2 (extend-env
                             env1
                             (symbol-append 'check- name)
                             (make-name
                               (symbol-append 'check- name)
                               (closeall (r++ initial-type-env type))
                               #f
                               0
                               #f
                               #f
                               #t
                               #f
                               pure
                               pred
                               #f
                               sel))))
                env2))))))
(define defprim
  (lambda (name type mode)
    (handle
      (r+ initial-type-env type)
      (match-lambda*
        (('type . args) (apply syntax-err type args))
        (x (apply raise x))))
    (let* ((attr (match mode
                        ('impure '((i)))
                        ('pure '())
                        ('pure-if-cons-is '((ic)))
                        ('mutates-cons
                         (set! cons-mutators (cons name cons-mutators))
                         '())
                        (x (use-error
                             "invalid attribute ~a for st:defprim"
                             x))))
           (info `(,name ,type ,@attr)))
      (unless
        (equal? info (assq name initial-info))
        (set! initial-info (cons info initial-info))
        (set! initial-env (init-prim info initial-env))))))
(init-types!)
(init-env!)
(define %not (lookup initial-env 'not))
(define %list (lookup initial-env 'list))
(define %cons (lookup initial-env 'cons))
(define %should-never-reach
  (lookup initial-env 'should-never-reach))
(define %false-object?
  (lookup initial-env 'false-object?))
(define %eq? (lookup initial-env 'eq?))
(define %eqv? (lookup initial-env 'eqv?))
(define %equal? (lookup initial-env 'equal?))
(define %null? (lookup initial-env 'null?))
(define %vector? (lookup initial-env 'vector?))
(define %cvector? (lookup initial-env 'cvector?))
(define %list? (lookup initial-env 'list?))
(define %boolean? (lookup initial-env 'boolean?))
(define %procedure?
  (lookup initial-env 'procedure?))
(define n-unbound 0)
(define bind-defs
  (lambda (defs env0 tenv0 old-unbound timestamp)
    (letrec ((cons-mutable #f)
             (unbound '())
             (use-var
               (lambda (x env context mk-node)
                 (match (lookup? env x)
                        (#f
                         (let* ((b (bind-var x)) (n (mk-node b)))
                           (set-name-timestamp! b context)
                           (set! unbound (cons n unbound))
                           n))
                        (b (when (and (name-primitive b)
                                      (memq x cons-mutators))
                                 (set! cons-mutable #t))
                           (set-name-occ! b (+ 1 (name-occ b)))
                           (mk-node b)))))
             (bind-var
               (lambda (x)
                 (make-name
                   x
                   #f
                   timestamp
                   0
                   #f
                   #f
                   #f
                   #f
                   #f
                   #f
                   #f
                   #f)))
             (bind (lambda (e env tenv context)
                     (let ((bind-cur (lambda (x) (bind x env tenv context))))
                       (match e
                              (($ var x) (use-var x env context make-var))
                              (($ prim x)
                               (use-var x initial-env context make-var))
                              (($ const c pred)
                               (use-var
                                 pred
                                 initial-env
                                 context
                                 (lambda (p) (make-const c p))))
                              (($ lam args e2)
                               (let* ((b-args (map bind-var args))
                                      (newenv (extend-env* env args b-args)))
                                 (make-lam
                                   b-args
                                   (bind e2 newenv tenv context))))
                              (($ vlam args rest e2)
                               (let* ((b-args (map bind-var args))
                                      (b-rest (bind-var rest))
                                      (newenv
                                        (extend-env*
                                          env
                                          (cons rest args)
                                          (cons b-rest b-args))))
                                 (make-vlam
                                   b-args
                                   b-rest
                                   (bind e2 newenv tenv context))))
                              (($ match e1 clauses)
                               (make-match
                                 (bind-cur e1)
                                 (map (lambda (x)
                                        (bind-mclause x env tenv context))
                                      clauses)))
                              (($ app e1 args)
                               (make-app (bind-cur e1) (map bind-cur args)))
                              (($ begin exps) (make-begin (map bind-cur exps)))
                              (($ and exps) (make-and (map bind-cur exps)))
                              (($ or exps) (make-or (map bind-cur exps)))
                              (($ if test then els)
                               (make-if
                                 (bind-cur test)
                                 (bind-cur then)
                                 (bind-cur els)))
                              (($ delay e2) (make-delay (bind-cur e2)))
                              (($ set! x e2)
                               (use-var
                                 x
                                 env
                                 context
                                 (lambda (b)
                                   (when (name-struct b)
                                         (syntax-err
                                           (pexpr e)
                                           "define-structure identifier ~a may not be assigned"
                                           x))
                                   (when (name-primitive b)
                                         (syntax-err
                                           (pexpr e)
                                           "(set! ~a ...) requires (define ~a ...)"
                                           x
                                           x))
                                   (when (and (not (name-mutated b))
                                              (not (= (name-timestamp b)
                                                      timestamp)))
                                         (syntax-err
                                           (pexpr e)
                                           "(set! ~a ...) missing from compilation unit defining ~a"
                                           x
                                           x))
                                   (set-name-mutated! b #t)
                                   (make-set! b (bind-cur e2)))))
                              (($ let args e2)
                               (let* ((b-args
                                        (map (match-lambda
                                               (($ bind x e)
                                                (make-bind
                                                  (bind-var x)
                                                  (bind-cur e))))
                                             args))
                                      (newenv
                                        (extend-env*
                                          env
                                          (map bind-name args)
                                          (map bind-name b-args))))
                                 (make-let
                                   b-args
                                   (bind e2 newenv tenv context))))
                              (($ let* args e2)
                               (recur loop
                                      ((args args) (b-args '()) (env env))
                                      (match args
                                             ((($ bind x e) . rest)
                                              (let ((b (bind-var x)))
                                                (loop rest
                                                      (cons (make-bind
                                                              b
                                                              (bind e
                                                                    env
                                                                    tenv
                                                                    context))
                                                            b-args)
                                                      (extend-env env x b))))
                                             (()
                                              (make-let*
                                                (reverse b-args)
                                                (bind e2 env tenv context))))))
                              (($ letr args e2)
                               (let* ((b-args
                                        (map (match-lambda
                                               (($ bind x e)
                                                (make-bind (bind-var x) e)))
                                             args))
                                      (newenv
                                        (extend-env*
                                          env
                                          (map bind-name args)
                                          (map bind-name b-args)))
                                      (b-args
                                        (map (match-lambda
                                               (($ bind b e)
                                                (let* ((n (name-occ b))
                                                       (e2 (bind e
                                                                 newenv
                                                                 tenv
                                                                 context)))
                                                  (set-name-occ! b n)
                                                  (make-bind b e2))))
                                             b-args)))
                                 (make-letr
                                   b-args
                                   (bind e2 newenv tenv context))))
                              (($ body defs exps)
                               (match-let*
                                 (((defs newenv newtenv)
                                   (bind-defn defs env tenv #f)))
                                 (make-body
                                   defs
                                   (map (lambda (x)
                                          (bind x newenv newtenv context))
                                        exps))))
                              (($ record args)
                               (make-record
                                 (map (match-lambda
                                        (($ bind x e)
                                         (new-field! x)
                                         (make-bind x (bind-cur e))))
                                      args)))
                              (($ field x e2)
                               (new-field! x)
                               (make-field x (bind-cur e2)))
                              (($ cast ty e2)
                               (match-let
                                 (((t absv)
                                   (handle
                                     (r+collect
                                       tenv
                                       (match ty
                                              (('rec bind ty2)
                                               `(rec ,bind (,ty2 -> ,ty2)))
                                              (_ `(,ty -> ,ty))))
                                     (match-lambda*
                                       (('type . args)
                                        (apply syntax-err ty args))
                                       (x (apply raise x))))))
                                 (make-cast
                                   (list ty t absv)
                                   (bind-cur e2))))))))
             (bind-mclause
               (lambda (clause env tenv context)
                 (match-let*
                   ((($ mclause pattern body failsym) clause)
                    (patenv empty-env)
                    (bp (recur loop
                               ((p pattern))
                               (match p
                                      (($ pvar x)
                                       (when (bound? patenv x)
                                             (syntax-err
                                               (ppat pattern)
                                               "pattern variable ~a repeated"
                                               x))
                                       (let ((b (bind-var x)))
                                         (set! patenv (extend-env patenv x b))
                                         (make-pvar b)))
                                      (($ pobj c args)
                                       (use-var
                                         c
                                         env
                                         context
                                         (lambda (b)
                                           (cond ((boolean? (name-predicate b))
                                                  (syntax-err
                                                    (ppat pattern)
                                                    "~a is not a predicate"
                                                    c))
                                                 ((and (not (eq? b %vector?))
                                                       (not (eq? b %cvector?))
                                                       (not (= (length
                                                                 (cdr (name-predicate
                                                                        b)))
                                                               (length args))))
                                                  (syntax-err
                                                    (ppat pattern)
                                                    "~a requires ~a sub-patterns"
                                                    c
                                                    (length
                                                      (cdr (name-predicate
                                                             b)))))
                                                 (else
                                                  (make-pobj
                                                    b
                                                    (map loop args)))))))
                                      (($ pand pats)
                                       (make-pand (map loop pats)))
                                      (($ pnot pat) (make-pnot (loop pat)))
                                      (($ ppred pred)
                                       (use-var
                                         pred
                                         env
                                         context
                                         (lambda (b)
                                           (unless
                                             (name-predicate b)
                                             (syntax-err
                                               (ppat pattern)
                                               "~a is not a predicate"
                                               pred))
                                           (make-ppred b))))
                                      (($ pany) p)
                                      (($ pelse) p)
                                      (($ pconst c pred)
                                       (use-var
                                         pred
                                         initial-env
                                         context
                                         (lambda (p) (make-pconst c p))))))))
                   (if failsym
                     (let ((b (bind-var failsym)))
                       (when (bound? patenv failsym)
                             (syntax-err
                               (ppat pattern)
                               "fail symbol ~a repeated"
                               failsym))
                       (set! patenv (extend-env patenv failsym b))
                       (make-mclause
                         bp
                         (bind body (join-env env patenv) tenv context)
                         b))
                     (make-mclause
                       bp
                       (bind body (join-env env patenv) tenv context)
                       #f)))))
             (bind-defn
               (lambda (defs env tenv glob)
                 (let* ((newenv empty-env)
                        (newtenv empty-env)
                        (struct-def
                          (lambda (x pure)
                            (when (or (bound? newenv x)
                                      (and glob (bound? initial-env x)))
                                  (syntax-err
                                    #f
                                    "~a defined more than once"
                                    x))
                            (let ((b (bind-var x)))
                              (set-name-primitive! b #t)
                              (set-name-struct! b #t)
                              (set-name-pure! b pure)
                              (set! newenv (extend-env newenv x b))
                              b)))
                        (bind1 (match-lambda
                                 ((and z ($ define x e))
                                  (cond ((not x) z)
                                        ((bound? newenv x)
                                         (if glob
                                           (make-define #f (make-set! x e))
                                           (syntax-err
                                             #f
                                             "~a defined more than once"
                                             x)))
                                        (else
                                         (let ((b (bind-var x)))
                                           (set-name-gdef! b glob)
                                           (set! newenv
                                             (extend-env newenv x b))
                                           (make-define b e)))))
                                 ((and d
                                       ($ defstruct
                                          tag
                                          args
                                          make
                                          pred
                                          get
                                          set
                                          getn
                                          setn
                                          mutable))
                                  (let* ((make (struct-def
                                                 make
                                                 (map not mutable)))
                                         (pred (struct-def pred #t))
                                         (bind-get
                                           (lambda (name n)
                                             (match name
                                                    (($ some x)
                                                     (let ((b (struct-def
                                                                x
                                                                #t)))
                                                       (set-name-selector!
                                                         b
                                                         (lambda (x)
                                                           (make-pobj
                                                             pred
                                                             (map-with-n
                                                               (lambda (_ m)
                                                                 (if (= m n)
                                                                   x
                                                                   (make-pany)))
                                                               get))))
                                                       (some b)))
                                                    (none none))))
                                         (bind-set
                                           (match-lambda
                                             (($ some x)
                                              (some (struct-def x #t)))
                                             (none none)))
                                         (get (map-with-n bind-get get))
                                         (getn (map-with-n bind-get getn))
                                         (set (map bind-set set))
                                         (setn (map bind-set setn))
                                         (_ (when (bound? newtenv tag)
                                                  (syntax-err
                                                    (pdef d)
                                                    "type constructor ~a defined more than once"
                                                    tag)))
                                         (tc (bind-tycon
                                               tag
                                               mutable
                                               (bound? tenv tag)
                                               (lambda args
                                                 (apply syntax-err
                                                        (cons (pdef d)
                                                              args))))))
                                    (set! newtenv (extend-env newtenv tag tc))
                                    (set-name-predicate!
                                      pred
                                      `(,tc ,@(map (lambda (_) (gensym)) get)))
                                    (make-defstruct
                                      tc
                                      args
                                      make
                                      pred
                                      get
                                      set
                                      getn
                                      setn
                                      mutable)))
                                 ((and d ($ datatype dt))
                                  (make-datatype
                                    (maplr (match-lambda
                                             (((tag . args) . bindings)
                                              (when (bound? newtenv tag)
                                                    (syntax-err
                                                      (pdef d)
                                                      "type constructor ~a defined more than once"
                                                      tag))
                                              (let ((tc (bind-tycon
                                                          tag
                                                          (map (lambda (_) #f)
                                                               args)
                                                          (bound? tenv tag)
                                                          (lambda args
                                                            (apply syntax-err
                                                                   (cons (pdef d)
                                                                         args))))))
                                                (set! newtenv
                                                  (extend-env newtenv tag tc))
                                                (cons (cons tc args)
                                                      (maplr (match-lambda
                                                               (($ variant
                                                                   con
                                                                   pred
                                                                   arg-types)
                                                                (let ((make (struct-def
                                                                              con
                                                                              #t))
                                                                      (pred (struct-def
                                                                              pred
                                                                              #t)))
                                                                  (set-name-predicate!
                                                                    pred
                                                                    (cons tc
                                                                          args))
                                                                  (set-name-variant!
                                                                    pred
                                                                    arg-types)
                                                                  (make-variant
                                                                    make
                                                                    pred
                                                                    arg-types))))
                                                             bindings)))))
                                           dt)))))
                        (defs2 (maplr bind1 defs))
                        (newenv2 (join-env env newenv))
                        (newtenv2 (join-env tenv newtenv))
                        (bind2 (match-lambda
                                 ((and ($ define (? name? x) ($ var y)))
                                  (=> fail)
                                  (if (eq? (name-name x) y)
                                    (if (bound? initial-env y)
                                      (make-define
                                        x
                                        (make-var (lookup initial-env y)))
                                      (begin
                                        (printf
                                          "Warning: (define ~a ~a) but ~a is not a primitive~%"
                                          y
                                          y
                                          y)
                                        (fail)))
                                    (fail)))
                                 ((and ($ define x e2) context)
                                  (when (and glob
                                             (name? x)
                                             (bound?
                                               initial-env
                                               (name-name x)))
                                        (printf
                                          "Note: (define ~a ...) hides primitive ~a~%"
                                          (name-name x)
                                          (name-name x)))
                                  (make-define
                                    (or x
                                        (let ((b (bind-var x)))
                                          (set-name-gdef! b glob)
                                          b))
                                    (bind e2 newenv2 newtenv2 context)))
                                 (d d))))
                   (list (maplr bind2 defs2) newenv2 newtenv2))))
             (bind-old
               (lambda (e env)
                 (match e
                        (($ var x)
                         (match (lookup? env (name-name x))
                                (#f (set! unbound (cons e unbound)))
                                (b (when (and (name-primitive b)
                                              (memq x cons-mutators))
                                         (set! cons-mutable #t))
                                   (set-name-occ! b (+ 1 (name-occ b)))
                                   (set-var-name! e b))))
                        (($ set! x _)
                         (match (lookup? env (name-name x))
                                (#f (set! unbound (cons e unbound)))
                                (b (when (name-struct b)
                                         (syntax-err
                                           (pexpr e)
                                           "define-structure identifier ~a may not be assigned"
                                           x))
                                   (when (name-primitive b)
                                         (syntax-err
                                           (pexpr e)
                                           "(set! ~a ...) requires (define ~a ...)"
                                           x
                                           x))
                                   (when (and (not (name-mutated b))
                                              (not (= (name-timestamp b)
                                                      timestamp)))
                                         (syntax-err
                                           (pexpr e)
                                           "(set! ~a ...) missing from compilation unit defining ~a"
                                           x
                                           x))
                                   (set-name-mutated! b #t)
                                   (set-name-occ! b (+ 1 (name-occ b)))
                                   (set-set!-name! e b))))))))
      (match-let
        (((defs env tenv) (bind-defn defs env0 tenv0 #t)))
        (for-each
          (lambda (x) (bind-old x env))
          old-unbound)
        (set-cons-mutability! cons-mutable)
        (set! n-unbound (length unbound))
        (list defs env tenv unbound)))))
(define rebind-var
  (lambda (b)
    (make-name
      (name-name b)
      (name-ty b)
      (name-timestamp b)
      (name-occ b)
      (name-mutated b)
      #f
      #f
      #f
      #f
      #f
      #f
      #f)))
(define warn-unbound
  (lambda (l)
    (let* ((names '())
           (node->name
             (match-lambda
               (($ var x) x)
               (($ set! x _) x)
               (($ pobj x _) x)
               (($ ppred x) x)))
           (warn (lambda (b)
                   (unless
                     (memq (name-name b) names)
                     (set! names (cons (name-name b) names))
                     (printf
                       "Warning: ~a is unbound in "
                       (name-name b))
                     (print-context (pexpr (name-timestamp b)) 2)))))
      (for-each (lambda (x) (warn (node->name x))) l))))
(define name-unbound?
  (lambda (x) (not (number? (name-timestamp x)))))
(define improve-defs
  (lambda (defs)
    (map (match-lambda
           (($ define x e2) (make-define x (improve e2)))
           (x x))
         defs)))
(define improve
  (match-lambda
    (($ match e clauses) (improve-match e clauses))
    (($ if tst thn els) (improve-if tst thn els))
    ((? var? e) e)
    ((? const? e) e)
    (($ lam args e2) (make-lam args (improve e2)))
    (($ vlam args rest e2)
     (make-vlam args rest (improve e2)))
    (($ app (and e1 ($ var x)) args)
     (let ((args (map improve args)))
       (if (and (eq? x %list) (< (length args) conslimit))
         (foldr (lambda (a rest)
                  (make-app (make-var %cons) (list a rest)))
                (make-const '() %null?)
                args)
         (make-app e1 args))))
    (($ app e1 args)
     (make-app (improve e1) (map improve args)))
    (($ begin exps) (make-begin (map improve exps)))
    (($ and exps) (make-and (map improve exps)))
    (($ or exps) (make-or (map improve exps)))
    (($ delay e2) (make-delay (improve e2)))
    (($ set! x e2) (make-set! x (improve e2)))
    (($ let args e2)
     (let ((args (map (match-lambda
                        (($ bind x e) (make-bind x (improve e))))
                      args)))
       (make-let args (improve e2))))
    (($ let* args e2)
     (let ((args (map (match-lambda
                        (($ bind x e) (make-bind x (improve e))))
                      args)))
       (make-let* args (improve e2))))
    (($ letr args e2)
     (let ((args (map (match-lambda
                        (($ bind x e) (make-bind x (improve e))))
                      args)))
       (make-letr args (improve e2))))
    (($ body defs exps)
     (let ((defs (improve-defs defs)))
       (make-body defs (map improve exps))))
    (($ record args)
     (make-record
       (map (match-lambda
              (($ bind x e) (make-bind x (improve e))))
            args)))
    (($ field x e2) (make-field x (improve e2)))
    (($ cast ty e2) (make-cast ty (improve e2)))))
(define improve-if
  (lambda (tst thn els)
    (let ((if->match
            (lambda (x p mk-s thn els)
              (let ((else-pat
                      (match els
                             (($ app ($ var q) _)
                              (if (eq? q %should-never-reach)
                                (make-pelse)
                                (make-pany)))
                             (_ (make-pany)))))
                (make-match
                  (make-var x)
                  (list (make-mclause
                          (mk-s (make-ppred p))
                          (make-body '() (list thn))
                          #f)
                        (make-mclause
                          (mk-s else-pat)
                          (make-body '() (list els))
                          #f)))))))
      (match tst
             (($ app ($ var v) (e))
              (=> fail)
              (if (eq? v %not) (improve-if e els thn) (fail)))
             (($ app ($ var eq) (($ const #f _) val))
              (=> fail)
              (if (or (eq? eq %eq?)
                      (eq? eq %eqv?)
                      (eq? eq %equal?))
                (improve-if val els thn)
                (fail)))
             (($ app ($ var eq) (val ($ const #f _)))
              (=> fail)
              (if (or (eq? eq %eq?)
                      (eq? eq %eqv?)
                      (eq? eq %equal?))
                (improve-if val els thn)
                (fail)))
             (($ app ($ var v) (($ var x)))
              (=> fail)
              (if (and (name-predicate v) (not (name-mutated x)))
                (improve (if->match x v (lambda (x) x) thn els))
                (fail)))
             (($ app ($ var v) (($ app ($ var s) (($ var x)))))
              (=> fail)
              (if (and (name-predicate v)
                       (name-selector s)
                       (not (name-mutated x)))
                (improve
                  (if->match x v (name-selector s) thn els))
                (fail)))
             (($ app ($ var v) (($ var x)))
              (=> fail)
              (if (and (name-selector v) (not (name-mutated x)))
                (improve
                  (if->match
                    x
                    %false-object?
                    (name-selector v)
                    els
                    thn))
                (fail)))
             (($ var v)
              (=> fail)
              (if (not (name-mutated v))
                (improve
                  (if->match
                    v
                    %false-object?
                    (lambda (x) x)
                    els
                    thn))
                (fail)))
             (_ (make-if
                  (improve tst)
                  (improve thn)
                  (improve els)))))))
(define improve-match
  (lambda (e clauses)
    (let ((clauses
            (map (match-lambda
                   (($ mclause p body fail)
                    (make-mclause p (improve body) fail)))
                 clauses)))
      (match e
             (($ var x)
              (if (not (name-mutated x))
                (let ((fix-clause
                        (match-lambda
                          ((and c ($ mclause p e fail))
                           (if (not (uses-x? e x))
                             c
                             (let ((y (rebind-var x)))
                               (make-mclause
                                 (make-flat-pand (list p (make-pvar y)))
                                 (sub e x y)
                                 fail)))))))
                  (make-match e (map fix-clause clauses)))
                (make-match e clauses)))
             (_ (make-match (improve e) clauses))))))
(define uses-x?
  (lambda (e x)
    (recur loop
           ((e e))
           (match e
                  (($ and exps) (ormap loop exps))
                  (($ app fun args)
                   (or (loop fun) (ormap loop args)))
                  (($ begin exps) (ormap loop exps))
                  (($ if e1 e2 e3)
                   (or (loop e1) (loop e2) (loop e3)))
                  (($ lam names body) (loop body))
                  (($ let bindings body)
                   (or (ormap (match-lambda (($ bind _ b) (loop b)))
                              bindings)
                       (loop body)))
                  (($ let* bindings body)
                   (or (ormap (match-lambda (($ bind _ b) (loop b)))
                              bindings)
                       (loop body)))
                  (($ letr bindings body)
                   (or (ormap (match-lambda (($ bind _ b) (loop b)))
                              bindings)
                       (loop body)))
                  (($ or exps) (ormap loop exps))
                  (($ delay e2) (loop e2))
                  (($ set! name exp) (or (eq? x name) (loop exp)))
                  (($ var name) (eq? x name))
                  (($ vlam names name body) (loop body))
                  (($ match exp clauses)
                   (or (loop exp)
                       (ormap (match-lambda
                                (($ mclause p b _) (or (loop p) (loop b))))
                              clauses)))
                  (($ body defs exps)
                   (or (ormap loop defs) (ormap loop exps)))
                  (($ record bindings)
                   (ormap (match-lambda (($ bind _ b) (loop b)))
                          bindings))
                  (($ field _ e) (loop e))
                  (($ cast _ e) (loop e))
                  (($ define _ e) (loop e))
                  ((? defstruct?) #f)
                  ((? datatype?) #f)
                  (($ pand pats) (ormap loop pats))
                  (($ pnot pat) (loop pat))
                  (($ pobj c args) (ormap loop args))
                  (($ ppred pred) (eq? x pred))
                  (_ #f)))))
(define sub
  (lambda (e x to)
    (let ((dos (lambda (y) (if (eq? x y) to y))))
      (recur sub
             ((e e))
             (match e
                    (($ define x e) (make-define x (sub e)))
                    ((? defstruct?) e)
                    ((? datatype?) e)
                    (($ match e clauses)
                     (let ((clauses
                             (map (match-lambda
                                    (($ mclause p e fail)
                                     (make-mclause p (sub e) fail)))
                                  clauses)))
                       (make-match (sub e) clauses)))
                    (($ if tst thn els)
                     (make-if (sub tst) (sub thn) (sub els)))
                    (($ var x) (make-var (dos x)))
                    ((? const? e) e)
                    (($ lam args e2) (make-lam args (sub e2)))
                    (($ vlam args rest e2)
                     (make-vlam args rest (sub e2)))
                    (($ app e1 args)
                     (make-app (sub e1) (map sub args)))
                    (($ begin exps) (make-begin (map sub exps)))
                    (($ and exps) (make-and (map sub exps)))
                    (($ or exps) (make-or (map sub exps)))
                    (($ delay e2) (make-delay (sub e2)))
                    (($ set! x e2) (make-set! (dos x) (sub e2)))
                    (($ let args e2)
                     (let ((args (map (match-lambda
                                        (($ bind x e) (make-bind x (sub e))))
                                      args)))
                       (make-let args (sub e2))))
                    (($ let* args e2)
                     (let ((args (map (match-lambda
                                        (($ bind x e) (make-bind x (sub e))))
                                      args)))
                       (make-let* args (sub e2))))
                    (($ letr args e2)
                     (let ((args (map (match-lambda
                                        (($ bind x e) (make-bind x (sub e))))
                                      args)))
                       (make-letr args (sub e2))))
                    (($ body defs exps)
                     (make-body (map sub defs) (map sub exps)))
                    (($ record args)
                     (make-record
                       (map (match-lambda
                              (($ bind x e) (make-bind x (sub e))))
                            args)))
                    (($ field x e) (make-field x (sub e)))
                    (($ cast ty e) (make-cast ty (sub e))))))))
(define improve-clauses
  (lambda (clauses)
    (recur loop
           ((clauses clauses))
           (match clauses
                  (() '())
                  ((_) clauses)
                  (((and m1 ($ mclause p _ fail)) . rest)
                   (cons m1
                         (if fail
                           (loop rest)
                           (recur loop2
                                  ((clauses (loop rest)))
                                  (match clauses
                                         (() '())
                                         (((and m ($ mclause p2 body2 fail2))
                                           .
                                           r)
                                          (match (improve-by-pattern p2 p)
                                                 (('stop . p)
                                                  (cons (make-mclause
                                                          p
                                                          body2
                                                          fail2)
                                                        r))
                                                 (('redundant . p)
                                                  (unless
                                                    (null? r)
                                                    (printf
                                                      "Warning: redundant pattern ~a~%"
                                                      (ppat p2)))
                                                  (cons (make-mclause
                                                          p
                                                          body2
                                                          fail2)
                                                        r))
                                                 (('continue . p)
                                                  (cons (make-mclause
                                                          p
                                                          body2
                                                          fail2)
                                                        (loop2 r))))))))))))))
(define improve-by-pattern
  (lambda (p2 p1)
    (call-with-current-continuation
      (lambda (k)
        (let* ((reject (lambda () (k (cons 'continue p2))))
               (p1covers #t)
               (p2covers #t)
               (p3 (recur m
                          ((p1 p1) (p2 p2))
                          '(printf "(M ~a ~a)~%" (ppat p1) (ppat p2))
                          (match (cons p1 p2)
                                 ((($ pand (a . _)) . p2) (m a p2))
                                 ((p1 $ pand (a . b))
                                  (make-flat-pand (cons (m p1 a) b)))
                                 ((($ pvar _) . _)
                                  (unless
                                    (or (pvar? p2) (pany? p2))
                                    (set! p2covers #f))
                                  p2)
                                 ((($ pany) . _)
                                  (unless
                                    (or (pvar? p2) (pany? p2))
                                    (set! p2covers #f))
                                  p2)
                                 ((($ pelse) . _)
                                  '(unless
                                     (or (pvar? p2) (pany? p2))
                                     (set! p2covers #f))
                                  p2)
                                 ((_ $ pvar _)
                                  (unless p1covers (reject))
                                  (set! p1covers #f)
                                  (make-flat-pand (list p2 (make-pnot p1))))
                                 ((_ $ pany)
                                  (unless p1covers (reject))
                                  (set! p1covers #f)
                                  (make-flat-pand (list p2 (make-pnot p1))))
                                 ((_ $ pelse)
                                  (unless p1covers (reject))
                                  (set! p1covers #f)
                                  (make-flat-pand (list p2 (make-pnot p1))))
                                 ((($ pconst a _) $ pconst b _)
                                  (unless (equal? a b) (reject))
                                  p2)
                                 ((($ pobj tag1 a) $ pobj tag2 b)
                                  (unless (eq? tag1 tag2) (reject))
                                  (make-pobj tag1 (map2 m a b)))
                                 ((($ ppred tag1) $ ppred tag2)
                                  (unless (eq? tag1 tag2) (reject))
                                  p2)
                                 ((($ ppred tag1) $ pobj tag2 _)
                                  (unless (eq? tag1 tag2) (reject))
                                  (set! p2covers #f)
                                  p2)
                                 ((($ ppred tag1) $ pconst c tag2)
                                  (unless (eq? tag1 tag2) (reject))
                                  (set! p2covers #f)
                                  p2)
                                 (_ (reject))))))
          (cond (p1covers (cons 'redundant p2))
                (p2covers (cons 'stop p3))
                (else (cons 'continue p3))))))))
(define improve-by-noisily
  (lambda (p2 p1)
    (let ((r (improve-by-pattern p2 p1)))
      (printf
        "~a by ~a returns ~a ~a~%"
        (ppat p2)
        (ppat p1)
        (car r)
        (ppat (cdr r))))))
(define make-components
  (lambda (d)
    (let* ((structs
             (filter-map
               (match-lambda ((? define?) #f) (x x))
               d))
           (defs (filter-map
                   (match-lambda ((? define? x) x) (_ #f))
                   d))
           (name-of (match-lambda (($ define x _) x)))
           (ref-of
             (match-lambda
               (($ define _ e) (references e name-gdef))))
           (comp (top-sort defs name-of ref-of)))
      (when #f
            (printf "Components:~%")
            (pretty-print
              (map (lambda (c)
                     (map (match-lambda
                            (($ define x _) (and x (name-name x))))
                          c))
                   comp)))
      (append structs comp))))
(define make-body-components
  (lambda (d)
    (let* ((structs
             (filter-map
               (match-lambda ((? define?) #f) (x x))
               d))
           (defs (filter-map
                   (match-lambda ((? define? x) x) (_ #f))
                   d))
           (name-of (match-lambda (($ define x _) x)))
           (bound (map name-of defs))
           (ref-of
             (match-lambda
               (($ define _ e)
                (references e (lambda (x) (memq x bound))))))
           (comp (top-sort defs name-of ref-of)))
      (when #f
            (printf "Components:~%")
            (pretty-print
              (map (lambda (c)
                     (map (match-lambda
                            (($ define x _) (and x (name-name x))))
                          c))
                   comp)))
      (append structs comp))))
(define make-letrec-components
  (lambda (bindings)
    (let* ((name-of bind-name)
           (bound (map name-of bindings))
           (ref-of
             (match-lambda
               (($ bind _ e)
                (references e (lambda (x) (memq x bound))))))
           (comp (top-sort bindings name-of ref-of)))
      (when #f
            (printf "Letrec Components:~%")
            (pretty-print
              (map (lambda (c)
                     (map (match-lambda (($ bind x _) (pname x))) c))
                   comp)))
      comp)))
(define references
  (lambda (e ref?)
    (recur loop
           ((e e))
           (match e
                  (($ define x e)
                   (if (and x (name-mutated x))
                     (union (set x) (loop e))
                     (loop e)))
                  ((? defstruct?) empty-set)
                  ((? datatype?) empty-set)
                  ((? const?) empty-set)
                  (($ var x) (if (ref? x) (set x) empty-set))
                  (($ lam _ e1) (loop e1))
                  (($ vlam _ _ e1) (loop e1))
                  (($ app e0 args)
                   (foldr union2 (loop e0) (map loop args)))
                  (($ let b e2)
                   (let ((do-bind (match-lambda (($ bind _ e) (loop e)))))
                     (foldr union2 (loop e2) (map do-bind b))))
                  (($ let* b e2)
                   (let ((do-bind (match-lambda (($ bind _ e) (loop e)))))
                     (foldr union2 (loop e2) (map do-bind b))))
                  (($ letr b e2)
                   (let ((do-bind (match-lambda (($ bind _ e) (loop e)))))
                     (foldr union2 (loop e2) (map do-bind b))))
                  (($ body defs exps)
                   (foldr union2
                          empty-set
                          (map loop (append defs exps))))
                  (($ record b)
                   (let ((do-bind (match-lambda (($ bind _ e) (loop e)))))
                     (foldr union2 empty-set (map do-bind b))))
                  (($ field _ e) (loop e))
                  (($ cast _ e) (loop e))
                  (($ and exps)
                   (foldr union2 empty-set (map loop exps)))
                  (($ or exps)
                   (foldr union2 empty-set (map loop exps)))
                  (($ begin exps)
                   (foldr union2 empty-set (map loop exps)))
                  (($ if test then els)
                   (union (loop test) (loop then) (loop els)))
                  (($ delay e) (loop e))
                  (($ set! x body)
                   (union (if (ref? x) (set x) empty-set)
                          (loop body)))
                  (($ match exp clauses)
                   (foldr union2
                          (loop exp)
                          (map (match-lambda (($ mclause _ exp _) (loop exp)))
                               clauses)))))))
(define top-sort
  (lambda (graph name-of references-of)
    (let* ((adj assq)
           (g (map (lambda (x)
                     (list (name-of x)
                           (box (references-of x))
                           (box #f)
                           x))
                   graph))
           (gt (let ((gt (map (match-lambda
                                ((n _ _ name)
                                 (list n (box empty-set) (box #f) n)))
                              g)))
                 (for-each
                   (match-lambda
                     ((n nay _ _)
                      (for-each
                        (lambda (v)
                          (match (adj v gt)
                                 (#f #f)
                                 ((_ b _ _) (set-box! b (cons n (unbox b))))))
                        (unbox nay))))
                   g)
                 gt))
           (visit (lambda (vg)
                    (letrec ((visit (lambda (g l)
                                      (match g
                                             (#f l)
                                             ((n nay mark name)
                                              (if (unbox mark)
                                                l
                                                (begin
                                                  (set-box! mark #t)
                                                  (cons name
                                                        (foldr (lambda (v l)
                                                                 (visit (adj v
                                                                             vg)
                                                                        l))
                                                               l
                                                               (unbox nay))))))))))
                      visit)))
           (visit-gt (visit gt))
           (visit-g (visit g))
           (post (foldr visit-gt '() gt))
           (pre (foldl (lambda (gg l)
                         (match (visit-g (adj gg g) '())
                                (() l)
                                (c (cons c l))))
                       '()
                       post)))
      (reverse pre))))
(define genlet #t)
(define genmatch #t)
(define letonce #f)
(define type-defs
  (lambda (d)
    (for-each
      (match-lambda
        ((? defstruct? b) (type-structure b))
        ((? datatype? b) (type-structure b))
        (c (type-component c #t)))
      (make-components d))
    (close '())))
(define type-structure
  (match-lambda
    (($ defstruct
        x
        _
        make
        pred
        get
        set
        getn
        setn
        mutable)
     (let* ((vars (map (lambda (_) (gensym)) get))
            (make-get-type
              (lambda (getter v)
                (match getter
                       (($ some b)
                        (set-name-ty!
                          b
                          (closeall
                            (r+ initial-type-env `((,x ,@vars) -> ,v)))))
                       (_ #f))))
            (make-set-type
              (lambda (setter v)
                (match setter
                       (($ some b)
                        (set-name-ty!
                          b
                          (closeall
                            (r+ initial-type-env `((,x ,@vars) ,v -> void)))))
                       (_ #f)))))
       (set-name-ty!
         make
         (closeall
           (r+ initial-type-env `(,@vars -> (,x ,@vars)))))
       (set-name-ty!
         pred
         (closeall
           (r+ initial-type-env
               `((+ (,x ,@vars) y) -> bool))))
       (for-each2 make-get-type get vars)
       (for-each2 make-set-type set vars)
       (for-each2 make-get-type getn vars)
       (for-each2 make-set-type setn vars)))
    (($ datatype dt)
     (for-each
       (match-lambda
         ((type . variants)
          (for-each
            (match-lambda
              (($ variant con pred arg-types)
               (set-name-ty!
                 con
                 (closeall
                   (r+ initial-type-env
                       `(,@(cdr arg-types) -> ,type))))
               (set-name-ty!
                 pred
                 (closeall
                   (r+ initial-type-env
                       `((+ ,(name-predicate pred) x) -> bool))))))
            variants)))
       dt))))
(define type-component
  (lambda (component top)
    (when verbose
          (let ((cnames
                  (filter-map
                    (match-lambda (($ define b _) (name-name b)))
                    component)))
            (unless
              (null? cnames)
              (printf "Typing ~a~%" cnames))))
    (let* ((f (match-lambda (($ define b e) (make-bind b e))))
           (bindings (map f component))
           (names (map (match-lambda (($ define b _) (pname b)))
                       component))
           (f1 (match-lambda
                 (($ define b _) (set-name-ty! b (tvar)))))
           (f2 (match-lambda
                 ((and d ($ define b e))
                  (set-define-exp! d (w e names)))))
           (f3 (match-lambda
                 (($ define b e) (unify (name-ty b) (typeof e)))))
           (f4 (match-lambda (($ define b _) (name-ty b))))
           (f5 (lambda (d ts)
                 (match d (($ define b _) (set-name-ty! b ts))))))
      (push-level)
      (for-each f1 component)
      (for-each f2 component)
      (for-each f3 component)
      (for-each limit-expansive component)
      (for-each
        f5
        component
        (close (map f4 component)))
      (pop-level))))
(define w
  (lambda (e component)
    (match e
           (($ const _ pred)
            (make-type
              (r+ initial-type-env (name-predicate pred))
              e))
           (($ var x)
            (unless
              (name-ty x)
              (set-name-ty!
                x
                (if (name-mutated x)
                  (monotvar)
                  (let* ((_1 (push-level))
                         (t (closeall (tvar)))
                         (_2 (pop-level)))
                    t))))
            (if (ts? (name-ty x))
              (match-let*
                ((tynode (make-type #f #f))
                 ((t absv) (instantiate (name-ty x) tynode)))
                (set-type-ty! tynode t)
                (set-type-exp!
                  tynode
                  (match (name-primitive x)
                         ('imprecise
                          (make-check (list absv #f #f #f component) e))
                         ('check
                          (make-check
                            (list (cons top absv) #f #f #f component)
                            e))
                         ('nocheck e)
                         (#t
                          (make-check
                            (list absv (mk-definite-prim t) #f #f component)
                            e))
                         (#f
                          (make-check (list absv #f #f #t component) e))))
                tynode)
              e))
           (($ lam x e1)
            (for-each (lambda (b) (set-name-ty! b (tvar))) x)
            (match-let*
              ((body (w e1 component))
               ((t absv)
                (r+collect
                  initial-type-env
                  `(,@(map name-ty x) -> ,(typeof body)))))
              (make-type
                t
                (make-check
                  (list absv (mk-definite-lam t) #f #f component)
                  (make-lam x body)))))
           (($ vlam x rest e1)
            (for-each (lambda (b) (set-name-ty! b (tvar))) x)
            (match-let*
              ((z (tvar))
               (_ (set-name-ty!
                    rest
                    (r+ initial-type-env `(list ,z))))
               (body (w e1 component))
               ((t absv)
                (r+collect
                  initial-type-env
                  `(,@(map name-ty x) (&list ,z) -> ,(typeof body)))))
              (make-type
                t
                (make-check
                  (list absv (mk-definite-lam t) #f #f component)
                  (make-vlam x rest body)))))
           (($ app e0 args)
            (match-let*
              ((t0 (w e0 component))
               (targs (maplr (lambda (e) (w e component)) args))
               (a* (map (lambda (_) (tvar)) args))
               (b (tvar))
               ((t absv)
                (r-collect initial-type-env `(,@a* -> ,b)))
               (definf (mk-definite-app t)))
              (unify (typeof t0) t)
              (for-each2 unify (map typeof targs) a*)
              (if (syntactically-a-procedure? t0)
                (make-type b (make-app t0 targs))
                (make-type
                  b
                  (make-check
                    (list absv definf #f #f component)
                    (make-app t0 targs))))))
           (($ let b e2)
            (let* ((do-bind
                     (match-lambda
                       (($ bind b e)
                        (if genlet
                          (let* ((_ (push-level))
                                 (e (w e (list (pname b))))
                                 (bind (make-bind b e)))
                            (limit-expansive bind)
                            (set-name-ty! b (car (close (list (typeof e)))))
                            (pop-level)
                            bind)
                          (let ((e (w e component)))
                            (set-name-ty! b (typeof e))
                            (make-bind b e))))))
                   (tb (map do-bind b))
                   (body (w e2 component)))
              (make-let tb body)))
           (($ let* b e2)
            (let* ((do-bind
                     (match-lambda
                       (($ bind b e)
                        (if genlet
                          (let* ((_ (push-level))
                                 (e (w e (list (pname b))))
                                 (bind (make-bind b e)))
                            (limit-expansive bind)
                            (set-name-ty! b (car (close (list (typeof e)))))
                            (pop-level)
                            bind)
                          (let ((e (w e component)))
                            (set-name-ty! b (typeof e))
                            (make-bind b e))))))
                   (tb (maplr do-bind b))
                   (body (w e2 component)))
              (make-let* tb body)))
           (($ letr b e2)
            (let* ((do-comp
                     (lambda (b)
                       (if genlet
                         (let* ((f1 (match-lambda
                                      (($ bind b _) (set-name-ty! b (tvar)))))
                                (names (map (match-lambda
                                              (($ bind b _) (pname b)))
                                            b))
                                (f2 (match-lambda
                                      (($ bind b e)
                                       (make-bind b (w e names)))))
                                (f3 (match-lambda
                                      (($ bind b e)
                                       (unify (name-ty b) (typeof e))
                                       (name-ty b))))
                                (f4 (lambda (bind ts)
                                      (match bind
                                             (($ bind b _)
                                              (set-name-ty! b ts)))))
                                (_1 (push-level))
                                (_2 (for-each f1 b))
                                (tb (maplr f2 b))
                                (_3 (for-each limit-expansive tb))
                                (ts-list (close (maplr f3 tb))))
                           (pop-level)
                           (for-each2 f4 tb ts-list)
                           tb)
                         (let* ((f1 (match-lambda
                                      (($ bind b _) (set-name-ty! b (tvar)))))
                                (f2 (match-lambda
                                      (($ bind b e)
                                       (make-bind b (w e component)))))
                                (f3 (match-lambda
                                      (($ bind b e)
                                       (unify (name-ty b) (typeof e)))))
                                (_1 (for-each f1 b))
                                (tb (maplr f2 b)))
                           (for-each f3 tb)
                           tb))))
                   (comps (make-letrec-components b))
                   (tb (foldr append '() (maplr do-comp comps))))
              (make-letr tb (w e2 component))))
           (($ body defs exps)
            (for-each
              (match-lambda
                ((? defstruct? b) (type-structure b))
                ((? datatype? b) (type-structure b))
                (c (type-component c #f)))
              (make-body-components defs))
            (let ((texps (maplr (lambda (x) (w x component)) exps)))
              (make-body defs texps)))
           (($ and exps)
            (let* ((texps (maplr (lambda (x) (w x component)) exps))
                   (t (match texps
                             (() (r+ initial-type-env 'true))
                             ((e) (typeof e))
                             (_ (let ((a (r+ initial-type-env 'false)))
                                  (unify (typeof (rac texps)) a)
                                  a)))))
              (make-type t (make-and texps))))
           (($ or exps)
            (let* ((texps (maplr (lambda (x) (w x component)) exps))
                   (t (match texps
                             (() (r+ initial-type-env 'false))
                             ((e) (typeof e))
                             (_ (let* ((t-last (typeof (rac texps)))
                                       (but-last (rdc texps))
                                       (a (tvar)))
                                  (for-each
                                    (lambda (e)
                                      (unify (typeof e)
                                             (r+ initial-type-env
                                                 `(+ (not false) ,a))))
                                    but-last)
                                  (unify t-last
                                         (r+ initial-type-env
                                             `(+ (not false) ,a)))
                                  t-last)))))
              (make-type t (make-or texps))))
           (($ begin exps)
            (let ((texps (maplr (lambda (x) (w x component)) exps)))
              (make-begin texps)))
           (($ if test then els)
            (let ((ttest (w test component))
                  (tthen (w then component))
                  (tels (w els component))
                  (a (tvar)))
              (unify (typeof tthen) a)
              (unify (typeof tels) a)
              (make-type a (make-if ttest tthen tels))))
           (($ delay e2)
            (let ((texp (w e2 component)))
              (make-type
                (r+ initial-type-env `(promise ,(typeof texp)))
                (make-delay texp))))
           (($ set! x body)
            (unless (name-ty x) (set-name-ty! x (monotvar)))
            (let* ((body (w body component))
                   (t (if (ts? (name-ty x))
                        (car (instantiate (name-ty x) #f))
                        (name-ty x))))
              (unify t (typeof body))
              (make-type
                (r+ initial-type-env 'void)
                (make-set! x body))))
           (($ record bind)
            (let* ((tbind (map (match-lambda
                                 (($ bind name exp)
                                  (make-bind name (w exp component))))
                               bind))
                   (t (r+ initial-type-env
                          `(record
                             ,@(map (match-lambda
                                      (($ bind name exp)
                                       (list name (typeof exp))))
                                    tbind)))))
              (make-type t (make-record tbind))))
           (($ field name exp)
            (match-let*
              ((texp (w exp component))
               (a (tvar))
               ((t absv)
                (r-collect initial-type-env `(record (,name ,a)))))
              (unify (typeof texp) t)
              (make-type
                a
                (make-check
                  (list absv #f #f #f component)
                  (make-field name texp)))))
           (($ cast (ty t absv) exp)
            (let ((texp (w exp component)) (a (tvar)))
              (unify (r+ initial-type-env `(,(typeof texp) -> ,a))
                     t)
              (make-type
                a
                (make-check
                  (list absv #f #f #f component)
                  (make-cast (list ty t absv) texp)))))
           (($ match exp clauses)
            (for-each
              (match-lambda
                (($ mclause p _ (? name? fail))
                 (set-name-ty!
                   fail
                   (r+ initial-type-env '(a ?-> b))))
                (_ #f))
              clauses)
            (match-let*
              ((iclauses
                 (improve-clauses
                   (append
                     clauses
                     (list (make-mclause (make-pelse) #f #f)))))
               ((tmatch absv precise)
                (w-match (rdc iclauses) (rac iclauses)))
               (texp (w exp component))
               (_ (unify (typeof texp) tmatch))
               (tclauses
                 (maplr (match-lambda
                          (($ mclause p e fail)
                           (make-mclause p (w e component) fail)))
                        clauses))
               (a (tvar)))
              (for-each
                (match-lambda
                  (($ mclause _ e _) (unify (typeof e) a)))
                tclauses)
              (make-type
                a
                (make-check
                  (list absv #f (not precise) #f component)
                  (make-match texp tclauses))))))))
(define w-match
  (lambda (clauses last)
    (letrec ((bindings '())
             (encode
               (match-lambda
                 (($ pand pats) (encode* pats))
                 (x (encode* (list x)))))
             (encode*
               (lambda (pats)
                 (let* ((concrete?
                          (lambda (p)
                            (or (pconst? p) (pobj? p) (ppred? p) (pelse? p))))
                        (var? (lambda (p) (or (pvar? p) (pany? p))))
                        (not-var?
                          (lambda (p)
                            (and (not (pvar? p)) (not (pany? p)))))
                        (t (match (filter concrete? pats)
                                  ((p)
                                   (r+ initial-type-env
                                       (match (template p)
                                              ((x) x)
                                              (x `(+ ,@x)))))
                                  (()
                                   (r+ initial-type-env
                                       `(+ ,@(apply append
                                                    (map template
                                                         (filter
                                                           not-var?
                                                           pats)))
                                           ,@(if (null? (filter var? pats))
                                               '()
                                               (list (out1tvar)))))))))
                   (for-each
                     (match-lambda
                       (($ pvar b)
                        (set! bindings (cons b bindings))
                        (set-name-ty! b (pat-var-bind t))))
                     (filter pvar? pats))
                   t)))
             (template
               (match-lambda
                 ((? pelse?) '())
                 (($ pconst _ pred) (list (name-predicate pred)))
                 ((and pat ($ pobj c args))
                  (list (cond ((or (eq? %vector? c) (eq? %cvector? c))
                               (cons (if (eq? %vector? c) 'vec 'cvec)
                                     (match (maplr encode args)
                                            (() (list (out1tvar)))
                                            ((first . rest)
                                             (list (foldr (lambda (x y)
                                                            (unify x y)
                                                            y)
                                                          first
                                                          rest))))))
                              (else
                               (cons (car (name-predicate c))
                                     (maplr encode args))))))
                 (($ ppred pred)
                  (cond ((eq? pred %boolean?) (list 'true 'false))
                        ((eq? pred %list?) (list `(list ,(out1tvar))))
                        (else
                         (list (cons (car (name-predicate pred))
                                     (maplr (lambda (_) (out1tvar))
                                            (cdr (name-predicate pred))))))))
                 (($ pnot (? pconst?)) '())
                 (($ pnot ($ ppred pred))
                  (cond ((eq? pred %boolean?) '((not true) (not false)))
                        ((eq? pred %procedure?) '((not ?->)))
                        ((eq? pred %list?) '())
                        (else `((not ,(car (name-predicate pred)))))))
                 (($ pnot ($ pobj pred pats))
                  (let ((m (foldr + 0 (map non-triv pats))))
                    (case m
                      ((0) `((not ,(car (name-predicate pred)))))
                      ((1)
                       `((,(car (name-predicate pred))
                          ,@(map (match-lambda
                                   (($ pobj pred _)
                                    `(+ (not ,(car (name-predicate pred)))
                                        ,(out1tvar)))
                                   (($ ppred pred)
                                    `(+ (not ,(car (name-predicate pred)))
                                        ,(out1tvar)))
                                   (_ (out1tvar)))
                                 pats))))
                      (else '()))))))
             (non-triv
               (match-lambda
                 ((? pvar?) 0)
                 ((? pany?) 0)
                 ((? pelse?) 0)
                 ((? pconst?) 2)
                 (($ pobj _ pats) (foldr + 1 (map non-triv pats)))
                 (_ 1)))
             (precise
               (match-lambda
                 ((? pconst?) #f)
                 (($ pand pats) (andmap precise pats))
                 (($ pnot pat) (precise pat))
                 (($ pobj pred pats)
                  (let ((m (foldr + 0 (map non-triv pats))))
                    (case m
                      ((0) #t)
                      ((1) (andmap precise pats))
                      (else #f))))
                 (($ ppred pred) (not (eq? pred %list?)))
                 (_ #t))))
      (push-level)
      (match-let*
        ((precise-match
           (and (andmap
                  (match-lambda (($ mclause _ _ fail) (not fail)))
                  clauses)
                (match last (($ mclause p _ _) (precise p)))))
         (types (maplr (match-lambda (($ mclause p _ _) (encode p)))
                       clauses))
         ((t absv)
          (r-match
            (foldr (lambda (x y) (unify x y) y) (tvar) types))))
        (unify (out1tvar) t)
        (for-each limit-name bindings)
        (for-each2
          set-name-ty!
          bindings
          (close (map name-ty bindings)))
        (pop-level)
        '(pretty-print
           `(match-input
              ,@(map (match-lambda (($ mclause p _ _) (ppat p)))
                     clauses)))
        '(pretty-print
           `(match-type
              ,(ptype t)
              ,@(map (lambda (b) (list (pname b) (ptype (name-ty b))))
                     bindings)))
        (list t absv precise-match)))))
(define syntactically-a-procedure?
  (match-lambda
    (($ type _ e) (syntactically-a-procedure? e))
    (($ check _ e) (syntactically-a-procedure? e))
    (($ var x) (name-primitive x))
    ((? lam?) #t)
    ((? vlam?) #t)
    (($ let _ body)
     (syntactically-a-procedure? body))
    (($ let* _ body)
     (syntactically-a-procedure? body))
    (($ letr _ body)
     (syntactically-a-procedure? body))
    (($ if _ e2 e3)
     (and (syntactically-a-procedure? e2)
          (syntactically-a-procedure? e3)))
    (($ begin exps)
     (syntactically-a-procedure? (rac exps)))
    (($ body _ exps)
     (syntactically-a-procedure? (rac exps)))
    (_ #f)))
(define typeof
  (match-lambda
    (($ type t _) t)
    (($ check _ e) (typeof e))
    (($ let _ body) (typeof body))
    (($ let* _ body) (typeof body))
    (($ letr _ body) (typeof body))
    (($ body _ exps) (typeof (rac exps)))
    (($ begin exps) (typeof (rac exps)))
    (($ var x) (name-ty x))))
(define limit-name
  (lambda (n)
    (when (name-mutated n)
          (unify (name-ty n) (out1tvar)))))
(define limit-expansive
  (letrec ((limit! (lambda (t) (unify t (out1tvar))))
           (expansive-pattern?
             (match-lambda
               ((? pconst?) #f)
               (($ pvar x) (name-mutated x))
               (($ pobj _ pats) (ormap expansive-pattern? pats))
               ((? pany?) #f)
               ((? pelse?) #f)
               (($ pand pats) (ormap expansive-pattern? pats))
               (($ ppred x) (name-mutated x))
               (($ pnot pat) (expansive-pattern? pat))))
           (limit-expr
             (match-lambda
               (($ bind b e)
                (if (name-mutated b)
                  (limit! (typeof e))
                  (limit-expr e)))
               ((? defstruct?) #f)
               ((? datatype?) #f)
               (($ define x e)
                (if (and x (name-mutated x))
                  (limit! (typeof e))
                  (limit-expr e)))
               (($ type
                   t
                   ($ app ($ type _ ($ check _ ($ var x))) exps))
                (cond ((list? (name-pure x))
                       (if (= (length (name-pure x)) (length exps))
                         (for-each2
                           (lambda (pure e)
                             (if pure (limit-expr e) (limit! (typeof e))))
                           (name-pure x)
                           exps)
                         (limit! t)))
                      ((or (eq? #t (name-pure x))
                           (and (eq? 'cons (name-pure x))
                                (not cons-is-mutable)))
                       (for-each limit-expr exps))
                      (else (limit! t))))
               (($ type t ($ app _ _)) (limit! t))
               (($ type t ($ check _ ($ app _ _))) (limit! t))
               (($ delay _) #f)
               (($ type t ($ set! _ _)) (limit! t))
               (($ var _) #f)
               ((? const?) #f)
               (($ lam _ _) #f)
               (($ vlam _ _ _) #f)
               (($ let bind body)
                (limit-expr body)
                (for-each limit-expr bind))
               (($ let* bind body)
                (limit-expr body)
                (for-each limit-expr bind))
               (($ letr bind body)
                (limit-expr body)
                (for-each limit-expr bind))
               (($ body defs exps)
                (for-each limit-expr defs)
                (for-each limit-expr exps))
               (($ and exps) (for-each limit-expr exps))
               (($ or exps) (for-each limit-expr exps))
               (($ begin exps) (for-each limit-expr exps))
               (($ if e1 e2 e3)
                (limit-expr e1)
                (limit-expr e2)
                (limit-expr e3))
               (($ record bind)
                (for-each
                  (match-lambda (($ bind _ e) (limit-expr e)))
                  bind))
               (($ field _ exp) (limit-expr exp))
               (($ cast _ exp) (limit-expr exp))
               (($ match exp clauses)
                (limit-expr exp)
                (for-each
                  (match-lambda
                    (($ mclause pat body fail)
                     (if (or (and fail (name-mutated fail))
                             (expansive-pattern? pat))
                       (limit! (typeof body))
                       (limit-expr body))))
                  clauses))
               (($ type _ e1) (limit-expr e1))
               (($ check _ e1) (limit-expr e1)))))
    limit-expr))
(define unparse
  (lambda (e check-action)
    (letrec ((pbind (match-lambda
                      (($ bind n e) (list (pname n) (pexpr e)))))
             (pexpr (match-lambda
                      ((and x ($ type _ (? check?)))
                       (check-action x pexpr))
                      (($ type _ exp) (pexpr exp))
                      (($ shape t exp) (pexpr exp))
                      (($ define x e)
                       (if (or (not x) (and (name? x) (not (name-name x))))
                         (pexpr e)
                         `(define ,(pname x) ,(pexpr e))))
                      (($ defstruct _ args _ _ _ _ _ _ _)
                       `(check-define-const-structure ,args))
                      (($ datatype d)
                       `(datatype
                          ,@(map (match-lambda
                                   (((tag . args) . bindings)
                                    (cons (cons (ptag tag) args)
                                          (map (match-lambda
                                                 (($ variant _ _ types) types))
                                               bindings))))
                                 d)))
                      (($ and exps) `(and ,@(maplr pexpr exps)))
                      (($ or exps) `(or ,@(maplr pexpr exps)))
                      (($ begin exps) `(begin ,@(maplr pexpr exps)))
                      (($ var x) (pname x))
                      (($ prim x) (pname x))
                      (($ const x _) (pconst x))
                      (($ lam x e1)
                       `(lambda ,(maplr pname x) ,@(pexpr e1)))
                      (($ vlam x rest e1)
                       `(lambda ,(append (maplr pname x) (pname rest))
                          ,@(pexpr e1)))
                      (($ match e1 clauses)
                       (let* ((pclause
                                (match-lambda
                                  (($ mclause p #f #f)
                                   `(,(ppat p) <last clause>))
                                  (($ mclause p exp fail)
                                   (if fail
                                     `(,(ppat p)
                                       (=> ,(pname fail))
                                       ,@(pexpr exp))
                                     `(,(ppat p) ,@(pexpr exp))))))
                              (p1 (pexpr e1)))
                         `(match ,p1 ,@(maplr pclause clauses))))
                      (($ app e1 args)
                       (let* ((p1 (pexpr e1))
                              (pargs (maplr pexpr args))
                              (unkwote
                                (match-lambda
                                  (('quote x) x)
                                  ((? boolean? x) x)
                                  ((? number? x) x)
                                  ((? char? x) x)
                                  ((? string? x) x)
                                  ((? null? x) x)
                                  ((? box? x) x)
                                  ((? vector? x) x))))
                         (cond ((eq? p1 qlist) `',(maplr unkwote pargs))
                               ((eq? p1 qcons)
                                (let ((unq (maplr unkwote pargs)))
                                  `',(cons (car unq) (cadr unq))))
                               ((eq? p1 qbox) (box (unkwote (car pargs))))
                               ((eq? p1 qvector)
                                (list->vector (maplr unkwote pargs)))
                               (else (cons p1 pargs)))))
                      (($ let b e2)
                       (let ((pb (maplr pbind b)))
                         `(let ,pb ,@(pexpr e2))))
                      (($ let* b e2)
                       (let ((pb (maplr pbind b)))
                         `(let* ,pb ,@(pexpr e2))))
                      (($ letr b e2)
                       (let ((pb (maplr pbind b)))
                         `(letrec ,pb ,@(pexpr e2))))
                      (($ body defs exps)
                       (let ((pdefs (maplr pexpr defs)))
                         (append pdefs (maplr pexpr exps))))
                      (($ if e1 e2 e3)
                       (let* ((p1 (pexpr e1)) (p2 (pexpr e2)) (p3 (pexpr e3)))
                         `(if ,p1 ,p2 ,p3)))
                      (($ record bindings)
                       `(record ,@(maplr pbind bindings)))
                      (($ field x e2) `(field ,x ,(pexpr e2)))
                      (($ cast (ty . _) e2) `(: ,ty ,(pexpr e2)))
                      (($ delay e) `(delay ,(pexpr e)))
                      (($ set! x e) `(set! ,(pname x) ,(pexpr e))))))
      (pexpr e))))
(define pexpr
  (lambda (ex)
    (unparse
      ex
      (lambda (e pexpr)
        (match e
               (($ type _ ($ check _ exp)) (pexpr exp)))))))
(define pdef pexpr)
(define ppat
  (match-lambda
    (($ pconst x _) (pconst x))
    (($ pvar x) (pname x))
    (($ pany) '_)
    (($ pelse) 'else)
    (($ pnot pat) `(not ,(ppat pat)))
    (($ pand pats) `(and ,@(maplr ppat pats)))
    (($ ppred pred)
     (match (pname pred)
            ('false-object? #f)
            ('true-object? #t)
            ('null? '())
            (x `(? ,x))))
    (($ pobj tag args)
     (match (cons (pname tag) args)
            (('box? x) (box (ppat x)))
            (('pair? x y) (cons (ppat x) (ppat y)))
            (('vector? . x) (list->vector (maplr ppat x)))
            ((tg . _) `($ ,(strip-? tg) ,@(maplr ppat args)))))))
(define strip-?
  (lambda (s)
    (let* ((str (symbol->string s))
           (n (string-length str)))
      (if (or (zero? n)
              (not (char=? #\? (string-ref str (- n 1)))))
        s
        (string->symbol (substring str 0 (- n 1)))))))
(define pname
  (match-lambda
    ((? name? x) (or (name-name x) '<expr>))
    ((? symbol? x) x)))
(define ptag
  (match-lambda
    ((? k? k) (k-name k))
    ((? symbol? x) x)))
(define pconst
  (match-lambda
    ((? symbol? x) `',x)
    ((? boolean? x) x)
    ((? number? x) x)
    ((? char? x) x)
    ((? string? x) x)
    ((? null? x) `',x)))
(define check
  (lambda (file)
    (output-checked file '() type-check?)))
(define profcheck
  (lambda (file)
    (output-checked #f '() type-check?)
    (output-checked
      #f
      (make-counters total-possible)
      type-check?)))
(define fullcheck
  (lambda (file)
    (let ((check? (lambda (_) #t)))
      (output-checked #f '() check?)
      (output-checked
        #f
        (make-counters total-possible)
        check?))))
(define make-counters
  (lambda (n)
    (let* ((init `(define check-counters (make-vector ,n 0)))
           (sum '(define check-total
                   (lambda ()
                     (let ((foldr (lambda (f i l)
                                    (recur loop
                                           ((l l))
                                           (match l
                                                  (() i)
                                                  ((x . y) (f x (loop y))))))))
                       (foldr + 0 (vector->list check-counters))))))
           (incr '(extend-syntax
                    (check-increment-counter)
                    ((check-increment-counter c)
                     (vector-set!
                       check-counters
                       c
                       (+ 1 (vector-ref check-counters c)))))))
      (list init sum incr))))
(define output-checked
  (lambda (file header check-test)
    (set! summary '())
    (set! total-possible 0)
    (set! total-cast 0)
    (set! total-err 0)
    (set! total-any 0)
    (let ((doit (lambda ()
                  (when (string? file)
                        (printf
                          ";; Generated by Soft Scheme ~a~%"
                          st:version)
                        (printf ";; (st:control")
                        (for-each
                          (lambda (x) (printf " '~a" x))
                          (show-controls))
                        (printf ")~%")
                        (unless
                          (= 0 n-unbound)
                          (printf
                            ";; CAUTION: ~a unbound references, this code is not safe~%"
                            n-unbound))
                        (printf "~%")
                        (for-each pretty-print header))
                  (for-each
                    (lambda (exp)
                      (match exp
                             (($ define x _)
                              (set! n-possible 0)
                              (set! n-clash 0)
                              (set! n-err 0)
                              (set! n-match 0)
                              (set! n-inexhaust 0)
                              (set! n-prim 0)
                              (set! n-lam 0)
                              (set! n-app 0)
                              (set! n-field 0)
                              (set! n-cast 0)
                              (if file
                                (pretty-print (pcheck exp check-test))
                                (pcheck exp check-test))
                              (make-summary-line x)
                              (set! total-possible
                                (+ total-possible n-possible))
                              (set! total-cast (+ total-cast n-cast))
                              (set! total-err (+ total-err n-err))
                              (set! total-any
                                (+ total-any
                                   n-match
                                   n-inexhaust
                                   n-prim
                                   n-lam
                                   n-app
                                   n-field
                                   n-cast)))
                             (_ (when file
                                      (pretty-print
                                        (pcheck exp check-test))))))
                    tree)
                  (when (string? file)
                        (newline)
                        (newline)
                        (print-summary "; ")))))
      (if (string? file)
        (begin
          (delete-file file)
          (with-output-to-file file doit))
        (doit)))))
(define total-possible 0)
(define total-err 0)
(define total-cast 0)
(define total-any 0)
(define n-possible 0)
(define n-clash 0)
(define n-err 0)
(define n-match 0)
(define n-inexhaust 0)
(define n-prim 0)
(define n-lam 0)
(define n-app 0)
(define n-field 0)
(define n-cast 0)
(define summary '())
(define make-summary-line
  (lambda (x)
    (let ((total (+ n-match
                    n-inexhaust
                    n-prim
                    n-lam
                    n-app
                    n-field
                    n-cast)))
      (unless
        (= 0 total)
        (let* ((s (sprintf
                    "~a~a "
                    (padr (pname x) 16)
                    (padl total 2)))
               (s (cond ((< 0 n-inexhaust)
                         (sprintf
                           "~a (~a match ~a inexhaust)"
                           s
                           n-match
                           n-inexhaust))
                        ((< 0 n-match)
                         (sprintf "~a (~a match)" s n-match))
                        (else s)))
               (s (if (< 0 n-prim)
                    (sprintf "~a (~a prim)" s n-prim)
                    s))
               (s (if (< 0 n-field)
                    (sprintf "~a (~a field)" s n-field)
                    s))
               (s (if (< 0 n-lam)
                    (sprintf "~a (~a lambda)" s n-lam)
                    s))
               (s (if (< 0 n-app) (sprintf "~a (~a ap)" s n-app) s))
               (s (if (< 0 n-err)
                    (sprintf "~a (~a ERROR)" s n-err)
                    s))
               (s (if (< 0 n-cast)
                    (sprintf "~a (~a TYPE)" s n-cast)
                    s)))
          (set! summary (cons s summary)))))))
(define print-summary
  (lambda (hdr)
    (for-each
      (lambda (s) (printf "~a~a~%" hdr s))
      (reverse summary))
    (printf
      "~a~a~a "
      hdr
      (padr "TOTAL CHECKS" 16)
      (padl total-any 2))
    (printf
      " (of ~s is ~s%)"
      total-possible
      (if (= 0 total-possible)
        0
        (string->number
          (chop-number
            (exact->inexact
              (* (/ total-any total-possible) 100))
            4))))
    (when (< 0 total-err)
          (printf " (~s ERROR)" total-err))
    (when (< 0 total-cast)
          (printf " (~s TYPE)" total-cast))
    (printf "~%")))
(define padl
  (lambda (arg n)
    (let ((s (sprintf "~a" arg)))
      (recur loop
             ((s s))
             (if (< (string-length s) n)
               (loop (string-append " " s))
               s)))))
(define padr
  (lambda (arg n)
    (let ((s (sprintf "~a" arg)))
      (recur loop
             ((s s))
             (if (< (string-length s) n)
               (loop (string-append s " "))
               s)))))
(define chop-number
  (lambda (x n)
    (substring
      (sprintf "~s00000000000000000000" x)
      0
      (- n 1))))
(define pcheck
  (lambda (ex check-test)
    (unparse
      ex
      (lambda (e pexpr)
        (match e
               ((and z ($ type _ ($ check inf ($ var x))))
                (cond ((name-primitive x)
                       (set! n-possible (+ 1 n-possible))
                       (match (check-test inf)
                              (#f (pname x))
                              ('def
                               (set! n-err (+ 1 n-err))
                               (set! n-prim (+ 1 n-prim))
                               `(,(symbol-append "CHECK-" (pname x))
                                 ,(tree-index z)
                                 ',(string->symbol "ERROR")))
                              (_ (set! n-prim (+ 1 n-prim))
                                 `(,(symbol-append "CHECK-" (pname x))
                                   ,(tree-index z)))))
                      ((name-unbound? x) `(check-bound ,(pname x)))
                      (else
                       (if (check-test inf)
                         (begin
                           (set! n-clash (+ 1 n-clash))
                           `(,(string->symbol "CLASH")
                             ,(pname x)
                             ,(tree-index z)))
                         (pname x)))))
               ((and z
                     ($ type _ ($ check inf (and m ($ lam x e1)))))
                (set! n-possible (+ 1 n-possible))
                (match (check-test inf)
                       (#f (pexpr m))
                       ('def
                        (set! n-err (+ 1 n-err))
                        (set! n-lam (+ 1 n-lam))
                        `(,(string->symbol "CHECK-lambda")
                          (,(tree-index z) ',(string->symbol "ERROR"))
                          ,(map pname x)
                          ,@(pexpr e1)))
                       (_ (set! n-lam (+ 1 n-lam))
                          `(,(string->symbol "CHECK-lambda")
                            (,(tree-index z))
                            ,(map pname x)
                            ,@(pexpr e1)))))
               ((and z
                     ($ type
                        _
                        ($ check inf (and m ($ vlam x rest e1)))))
                (set! n-possible (+ 1 n-possible))
                (match (check-test inf)
                       (#f (pexpr m))
                       ('def
                        (set! n-err (+ 1 n-err))
                        (set! n-lam (+ 1 n-lam))
                        `(,(string->symbol "CHECK-lambda")
                          (,(tree-index z) ',(string->symbol "ERROR"))
                          ,(append (map pname x) (pname rest))
                          ,@(pexpr e1)))
                       (_ (set! n-lam (+ 1 n-lam))
                          `(,(string->symbol "CHECK-lambda")
                            (,(tree-index z))
                            ,(append (map pname x) (pname rest))
                            ,@(pexpr e1)))))
               ((and z
                     ($ type _ ($ check inf (and m ($ app e1 args)))))
                (set! n-possible (+ 1 n-possible))
                (match (check-test inf)
                       (#f (pexpr m))
                       ('def
                        (set! n-err (+ 1 n-err))
                        (set! n-app (+ 1 n-app))
                        `(,(string->symbol "CHECK-ap")
                          (,(tree-index z) ',(string->symbol "ERROR"))
                          ,(pexpr e1)
                          ,@(map pexpr args)))
                       (_ (set! n-app (+ 1 n-app))
                          (let ((p1 (pexpr e1)))
                            `(,(string->symbol "CHECK-ap")
                              (,(tree-index z))
                              ,p1
                              ,@(map pexpr args))))))
               ((and z
                     ($ type _ ($ check inf (and m ($ field x e1)))))
                (set! n-possible (+ 1 n-possible))
                (match (check-test inf)
                       (#f (pexpr m))
                       ('def
                        (set! n-err (+ 1 n-err))
                        (set! n-field (+ 1 n-field))
                        `(,(string->symbol "CHECK-field")
                          (,(tree-index z) ',(string->symbol "ERROR"))
                          ,x
                          ,(pexpr e1)))
                       (_ (set! n-field (+ 1 n-field))
                          `(,(string->symbol "CHECK-field")
                            (,(tree-index z))
                            ,x
                            ,(pexpr e1)))))
               ((and z
                     ($ type
                        _
                        ($ check inf (and m ($ cast (x . _) e1)))))
                (set! n-possible (+ 1 n-possible))
                (match (check-test inf)
                       (#f (pexpr m))
                       (_ (set! n-cast (+ 1 n-cast))
                          `(,(string->symbol "CHECK-:")
                            (,(tree-index z))
                            ,x
                            ,(pexpr e1)))))
               ((and z
                     ($ type
                        _
                        ($ check inf (and m ($ match e1 clauses)))))
                (set! n-possible (+ 1 n-possible))
                (match (check-test inf)
                       (#f (pexpr m))
                       (inx (let* ((pclause
                                     (match-lambda
                                       (($ mclause p exp fail)
                                        (if fail
                                          `(,(ppat p)
                                            (=> ,(pname fail))
                                            ,@(pexpr exp))
                                          `(,(ppat p) ,@(pexpr exp))))))
                                   (p1 (pexpr e1)))
                              (if (eq? 'inexhaust inx)
                                (begin
                                  (set! n-inexhaust (+ 1 n-inexhaust))
                                  `(,(string->symbol "CHECK-match")
                                    (,(tree-index z)
                                     ,(string->symbol "INEXHAUST"))
                                    ,p1
                                    ,@(maplr pclause clauses)))
                                (begin
                                  (set! n-match (+ 1 n-match))
                                  `(,(string->symbol "CHECK-match")
                                    (,(tree-index z))
                                    ,p1
                                    ,@(maplr pclause clauses)))))))))))))
(define tree-index-list '())
(define reinit-output!
  (lambda () (set! tree-index-list '())))
(define tree-index
  (lambda (syntax)
    (match (assq syntax tree-index-list)
           (#f
            (let ((n (length tree-index-list)))
              (set! tree-index-list
                (cons (cons syntax n) tree-index-list))
              n))
           ((_ . n) n))))
(define tree-unindex
  (lambda (n)
    (let ((max (length tree-index-list)))
      (when (<= max n)
            (use-error "Invalid CHECK number ~a" n))
      (car (list-ref tree-index-list (- (- max 1) n))))))
(define cause
  (lambda ()
    (for-each
      (lambda (def)
        (for-each pretty-print (exp-cause def)))
      tree)))
(define cause*
  (lambda names
    (if (null? names)
      (for-each
        (lambda (def)
          (for-each pretty-print (exp-cause def)))
        tree)
      (for-each
        (match-lambda
          ((? symbol? dname)
           (for-each
             pretty-print
             (exp-cause (find-global dname)))))
        names))))
(define exp-cause
  (let ((sum (lambda (exps)
               (foldr (lambda (x y) (append (exp-cause x) y))
                      '()
                      exps)))
        (src (lambda (inf)
               (let ((nonlocal (map tree-index (check-sources inf))))
                 (if (type-check1? inf)
                   (cons (check-local-sources inf) nonlocal)
                   nonlocal)))))
    (match-lambda
      ((and z ($ type ty ($ check inf ($ var x))))
       (if (name-primitive x)
         (if (type-check? inf)
           (list `((,(symbol-append 'check- (pname x))
                    ,(tree-index z))
                   ,@(src inf)))
           '())
         (if (type-check1? inf)
           (list `((clash ,(pname x) ,(tree-index z)) ,@(src inf)))
           '())))
      ((and z ($ type ty ($ check inf ($ lam x e1))))
       (append
         (if (type-check? inf)
           (list `((check-lambda ,(tree-index z) ,(map pname x) ...)
                   ,@(src inf)))
           '())
         (exp-cause e1)))
      ((and z
            ($ type ty ($ check inf ($ vlam x rest e1))))
       (append
         (if (type-check? inf)
           (list `((check-lambda
                     ,(tree-index z)
                     ,(append (map pname x) (pname rest))
                     ...)
                   ,@(src inf)))
           '())
         (exp-cause e1)))
      ((and z ($ type _ ($ check inf ($ app e1 args))))
       (append
         (if (type-check? inf)
           (list `((check-ap ,(tree-index z)) ,@(src inf)))
           '())
         (exp-cause e1)
         (sum args)))
      ((and z ($ type _ ($ check inf ($ field x e1))))
       (append
         (if (type-check? inf)
           (list `((check-field ,(tree-index z) ,x ...)
                   ,@(src inf)))
           '())
         (exp-cause e1)))
      ((and z
            ($ type _ ($ check inf ($ cast (x . _) e1))))
       (append
         (if (type-check? inf)
           (list `((check-: ,(tree-index z) ,x ...) ,@(src inf)))
           '())
         (exp-cause e1)))
      ((and z
            ($ type
               _
               ($ check inf (and m ($ match e1 clauses)))))
       (append
         (if (type-check? inf)
           (list `((check-match ,(tree-index z) ...) ,@(src inf)))
           '())
         (exp-cause m)))
      (($ define _ e) (exp-cause e))
      ((? defstruct?) '())
      ((? datatype?) '())
      (($ app e1 args) (sum (cons e1 args)))
      (($ match exp clauses)
       (foldr (lambda (x y)
                (append
                  (match x (($ mclause _ e _) (exp-cause e)))
                  y))
              (exp-cause exp)
              clauses))
      (($ var _) '())
      (($ and exps) (sum exps))
      (($ begin exps) (sum exps))
      ((? const?) '())
      (($ if test then els)
       (append
         (exp-cause test)
         (exp-cause then)
         (exp-cause els)))
      (($ let bindings body)
       (foldr (lambda (x y)
                (append (match x (($ bind _ e) (exp-cause e))) y))
              (exp-cause body)
              bindings))
      (($ let* bindings body)
       (foldr (lambda (x y)
                (append (match x (($ bind _ e) (exp-cause e))) y))
              (exp-cause body)
              bindings))
      (($ letr bindings body)
       (foldr (lambda (x y)
                (append (match x (($ bind _ e) (exp-cause e))) y))
              (exp-cause body)
              bindings))
      (($ body defs exps) (sum (append defs exps)))
      (($ or exps) (sum exps))
      (($ delay e) (exp-cause e))
      (($ set! var body) (exp-cause body))
      (($ record bindings)
       (foldr (lambda (x y)
                (append (match x (($ bind _ e) (exp-cause e))) y))
              '()
              bindings))
      (($ type _ exp) (exp-cause exp)))))
(define display-type tidy)
(define type
  (lambda names
    (if (null? names)
      (for-each globaldef tree)
      (for-each
        (match-lambda
          ((? symbol? x)
           (match (lookup? global-env x)
                  (#f (use-error "~a is not defined" x))
                  (ty (pretty-print
                        `(,x : ,(display-type (name-ty ty)))))))
          ((? number? n)
           (let* ((ty (check-type (tree-unindex n)))
                  (type (display-type ty)))
             (pretty-print `(,n : ,type))))
          (_ (use-error
               "arguments must be identifiers or CHECK numbers")))
        names))))
(define localtype
  (lambda names
    (if (null? names)
      (for-each localdef tree)
      (for-each
        (lambda (x) (localdef (find-global x)))
        names))))
(define find-global
  (lambda (name)
    (let ((d (ormap (match-lambda
                      ((and d ($ define x _))
                       (and (eq? name (name-name x)) d))
                      (_ #f))
                    tree)))
      (unless d (use-error "~a is not defined" name))
      d)))
(define globaldef
  (lambda (e)
    (match e
           (($ define x _)
            (let ((type (display-type (name-ty x))))
              (pretty-print `(,(pname x) : ,type))))
           (_ #f))))
(define localdef
  (lambda (e) (pretty-print (expdef e))))
(define expdef
  (let* ((show (lambda (x)
                 `(,(pname x) : ,(display-type (name-ty x)))))
         (pbind (match-lambda
                  (($ bind x e) `(,(show x) ,(expdef e))))))
    (match-lambda
      (($ define x e)
       (if (or (not x) (and (name? x) (not (name-name x))))
         (expdef e)
         `(define ,(show x) ,(expdef e))))
      ((? defstruct? d) (pdef d))
      ((? datatype? d) (pdef d))
      (($ and exps) `(and ,@(maplr expdef exps)))
      (($ app fun args)
       `(,(expdef fun) ,@(maplr expdef args)))
      (($ begin exps) `(begin ,@(maplr expdef exps)))
      (($ const c _) (pconst c))
      (($ if test then els)
       `(if ,(expdef test) ,(expdef then) ,(expdef els)))
      (($ lam params body)
       `(lambda ,(map show params) ,@(expdef body)))
      (($ vlam params rest body)
       `(lambda ,(append (map show params) (show rest))
          ,@(expdef body)))
      (($ let bindings body)
       `(let ,(map pbind bindings) ,@(expdef body)))
      (($ let* bindings body)
       `(let* ,(map pbind bindings) ,@(expdef body)))
      (($ letr bindings body)
       `(letrec ,(map pbind bindings) ,@(expdef body)))
      (($ body defs exps)
       (let ((pdefs (maplr expdef defs)))
         (append pdefs (maplr expdef exps))))
      (($ record bindings)
       `(record ,@(maplr pbind bindings)))
      (($ field x e) `(field ,x ,(expdef e)))
      (($ cast (ty . _) e) `(: ,ty ,(expdef e)))
      (($ or exps) `(or ,@(maplr expdef exps)))
      (($ delay e) `(delay ,(expdef e)))
      (($ set! x body)
       `(set! ,(pname x) ,(expdef body)))
      (($ var x) (pname x))
      (($ match e1 clauses)
       (let* ((pclause
                (match-lambda
                  (($ mclause p exp fail)
                   (if fail
                     `(,(expdef p) (=> ,(pname fail)) ,@(expdef exp))
                     `(,(expdef p) ,@(expdef exp))))))
              (p1 (expdef e1)))
         `(match ,p1 ,@(maplr pclause clauses))))
      (($ pconst x _) (pconst x))
      (($ pvar x) (show x))
      (($ pany) '_)
      (($ pelse) 'else)
      (($ pnot pat) `(not ,(expdef pat)))
      (($ pand pats) `(and ,@(maplr expdef pats)))
      (($ ppred pred)
       (match (pname pred)
              ('false-object? #f)
              ('true-object? #t)
              ('null? '())
              (x `(? ,x))))
      (($ pobj tag args)
       (match (cons (pname tag) args)
              (('pair? x y) (cons (expdef x) (expdef y)))
              (('box? x) (box (expdef x)))
              (('vector? . x) (list->vector (maplr expdef x)))
              ((tg . _)
               `($ ,(strip-? tg) ,@(maplr expdef args)))))
      (($ type _ exp) (expdef exp))
      (($ check _ exp) (expdef exp)))))
(define check-type
  (match-lambda
    (($ type ty ($ check inf ($ var x))) ty)
    (($ type ty ($ check inf ($ lam x e1))) ty)
    (($ type ty ($ check inf ($ vlam x rest e1))) ty)
    (($ type _ ($ check inf ($ app e1 args)))
     (typeof e1))
    (($ type _ ($ check inf ($ field x e1)))
     (typeof e1))
    (($ type _ ($ check inf ($ cast (x . _) e1)))
     (typeof e1))
    (($ type _ ($ check inf ($ match e1 clauses)))
     (typeof e1))))
(define tree '())
(define global-env empty-env)
(define verbose #f)
(define times #t)
(define benchmarking #f)
(define cons-mutators '(set-car! set-cdr!))
(define st:check
  (lambda args
    (parameterize
      ((print-level #f)
       (print-length #f)
       (pretty-maximum-lines #f))
      (let ((output (apply do-soft args)))
        (when output
              (printf
                "Typed program written to file ~a~%"
                output))))))
(define st:run
  (lambda (file)
    (parameterize
      ((optimize-level 3))
      (when benchmarking
            (printf "Reloading slow CHECKs...~%")
            (load (string-append
                    installation-directory
                    "checklib.scm"))
            (set! benchmarking #f))
      (load file))))
(define st:bench
  (lambda (file)
    (parameterize
      ((optimize-level 3))
      (unless
        benchmarking
        (unless
          fastlibrary-file
          (use-error
            "No benchmarking mode in this version"))
        (printf "Reloading fast CHECKs...~%")
        (load (string-append
                installation-directory
                fastlibrary-file))
        (set! benchmarking #t))
      (load file))))
(define st:
  (lambda args
    (parameterize
      ((print-level #f)
       (print-length #f)
       (pretty-maximum-lines #f))
      (let ((output (apply do-soft args)))
        (cond ((not output)
               (use-error "Output file name required to run"))
              ((= 0 n-unbound)
               (printf
                 "Typed program written to file ~a, executing ...~%"
                 output)
               (flush-output)
               (st:run output))
              (else
               (printf
                 "Typed program written to file ~a, not executing (unbound refs)~%"
                 output)))))))
(define do-soft
  (match-lambda*
    ((input (? string? output))
     (when (strip-suffix output)
           (use-error
             "output file name cannot end in .ss or .scm"))
     (cond ((string? input)
            (soft-files (list input) output)
            output)
           ((and (list? input) (andmap string? input))
            (soft-files input output)
            output)
           (else (soft-def input output) output)))
    ((input #f)
     (cond ((string? input) (soft-files (list input) #f) #f)
           ((and (list? input) (andmap string? input))
            (soft-files input #f)
            #f)
           (else (soft-def input #f) #f)))
    ((input)
     (cond ((string? input)
            (let ((o (string-append
                       (or (strip-suffix input) input)
                       ".soft")))
              (soft-files (list input) o)
              o))
           ((and (list? input) (andmap string? input))
            (use-error "Output file name required"))
           (else (soft-def input #t) #f)))
    (else (use-error
            "Input must be a file name or list of file names"))))
(define rawmode #f)
(define st:control
  (lambda args
    (let ((dbg (match-lambda
                 ('raw
                  (set! display-type ptype)
                  (set! rawmode #t))
                 ('!raw
                  (set! display-type tidy)
                  (set! rawmode #f))
                 ('verbose (set! verbose #t))
                 ('!verbose (set! verbose #f))
                 ('times (set! times #t))
                 ('!times (set! times #f))
                 ('partial (set! fullsharing #f))
                 ('!partial (set! fullsharing #t))
                 ('pseudo (set! pseudo pseudo-subtype))
                 ('!pseudo (set! pseudo #f))
                 ('populated (set! populated #t))
                 ('!populated (set! populated #f))
                 ('matchst (set! matchst #t))
                 ('!matchst (set! matchst #f))
                 ('genmatch (set! genmatch #t))
                 ('!genmatch (set! genmatch #f))
                 ('letonce (set! letonce #t))
                 ('!letonce (set! letonce #f))
                 ('global-error (set! global-error #t))
                 ('!global-error (set! global-error #f))
                 ('share (set! share #t))
                 ('!share (set! share #f))
                 ('flags (set! flags #t))
                 ('!flags (set! flags #f))
                 ('depths (set! dump-depths #t))
                 ('!depths (set! dump-depths #f))
                 ('match (set! keep-match #t))
                 ('!match (set! keep-match #f))
                 (x (printf "Error: unknown debug switch ~a~%" x)
                    (st:control)))))
      (if (null? args)
        (begin
          (printf "Current values:")
          (for-each
            (lambda (x) (printf " ~a" x))
            (show-controls))
          (printf "~%"))
        (for-each dbg args)))))
(define show-controls
  (lambda ()
    (list (if rawmode 'raw '!raw)
          (if verbose 'verbose '!verbose)
          (if times 'times '!times)
          (if share 'share '!share)
          (if flags 'flags '!flags)
          (if dump-depths 'depths '!depths)
          (if fullsharing '!partial 'partial)
          (if pseudo 'pseudo '!pseudo)
          (if populated 'populated '!populated)
          (if letonce 'letonce '!letonce)
          (if matchst 'matchst '!matchst)
          (if genmatch 'genmatch '!genmatch)
          (if global-error 'global-error '!global-error)
          (if keep-match 'match '!match))))
(define soft-def
  (lambda (exp output)
    (reinit-macros!)
    (reinit-types!)
    (reinit-output!)
    (set! visible-time 0)
    (match-let*
      ((before-parse (cpu-time))
       (defs (parse-def exp))
       (before-bind (cpu-time))
       ((defs env tenv unbound)
        (bind-defs
          defs
          initial-env
          initial-type-env
          '()
          0))
       (_ (warn-unbound unbound))
       (_ (if cons-is-mutable
            (printf
              "Note: use of ~a, treating cons as MUTABLE~%"
              cons-mutators)
            (printf
              "Note: no use of ~a, treating cons as immutable~%"
              cons-mutators)))
       (before-improve (cpu-time))
       (defs (improve-defs defs))
       (before-typecheck (cpu-time))
       (_ (type-check defs))
       (_ (set! global-env env))
       (before-output (cpu-time))
       (_ (check output))
       (_ (print-summary ""))
       (before-end (cpu-time)))
      (when times
            (printf
              "~a seconds parsing,~%"
              (exact->inexact
                (* (- before-bind before-parse)
                   clock-granularity)))
            (printf
              "~a seconds binding,~%"
              (exact->inexact
                (* (- before-improve before-bind)
                   clock-granularity)))
            (printf
              "~a seconds improving,~%"
              (exact->inexact
                (* (- before-typecheck before-improve)
                   clock-granularity)))
            (printf
              "~a seconds type checking,~%"
              (exact->inexact
                (* (- (- before-output before-typecheck)
                      visible-time)
                   clock-granularity)))
            (printf
              "~a seconds setting visibility,~%"
              (exact->inexact
                (* visible-time clock-granularity)))
            (printf
              "~a seconds writing output,~%"
              (exact->inexact
                (* (- before-end before-output)
                   clock-granularity)))
            (printf
              "~a seconds in total.~%"
              (exact->inexact
                (* (- before-end before-parse) clock-granularity)))))))
(define type-check
  (lambda (defs)
    (set! tree defs)
    (type-defs defs)
    defs))
(define soft-files
  (lambda (files output)
    (let ((contents
            (map (lambda (f) `(begin ,@(readfile f))) files)))
      (soft-def `(begin ,@contents) output))))
(define strip-suffix
  (lambda (name)
    (let ((n (string-length name)))
      (or (and (<= 3 n)
               (equal? ".ss" (substring name (- n 3) n))
               (substring name 0 (- n 3)))
          (and (<= 4 n)
               (equal? ".scm" (substring name (- n 4) n))
               (substring name 0 (- n 4)))))))
(define st:deftype
  (match-lambda*
    (((? symbol? x) ? list? mutability)
     (=> fail)
     (if (andmap boolean? mutability)
       (deftype x mutability)
       (fail)))
    (args (use-error
            "Invalid command ~a"
            `(st:deftype ,@args)))))
(define st:defprim
  (match-lambda*
    (((? symbol? x) type) (defprim x type 'impure))
    (((? symbol? x) type (? symbol? mode))
     (defprim x type mode))
    (args (use-error
            "Invalid command ~a"
            `(st:defprim ,@args)))))
(define st:help
  (lambda ()
    (printf
      "Commands for Soft Scheme (~a)~%"
      st:version)
    (printf
      "  (st:         file (output))    type check file and execute~%")
    (printf
      "  (st:type     (name))           print types of global defs~%")
    (printf
      "  (st:check    file (output))    type check file~%")
    (printf
      "  (st:run      file)             execute type checked file~%")
    (printf
      "  (st:bench    file)             execute type checked file fast~%")
    (printf
      "  (st:ltype    (name))           print types of local defs~%")
    (printf
      "  (st:cause)                     print cause of CHECKs~%")
    (printf
      "  (st:summary)                   print summary of CHECKs~%")
    (printf
      "  (st:help)                      prints this message~%")
    (printf
      "  (st:defprim  name type (mode)) define a new primitive~%")
    (printf
      "  (st:deftype  name bool ...)    define a new type constructor~%")
    (printf
      "  (st:control  flag ...)         set internal flags~%")
    (printf
      "For more info, see ftp://ftp.nj.nec.com/pub/wright/ssmanual/softscheme.html~%")
    (printf
      "Copyright (c) 1993, 1994, 1995 by Andrew K. Wright under the~%")
    (printf
      "terms of the Gnu Public License. No warranties of any kind apply.~%")))
(define st:type type)
(define st:ltype localtype)
(define st:cause cause)
(define st:summary
  (lambda () (print-summary "")))
(define init!
  (lambda ()
    (when customization-file
          (load (string-append
                  installation-directory
                  customization-file)))
    (let ((softrc
            (string-append home-directory "/.softschemerc")))
      (when (file-exists? softrc) (load softrc)))
    (set! global-env initial-env)
    (st:help)))
(init!)
