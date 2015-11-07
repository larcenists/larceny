; Copyright (c) 1991, Marc Feeley.            -*- indent-tabs-mode: nil -*-
; 
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is hereby granted.
; 
; $Id$
;
; Pretty printer.  
; [From the Scheme Repository, August 5, 1995.  Since modified.]
;
; (pretty-print obj [output-port])    => unspecified
; (pretty-line-length [length])       => length
;
; Modified January 2015 by Will Clinger to honor print-length, print-level
; and to use R7RS syntax when that appears to be appropriate.
;
; FIXME:
;  - does not fully honor print-length, print-level
;
; '%generic-write' is a procedure that transforms a Scheme data value (or
; Scheme program expression) into its textual representation.  The interface
; to the procedure is sufficiently general to easily implement other useful
; formatting procedures such as pretty printing, output to a string and
; truncated output.
;
; Parameters:
;
;   OBJ       Scheme data value to transform.
;   DISPLAY?  Boolean, controls whether characters and strings are quoted.
;   UNICODE?  Boolean, tells whether the output procedure is capable of
;               printing any Unicode character.
;   WIDTH     Extended boolean, selects format:
;               #f = single line format
;               integer > 0 = pretty-print (value = max nb of chars per line)
;   OUTPUT    Procedure of 1 argument of string type, called repeatedly
;               with successive substrings of the textual representation.
;               This procedure can return #f to stop the transformation.
;   lvl0      If non-negative, maximum depth of printing.
;   len0      If non-negative, maximum length of printing lists/vectors.
;
; The value returned by '%generic-write' is undefined.
;
; Examples:
;
;   (write obj)   = (generic-write obj #f #f #f display-string)
;   (display obj) = (generic-write obj #t #f #f display-string)
;
; where display-string = (lambda (s) (for-each write-char (string->list s)) #t)

(define pretty-print)
(define pretty-line-length)

(let ()

  ;; If non-negative, lvl0 and len0 are the maximum depth and length
  ;; to use when printing structures.  If negative, there is no limit.

  (define (%generic-write obj display? unicode? width output lvl0 len0)

    ;; Which characters are written in hex and which are not
    ;; is completely implementation-dependent, so long as
    ;; get-datum can reconstruct the datum.
    ;;
    ;; Differences between this predicate and the rule for
    ;; hexifying the characters of an identifier:
    ;;     does not hexify Nd, Mc, or Me even at beginning of string
    ;;     does not hexify Ps, Pe, Pi, or Pf
    ;;     hexifies Co (private use)

    (define (print-in-string-without-hexifying? c)
      (let ((sv (char->integer c)))
        (or (<= 32 sv 126)
            (and (<= 128 sv)
                 (not (memq (char-general-category c)
                            '(Zs Zl Zp Cc Cf Cs Co Cn)))))))

    ;; Same as above but also hexifies Mn, Mc, and Me.

    (define (print-as-char-without-hexifying? c)
      (let ((sv (char->integer c)))
        (or (<= 32 sv 126)
            (and (<= 128 sv)
                 (not (memq (char-general-category c)
                            '(Mn Mc Me Zs Zl Zp Cc Cf Cs Co Cn)))))))

    ; (reverse-string-append l) = (apply string-append (reverse l))

    (define (reverse-string-append l)

      (define (rev-string-append l i)
        (if (pair? l)
            (let* ((str (car l))
                   (len (string-length str))
                   (result (rev-string-append (cdr l) (+ i len))))
              (let loop ((j 0) (k (- (- (string-length result) i) len)))
                (if (< j len)
                    (begin
                      (string-set! result k (string-ref str j))
                      (loop (+ j 1) (+ k 1)))
                    result)))
            (make-string i)))

      (rev-string-append l 0))

    ; FIXME: the R6RS generalizes this syntax.

    (define (read-macro? l)
      (define (length1? l) (and (pair? l) (null? (cdr l))))
      (let ((head (car l)) (tail (cdr l)))
        (case head
          ((quote quasiquote unquote unquote-splicing
            syntax quasisyntax unsyntax unsyntax-splicing)
           (length1? tail))
          (else                                        #f))))

    (define (read-macro-body l)
      (cadr l))

    (define (read-macro-prefix l)
      (let ((head (car l)) (tail (cdr l)))
        (case head
          ((quote)             "'")
          ((quasiquote)        "`")
          ((unquote)           ",")
          ((unquote-splicing)  ",@")
          ((syntax)            "#'")
          ((quasisyntax)       "#`")
          ((unsyntax)          "#,")
          ((unsyntax-splicing) "#,@"))))

    (define (out str col)
      (and col (output str) (+ col (string-length str))))

    ;; Writes everything on a single line.

    (define (wr obj col lvl)

      (define (wr-expr expr col lvl)
        (if (read-macro? expr)
            (wr (read-macro-body expr)
                (out (read-macro-prefix expr) col)
                lvl)
            (wr-lst expr col lvl)))

      ;; FIXME: still ignores print-level and print-length

      (define (wr-lst l col lvl)
        (if (pair? l)
            (let loop ((l (cdr l)) (col (wr (car l) (out "(" col) lvl)))
              (and col
                   (cond ((pair? l)
                          (loop (cdr l)
                                (wr (car l) (out " " col) lvl)))
                         ((null? l) (out ")" col))
                         (else      (out ")"
                                         (wr l (out " . " col) lvl))))))
            (out "()" col)))

      ;; Let the standard display and write procedures do the work.

      (define (wr-atom obj col)
        (if display?
            (out obj col)
            (let ((q (open-output-string)))
              (write obj q)
              (out (get-output-string q) col))))

      (cond ((pair? obj) 
             (wr-expr obj col lvl))
            ((null? obj)
             (wr-lst obj col lvl))
            ((vector? obj)
             (wr-lst (vector->list obj) (out "#" col) lvl))
            ((or (boolean? obj)
                 (number? obj)
                 (symbol? obj)
                 (string? obj)
                 (char? obj)
                 (procedure? obj)
                 (input-port? obj)
                 (output-port? obj)
                 (port? obj)
                 (eof-object? obj)
                 (eq? obj (unspecified))
                 (eq? obj (undefined))
                 (environment? obj))
             (wr-atom obj col))
            ((structure? obj)                                     ; FIXME
             (let ((temp (open-output-string)))
               ((structure-printer) obj temp #t)
               (out (get-output-string temp) col)))
            ((bytevector? obj)
             (let* ((bvec-start
                     (cond ((read-r7rs-weirdness?) "#u8")
                           ((read-r6rs-weirdness?) "#vu8")
                           (else "#u8")))
                    (col (out bvec-start col)))
               (wr-lst (bytevector->list obj) col lvl)))
            (else                                                 ; FIXME
             (out "#<WEIRD>" col))))

    ;; Pretty-prints using as many lines as needed.

    (define (pp obj col lvl)

      (define (spaces n col)
        (if (> n 0)
            (if (> n 7)
                (spaces (- n 8) (out "        " col))
                (out (substring "        " 0 n) col))
            col))

      (define (indent to col)
        (and col
             (if (< to col)
                 (and (out (make-string 1 #\newline) col) (spaces to 0))
                 (spaces (- to col) col))))

      (define (pr obj col extra pp-pair lvl)
        (if (or (pair? obj) (vector? obj)) ; may have to split on mult. lines
            (let ((result '())
                  (left (min (+ (- (- width col) extra) 1) max-expr-width)))
              (%generic-write obj display? unicode? #f
                              (lambda (str)
                                (set! result (cons str result))
                                (set! left (- left (string-length str)))
                                (> left 0))
                              lvl len0)
              (if (> left 0)            ; all can be printed on one line
                  (out (reverse-string-append result) col)
                  (if (pair? obj)
                      (pp-pair obj col extra lvl)
                      (pp-list (vector->list obj) 
                               (out "#" col) 
                               extra 
                               pp-expr lvl))))
            (wr obj col lvl)))

      (define (pp-expr expr col extra lvl)
        (if (read-macro? expr)
            (pr (read-macro-body expr)
                (out (read-macro-prefix expr) col)
                extra
                pp-expr lvl)
            (let ((head (car expr)))
              (if (symbol? head)
                  (let ((proc (style head)))
                    (if proc
                        (proc expr col extra lvl)
                        (if (> (string-length (symbol->string head))
                               max-call-head-width)
                            (pp-general expr col extra #f #f #f pp-expr lvl)
                            (pp-call expr col extra pp-expr lvl))))
                  (pp-list expr col extra pp-expr lvl)))))

      ; (head item1
      ;       item2
      ;       item3)
      (define (pp-call expr col extra pp-item lvl)
        (let ((col* (wr (car expr) (out "(" col) lvl)))
          (and col
               (pp-down (cdr expr) col* (+ col* 1) extra pp-item lvl len0))))

      ; (item1
      ;  item2
      ;  item3)
      (define (pp-list l col extra pp-item lvl)
        (if (= 0 lvl)
            (out "..." col)
            (let ((col (out "(" col)))
              (pp-down l col col extra pp-item
                       (- lvl 1)
                       len0))))

      (define (pp-down l col1 col2 extra pp-item lvl len)
        (let loop ((l l) (col col1) (len len))
          (and col
               (cond ((null? l)
                      (out ")" col))
                     ((not (pair? l))
                      (out ")"
                           (pr l
                               (indent col2 (out "." (indent col2 col)))
                               (+ extra 1)
                               pp-item lvl)))
                     ((= 0 len)
                      (loop '() (out " ..." col) (- len 1)))
                     (else
                      (let ((rest (cdr l)))
                        (let ((extra (if (null? rest) (+ extra 1) 0)))
                          (loop rest
                                (pr (car l) 
                                    (indent col2 col)
                                    extra
                                    pp-item lvl)
                                (- len 1)))))))))

      (define (pp-general expr col extra named? pp-1 pp-2 pp-3 lvl)

        (define (tail1 rest col1 col2 col3 lvl)
          (if (and pp-1 (pair? rest))
              (let* ((val1 (car rest))
                     (rest (cdr rest))
                     (extra (if (null? rest) (+ extra 1) 0)))
                (tail2 rest col1
                            (pr val1 (indent col3 col2) extra pp-1 lvl)
                            col3 lvl))
              (tail2 rest col1 col2 col3 lvl)))

        (define (tail2 rest col1 col2 col3 lvl)
          (if (and pp-2 (pair? rest))
              (let* ((val1 (car rest))
                     (rest (cdr rest))
                     (extra (if (null? rest) (+ extra 1) 0)))
                (tail3 rest col1
                            (pr val1 (indent col3 col2) extra pp-2 lvl)
                            lvl))
              (tail3 rest col1 col2 lvl)))

        (define (tail3 rest col1 col2 lvl)
          (pp-down rest col2 col1 extra pp-3 lvl len0))

        (let* ((head (car expr))
               (rest (cdr expr))
               (col* (wr head (out "(" col) lvl)))
          (if (and named? (pair? rest))
              (let* ((name (car rest))
                     (rest (cdr rest))
                     (col** (wr name (out " " col*) lvl)))
                (tail1 rest (+ col indent-general) col** (+ col** 1) lvl))
              (tail1 rest (+ col indent-general) col* (+ col* 1) lvl))))

      (define (pp-expr-list l col extra lvl)
        (pp-list l col extra pp-expr lvl))

      (define (pp-LAMBDA expr col extra lvl)
        (pp-general expr col extra #f pp-expr-list #f pp-expr lvl))

      (define (pp-IF expr col extra lvl)
        (pp-general expr col extra #f pp-expr #f pp-expr lvl))

      (define (pp-COND expr col extra lvl)
        (pp-call expr col extra pp-expr-list lvl))

      (define (pp-CASE expr col extra lvl)
        (pp-general expr col extra #f pp-expr #f pp-expr-list lvl))

      (define (pp-AND expr col extra lvl)
        (pp-call expr col extra pp-expr lvl))

      (define (pp-LET expr col extra lvl)
        (let* ((rest (cdr expr))
               (named? (and (pair? rest) (symbol? (car rest)))))
          (pp-general expr col extra named? pp-expr-list #f pp-expr lvl)))

      (define (pp-BEGIN expr col extra lvl)
        (pp-general expr col extra #f #f #f pp-expr lvl))

      (define (pp-DO expr col extra lvl)
        (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr lvl))

      ; define formatting style (change these to suit your style)

      (define indent-general 2)

      (define max-call-head-width 5)

      (define max-expr-width 50)

      (define (style head)
        (case head
          ((lambda let* letrec define) pp-LAMBDA)
          ((set!)                      pp-IF)     ; Used to handle IF, too.
          ((cond)                      pp-COND)
          ((case)                      pp-CASE)
          ((and or)                    pp-AND)
          ((let)                       pp-LET)
          ((begin)                     pp-BEGIN)
          ((do)                        pp-DO)
          (else                        #f)))

      (pr obj col 0 pp-expr lvl0))

    (if width
        (out (make-string 1 #\newline) (pp obj 0 lvl0))
        (wr obj 0 lvl0)))

  ; Don't let the pretty printer go into an infinite loop.

  (define circular:limit 200)
  (define circular:notation
    ";;; circularity detected by pretty printer; giving up\n#<WEIRD>")

  (define (pretty obj . opt)
    (let* ((port (if (pair? opt) (car opt) (current-output-port)))
           (circular? (object-is-circular? obj))
           (char-count 0)
           (lvl (or (print-level) -1))
           (len (or (print-length) -1)))
      (call-with-current-continuation
       (lambda (quit)

         (define (abandon-circular-output)
           (newline port)
           (display-simple circular:notation port)
           (newline port)
          ;(quit (unspecified))
           #f)

         (%generic-write obj
                         #f
                         (memq (transcoder-codec (port-transcoder port))
                               '(utf-8 utf-16))
                         (line-length)
                         (lambda (s)
                           (display-simple s port)
                           (set! char-count
                                 (+ char-count (string-length s)))
                           (if (and circular?
                                    (> char-count circular:limit))
                               (abandon-circular-output)
                               #t))
                         lvl
                         len)))
      (unspecified)))

  (define line-length 
    (let ((length 79))
      (lambda args
        (cond ((null? args)
               length)
              ((and (null? (cdr args))
                    (integer? (car args))
                    (exact? (car args))
                    (> (car args) 0))
               (set! length (car args)))
              (else
               (error "pretty-line-length: invalid: " args))))))
  
  (set! pretty-print pretty)
  (set! pretty-line-length line-length)
  'pretty-print)

; eof
