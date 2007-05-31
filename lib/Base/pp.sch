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
; FIXME:
;  - does not honor print-length, print-level
;  - does not support all the control characters supported by the reader 
;    (return, linefeed, page, backspace).
;  - does not support structures.
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
;   WIDTH     Extended boolean, selects format:
;               #f = single line format
;               integer > 0 = pretty-print (value = max nb of chars per line)
;   OUTPUT    Procedure of 1 argument of string type, called repeatedly
;               with successive substrings of the textual representation.
;               This procedure can return #f to stop the transformation.
;
; The value returned by '%generic-write' is undefined.
;
; Examples:
;
;   (write obj)   = (generic-write obj #f #f display-string)
;   (display obj) = (generic-write obj #t #f display-string)
;
; where display-string = (lambda (s) (for-each write-char (string->list s)) #t)

(define pretty-print)
(define pretty-line-length)

(let ()

  (define (%generic-write obj display? width output)

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

    (define (read-macro? l)
      (define (length1? l) (and (pair? l) (null? (cdr l))))
      (let ((head (car l)) (tail (cdr l)))
        (case head
          ((QUOTE QUASIQUOTE UNQUOTE UNQUOTE-SPLICING) (length1? tail))
          (else                                        #f))))

    (define (read-macro-body l)
      (cadr l))

    (define (read-macro-prefix l)
      (let ((head (car l)) (tail (cdr l)))
        (case head
          ((QUOTE)            "'")
          ((QUASIQUOTE)       "`")
          ((UNQUOTE)          ",")
          ((UNQUOTE-SPLICING) ",@"))))

    (define (out str col)
      (and col (output str) (+ col (string-length str))))

    (define (wr obj col)

      (define (wr-expr expr col)
        (if (read-macro? expr)
            (wr (read-macro-body expr) (out (read-macro-prefix expr) col))
            (wr-lst expr col)))

      (define (wr-lst l col)
        (if (pair? l)
            (let loop ((l (cdr l)) (col (wr (car l) (out "(" col))))
              (and col
                   (cond ((pair? l) (loop (cdr l) (wr (car l) (out " " col))))
                         ((null? l) (out ")" col))
                         (else      (out ")" (wr l (out " . " col)))))))
            (out "()" col)))

      (cond ((pair? obj) 
             (wr-expr obj col))
            ((null? obj)
             (wr-lst obj col))
            ((environment? obj)
             (out ">"
                  (out (environment-name obj)
                       (out "#<ENVIRONMENT " col))))
            ((vector? obj)
             (wr-lst (vector->list obj) (out "#" col)))
            ((boolean? obj)
             (out (if obj "#t" "#f") col))
            ((number? obj)
             (out (number->string obj) col))
            ((symbol? obj)
             (out (symbol->string obj) col))
            ((procedure? obj)
             (let ((n (procedure-name obj)))
               (if n
                   (out ">"
                        (out (symbol->string n)
                             (out "#<PROCEDURE " col)))
                   (out "#<PROCEDURE>" col))))
            ((string? obj)      
             (if display?
                 (out obj col)
                 (let loop ((i 0) (j 0) (col (out "\"" col)))
                   (if (and col (< j (string-length obj)))
                       (let* ((c (string-ref obj j))
                              (k (char->integer c)))
                         (cond ((or (char=? c #\\)
                                    (char=? c #\"))
                                (loop j
                                      (+ j 1)
                                      (out "\\"
                                           (out (substring obj i j)
                                                col))))
                               ((< k 32)
                                (let ((col (out (substring obj i j) col))
                                      (j+1 (+ j 1)))
                                  (case k
                                   ((7) (loop j+1 j+1 (out "\\a" col)))
                                   ((8) (loop j+1 j+1 (out "\\b" col)))
                                   ((9) (loop j+1 j+1 (out "\\t" col)))
                                   ((10) (loop j+1 j+1 (out "\\n" col)))
                                   ((11) (loop j+1 j+1 (out "\\v" col)))
                                   ((12) (loop j+1 j+1 (out "\\f" col)))
                                   ((13) (loop j+1 j+1 (out "\\r" col)))
                                   (else
                                    (let ((s (number->string k 16)))
                                      (loop j+1
                                            j+1
                                            (out ";"
                                                 (out s
                                                      (out "\\x" col)))))))))
                               ((< k 127)
                                (loop i (+ j 1) col))
                               (else
                                (let ((col (out (substring obj i j) col))
                                      (j+1 (+ j 1))
                                      (s (number->string k 16)))
                                  (loop j+1
                                        j+1
                                        (out ";"
                                             (out s
                                                  (out "\\x" col))))))))
                       (out "\""
                            (out (substring obj i j) col))))))
            ((char? obj) 
             (if display?
                 (out (make-string 1 obj) col)
                 (out (let ((k (char->integer obj)))
                        (cond ((<= k 32)
                               (if (char=? obj #\newline)
                                   "newline"
                                   (case k
                                     ((0) "nul")
                                     ((7) "alarm")
                                     ((8) "backspace")
                                     ((9) "tab")
                                     ((10) "linefeed")
                                     ((11) "vtab")
                                     ((12) "page")
                                     ((13) "return")
                                     ((27) "esc")
                                     ((32) "space")
                                     (else
                                      (string-append
                                       "x" (number->string k 16))))))
                              ((< k 127)
                               (make-string 1 obj))
                              ((= k 127) "delete")
                              (else
                               (string-append "x" (number->string k 16)))))
                      (out "#\\" col))))
            ((input-port? obj)
             (out ">"
                  (out (port-name obj)
                       (out "#<INPUT PORT " col))))
            ((output-port? obj)
             (out ">"
                  (out (port-name obj)
                       (out "#<OUTPUT PORT " col))))
            ((port? obj)
             (out "#<PORT>" col))
            ((eof-object? obj)
             (out "#<EOF>" col))
            ((eq? obj (unspecified))
             (out "#!unspecified" col))
            ((eq? obj (undefined))
             (out "#!undefined" col))
            ((structure? obj)
             (let ((temp (open-output-string)))
               ((structure-printer) obj temp #t)
               (out (get-output-string temp) col)))
            ((bytevector? obj)
             (let ((col (out "#vu8" col)))
               (wr-lst (bytevector->list obj) col)))
            (else
             (out "#<WEIRD>" col))))

    (define (pp obj col)

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

      (define (pr obj col extra pp-pair)
        (if (or (pair? obj) (vector? obj)) ; may have to split on mult. lines
            (let ((result '())
                  (left (min (+ (- (- width col) extra) 1) max-expr-width)))
              (%generic-write obj display? #f
                              (lambda (str)
                                (set! result (cons str result))
                                (set! left (- left (string-length str)))
                                (> left 0)))
              (if (> left 0)            ; all can be printed on one line
                  (out (reverse-string-append result) col)
                  (if (pair? obj)
                      (pp-pair obj col extra)
                      (pp-list (vector->list obj) 
                               (out "#" col) 
                               extra 
                               pp-expr))))
            (wr obj col)))

      (define (pp-expr expr col extra)
        (if (read-macro? expr)
            (pr (read-macro-body expr)
                (out (read-macro-prefix expr) col)
                extra
                pp-expr)
            (let ((head (car expr)))
              (if (symbol? head)
                  (let ((proc (style head)))
                    (if proc
                        (proc expr col extra)
                        (if (> (string-length (symbol->string head))
                               max-call-head-width)
                            (pp-general expr col extra #f #f #f pp-expr)
                            (pp-call expr col extra pp-expr))))
                  (pp-list expr col extra pp-expr)))))

      ; (head item1
      ;       item2
      ;       item3)
      (define (pp-call expr col extra pp-item)
        (let ((col* (wr (car expr) (out "(" col))))
          (and col
               (pp-down (cdr expr) col* (+ col* 1) extra pp-item))))

      ; (item1
      ;  item2
      ;  item3)
      (define (pp-list l col extra pp-item)
        (let ((col (out "(" col)))
          (pp-down l col col extra pp-item)))

      (define (pp-down l col1 col2 extra pp-item)
        (let loop ((l l) (col col1))
          (and col
               (cond ((pair? l)
                      (let ((rest (cdr l)))
                        (let ((extra (if (null? rest) (+ extra 1) 0)))
                          (loop rest
                                (pr (car l) 
                                    (indent col2 col)
                                    extra
                                    pp-item)))))
                     ((null? l)
                      (out ")" col))
                     (else
                      (out ")"
                           (pr l
                               (indent col2 (out "." (indent col2 col)))
                               (+ extra 1)
                               pp-item)))))))

      (define (pp-general expr col extra named? pp-1 pp-2 pp-3)

        (define (tail1 rest col1 col2 col3)
          (if (and pp-1 (pair? rest))
              (let* ((val1 (car rest))
                     (rest (cdr rest))
                     (extra (if (null? rest) (+ extra 1) 0)))
                (tail2 rest col1 (pr val1 (indent col3 col2) extra pp-1) col3))
              (tail2 rest col1 col2 col3)))

        (define (tail2 rest col1 col2 col3)
          (if (and pp-2 (pair? rest))
              (let* ((val1 (car rest))
                     (rest (cdr rest))
                     (extra (if (null? rest) (+ extra 1) 0)))
                (tail3 rest col1 (pr val1 (indent col3 col2) extra pp-2)))
              (tail3 rest col1 col2)))

        (define (tail3 rest col1 col2)
          (pp-down rest col2 col1 extra pp-3))

        (let* ((head (car expr))
               (rest (cdr expr))
               (col* (wr head (out "(" col))))
          (if (and named? (pair? rest))
              (let* ((name (car rest))
                     (rest (cdr rest))
                     (col** (wr name (out " " col*))))
                (tail1 rest (+ col indent-general) col** (+ col** 1)))
              (tail1 rest (+ col indent-general) col* (+ col* 1)))))

      (define (pp-expr-list l col extra)
        (pp-list l col extra pp-expr))

      (define (pp-LAMBDA expr col extra)
        (pp-general expr col extra #f pp-expr-list #f pp-expr))

      (define (pp-IF expr col extra)
        (pp-general expr col extra #f pp-expr #f pp-expr))

      (define (pp-COND expr col extra)
        (pp-call expr col extra pp-expr-list))

      (define (pp-CASE expr col extra)
        (pp-general expr col extra #f pp-expr #f pp-expr-list))

      (define (pp-AND expr col extra)
        (pp-call expr col extra pp-expr))

      (define (pp-LET expr col extra)
        (let* ((rest (cdr expr))
               (named? (and (pair? rest) (symbol? (car rest)))))
          (pp-general expr col extra named? pp-expr-list #f pp-expr)))

      (define (pp-BEGIN expr col extra)
        (pp-general expr col extra #f #f #f pp-expr))

      (define (pp-DO expr col extra)
        (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr))

      ; define formatting style (change these to suit your style)

      (define indent-general 2)

      (define max-call-head-width 5)

      (define max-expr-width 50)

      (define (style head)
        (case head
          ((LAMBDA LET* LETREC DEFINE) pp-LAMBDA)
          ((SET!)                      pp-IF)     ; Used to handle IF, too.
          ((COND)                      pp-COND)
          ((CASE)                      pp-CASE)
          ((AND OR)                    pp-AND)
          ((LET)                       pp-LET)
          ((BEGIN)                     pp-BEGIN)
          ((DO)                        pp-DO)
          (else                        #f)))

      (pr obj col 0 pp-expr))

    (if width
        (out (make-string 1 #\newline) (pp obj 0))
        (wr obj 0)))

  (define (pretty obj . opt)
    (let ((port (if (pair? opt) (car opt) (current-output-port))))
      (%generic-write obj
                      #f
                      (line-length)
                      (lambda (s)
                        (display s port)
                        #t))
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
