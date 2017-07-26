; Copyright 1991 Lightship Software, Incorporated.
;
; $Id$
;
; Larceny -- Print procedures.

($$trace "print")

;;; Parameterized hooks to customize the printer.
(define code-object-printer
  (make-parameter
   "code-object-printer"
   (lambda (co port slashify)
     (print (string-append "#<"
                           (car (vector-ref co 0))
                           ">")
            port
            #f))
   procedure?))

(define environment-printer
  (make-parameter
   "environment-printer"
   (lambda (environment port slashify)
     (print (string-append "#<ENVIRONMENT "
                           (environment-name environment)
                           ">")
            port
            #f))
   procedure?))

(define hashtable-printer
  (make-parameter
   "hashtable-printer"
   (lambda (hashtable port slashify)
     (print "#<HASHTABLE>" port #f))
   procedure?))

(define procedure-printer
  (make-parameter
   "procedure-printer"
   (lambda (procedure port slashify)
     (print (string-append "#<PROCEDURE"
                           (let ((doc (procedure-name procedure)))
                             (if doc
                                 (string-append " " (symbol->string doc))
                                 ""))
                           ">")
            port
            #f))
   procedure?))

(define weird-printer
  (make-parameter
   "weird-printer"
   (lambda (weirdo port slashify)
     (print "#<WEIRD OBJECT>" port #f))
   procedure?))

; If slashify is true, print something that can be read back in.
; If slashify is false, use display semantics.

(define (print x p slashify)

  (define write-char io/write-char)

  (define quoters '(quote quasiquote unquote unquote-splicing
                    syntax quasisyntax unsyntax unsyntax-splicing))

  (define quoter-strings '((quote . "'")
                           (quasiquote . "`")
                           (unquote . ",")
                           (unquote-splicing . ",@")
                           (syntax . "#'")
                           (quasisyntax . "#`")
                           (unsyntax . "#,")
                           (unsyntax-splicing . "#,@")))

 ;FIXME: R6RS won't allow a backslash before semicolon
 ;(define funny-characters (list #\" #\\ #\;))

  (define funny-characters (list #\" #\\))

  (define ctrl-B (integer->char 2))
  (define ctrl-C (integer->char 3))
  (define ctrl-F (integer->char 6))

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
               (not (= sv #x00ab))    ; left double angle quote
               (not (= sv #x00bb))    ; right double angle quote
               (not (memq (char-general-category c)
                          '(Zs Zl Zp Cc Cf Cs Co Cn)))))))

  ;; Same as above but also hexifies Mn, Mc, and Me.

  (define (print-as-char-without-hexifying? c)
    (let ((sv (char->integer c)))
      (or (<= 32 sv 126)
          (and (<= 128 sv)
               (not (memq (char-general-category c)
                          '(Mn Mc Me Zs Zl Zp Cc Cf Cs Co Cn)))))))

  ;; Don't print ellipsis when slashifying (that is, when
  ;; using WRITE rather than DISPLAY) because result is
  ;; being printed with intent to read it back in.

  (define (print x p slashify level)
    (cond ((and (not slashify)
                (zero? level))
           (printstr "..." p))
          ((not (pair? x)) (patom x p slashify level))
          ((and (memq (car x) quoters)
                (pair? (cdr x))
                (null? (cddr x)))
           (print-quoted x p slashify level))
          ((and (not slashify)
                (zero? (- level 1)))
           (printstr "(...)" p))
          ((and (not slashify)
                (eqv? 0 (print-length)))
           (printstr "(...)" p))
          (else
           (write-char (string-ref "(" 0) p)
           (print (car x) p slashify (- level 1))
           (print-cdr (cdr x) p slashify
                      (- level 1)
                      (- (or (print-length) 0) 1)))))

  (define (print-cdr x p slashify level length)
    (cond ((null? x)
           (write-char (string-ref ")" 0) p))
          ((and (not slashify)
                (zero? length))
           (printstr " ...)" p))
          ((pair? x)
           (write-char #\space p)
           (print (car x) p slashify level)
           (print-cdr (cdr x) p slashify level (- length 1)))
          (else
           (printstr " . " p)
           (patom x p slashify level)
           (write-char (string-ref ")" 0) p))))

  (define (printsym s p) (printstr s p))

  (define (printstr s p)

    (define (loop x p i n)
      (if (< i n)
          (begin (write-char (string-ref x i) p)
                 (loop x p (+ 1 i) n))))

    (loop s p 0 (string-length s)))

  (define (print-slashed-symbol x p)
    (let* ((s (symbol->string x))
           (n (string-length s)))
      (cond ((vanilla-symbol? x s)
             (printstr s p))
            ((io/port-allows-r7rs-weirdness? p)
             (write-char #\| p)
             (print-slashed-symbol-string s p #t)
             (write-char #\| p))
            (else
             (print-slashed-symbol-string s p #f)))))

  ;; A symbol is vanilla if it's safe to print by displaying its string.

  (define (vanilla-symbol? x s)

    (let ((n (string-length s)))

      (define (loop i)
        (if (= i n)
            #t
            (let ((c (string-ref s i)))
              (cond ((or (char<=? #\a c #\z)
                         (char<=? #\A c #\Z)
                         (case c
                          ((#\! #\$ #\% #\& #\* #\/ #\: 
                            #\< #\= #\> #\? #\^ #\_ #\~)
                           ; special initial
                           #t)
                          ((#\0 #\1 #\2 #\3 #\4
                            #\5 #\6 #\7 #\8 #\9)
                           ; special subsequent
                           (< 0 i))
                          ((#\@)
                           (or (< 0 i)
                               (io/port-allows-r7rs-weirdness? p)))
                          ((#\.)
                           ; check for peculiar identifiers
                           (or (< 0 i)
                               (eq? x '...)
                               (and (io/port-allows-r7rs-weirdness? p)
                                    (< (+ i 1) n)
                                    (let ((c (string-ref s (+ i 1))))
                                      (case c
                                       ((#\. #\+ #\- #\@)
                                        #t)
                                       ((#\0 #\1 #\2 #\3 #\4
                                         #\5 #\6 #\7 #\8 #\9)
                                        #f)
                                       (else #t))))))
                          ((#\+ #\-)
                           ; check for peculiar identifiers
                           (or (< 0 i)
                               (eq? x '+)
                               (eq? x '-)
                               (and (io/port-allows-r6rs-weirdness? p)
                                    (char=? c #\-)
                                    (< (+ i 1) n)
                                    (char=? (string-ref s (+ i 1)) #\>))))
                          (else
                           (if (memq (transcoder-codec (port-transcoder p))
                                     '(utf-8 utf-16))
                               (let ((cat (char-general-category c)))
                                 (or (and (< 127 (char->integer c))
                                          (memq cat
                                                '(Lu Ll Lt Lm Lo Mn Nl No
                                                  Pd Pc Po Sc Sm Sk So Co)))
                                     (and (< 0 i)
                                          (memq cat '(Nd Mc Me)))))
                               #f))))
                     (loop (+ i 1)))
                    (else #f)))))

      (and (> n 0) (loop 0))))

  ;; Prints the string as though it were enclosed within vertical bars.
  ;; If r7rs? is true, rely on R7RS lexical syntax for strings and symbols.
  ;; Otherwise use inline hex escapes for all odd characters.

  (define (print-slashed-symbol-string s p r7rs?)

    (let* ((n (string-length s)))

      (define (loop i)
        (if (< i n)
            (let ((c (string-ref s i)))
              (cond ((or (char<=? #\a c #\z)
                         (char<=? #\A c #\Z)
                         (case c
                          ((#\! #\$ #\% #\& #\* #\/ #\: 
                            #\< #\= #\> #\? #\^ #\_ #\~)
                           ; special initial
                           #t)
                          ((#\0 #\1 #\2 #\3 #\4
                            #\5 #\6 #\7 #\8 #\9
                            #\+ #\- #\. #\@)
                           ; special subsequent
                           (or r7rs? (< 0 i)))
                          (else
                           (if (memq (transcoder-codec (port-transcoder p))
                                     '(utf-8 utf-16))
                               (let ((cat (char-general-category c)))
                                 (or (and (< 127 (char->integer c))
                                          (memq cat
                                                '(Lu Ll Lt Lm Lo Mn Nl No
                                                  Pd Pc Po Sc Sm Sk So Co)))
                                     (and (or r7rs? (< 0 i))
                                          (memq cat '(Nd Mc Me)))))
                               #f))))
                     (write-char c p)
                     (loop (+ i 1)))
                    (r7rs?
                     (case c
                      ((#\\ #\|)
                       (write-char #\\ p)
                       (write-char c p))
                      ((#\alarm #\backspace #\tab #\newline #\return)
                       (write-char #\\ p)
                       (write-char (cdr (assq c mnemonic-escape-table)) p))
                      (else
                       (if (char<=? #\space c #\~)
                           (write-char c p)
                           (print-inline-hex-escape c))))
                     (loop (+ i 1)))
                    (else
                     (print-inline-hex-escape c)
                     (loop (+ i 1)))))))

      (define (print-inline-hex-escape c)
        (let ((hexstring (number->string (char->integer c) 16)))
          (write-char #\\ p)
          (write-char #\x p)
          (print-slashed-string hexstring p)
          (write-char #\; p)))

      (define mnemonic-escape-table
        '((#\alarm . #\a)
          (#\backspace . #\b)
          (#\tab . #\t)
          (#\newline . #\n)
          (#\return . #\r)))

      (loop 0)))

  (define (print-slashed-string s p)

    (define (loop i n)
      (if (< i n)
          (let* ((c (string-ref s i))
                 (sv (char->integer c)))
            (cond ((<= 32 sv 126)
                   (if (or (char=? c #\\)
                           (char=? c #\"))
                       (write-char #\\ p))
                   (write-char c p))
                  ((and (<= 128 sv)
                        (memq (transcoder-codec (port-transcoder p))
                              '(utf-8 utf-16))
                        (print-in-string-without-hexifying? c))
                   (write-char c p))
                  (else
                   (write-char #\\ p)
                   (case sv
                    ((7) (write-char #\a p))
                    ((8) (write-char #\b p))
                    ((9) (write-char #\t p))
                    ((10) (write-char #\n p))
                    ;((11) (write-char #\v p))     ; not legal in R7RS
                    ;((12) (write-char #\f p))     ; not legal in R7RS
                    ((13) (write-char #\r p))
                    (else
                     (let ((hexstring (number->string sv 16)))
                       (write-char #\x p)
                       (print-slashed-string hexstring p)
                       (write-char #\; p))))))
            (loop (+ i 1) n))))

    (loop 0 (string-length s)))

  (define (print-slashed-bytevector s p)

    (define (loop x p i n)
      (if (< i n)
          (let ((c (integer->char (bytevector-ref x i))))
            (if (memq c funny-characters)
                (write-char #\\ p))
            (write-char c p)
            (loop x p (+ 1 i) n))))

    (loop s p 0 (bytevector-length s)))

  (define (patom x p slashify level)
    (cond ((eq? x '())              (printstr "()" p))
          ((not x)                  (printstr "#f" p))
          ((eq? x #t)               (printstr "#t" p))
          ((symbol? x)
           (if slashify
               (print-slashed-symbol x p)
               (printsym (symbol->string x) p)))
          ((number? x)              (printnumber x p slashify))
          ((char? x)
           (if slashify
               (printcharacter x p)
               (write-char x p)))
          ((string? x)
           (if slashify
               (begin (write-char #\" p)
                      (print-slashed-string x p)
                      (write-char #\" p))
               (printstr x p)))
          ((text? x)
           (let ((x (textual->string x)))
             (if slashify
                 (begin (write-char (integer->char #x00ab) p)
                        (print-slashed-string x p)
                        (write-char (integer->char #x00bb) p))
                 (printstr x p))))

          ;; FIXME: The environment?, code-object?, and hashtable? clauses
          ;; look silly and probably are.  The code-object? part appears
          ;; to have come from a pre-2006 version of Twobit (see
          ;; src/Compiler/pass2.aux.sch).

          ;; The special case for 2-element vectors prevents #(quote foo)
          ;; and #(syntax foo) from printing as #'foo and ##'foo.

          ((vector? x)
           (cond ((environment? x) (printenvironment x p slashify))
                 ((code-object? x) (printcodeobject x p slashify))
                 ((hashtable? x)   (printhashtable x p slashify))
                 ((= 2 (vector-length x))
                  (write-char #\# p)
                  (write-char #\( p)
                  (print (vector-ref x 0) p slashify level)
                  (write-char #\space p)
                  (print (vector-ref x 1) p slashify level)
                  (write-char #\) p))
                 (else (write-char #\# p)
                       (print (vector->list x) p slashify level))))

          ((procedure? x)           (printprocedure x p slashify))
          ((bytevector? x)          (printbytevector x p slashify level))
          ((eof-object? x)          (printeof x p slashify))
          ((port? x)                (printport x p slashify))
          ((eq? x (unspecified))    (printstr "#!unspecified" p))
          ((eq? x (undefined))      (printstr "#!undefined" p))
          ((environment? x)
           ((environment-printer) x p slashify))
          ((structure? x)
           ((structure-printer) x p slashify))
          (else                     (printweird x p slashify))))

  (define (printnumber n p slashify)
    (if (eq? slashify **lowlevel**)
        (cond ((flonum? n)
               (write-char #\# p)
               (write-char ctrl-F p)
               (do ((i 4 (+ i 1)))
                   ((= i 12))
                 (write-char (integer->char (bytevector-like-ref n i)) p)))
              ((compnum? n)
               (write-char #\# p)
               (write-char ctrl-C p)
               (do ((i 4 (+ i 1)))
                   ((= i 20))
                 (write-char (integer->char (bytevector-like-ref n i)) p)))
              (else
               (printstr (number->string n) p)))
        (printstr (number->string n) p)))

  (define (printcharacter c p)
    (write-char #\# p)
    (write-char #\\ p)
    (let ((k (char->integer c)))
      (cond ((<= k **space**)
             (cond ((= k **space**)  (printstr "space" p))
                   ((= k **newline**) (printstr "newline" p))
                   ((= k **linefeed**) (printstr "linefeed" p))
                   ((= k **return**) (printstr "return" p))
                   ((= k **tab**) (printstr "tab" p))
                   ((= k **alarm**) (printstr "alarm" p))
                   ((= k **backspace**) (printstr "backspace" p))
                   ((= k **vtab**) (printstr "vtab" p))
                   ((= k **page**) (printstr "page" p))
                   (else
                    (let ((r7rs?
                           (or (io/port-allows-r7rs-weirdness? p)
                               (not (io/port-allows-r6rs-weirdness? p)))))
                      (cond ((= k **nul**)
                             (printstr (if r7rs? "null" "nul") p))
                            ((= k **esc**)
                             (printstr (if r7rs? "escape" "esc") p))
                            (else
                             (printstr "x" p)
                             (printstr (number->string k 16) p)))))))
            ((< k **delete**) (write-char c p))
            ((= k **delete**) (printstr "delete" p))
            ((and (memq (transcoder-codec (port-transcoder p))
                        '(utf-8 utf-16))
                  (print-as-char-without-hexifying? c))
             (write-char c p))
            (else
             (printstr "x" p)
             (printstr (number->string k 16) p)))))

  (define (printcodeobject x p slashify)
    ((code-object-printer) x p slashify))

  (define (printenvironment x p slashify)
    ((environment-printer) x p slashify))

  (define (printhashtable x p slashify)
    ((hashtable-printer) x p slashify))

  (define (printprocedure x p slashify)
    ((procedure-printer) x p slashify))

  (define (printbytevector x p slashify level)
    (if (eq? slashify **lowlevel**)
        (begin (write-char #\# p)
               (write-char ctrl-B p)
               (write-char #\" p)
               (print-slashed-bytevector x p)
               (write-char #\" p))
        (begin (write-char #\# p)
               (cond ((io/port-allows-r7rs-weirdness? p) #t)
                     ((io/port-allows-r6rs-weirdness? p)
                      (write-char #\v p))
                     (else #t))
               (write-char #\u p)
               (write-char #\8 p)
               (print (bytevector->list x) p slashify (- level 1)))))

  (define (printport x p slashify)
    (printstr (string-append "#<" (cond ((input-port? x) "INPUT PORT ")
                                        ((output-port? x) "OUTPUT PORT ")
                                        (else "PORT "))
                             (port-name x)
                             ">")
              p))

  (define (printeof x p slashify)
    (printstr "#<EOF>" p))

  (define (printweird x p slashify)
    ((weird-printer) x p slashify))

  (define (print-quoted x p slashify level)
    (printstr (cdr (assq (car x) quoter-strings)) p)
    (print (cadr x) p slashify (- level 1)))

  (print x p slashify (+ (or (print-level) -2) 1)))

;;; Don't print more than (print-length) elements of a list or vector,
;;; and don't print more than (print-level) nested lists or vectors.
;;;
;;; FIXME: Not sure if (print-level) is being respected.

(define print-length
  (make-parameter "print-length"
                  #f
                  (lambda (x)
                    (or (not x)
                        (and (fixnum? x) (>= x 0))))))

(define print-level
  (make-parameter "print-level"
                  #f
                  (lambda (x)
                    (or (not x)
                        (and (fixnum? x) (>= x 0))))))

(define **lowlevel** (list 0))   ; any unforgeable value

(define **nonprinting-value** (unspecified))

;;; For the write procedure, see print-shared.sch

(define write-simple
  (lambda (x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (print x p #t)
      (io/discretionary-flush p)
      **nonprinting-value**)))

;;; For the display procedure, see print-shared.sch

(define display-simple
  (lambda (x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (print x p #f)
      (io/discretionary-flush p)
      **nonprinting-value**)))

;;; Printing fasl files with shared structures as below does work,
;;; but it doesn't appear to save enough space to justify the
;;; compile-time overhead of detecting shared structures.

;(define lowlevel-write
;  (lambda (x . rest)
;    (let ((p (if (pair? rest) (car rest) (current-output-port)))
;          (simple-printer (lambda (x p) (print x p **lowlevel**))))
;      (print-with-shared-structure x p simple-printer)
;      (io/discretionary-flush p)
;      **nonprinting-value**)))

(define lowlevel-write
  (lambda (x . rest)
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (print x p **lowlevel**)
      (io/discretionary-flush p)
      **nonprinting-value**)))

(define newline
  (lambda rest
    (let ((p (if (pair? rest) (car rest) (current-output-port))))
      (write-char #\newline p)
      (io/discretionary-flush p)
      **nonprinting-value**)))

; eof
