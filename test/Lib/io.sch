; Copyright 2007 William D Clinger
;
; $Id$
;
; Test cases for R6RS i/o.

(define (run-io-tests . rest)
  (display "Input/output") (newline)
  (io-basic-tests)
  (io-eol-tests)
  (io-input/output-tests)
  (if (and #f (null? rest)) ;FIXME
      (io-test-error)
      #t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The deprecated eol-style, buffer-mode, and error-handling-mode
; syntaxes are not present in R5RS mode.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax eol-style
  (syntax-rules ()
   ((eol-style x)
    (quote x))))

(define-syntax buffer-mode
  (syntax-rules ()
   ((buffer-mode x)
    (quote x))))

(define-syntax error-handling-mode
  (syntax-rules ()
   ((error-handling-mode x)
    (quote x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End of R6RS silliness.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Port i/o.

(define (io-basic-tests)

  (define buffer-modes '(none line block))
  (define codecs '(latin-1 utf-8 utf-16))         ; nonstandard representations
  (define eol-styles '(none lf cr crlf nel crnel ls))
  (define err-modes '(ignore replace raise))

  (allof "basic i/o tests"

   ; R6RS silliness.

   ; FIXME
   (test "(file-options)" (begin (file-options) #t) #t)

   (test "(buffer-mode none)" (buffer-mode none) 'none)
   (test "(buffer-mode line)" (buffer-mode line) 'line)
   (test "(buffer-mode block)" (buffer-mode block) 'block)

   (test "(eol-style none)"  (eol-style none)  'none)
   (test "(eol-style lf)"    (eol-style lf)    'lf)
   (test "(eol-style cr)"    (eol-style cr)    'cr)
   (test "(eol-style crlf)"  (eol-style crlf)  'crlf)
   (test "(eol-style nel)"   (eol-style nel)   'nel)
   (test "(eol-style crnel)" (eol-style crnel) 'crnel)
   (test "(eol-style ls)"    (eol-style ls)    'ls)

   (test "(error-handling-mode ignore)"
         (error-handling-mode ignore)   'ignore)
   (test "(error-handling-mode raise)"
         (error-handling-mode raise)    'raise)
   (test "(error-handling-mode replace)"
         (error-handling-mode replace)  'replace)

   ; Real stuff.

   (test "(buffer-mode? 'none)"  (buffer-mode? 'none)  #t)
   (test "(buffer-mode? 'line)"  (buffer-mode? 'line)  #t)
   (test "(buffer-mode? 'datum)" (buffer-mode? 'datum) #f)
   (test "(buffer-mode? 'block)" (buffer-mode? 'block) #t)
   (test "(buffer-mode? 'foo)"   (buffer-mode? 'foo)   #f)
   (test "(buffer-mode? 17)"     (buffer-mode? 17)     #f)

   ; FIXME:  The results here are Larceny-specific.

   (test "standard codecs"
         (list (latin-1-codec) (utf-8-codec) (utf-16-codec))
         '(latin-1 utf-8 utf-16))

   (test "(native-eol-style)"
         (and (memq (native-eol-style) '(none lf cr crlf nel crnel ls)) #t)
         #t)

   (test "make-transcoder with 3 arguments"
         (map (lambda (codec)
                (map (lambda (eol)
                       (map (lambda (err)
                              (let ((t (make-transcoder codec eol err)))
                                (and (eq? (transcoder-codec t) codec)
                                     (eq? (transcoder-eol-style t) eol)
                                     (eq? (transcoder-error-handling-mode t)
                                          err))))
                            err-modes))
                     eol-styles))
              codecs)
         (map (lambda (x) (map (lambda (y) (map (lambda (t) #t)
                                                err-modes))
                               eol-styles))
              codecs))

   (test "make-transcoder with 2 arguments"
         (map (lambda (codec)
                (map (lambda (eol)
                       (map (lambda (err)
                              (let ((t (make-transcoder codec eol)))
                                (and (eq? (transcoder-codec t) codec)
                                     (eq? (transcoder-eol-style t) eol)
                                     (eq? (transcoder-error-handling-mode t)
                                          'raise))))
                            err-modes))
                     eol-styles))
              codecs)
         (map (lambda (x) (map (lambda (y) (map (lambda (t) #t)
                                                err-modes))
                               eol-styles))
              codecs))

   (test "make-transcoder with 1 argument"
         (map (lambda (codec)
                (map (lambda (eol)
                       (map (lambda (err)
                              (let ((t (make-transcoder codec)))
                                (and (eq? (transcoder-codec t) codec)
                                     (eq? (transcoder-eol-style t)
                                          (native-eol-style))
                                     (eq? (transcoder-error-handling-mode t)
                                          'raise))))
                            err-modes))
                     eol-styles))
              codecs)
         (map (lambda (x) (map (lambda (y) (map (lambda (t) #t)
                                                err-modes))
                               eol-styles))
              codecs))

   (test "(native-transcoder)"
         (let ((t (native-transcoder)))
           (and (memq (transcoder-codec t) codecs)
                (eq? (transcoder-eol-style t) (native-eol-style))
                (memq (transcoder-error-handling-mode t) err-modes)
                #t))
         #t)

   (test "bytevector->string 1"
         (let* ((s0 "a\x3bb;b\x7834;c\xffff;d\x100000;e\x10ffff;f")
                (bv (string->utf8 s0))
                (t (make-transcoder (utf-8-codec) 'none 'ignore)))
           (string=? s0 (bytevector->string bv t)))
         #t)

   (test "bytevector->string 2"
         (let* ((s0 "a\x3bb;b\x7834;c\xffff;d\x100000;e\x10ffff;f")
                (bv (string->utf8 s0))
                (t (make-transcoder (latin-1-codec) 'none 'ignore)))
           (string=? (list->string
                      (map integer->char
                           (bytevector->list bv)))
                     (bytevector->string bv t)))
         #t)

   (test "string->bytevector 1"
         (let* ((s0 "a\x3bb;b\x7834;c\xffff;d\x100000;e\x10ffff;f")
                (t (make-transcoder (utf-8-codec) 'none 'ignore))
                (bv (string->bytevector s0 t)))
           (string=? s0
                     (bytevector->string bv t)))
         #t)

   (test "string->bytevector 2"
         (let* ((s0 "a\x3bb;b\x7834;c\xffff;d\x100000;e\x10ffff;f")
                (t (make-transcoder (latin-1-codec) 'none 'ignore))
                (bv (string->bytevector s0 t)))
           (string=? (list->string
                      (filter (lambda (c)
                                (char<=? c (integer->char #xff)))
                              (string->list s0)))
                     (bytevector->string bv t)))
         #t)

   (test "eof objects" (eof-object? (eof-object)) #t)

   (test "port?"
         (map port? (list (current-input-port)
                          (current-output-port)
                          (open-string-input-port "abc")
                          (open-string-output-port)
                          "abc"
                          '#(...)))
         '(#t #t #t #t #f #f))

   (let ((t (native-transcoder)))
     (test "port-transcoder"
           (map port-transcoder
                (list (open-bytevector-output-port)
                      (transcoded-port (open-bytevector-output-port) t)))
           (list #f t)))

   (test "textual-port?"
         (map textual-port?
              (list (open-bytevector-input-port '#vu8(1 2 3 4 5))
                    (open-bytevector-output-port)
                    (transcoded-port (open-bytevector-output-port)
                                     (native-transcoder))))
         '(#f #f #t))

   (test "binary-port?"
         (map binary-port?
              (list (open-bytevector-input-port '#vu8(1 2 3 4 5))
                    (open-bytevector-output-port)
                    (transcoded-port (open-bytevector-output-port)
                                     (native-transcoder))))
         '(#t #t #f))

   (test "transcoded-port"
         (let* ((p (open-bytevector-output-port))
                (q (transcoded-port p (native-transcoder))))
           (textual-port? q))
         #t)

   (test "port-has-port-position?"
         (let ((p (open-bytevector-input-port '#vu8(1 2 3 4 5))))
           (list (port-has-port-position? p)
                 (port-position p)))
         '(#t 0))

   (test "port-has-set-port-position!?"
         (let ((p (current-output-port)))
           (port-has-set-port-position!? p))
         #f)

   (test "close-port"
         (let ((p (open-string-output-port)))
           (close-port p)
           (port? p))
         #t)

   (test "call-with-port"
         (call-with-values
          (lambda () (open-string-output-port))
          (lambda (out f)
            (call-with-port
             out
             (lambda (out) (put-string out "abc") (f)))))
         "abc")

   (test "input-port?"
         (map input-port?
              (list (current-input-port)
                    (open-bytevector-input-port '#vu8(1 2 3))
                    (open-string-input-port "abcd")
                    (current-output-port)
                    '#(a b c)))
         '(#t #t #t #f #f))

   (test "port-eof?"
         (let* ((p (open-string-input-port "abc"))
                (x1 (begin (get-char p) (port-eof? p)))
                (x2 (begin (get-char p) (port-eof? p)))
                (x3 (begin (get-char p) (port-eof? p)))
                (x4 (begin (get-char p) (port-eof? p))))
           (list x1 x2 x3 x4))
         '(#f #f #t #t))

   ; open-file-input-port is tested later, with open-file-output-port

   (test "open-bytevector-input-port 1"
         (get-bytevector-all (open-bytevector-input-port '#vu8(3 4 5 6 7)))
         '#vu8(3 4 5 6 7))

   (test "open-bytevector-input-port 2"
         (get-bytevector-all (open-bytevector-input-port '#vu8(3 4 5 6 7) #f))
         '#vu8(3 4 5 6 7))

   (test "open-bytevector-input-port 3"
         (get-string-all
          (open-bytevector-input-port '#vu8(97 98 99) (native-transcoder)))
         "abc")

   (test "open-string-input-port"
         (let* ((p (open-string-input-port "a\x3bb;\xffff;\x10ffff;b"))
                (c0 (get-char p))
                (c1 (get-char p))
                (c2 (get-char p))
                (c3 (get-char p))
                (c4 (get-char p))
                (c5 (get-char p)))
           (list c0 c1 c2 c3 c4 c5))
         (list #\a #\x3bb #\xffff #\x10ffff #\b (eof-object)))

   ; FIXME: standard-input-port isn't implemented yet

   (test "current-input-port"
         (and (input-port? (current-input-port))
              (textual-port? (current-input-port)))
         #t)

   (test "make-custom-binary-input-port"
         (get-bytevector-n
          (make-custom-binary-input-port
           "two-hundred"
           (lambda (bv start count)
             (do ((n (+ start count))
                  (i start (+ i 1)))
                 ((= i n) count)
               (bytevector-set! bv i 200)))
           #f #f #f)
          5000)
         (make-bytevector 5000 200))

   (test "make-custom-binary-input-port (Zeno)"
         (get-bytevector-n
          (make-custom-binary-input-port
           "two-hundred"
           (lambda (bv start count)
             (let ((count (max 1 (quotient count 2))))
               (do ((n (+ start count))
                    (i start (+ i 1)))
                   ((= i n) count)
                 (bytevector-set! bv i 200))))
           #f #f #f)
          5000)
         (make-bytevector 5000 200))

   (test "make-custom-textual-input-port"
         (get-string-n
          (make-custom-textual-input-port
           "two-hundred"
           (lambda (s start count)
             (do ((n (+ start count))
                  (i start (+ i 1)))
                 ((= i n) count)
               (string-set! s i #\*)))
           #f #f #f)
          5000)
         (make-string 5000 #\*))

   (test "make-custom-textual-input-port (Zeno)"
         (get-string-n
          (make-custom-textual-input-port
           "two-hundred"
           (lambda (s start count)
             (let ((count (max 1 (quotient count 2))))
               (do ((n (+ start count))
                    (i start (+ i 1)))
                   ((= i n) count)
                 (string-set! s i #\x3bb))))
           #f #f #f)
          5000)
         (make-string 5000 #\x3bb))

   (test "make-custom-binary-output-port"
         (let* ((bytes '())
                (p (make-custom-binary-output-port
                    "one-hundred"
                    (lambda (bv start count)
                      (do ((n (+ start count))
                           (i start (+ i 1)))
                          ((= i n) count)
                        (set! bytes (cons (bytevector-ref bv i) bytes))))
                    #f #f #f)))
           (put-u8 p 1)
           (put-bytevector p (make-bytevector 5000 100))
           (close-output-port p)
           (reverse bytes))
         (cons 1 (vector->list (make-vector 5000 100))))

   (test "make-custom-binary-output-port (Zeno)"
         (let* ((bytes '())
                (p (make-custom-binary-output-port
                    "one-hundred"
                    (lambda (bv start count)
                      (let ((count (if (positive? count)
                                       (max 1 (quotient count 2))
                                       0)))
                        (do ((n (+ start count))
                             (i start (+ i 1)))
                            ((= i n)
                             count)
                          (set! bytes (cons (bytevector-ref bv i) bytes)))))
                    #f #f #f)))
           (put-u8 p 1)
           (put-bytevector p (make-bytevector 5000 100))
           (close-output-port p)
           (reverse bytes))
         (cons 1 (vector->list (make-vector 5000 100))))

   (test "make-custom-textual-output-port"
         (let* ((chars '())
                (p (make-custom-textual-output-port
                    "one-hundred"
                    (lambda (s start count)
                      (do ((n (+ start count))
                           (i start (+ i 1)))
                          ((= i n) count)
                        (set! chars (cons (string-ref s i) chars))))
                    #f #f #f)))
           (put-char p #\x10001)
           (put-string p (make-string 5000 #\.))
           (close-output-port p)
           (reverse chars))
         (cons #\x10001 (vector->list (make-vector 5000 #\.))))

   (test "make-custom-textual-output-port (Zeno)"
         (let* ((chars '())
                (p (make-custom-textual-output-port
                    "one-hundred"
                    (lambda (s start count)
                      (let ((count (if (positive? count)
                                       (max 1 (quotient count 2))
                                       0)))
                        (do ((n (+ start count))
                             (i start (+ i 1)))
                            ((= i n)
                             count)
                          (set! chars (cons (string-ref s i) chars)))))
                    #f #f #f)))
           (put-char p #\$)
           (put-string p (make-string 5000 #\x3bb))
           (close-output-port p)
           (reverse chars))
         (cons #\$ (vector->list (make-vector 5000 #\x3bb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (test "output-port?"
         (map output-port?
              (list (current-input-port)
                    (open-bytevector-output-port)
                    (open-string-input-port "abcd")
                    (current-output-port)
                    '#(a b c)))
         '(#f #t #f #t #f))

))


; Tests of eol-style processing and
; port-position, port-lines-read, port-line-start.

(define (io-eol-tests)

  (define lf (string #\newline))
  (define cr (string #\return))
  (define nel (string #\x85))
  (define ls (string #\x2028))

  (define s0 "")
  (define s1 "A line w/ 24 characters.")

  ; Generates n random bytes and returns the string obtained
  ; by treating them as UTF-8.

  (define (line-noise n)
    (let ((bytes (map (lambda (x) (random 256))
                      (vector->list (make-vector n)))))
      (utf8->string (list->bytevector bytes))))

  (let* ((n (string-length s1))
         (f (lambda () (line-noise n)))
         (s (string-append s1 lf s1 cr s1 cr lf s1 cr nel s1 nel s1 ls
                           s1 lf lf s1 cr lf s1 cr lf lf
                           s1 cr nel lf s1 nel lf s1 ls lf
                           s1 lf cr s1 cr cr s1 cr lf cr
                           s1 cr nel cr s1 nel cr s1 ls cr
                           s1 lf nel s1 cr nel s1 cr lf nel
                           s1 cr nel nel s1 nel nel s1 ls nel
                           s1 lf ls s1 cr ls s1 cr lf ls
                           s1 cr nel ls s1 nel ls s1 ls ls))
         (u (string-append (f) lf (f) cr (f) cr lf (f) cr nel (f) nel (f) ls
                           (f) lf lf (f) cr lf (f) cr lf lf
                           (f) cr nel lf (f) nel lf (f) ls lf
                           (f) lf cr (f) cr cr (f) cr lf cr
                           (f) cr nel cr (f) nel cr (f) ls cr
                           (f) lf nel (f) cr nel (f) cr lf nel
                           (f) cr nel nel (f) nel nel (f) ls nel
                           (f) lf ls (f) cr ls (f) cr lf ls
                           (f) cr nel ls (f) nel ls (f) ls ls)))

    ; Given a textual input port, returns a list of the
    ; port positions that follow a #\linefeed.

    (define (port-line-starts p)
      (define (loop starts)
        (let ((c (get-char p)))
          (cond ((not (char? c))
                 (reverse starts))
                ((char=? c #\linefeed)
                 (loop (cons (port-position p) starts)))
                (else
                 (loop starts)))))
      (loop '()))

    ; Given a string s, returns the list of indexes into s
    ; that follow a #\linefeed.

    (define (line-starts s)
      (let ((n (string-length s)))
        (define (loop i starts)
          (cond ((= i n)
                 (reverse starts))
                ((char=? #\linefeed (string-ref s i))
                 (loop (+ i 1) (cons (+ i 1) starts)))
                (else
                 (loop (+ i 1) starts))))
        (loop 0 '())))

    ; Given a string s, returns the string obtained from s
    ; by converting all standard end-of-line sequences into
    ; linefeeds.

    (define (canonical s)
      (let ((n (string-length s)))
        (define (loop i chars)
          (cond ((= i n)
                 (list->string (reverse chars)))
                ((or (char=? (string-ref s i) #\linefeed)
                     (char=? (string-ref s i) #\x85)
                     (char=? (string-ref s i) #\x2028))
                 (loop (+ i 1) (cons #\linefeed chars)))
                ((char=? (string-ref s i) #\return)
                 (let* ((i+1 (+ i 1))
                        (next (if (< i+1 n)
                                  (string-ref s i+1)
                                  #\space)))
                   (cond ((or (char=? next #\linefeed)
                              (char=? next #\x85))
                          (loop (+ i 2) (cons #\linefeed chars)))
                         (else
                          (loop (+ i 1) (cons #\linefeed chars))))))
                (else
                 (loop (+ i 1) (cons (string-ref s i) chars)))))
        (loop 0 '())))

    ; Given a test name, a test string, and a transcoder, determines
    ; whether the lines read from the string have the correct lengths.

    (define (test-line-lengths name s t)
      (define (string->latin-1 s)
        (list->string
         (map (lambda (c) (if (< (char->integer c) 256) c #\?))
              (string->list s))))
      (let* ((codec (transcoder-codec t))
             (eolstyle (transcoder-eol-style t))
             (s (if (eq? codec 'latin-1)
                    (string->latin-1 s)
                    s))
             (bv (cond ((eq? codec 'latin-1)
                        (list->bytevector
                         (map char->integer (string->list s))))
                       ((eq? codec 'utf-8)
                        (string->utf8 s))
                       ((eq? codec 'utf-16)
                        (string->utf16 s))
                       (else
                        (assert #f)))))
        (call-with-port
         (open-bytevector-input-port bv t)
         (lambda (p)
           (test name (port-line-starts p)
                      (line-starts (if (eq? eolstyle 'none)
                                       s
                                       (canonical s))))))))

    ; Given a test name, a test string, a transcoder, and
    ; the expected output (as a bytevector), tests whether
    ; the transcoder transcodes the test string for output
    ; as expected.

    (define (test-output name s t expected)
      (let ((bv (call-with-values
                 (lambda () (open-bytevector-output-port t))
                 (lambda (out g)
                   (put-string out s)
                   (g)))))
        (test name bv expected)))                 

    (allof "eol tests"

     (test-line-lengths "native a" s (native-transcoder))

     (test-line-lengths "native u" u (native-transcoder))

     (test-line-lengths
      "latin-1 none a" s (make-transcoder (latin-1-codec) 'none 'ignore))

     (test-line-lengths
      "latin-1 none u" u (make-transcoder (latin-1-codec) 'none 'ignore))

     (test-line-lengths
      "latin-1 lf a" s (make-transcoder (latin-1-codec) 'lf 'ignore))

     (test-line-lengths
      "latin-1 lf u" u (make-transcoder (latin-1-codec) 'lf 'ignore))

     (test-line-lengths
      "latin-1 cr a" s (make-transcoder (latin-1-codec) 'cr 'ignore))

     (test-line-lengths
      "latin-1 cr u" u (make-transcoder (latin-1-codec) 'cr 'ignore))

     (test-line-lengths
      "latin-1 crlf a" s (make-transcoder (latin-1-codec) 'crlf 'ignore))

     (test-line-lengths
      "latin-1 crlf u" u (make-transcoder (latin-1-codec) 'crlf 'ignore))

     (test-line-lengths
      "latin-1 nel a" s (make-transcoder (latin-1-codec) 'nel 'ignore))

     (test-line-lengths
      "latin-1 nel u" u (make-transcoder (latin-1-codec) 'nel 'ignore))

     (test-line-lengths
      "latin-1 crnel a" s (make-transcoder (latin-1-codec) 'crnel 'ignore))

     (test-line-lengths
      "latin-1 crnel u" u (make-transcoder (latin-1-codec) 'crnel 'ignore))

     (test-line-lengths
      "latin-1 ls a" s (make-transcoder (latin-1-codec) 'ls 'ignore))

     (test-line-lengths
      "latin-1 ls u" u (make-transcoder (latin-1-codec) 'ls 'ignore))

     (test-line-lengths
      "utf-8 none a" s (make-transcoder (utf-8-codec) 'none 'ignore))

     (test-line-lengths
      "utf-8 none u" u (make-transcoder (utf-8-codec) 'none 'ignore))

     (test-line-lengths
      "utf-8 lf a" s (make-transcoder (utf-8-codec) 'lf 'ignore))

     (test-line-lengths
      "utf-8 lf u" u (make-transcoder (utf-8-codec) 'lf 'ignore))

     (test-line-lengths
      "utf-8 cr a" s (make-transcoder (utf-8-codec) 'cr 'ignore))

     (test-line-lengths
      "utf-8 cr u" u (make-transcoder (utf-8-codec) 'cr 'ignore))

     (test-line-lengths
      "utf-8 crlf a" s (make-transcoder (utf-8-codec) 'crlf 'ignore))

     (test-line-lengths
      "utf-8 crlf u" u (make-transcoder (utf-8-codec) 'crlf 'ignore))

     (test-line-lengths
      "utf-8 nel a" s (make-transcoder (utf-8-codec) 'nel 'ignore))

     (test-line-lengths
      "utf-8 nel u" u (make-transcoder (utf-8-codec) 'nel 'ignore))

     (test-line-lengths
      "utf-8 crnel a" s (make-transcoder (utf-8-codec) 'crnel 'ignore))

     (test-line-lengths
      "utf-8 crnel u" u (make-transcoder (utf-8-codec) 'crnel 'ignore))

     (test-line-lengths
      "utf-8 ls a" s (make-transcoder (utf-8-codec) 'ls 'ignore))

     (test-line-lengths
      "utf-8 ls u" u (make-transcoder (utf-8-codec) 'ls 'ignore))

     (test-line-lengths
      "utf-16 none a" s (make-transcoder (utf-16-codec) 'none 'ignore))

     (test-line-lengths
      "utf-16 none u" u (make-transcoder (utf-16-codec) 'none 'ignore))

     (test-line-lengths
      "utf-16 lf a" s (make-transcoder (utf-16-codec) 'lf 'ignore))

     (test-line-lengths
      "utf-16 lf u" u (make-transcoder (utf-16-codec) 'lf 'ignore))

     (test-line-lengths
      "utf-16 cr a" s (make-transcoder (utf-16-codec) 'cr 'ignore))

     (test-line-lengths
      "utf-16 cr u" u (make-transcoder (utf-16-codec) 'cr 'ignore))

     (test-line-lengths
      "utf-16 crlf a" s (make-transcoder (utf-16-codec) 'crlf 'ignore))

     (test-line-lengths
      "utf-16 crlf u" u (make-transcoder (utf-16-codec) 'crlf 'ignore))

     (test-line-lengths
      "utf-16 nel a" s (make-transcoder (utf-16-codec) 'nel 'ignore))

     (test-line-lengths
      "utf-16 nel u" u (make-transcoder (utf-16-codec) 'nel 'ignore))

     (test-line-lengths
      "utf-16 crnel a" s (make-transcoder (utf-16-codec) 'crnel 'ignore))

     (test-line-lengths
      "utf-16 crnel u" u (make-transcoder (utf-16-codec) 'crnel 'ignore))

     (test-line-lengths
      "utf-16 ls a" s (make-transcoder (utf-16-codec) 'ls 'ignore))

     (test-line-lengths
      "utf-16 ls u" u (make-transcoder (utf-16-codec) 'ls 'ignore))

     (test-output
      "native" "a\nb" (native-transcoder) '#vu8(97 10 98))

     (test-output
      "latin-1 none" "a\nb\rc\x85;d\x20;e\r\nf\r\x85;g"
      (make-transcoder (latin-1-codec) 'none 'ignore)
      '#vu8(97 10 98 13 99 133 100 32 101 13 10 102 13 133 103))

     (test-output
      "latin-1 lf" "a\nb\rc\x85;d\x20;e\r\nf\r\x85;g"
      (make-transcoder (latin-1-codec) 'lf 'ignore)
      '#vu8(97 10 98 13 99 133 100 32 101 13 10 102 13 133 103))

     (test-output
      "latin-1 cr" "a\nb\rc\x85;d\x20;e\r\nf\r\x85;g"
      (make-transcoder (latin-1-codec) 'cr 'ignore)
      '#vu8(97 13 98 13 99 133 100 32 101 13 13 102 13 133 103))

     (test-output
      "latin-1 nel" "a\nb\rc\x85;d\x20;e\r\nf\r\x85;g"
      (make-transcoder (latin-1-codec) 'nel 'ignore)
      '#vu8(97 133 98 13 99 133 100 32 101 13 133 102 13 133 103))

     (test-output
      "latin-1 crlf" "a\nb\rc\x85;d\x20;e\r\nf\r\x85;g"
      (make-transcoder (latin-1-codec) 'crlf 'ignore)
      '#vu8(97 13 10 98 13 99 133 100 32 101 13 13 10 102 13 133 103))

     (test-output
      "latin-1 crnel" "a\nb\rc\x85;d\x20;e\r\nf\r\x85;g"
      (make-transcoder (latin-1-codec) 'crnel 'ignore)
      '#vu8(97 13 133 98 13 99 133 100 32 101 13 13 133 102 13 133 103))

     (test-output
      "utf-8 none" "a\nb\rc\x85;d\x2028;e\r\nf\r\x85;g"
      (make-transcoder (utf-8-codec) 'none 'ignore)
      '#vu8(97 10 98 13 99 194 133 100 226 128 168 101 13 10 102 13 194 133 103))

     (test-output
      "utf-8 lf" "a\nb\rc\x85;d\x2028;e\r\nf\r\x85;g"
      (make-transcoder (utf-8-codec) 'lf 'ignore)
      '#vu8(97 10 98 13 99 194 133 100 226 128 168 101 13 10 102 13 194 133 103))

     (test-output
      "utf-8 cr" "a\nb\rc\x85;d\x2028;e\r\nf\r\x85;g"
      (make-transcoder (utf-8-codec) 'cr 'ignore)
      '#vu8(97 13 98 13 99 194 133 100 226 128 168 101 13 13 102 13 194 133 103))

     (test-output
      "utf-8 nel" "a\nb\rc\x85;d\x2028;e\r\nf\r\x85;g"
      (make-transcoder (utf-8-codec) 'nel 'ignore)
      '#vu8(97 194 133 98 13 99 194 133 100 226 128 168 101 13 194 133 102 13 194 133 103))

     (test-output
      "utf-8 crlf" "a\nb\rc\x85;d\x2028;e\r\nf\r\x85;g"
      (make-transcoder (utf-8-codec) 'crlf 'ignore)
      '#vu8(97 13 10 98 13 99 194 133 100 226 128 168 101 13 13 10 102 13 194 133 103))

     (test-output
      "utf-8 crnel" "a\nb\rc\x85;d\x2028;e\r\nf\r\x85;g"
      (make-transcoder (utf-8-codec) 'crnel 'ignore)
      '#vu8(97 13 194 133 98 13 99 194 133 100 226 128 168
            101 13 13 194 133 102 13 194 133 103))

     (test-output
      "utf-8 " "a\nb\rc\x85;d\x2028;e\r\nf\r\x85;g"
      (make-transcoder (utf-8-codec) 'ls 'ignore)
      '#vu8(97 226 128 168 98 13 99 194 133 100 226 128 168
            101 13 226 128 168 102 13 194 133 103))

)))

(define (io-input/output-tests)

  (allof "input/output tests"

   (test "make-custom-binary-input/output-port"
         (get-bytevector-n
          (make-custom-binary-input/output-port
           "two-hundred"
           (lambda (bv start count)
             (do ((n (+ start count))
                  (i start (+ i 1)))
                 ((= i n) count)
               (bytevector-set! bv i 200)))
           (lambda (bv start count) count)
           #f #f #f)
          5000)
         (make-bytevector 5000 200))

   (test "make-custom-binary-input/output-port (Zeno)"
         (get-bytevector-n
          (make-custom-binary-input/output-port
           "two-hundred"
           (lambda (bv start count)
             (let ((count (max 1 (quotient count 2))))
               (do ((n (+ start count))
                    (i start (+ i 1)))
                   ((= i n) count)
                 (bytevector-set! bv i 200))))
           (lambda (bv start count) count)
           #f #f #f)
          5000)
         (make-bytevector 5000 200))

   (test "make-custom-binary-input/output-port"
         (let* ((bytes '())
                (p (make-custom-binary-input/output-port
                    "one-hundred"
                    (lambda (bv start count) count)
                    (lambda (bv start count)
                      (do ((n (+ start count))
                           (i start (+ i 1)))
                          ((= i n) count)
                        (set! bytes (cons (bytevector-ref bv i) bytes))))
                    #f #f #f)))
           (put-u8 p 1)
           (put-bytevector p (make-bytevector 5000 100))
           (close-port p)
           (reverse bytes))
         (cons 1 (vector->list (make-vector 5000 100))))

   (test "make-custom-binary-input/output-port (Zeno)"
         (let* ((bytes '())
                (p (make-custom-binary-input/output-port
                    "one-hundred"
                    (lambda (bv start count) count)
                    (lambda (bv start count)
                      (let ((count (if (positive? count)
                                       (max 1 (quotient count 2))
                                       0)))
                        (do ((n (+ start count))
                             (i start (+ i 1)))
                            ((= i n)
                             count)
                          (set! bytes (cons (bytevector-ref bv i) bytes)))))
                    #f #f #f)))
           (put-u8 p 1)
           (put-bytevector p (make-bytevector 5000 100))
           (close-port p)
           (reverse bytes))
         (cons 1 (vector->list (make-vector 5000 100))))

   (test "make-custom-binary-input/output-port (lookahead)"
         (let ((p (make-custom-binary-input/output-port
                   "looking"
                   (lambda (bv start count)
                     (do ((i 0 (+ i 1)))
                         ((= i count)
                          count)
                       (bytevector-set! bv (+ start i) 33)))
                   (lambda (bv start count)
                     (let ((bv2 (make-bytevector count)))
                       (bytevector-copy! bv start bv2 0 count)
                       count))
                   #f
                   (lambda (posn) #t)
                   #f)))
           (let* ((n1 (get-u8 p))
                  (n2 (lookahead-u8 p))
                  (n3 (begin (put-u8 p 44) (lookahead-u8 p)))
                  (n4 (get-u8 p)))
             (list n1 n2 n3 n4)))
         '(33 33 33 33))

   (test "make-custom-textual-input/output-port (lookahead)"
         (let* ((strings '())
                (p (make-custom-textual-input/output-port
                    "looking"
                    (lambda (s start count)
                      (do ((i 0 (+ i 1)))
                          ((= i count)
                           count)
                        (string-set! s (+ start i) #\*)))
                    (lambda (s start count)
                      (let ((s2 (substring s start (+ start count))))
                        (set! strings (cons s2 strings))
                        count))
                    #f
                    (lambda (posn) #t)
                    #f)))
           (let* ((c1 (begin (put-char p #\a) (get-char p)))
                  (c2 (lookahead-char p))
                  (c3 (begin (put-char p #\b) (lookahead-char p)))
                  (c4 (get-char p)))
             (cons (string c1 c2 c3 c4) (reverse strings))))
         '("****" "a" "b"))

   (test "bytevector input/output lookahead"
         (let* ((p (open-input/output-bytevector
                    '#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12)))
                (b0 (get-u8 p))
                (b1 (get-u8 p))
                (ateof? (port-eof? p)) ; advances index for lookahead
                (x (put-u8 p 101))     ; set-port-position! undoes lookahead
                (b3 (get-u8 p))
                (output (get-output-bytevector p)))
           (list b0 b1 ateof? b3 output))
         '(0 1 #f 3 #vu8(0 1 101 3 4 5 6 7 8 9 10 11 12)))

  ))
