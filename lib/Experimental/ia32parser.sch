; Parser for the instruction format data in Asm/Intel/NASM/insns.dat.
;
; Given the name of a file containing the data, returns a list of
; entries of the form
;
; (<mnemonic> (<mode> ...) (<byte> ...) (<subset> ...))
;
; where
;
; <mnemonic> is a symbol for the mnemonic
; <mode> is a symbol that describes the address mode of an operand
; <byte> is an exact integer to be interpreted as in assemble.c
; <subset> is a symbol naming some subset of the instruction set
;

(define (parse-ia32-instruction-data-file filename)
  (call-with-input-file filename parse-ia32-instruction-data))

(define (parse-ia32-instruction-data in)
  (do ((entries '() (cons x entries))
       (x (parse-instruction-data in) (parse-instruction-data in)))
      ((eof-object? x)
       (reverse entries))))

(define (parse-instruction-data in)
  (flush-comment-lines in)
  (parse-mnemonic in))

(define (parse-mnemonic in)
  (do ((chars '() (cons c chars))
       (c (peek-char in) (begin (read-char in) (peek-char in))))
      ((or (eof-object? c)
           (char-whitespace? c))
       (cond ((eof-object? c)
              c)
             ((null? chars)
              (parse-error 'parse-mnemonic in chars))
             (else
              (parse-operands in (rlist->symbol chars)))))))

(define (parse-operands in mnemonic)
  (flush-whitespace in)
  (do ((operands '() (cons x operands))
       (x (parse-operand in) (parse-operand in)))
      ((not x)
       (if (and (= 1 (length operands))
                (eq? 'void (car operands)))
           (parse-bytes in mnemonic '())
           (parse-bytes in mnemonic (reverse operands))))))

(define (parse-operand in)
  (do ((chars '() (cons c chars))
       (c (peek-char in) (begin (read-char in) (peek-char in))))
      ((or (eof-object? c)
           (char-whitespace? c)
           (char=? c #\,))
       (if (and (char? c) (char=? c #\,))
           (read-char in))
       (if (null? chars)
           #f
           (rlist->symbol chars)))))

(define (parse-bytes in mnemonic operands)
  (flush-whitespace in)
  (do ((bytes '() (cons x bytes))
       (x (parse-byte in) (parse-byte in)))
      ((not x)
       (parse-subsets in mnemonic operands (reverse bytes)))))

(define (parse-byte in)
  (case (peek-char in)
    ((#\\)
     (read-char in)
     (case (peek-char in)
       ((#\x)
        (read-char in)
        (parse-hexadecimal in))
       (else
        (parse-octal in))))
    (else #f)))

(define (parse-hexadecimal in)
  (let* ((c1 (read-char in))
         (c2 (read-char in))
         (c3 (peek-char in)))
    (if (and (char? c1)
             (char? c2)
             (char? c3)
             (hex-digit? c1)
             (hex-digit? c2)
             (or (char=? c3 #\\)
                 (char-whitespace? c3)))
        (+ (* 16 (hex-value c1)) (hex-value c2))
        (parse-error 'parse-hexadecimal in c1 c2 c3))))

(define (parse-octal in)
  (do ((n 0 (+ (* 8 n) (octal-value c)))
       (c (peek-char in) (begin (read-char in) (peek-char in)))
       (count 0 (+ count 1)))
      ((or (eof-object? c)
           (not (octal-digit? c)))
       (if (> count 0)
           n
           (parse-error 'parse-octal in c)))))

(define (parse-subsets in mnemonic operands bytes)
  (flush-whitespace in)
  (do ((subsets '() (cons x subsets))
       (x (parse-subset in) (parse-subset in)))
      ((not x)
       (flush-whitespace in)
       (list mnemonic operands bytes (reverse subsets)))))

(define (parse-subset in) (parse-operand in))

(define (flush-comment-lines in)
  (flush-whitespace in))

(define (flush-whitespace in)
  (let ((c (peek-char in)))
    (if (and (char? c) (char-whitespace? c))
        (begin (read-char in)
               (flush-whitespace in)))
    (if (and (char? c) (char=? c #\;))
        (begin (flush-line in)
               (flush-whitespace in)))))

(define (flush-line in)
  (let ((c (read-char in)))
    (cond ((eof-object? c) c)
          ((char=? c #\newline) c)
          (else
           (flush-line in)))))

(define (rlist->symbol chars)
  (string->symbol
   (list->string
    (reverse chars))))

(define (parse-error name in . args)
  (display "Error in ")
  (write name)
  (display " while parsing IA32 instruction encodings.")
  (newline)
  (for-each (lambda (x) (write x) (newline)) args)
  ???)

; Non-portable: assumes ASCII-compatible encodings.

(define (hex-digit? c)
  (or (and (char<=? #\0 c)
           (char<=? c #\9))
      (and (char<=? #\a c)
           (char<=? c #\f))
      (and (char<=? #\A c)
           (char<=? c #\F))))

(define (octal-digit? c)
  (and (char<=? #\0 c)
       (char<=? c #\7)))

(define (hex-value c)
  (cond ((and (char<=? #\0 c) (char<=? c #\9))
         (- (char->integer c) (char->integer #\0)))
        ((and (char<=? #\a c) (char<=? c #\f))
         (+ 10 (- (char->integer c) (char->integer #\a))))
        ((and (char<=? #\A c) (char<=? c #\F))
         (+ 10 (- (char->integer c) (char->integer #\A))))
        (else ???)))

(define (octal-value c)
  (hex-value c))
