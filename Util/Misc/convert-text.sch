; Text file format conversions
;
; 5 June 1999 / lth
;
; You should run an A->native conversion only on the platform you're converting
; to, because these procedures depend on CALL-WITH-OUTPUT-FILE and WRITE-CHAR
; doing the right thing with a text file.
;
; You should run a native->A conversion only on the platform you're converting
; from, because these procedures depend on CALL-WITH-INPUT-FILE and READ-CHAR
; doing the right thing with a text file.

; Convert a MacOS text file to native format.
;
; Lines are terminated with CR and there is no explicit end-of-file marker.

(define (macos->native infile outfile)
  (call-with-binary-input-file infile
    (lambda (in)
      (call-with-output-file outfile
        (lambda (out)
          (do ((c (read-char in) (read-char in)))
              ((eof-object? c))
            (if (char=? c #\return)
                (write-char #\newline out)
                (write-char c out))))))))

(define (native->macos infile outfile)
  (call-with-input-file infile
    (lambda (in)
      (call-with-binary-output-file outfile
        (lambda (out)
          (do ((c (read-char in) (read-char in)))
              ((eof-object? c))
            (if (char=? c #\newline)
                (write-char #\return out)
                (write-char c out))))))))

; Convert an MS-DOS text file to native format.
;
; Lines are terminated with CRLF, and there is optionally a ^Z to
; mark the end of file.

(define (msdos->native infile outfile)
  (call-with-binary-input-file infile
    (lambda (in)
      (call-with-output-file outfile
        (lambda (out)
          (do ((c (read-char in) (read-char in)))
              ((or (eof-object? c) (char=? c (integer->char 26))))
            (cond ((char=? c #\return)
                   (let ((d (read-char in)))
                     (cond ((eof-object? d)
                            (write-char c out))
                           ((char=? d #\linefeed)
                            (write-char #\newline out))
                           (else
                            (write-char c out)
                            (write-char d out)))))
                  (else
                   (write-char c out)))))))))

(define (native->msdos infile outfile)
  (call-with-input-file infile
    (lambda (in)
      (call-with-binary-output-file outfile
        (lambda (out)
          (do ((c (read-char in) (read-char in)))
              ((eof-object? c)
               (write-char (integer->char 26) out))
            (if (char=? c #\newline)
                (begin (write-char #\return out)
                       (write-char #\linefeed out))
                (write-char c out))))))))

; Convert a Unix text file to native format.
;
; Lines are terminated with LF, and there is no explicit end-of-file marker.

(define (unix->native infile outfile)
  (call-with-binary-input-file infile
    (lambda (in)
      (call-with-output-file outfile
        (lambda (out)
          (do ((c (read-char in) (read-char in)))
              ((eof-object? c))
            (if (char=? c #\linefeed)
                (write-char #\newline out)
                (write-char c out))))))))

(define (native->unix infile outfile)
  (call-with-input-file infile
    (lambda (in)
      (call-with-binary-output-file outfile
        (lambda (out)
          (do ((c (read-char in) (read-char in)))
              ((eof-object? c))
            (if (char=? c #\newline)
                (write-char #\linefeed out)
                (write-char c out))))))))


; eof
