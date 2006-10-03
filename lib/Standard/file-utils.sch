; Copyright 1999 Lars T Hansen
;
; Permission to use this code for any purpose whatsoever is hereby
; granted, provided that the above copyright notice and this legend
; are preserved in any work using this code.
;
; 2003-01-04 / lth

;@doc FILE-UTILS file
;
; Procedures for processing files

(require 'io)

;@doc DIFF procedure
;
; Simple text file difference utility.
;
; Given two file names and some optional flags, print a specification
; that allows the first file to be transformed into the second.  Each
; directive relates to a line number n in the output, following which
; the text of the two files are in disagreement and must be brought
; into agreement by deleting lines from FILE1 and inserting lines from
; FILE2.
;
;   n: INSERT k      at line n of output, insert k lines starting at line
;                    n of FILE2
;   n: DELETE a,k    at line n of output, delete k lines starting at line
;                    m of FILE1
;   n: CHANGE m,k;k' at line n of output, delete k lines starting at line
;                    m of FILE1 and insert k' lines starting at line n
;                    of FILE2
;
; The program uses a simple heuristic to find places where the file is
; in agreement: at least two consecutive lines must match.  Single line
; matches tend to throw the program off.  The program can be generalized
; to longer matches if necessary.  Other heuristics are possible.
; Presumably there is an "optimal" set of differences in the sense that
; the output for this set is smaller than for any other set.  The set
; depends on the matching algorithm; I suspect the 2-line match is
; within spitting distance of optimal in common cases.
;
; Flags: the flag 'blanks makes the matching algorithm ignore all blanks
; in the lines.  This is really the wrong thing: it should instead collapse
; sequences of blanks into a single "generic" blank.  FIXME.

(define (diff file1 file2 . flags)
  
  (define ignore-blanks (memq 'blanks flags))
  
  (define (display-lines v i limit tag)
    (do ((i i (+ i 1)))
        ((= i limit))
      (display tag)
      (display (vector-ref v i))
      (newline)))

  (define (same? x y)
    (or (string=? x y)
        (and ignore-blanks
             (string-no-blanks=? x y))))
  
  (define (string-no-blanks=? x y)
    (let ((l1 (string-length x))
          (l2 (string-length y)))
      (let loop ((i 0) (j 0))
        (cond ((and (= i l1) (= j l2)) #t)
              ((= i l1)
               (if (char-whitespace? (string-ref y j))
                   (loop i (+ j 1))
                   #f))
              ((= j l2)
               (if (char-whitespace? (string-ref x i))
                   (loop (+ i 1) j)
                   #f))
              ((char-whitespace? (string-ref x i))
               (loop (+ i 1) j))
              ((char-whitespace? (string-ref y j))
               (loop i (+ j 1)))
              ((char=? (string-ref x i) (string-ref y j))
               (loop (+ i 1) (+ j 1)))
              (else #f)))))
  
  (format #t "=== diff \"~a\" \"~a\"~%" file1 file2)
  (let* ((f1 (list->vector (read-file-as-lines file1)))
         (l1 (vector-length f1))
         (f2 (list->vector (read-file-as-lines file2)))
         (l2 (vector-length f2)))
    (let loop ((i 0) (j 0) (x 1))
      
      (define (match i j)
        (let outer ((i i))
          (if (= i l1)
              (values #f #f)
              (let inner ((j j))
                (cond ((= j l2)
                       (outer (+ i 1)))
                      ((and (same? (vector-ref f1 i) (vector-ref f2 j))
                            (< i (- l1 1))
                            (< j (- l2 1))
                            (same? (vector-ref f1 (+ i 1)) 
                                   (vector-ref f2 (+ j 1))))
                       (values i j))
                      (else
                       (inner (+ j 1))))))))

      (cond ((and (= i l1) (= j l2)))
            ((= i l1) 
             (format #t "--- ~a: insert ~a,~a~%" x j (- l2 j))
             (display-lines f2 j l2 "> "))
            ((= j l2) 
             (format #t "--- ~a: delete ~a,~a~%" x i (- l1 i))
             (display-lines f1 i l1 "< "))
            ((same? (vector-ref f1 i) (vector-ref f2 j))
             (loop (+ i 1) (+ j 1) (+ x 1)))
            (else
             (call-with-values
               (lambda () (match i j))
               (lambda (m1 m2)
                 (cond ((and m1 (= m1 i))
                        (format #t "--- ~a: insert ~a~%" x (- m2 j))
                        (display-lines f2 j m2 "> ")
                        (loop m1 m2 (+ x (- m2 j))))
                       ((and m1 (= m2 j))
                        (format #t "--- ~a: delete ~a,~a~%" x (+ i 1) (- m1 i))
                        (display-lines f1 i m1 "< ")
                        (loop m1 m2 x))
                       (m1
                        (format #t "--- ~a: change ~a,~a;~a~%" 
                                   x (+ i 1) (- m1 i) (- m2 j))
                        (display-lines f1 i m1 "< ")
                        (format #t "---~%")
                        (display-lines f2 j m2 "> ")
                        (loop m1 m2 (+ x (- m2 j))))
                       (else
                        (format #t "--- ~a: change ~a,~a;~a~%" 
                                   x (+ i 1) (- l1 i)(- l2 j))
                        (display-lines f1 i l1 "< ")
                        (format #t "---~%")
                        (display-lines f2 j l2 "> ")))))))))
  (format #t "===~%"))

;@doc HEXDUMP procedure
;@syn (hexdump filename)
;
; Dump contents of (possibly non-text) file as hexadecimal and text to 
; current output port.

(define (hexdump filename)
  (call-with-binary-input-file filename
    (lambda (in)
      (let ((chars (make-string 16)))
        (do ((i 0 (+ i 1))
             (c (read-char in) (read-char in)))
            ((eof-object? c)
             (if (not (zero? (remainder i 16)))
                 (do ((j i (+ j 1)))
                     ((= (remainder j 16) 0)
                      (display "   ")
                      (display chars)
                      (newline))
                   (display "   "))))
          (let ((k (remainder i 16)))
            (if (char>=? c #\space)
                (string-set! chars k c)
                (string-set! chars k #\.))
	    (if (= k 0)
		(begin 
		  ; Address wraps around after 64K.
		  (display (substring (number->string (+ #x10000 (remainder i 65536)) 16) 1 5))
		  (display "  ")))
            (display (substring (number->string (+ #x100 (char->integer c)) 16) 1 3))
            (display " ")
            (if (= k 15) 
                (begin (display "   ")
                       (display chars)
                       (string-fill! chars #\space)
                       (newline)))))))))


;@doc TRANSLATE-TEXTFILE-TO-NATIVE procedure
;
; Read the text file named by 'in' and write a copy to 'out',
; translating line breaks for Mac, MS-DOS, and Unix to whatever the
; native format is.

(define (translate-textfile-to-native in out)
  (call-with-binary-input-file in
    (lambda (in)
      (delete-file out)
      (call-with-output-file out
        (lambda (out)
	  (let ((CR (integer->char 13))
		(LF (integer->char 10)))
	    (do ((c (read-char in) (read-char in)))
		((eof-object? c))
              (cond ((eq? c CR)
		     (if (eq? (peek-char in) LF)
			 (read-char in))
		     (write-char #\newline out))
		    ((eq? c LF)
		     (write-char #\newline out))
		    (else
		     (write-char c out))))))))))

;@doc WORD-COUNT procedure
;
; Count lines, words, and characters in the text file.

(define (word-count filename)
  (call-with-input-file filename
    (lambda (input)

      (define (next chars words lines consumer)
        (let ((c (read-char input)))
          (if (eof-object? c)
              (list chars words lines)
              (consumer c (+ chars 1) words lines))))
      
      (define (between-words c chars words lines)
        (cond ((char=? c #\newline)
               (next chars words (+ lines 1) between-words))
              ((char-whitespace? c)
               (next chars words lines between-words))
              (else
               (next chars (+ words 1) lines in-word))))
      
      (define (in-word c chars words lines)
        (cond ((char-whitespace? c)
               (between-words c chars words lines))
              (else
               (next chars words lines in-word))))

      (next 0 0 0 between-words))))

;@doc CAT-FILES procedure
;
; Concatenate the input-files and write result to output-file.

(define (cat-files input-files output-file)
  (delete-file output-file)
  (call-with-binary-output-file output-file
    (lambda (out)
      (for-each (lambda (fn)
                  (call-with-binary-input-file fn
                    (lambda (in)
                      (do ((c (read-char in) (read-char in)))
                          ((eof-object? c))
                        (write-char c out)))))
                input-files))))

; eof
