; Copyright (c) 2006 Michael Sperber
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 3. The name of the authors may not be used to endorse or promote products
;    derived from this software without specific prior written permission.
; 
; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Majorly hacked for Larceny by William D Clinger, 25 May 2007.
; Based on Clinger's hacked version of Michael Sperber's
; check-normalization.scm.
;
; This file contains an R6RS top-level program that tests the
; string-next-word-break procedure exported by (local unicode2)
; of this reference implementation.
;
; To run this test program, first get the test input:
;
; http://www.unicode.org/Public/UNIDATA/auxiliary/WordBreakTest.txt

(import (rnrs base)
        (rnrs control)
        (rnrs io ports)
        (rnrs io simple)
        (local unicode)
        (local unicode2))

(define wordbreaking-tests-filename "WordBreakTest.txt")

; Given a string and a starting index,
; returns the index of the first character in the string
; at or following the starting index that is not a hex digit.
; Returns the length of the string if no such character is found.

(define (index-of-next-non-hex-digit s i)
  (let ((n (string-length s)))
    (let loop ((i i))
      (if (< i n)
          (case (string-ref s i)
           ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
             #\a #\b #\c #\d #\e #\f 
             #\A #\B #\C #\D #\E #\F)
            (loop (+ i 1)))
           (else
            i))
          n))))

; Given a string of hex digits, returns its value as an integer.

(define (hexstring->int s)
  (define (hexdigit->int c)
    (case c
     ((#\0) 0)
     ((#\1) 1)
     ((#\2) 2)
     ((#\3) 3)
     ((#\4) 4)
     ((#\5) 5)
     ((#\6) 6)
     ((#\7) 7)
     ((#\8) 8)
     ((#\9) 9)
     ((#\a #\A) 10)
     ((#\b #\B) 11)
     ((#\c #\C) 12)
     ((#\d #\D) 13)
     ((#\e #\E) 14)
     ((#\f #\F) 15)
     (else (error "Bad hex digit: " c))))
  (let ((n (string-length s)))
    (do ((i 0 (+ i 1))
         (result 0 (+ (* 16 result) (hexdigit->int (string-ref s i)))))
        ((= i n) result))))

; Given a non-comment line from WordBreakTest.txt,
; returns two values parsed from that line (as multiple values):
;     a string of Unicode values (the test)
;     a vector of booleans, the same length as the string,
;         indicating whether a break is permitted before
;         the corresponding element of the string
;         (true means a word break is permitted)
;
; Note that a break is always permitted following the last
; character in the string.
;
; WordBreakTest.txt contains UTF-8 encodings for two characters:
;
;     division sign        #\x00f7   (UTF-8: #xc3 #xb7)
;     multiplication sign  #\x00d7   (UTF-8: #xc3 #x97)
;
; As of Unicode 5.1.0, the following procedure works
; regardless of whether the file is read as UTF-8 or
; as Latin-1.  That explains the #xc3 test below.

(define (parse-wordbreaktest-line s)
  (let ((n (string-length s)))
    (define (loop i chars flags)
      (if (< i n)
          (let* ((c (string-ref s i))
                 (sv (char->integer c)))
            (cond ((char=? c #\#)
                   (values (list->string (reverse chars))
                           (list->vector (reverse (cdr flags)))))
                  ((char-whitespace? c)
                   (loop (+ i 1) chars flags))
                  ((= sv #x00f7)
                   (loop (+ i 2) chars (cons #t flags)))
                  ((= sv #x00d7)
                   (loop (+ i 2) chars (cons #f flags)))
                  ((= sv #xc3)
                   (let ((sv2 (char->integer (string-ref s (+ i 1)))))
                     (cond ((= sv2 #xb7)
                            (loop (+ i 2) chars (cons #t flags)))
                           ((= sv2 #x97)
                            (loop (+ i 2) chars (cons #f flags)))
                           (else (assert #f)))))
                  (else
                   (let* ((j (index-of-next-non-hex-digit s (+ i 1)))
                          (n (hexstring->int (substring s i j))))
                     (loop j (cons (integer->char n) chars) flags)))))
          (assert #f)))
    (loop 0 '() '())))

; Crude test rig.

(define total-tests 0)
(define total-failed 0)
(define total-inputs 0)
(define failed-inputs '())

(define (wordbreak-test-init!)
  (set! total-tests 0)
  (set! total-failed 0)
  (set! total-inputs 0)
  (set! failed-inputs '()))

(define (wordbreak-test-start)
  (display ".")
  (set! total-inputs (+ total-inputs 1)))

(define (wordbreak-test input expected obtained)
  (set! total-tests (+ total-tests 1))
  (if (not (equal? expected obtained))
      (begin (set! total-failed (+ total-failed 1))
             (set! failed-inputs (cons input failed-inputs))
             (newline)
             (display "***** test failed ***** ")
             (display total-inputs)
             (newline)
             (write (string->list input))
             (newline)
             (write expected)
             (newline)
             (write obtained)
             (newline))))

(define (wordbreak-test-summarize)
  (newline)
  (display "Failed ")
  (write total-failed)
  (display " out of ")
  (write total-tests)
  (newline))

(define (wordbreak-check-line s)
  (call-with-values
   (lambda ()
     (parse-wordbreaktest-line s))
   (lambda (str flags)
     (wordbreak-test-start)
     (check-word-breaks str flags))))

(define (check-word-breaks s flags)

  (define (compute-word-breaks)
    (let* ((n (string-length s))
           (breaks (make-vector n #f)))
      (do ((i (string-next-word-break s -1)
              (string-next-word-break s i)))
          ((= i n))
        (vector-set! breaks i #t))
      breaks))

  (wordbreak-test s flags (compute-word-breaks)))

(define current-line "")

(define (wordbreak-check-all filename)
  (call-with-port
   (open-file-input-port filename (file-options) 'block
                         (make-transcoder (latin-1-codec))) ;(utf-8-codec)))
   (lambda (port)
     (let loop ()
       (let ((thing (get-line port)))
         (set! current-line thing)
         (if (string? thing)
             (begin
              (if (and (not (string=? "" thing))
                       (not (char=? (string-ref thing 0) #\#))
                       (not (char=? (string-ref thing 0) #\@)))
                  (wordbreak-check-line thing))
              (loop))))))))

(define (wordbreak-run-tests)
  (wordbreak-test-init!)
  (wordbreak-check-all wordbreaking-tests-filename)
  (wordbreak-test-summarize))

(wordbreak-run-tests)
