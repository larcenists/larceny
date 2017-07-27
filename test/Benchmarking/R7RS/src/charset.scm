;;; CHARSET benchmark
;;;
;;; Uses character sets to to parse the KVJ into tokens
;;; represented as short strings, and then repeatedly parses
;;; those tokens to categorize them as identifiers or numbers.
;;; Basically a test of the char-set-contains? procedure and
;;; a few standard character sets.

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme time)
        (scheme file)
        (scheme charset)
        (scheme sort))

(define (tokens-from-file filename)
  (call-with-input-file filename tokens-from-port))

(define (tokens-from-port p)
  (define (loop tokens)
    (let ((x (next-token)))
      (if (eof-object? x)
          tokens
          (loop (cons x tokens)))))
  (define (next-token)
    (let loop ((c (read-char p))
               (chars '()))
      (cond ((not (char? c))
             (if (null? chars)
                 (eof-object)
                 (return-token chars)))
            ((char-set-contains? char-set:letter+digit c)
             (loop (read-char p)
                   (cons c chars)))
            ((char-set-contains? char-set:whitespace c)
             (return-token chars))
            (else
             (return-token chars)))))
  (define (return-token chars)
    (if (null? chars)
        (next-token)
        (list->string (reverse chars))))
  (loop '()))

;;; Given a character set, a string, and indexes i and j
;;; specifying a substring of s, does every character character
;;; in that substring belong to the character set?

(define (char-set-contains-every-char? cs s i j)
  (cond ((>= i j)
         #t)
        ((char-set-contains? cs (string-ref s i))
         (char-set-contains-every-char? cs s (+ i 1) j))
        (else
         #f)))

(define (identifier? s)
  (let ((n (string-length s)))
    (and (> n 0)
         (char-set-contains? char-set:letter (string-ref s 0))
         (char-set-contains-every-char? char-set:letter+digit s 1 n))))

(define (number? s)
  (let ((n (string-length s)))
    (and (> n 0)
         (char-set-contains-every-char? char-set:digit s 0 n))))

(define (category-counts tokens)
  (let loop ((tokens tokens)
             (n-identifiers 0)
             (n-numbers     0)
             (n-others      0))
    (if (null? tokens)
        (list n-identifiers n-numbers n-others)
        (let ((t (car tokens))
              (tokens (cdr tokens)))
          (cond ((identifier? t)
                 (loop tokens (+ 1 n-identifiers) n-numbers n-others))
                ((number? t)
                 (loop tokens n-identifiers (+ 1 n-numbers) n-others))
                (else
                 (loop tokens n-identifiers n-numbers (+ 1 n-others))))))))

(define (count-tokens tokens iterations)
  (if (> iterations 1)
      (let* ((counts (count-tokens tokens (- iterations 1)))
             (counts2 (category-counts tokens)))
        (if (equal? counts counts2)
            counts
            (map (lambda (x) 0) counts)))
      (category-counts tokens)))

(define (go input-file iterations)
  (count-tokens (tokens-from-file input-file)
                iterations))

(define (main)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 input1)
         (name "charset"))
    (run-r7rs-benchmark
     (string-append name ":" s2)
     1 ; because the count is passed to go
     (lambda () (go (hide count input1) (hide count count)))
     (lambda (result) (equal? result output)))))
