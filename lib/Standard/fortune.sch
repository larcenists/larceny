; 2000-08-29 / lth
; $Id$
;
; TXT format:
;   Each item is terminated by a line with only "%%" on it.
;
; DB format:
;   First item is a number: number of entries following.
;   Following items are lists of strings.  This is gross but works OK.
;
; BUG: Should check if dat file is writeable before trying to regenerate,
;      but no way to do that in Larceny right now (could catch exception).
;
; BUG: The seek code below is a crock.  Larceny needs seekable streams.

(require 'random)
(require 'file-system)
(require 'io)

(define dat-file (string-append (current-larceny-root) "/lib/Standard/" "fortune.dat"))
(define txt-file (string-append (current-larceny-root) "/lib/Standard/" "fortune.txt"))

(define fortune
  (let ((random  #f)
        (entries 0)
        (short-cutoff 5))

    (define (regenerate-if-missing-or-updated)
      (if (or (not (file-exists? dat-file))
              (and (file-exists? txt-file)
                   (file-exists? dat-file)
                   (file-newer? txt-file dat-file)))
          (begin
            (set! random #f)
            (fortune/regenerate-dat-file))))

    (define (get)
      (call-with-input-file dat-file
        (lambda (p)
          (let ((new-entries (read p)))
            (if (or (not random)
                    (not (= new-entries entries)))
                (begin
                  (set! random (make-random-generator))
                  (set! entries new-entries)))
	    (let ((n (random new-entries)))
	      (do ((i 0 (+ i 1)))
		  ((= i n))
		(read p))
	      (read p))))))

    (define (writeln x)
      (display x)
      (newline))

    (define (get-short)
      (let loop ((f (get)))
        (if (> (length f) short-cutoff)
            (loop (get))
            f)))

    (define (get-long)
      (let loop ((f (get)))
        (if (<= (length f) short-cutoff)
            (loop (get))
            f)))

    (lambda args
      (regenerate-if-missing-or-updated)
      (if (not (file-exists? dat-file))
          (error "FORTUNE: file " dat-file " is missing."))
      (let ((short? (memq 'short args))
            (long? (memq 'long args)))
        (cond (short?
               (for-each writeln (get-short)))
              (long?
               (for-each writeln (get-long)))
              (else
               (for-each writeln (get))))))))

(define (fortune/regenerate-dat-file)
  (display "; Regenerating fortune.dat...")
  (newline)
  (call-with-input-file txt-file
    (lambda (in)
      (call-with-output-file dat-file
        (lambda (out)

          (define texts '())

          (define (writeln x)
            (write x out)
            (newline out))

          (define (loop text)
            (let ((l (read-line in)))
              (cond ((eof-object? l))
                    ((string=? l "%%")
                     (set! texts (cons (reverse text) texts))
                     (loop '()))
                    (else
                     (loop (cons l text))))))

          (loop '())
          (writeln (length texts))
          (for-each writeln texts))))))

; eof
