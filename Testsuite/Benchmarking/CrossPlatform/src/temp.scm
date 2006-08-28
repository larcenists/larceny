; Hacked to estimate cost of one closed call per character.

(define transcoder 'usual)

(define (slow-read-char in)
  (if (eq? transcoder 'usual)
      (read-char in)
      (read-char (car (vector->list (make-vector 3 in))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Reading benchmarks.
;
; The timed portion of read-characters-from-file-port-benchmark
; uses read-char to read nboyer.sch 1000 times, performing
; file i/o each time.
;
; The timed portion of read-from-file-port-benchmark
; parses nboyer.sch 1000 times, performing file i/o
; each time.
;
; The timed portion of read-from-string-port-benchmark
; parses the string representation of nboyer.sch 1000 times.
;
; The output of that parse is checked by comparing it
; the the value returned by the read procedure.
;
; Usage:
;     (read-from-file-port-benchmark n input)
;     (read-from-string-port-benchmark n input)
;
; n defaults to 1000, and input defaults to "nboyer.sch".
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-characters-from-file-benchmark . rest)
  (let* ((n (if (null? rest) 1000 (car rest)))
         (input (if (or (null? rest) (null? (cdr rest)))
                    "nboyer.sch"
                    (cadr rest)))
         (benchmark-name
          (string-append "chario:file:" input ":" (number->string n))))
    (run-benchmark benchmark-name
                   n
                   (lambda ()
                     (call-with-input-file
                      input
                      (lambda (in)
                        (do ((x (slow-read-char in) (slow-read-char in))
                             (n 0 (+ n 1)))
                            ((eof-object? x) n)))))
                   (lambda (x) #t))))

(define (read-characters-from-string-benchmark . rest)
  (let* ((n (if (null? rest) 1000 (car rest)))
         (input (if (or (null? rest) (null? (cdr rest)))
                    "nboyer.sch"
                    (cadr rest)))
         (input-string (read-file-as-string input))
         (benchmark-name
          (string-append "chario:string:" input ":" (number->string n))))
    (run-benchmark benchmark-name
                   n
                   (lambda ()
                     (let ((in (open-input-string input-string)))
                       (do ((x (slow-read-char in) (slow-read-char in))
                            (n 0 (+ n 1)))
                           ((eof-object? x) n))))
                   (lambda (x) #t))))

(define (read-from-file-benchmark . rest)
  (let* ((n (if (null? rest) 1000 (car rest)))
         (input (if (or (null? rest) (null? (cdr rest)))
                    "nboyer.sch"
                    (cadr rest)))
         (answer (call-with-input-file
                  input
                  (lambda (in)
                    (do ((x (read in) (read in))
                         (answer '() x))
                        ((eof-object? x)
                         answer)))))
         (benchmark-name
          (string-append "reading:file:" input ":" (number->string n))))
    (run-benchmark benchmark-name
                   n
                   (lambda ()
                     (call-with-input-file
                      input
                      (lambda (in)
                        (do ((x (read in) (read in))
                             (y #f x))
                            ((eof-object? x) y)))))
                   (lambda (x) (equal? x answer)))))

(define (read-from-string-benchmark . rest)
  (let* ((n (if (null? rest) 1000 (car rest)))
         (input (if (or (null? rest) (null? (cdr rest)))
                    "nboyer.sch"
                    (cadr rest)))
         (input-string (read-file-as-string input))
         (answer (call-with-input-file
                  input
                  (lambda (in)
                    (do ((x (read in) (read in))
                         (answer '() x))
                        ((eof-object? x)
                         answer)))))
         (benchmark-name
          (string-append "reading:string:" input ":" (number->string n))))
    (run-benchmark benchmark-name
                   n
                   (lambda ()
                     (let ((in (open-input-string input-string)))
                       (do ((x (read in) (read in))
                            (y #f x))
                           ((eof-object? x) y))))
                   (lambda (x) (equal? x answer)))))

(define (read-file-as-string name)
  (call-with-input-file
   name
   (lambda (in)
     (do ((x (read-char in) (read-char in))
          (chars '() (cons x chars)))
         ((eof-object? x)
          (list->string (reverse chars)))))))

(define (main . args)
  (run-benchmark
    "slow-read-chars-from-string"
    1
    (lambda ()
      (read-characters-from-string-benchmark reading-iters))
    (lambda (result) #t)))
