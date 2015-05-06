;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme file) procedures:
;;;
;;;     call-with-input-file
;;;     call-with-output-file
;;;     with-input-from-file
;;;     with-output-to-file
;;;     open-input-file
;;;     open-output-file
;;;     open-binary-input-file
;;;     open-binary-output-file
;;;
;;;     file-exists?
;;;     delete-file


(define-library (tests scheme file)
  (export run-file-tests)
  (import (scheme base)
          (scheme file)
          (tests scheme test))

  (cond-expand
   ((library (scheme read))
    (import (scheme read)))
   ((not (library (scheme read)))
    (begin (define (read . args) #t))))

  (cond-expand
   ((library (scheme write))
    (import (scheme write)))
   ((not (library (scheme write)))
    (begin (define (display . args) #t)
           (define (write . args) #t)
           (define (write-shared . args) #t)
           (define (write-simple . args) #t))))

  (cond-expand
   ((and (library (scheme read))
         (library (scheme write)))
    (begin (define okay-to-read-and-write? #t)))
   ((not (and (library (scheme read))
              (library (scheme write))))
    (begin (define okay-to-read-and-write? #f))))


  (begin

   (define tempfile "io-tmp2")

   (define (run-file-tests)

     (test/unspec (if (file-exists? tempfile)
                      (delete-file tempfile)))

     (test (file-exists? tempfile) #f)

     (test (call-with-output-file tempfile output-port-open?)
           #t)

     (test (file-exists? tempfile) #t)

     (test (call-with-input-file tempfile input-port-open?)
           #t)

     (test (call-with-input-file tempfile read-char)
           (eof-object))

     (test (with-input-from-file tempfile (lambda () (read-char)))
           (eof-object))

     (test/unspec (delete-file tempfile))

     (test (file-exists? tempfile) #f)

     (test/unspec (with-output-to-file
                   tempfile
                   (lambda ()
                    (string-for-each write-char "panurgic\nfamulus\n"))))

     (test (let* ((p (open-input-file tempfile))
                  (w1 (read-line p))
                  (w2 (read-line p))
                  (w3 (read-line p))
                  (b (input-port-open? p)))
             (close-input-port p)
             (list w1 w2 w3 b (input-port-open? p)))
           (list "panurgic" "famulus" (eof-object) #t #f))

     (test/unspec (delete-file tempfile))

     (test/unspec (call-with-output-file
                   tempfile
                   (lambda (q)
                     (string-for-each (lambda (c) (write-char c q))
                                      "rocky road\nto Dublin\n"))))

     (test ((call-with-input-file
             tempfile
             (lambda (p)
               (let* ((w1 (read-line p))
                      (w2 (read-line p))
                      (w3 (read-line p))
                      (b (input-port-open? p)))
                 (lambda ()
                   (list w1 w2 w3 b (input-port-open? p)))))))
           (list "rocky road" "to Dublin" (eof-object) #t #f))

     (test/unspec (delete-file tempfile))

     (test/unspec (call-with-output-file
                   tempfile
                   (lambda (q)
                     (string-for-each
                      (lambda (c) (write-char c q))
                      "The thirty years' truce\n...lasted fourteen years\n"))))

     (test ((with-input-from-file
             tempfile
             (lambda ()
               (let* ((w1 (read-line))
                      (w2 (read-line))
                      (w3 (read-line))
                      (p (current-input-port))
                      (b (input-port-open? p)))
                 (lambda ()
                   (list w1 w2 w3 b (input-port-open? p)))))))
           (list "The thirty years' truce"
                 "...lasted fourteen years"
                 (eof-object)
                 #t
                 #f))

     (test/unspec (delete-file tempfile))

     ;; FIXME: the R7RS allows binary ports to be textual as well.

     (test (let* ((p (open-binary-output-file tempfile))
                  (flags (map (lambda (f) (f p))
                              (list input-port?
                                    output-port?
;                                   textual-port?    ; textual is allowed
                                    binary-port?
                                    port?
                                    input-port-open?
                                    output-port-open?))))
             (for-each (lambda (b) (write-u8 b p))
                       '(3 2 1 0 255 254 129 128 127 10 13 32))
             (close-output-port p)
             (cons (output-port-open? p) flags))
           '(#f #f #t #;#f #t #t #f #t))             ; textual is allowed

     (test (let* ((p (open-binary-input-file tempfile))
                  (flags (map (lambda (f) (f p))
                              (list input-port?
                                    output-port?
;                                   textual-port?    ; textual is allowed
                                    binary-port?
                                    port?
                                    input-port-open?
                                    output-port-open?)))
                  (bv (read-bytevector 1000 p)))
             (close-input-port p)
             (list (cons (input-port-open? p) flags) bv))
           '((#f #t #f #;#f #t #t #t #f)             ; textual is allowed
             #u8(3 2 1 0 255 254 129 128 127 10 13 32)))

     (test/unspec (delete-file tempfile))

     (if okay-to-read-and-write?
         (run-file-tests-assuming-read-and-write)))

   (define (run-file-tests-assuming-read-and-write)

     ;; Modified from Racket tests/r6rs/io/simple.sls

     (test/unspec
      (when (file-exists? tempfile)
        (delete-file tempfile)))

     (test/values (call-with-output-file tempfile
                    (lambda (p)
                      (test (output-port? p) #t)
                      (test (binary-port? p) #f)
                      (test (textual-port? p) #t)
                      (test/unspec (write-char #\q p))
                      (test/unspec (newline p))
                      (test/unspec (display "more" p))
                      (test/unspec (write "last" p))
                      (values 3 4)))
                  3 4)

     (test/values (call-with-input-file tempfile
		    (lambda (p)
		      (test (input-port? p) #t)
		      (test (binary-port? p) #f)
		      (test (textual-port? p) #t)
		      (test (peek-char p) #\q)
		      (test (read-char p) #\q)
		      (test (read-char p) #\newline)
		      (test (read-char p) #\m)
		      (test (read-char p) #\o)
		      (test (peek-char p) #\r)
		      (test (read-char p) #\r)
		      (test (read-char p) #\e)
		      (test (read p) "last")
		      (test (read p) (eof-object))
		      (values 7 8 9)))
                  7 8 9)

    (test/unspec (delete-file tempfile))

    (let ((p (open-output-file tempfile)))
      (test (output-port? p) #t)
      (test (binary-port? p) #f)
      (test (textual-port? p) #t)
      (test/unspec (write-char #\! p))
      (test/unspec (close-output-port p)))

    (let ((p (open-input-file tempfile)))
      (test (input-port? p) #t)
      (test (binary-port? p) #f)
      (test (textual-port? p) #t)
      (test (read-char p) #\!)
      (test/unspec (close-input-port p)))

    (test/unspec (delete-file tempfile))

    (test/values (with-output-to-file tempfile
                   (lambda ()
                     (test/unspec (write-char #\z))
                     (test/unspec (newline))
                     (test/unspec (display "a"))
                     (test/unspec (write "a"))
                     (values 30 40)))
                 30 40)

    (test/values (with-input-from-file tempfile
                   (lambda ()
                     (test (peek-char) #\z)
                     (test (read-char) #\z)
                     (test (read) 'a)
                     (test (read) "a")
                     (test (read) (eof-object))
                     (values 70 80 90)))
                 70 80 90)

    (test/unspec
     (when (file-exists? tempfile)
       (delete-file tempfile)))
    
    (test (input-port? (current-input-port)) #t)
    (test (binary-port? (current-input-port)) #f)
    (test (textual-port? (current-input-port)) #t)

    (test (output-port? (current-output-port)) #t)
;   (test (binary-port? (current-output-port)) #f)   ; binary is allowed
    (test (textual-port? (current-output-port)) #t)

    (test (output-port? (current-error-port)) #t)
;   (test (binary-port? (current-error-port)) #f)    ; binary is allowed
    (test (textual-port? (current-error-port)) #t)

    ;;
    )))

