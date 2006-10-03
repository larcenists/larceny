; Test code for lib/format.sch
; 2002-09-28 / lth

; Currently missing tests for ~f

(require 'format)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(define (with-output-to-string thunk)
  (let ((s (open-output-string)))
    (with-output-to-port s thunk)
    (get-output-string s)))

(let ((x `("test" #\a a 33 33.0 #!unspecified ,(current-input-port))))
  (or (string=? (format #f "~a" x) 
                (with-output-to-string
                  (lambda ()
                    (display x))))
      (fail 'format:a:1)))
(or (string=? (format #f "~10a" 'foo) "foo       ")
    (fail 'format:a:2))
(or (string=? (format #f "~10@a" 'foo) "       foo")
    (fail 'format:a:3))

(let ((x `("test" #\a a 33 33.0 #!unspecified ,(current-input-port))))
  (or (and (string=? (format #f "~w" x) 
                     (format #f "~s" x))
           (string=? (format #f "~s" x)
                     (with-output-to-string
                       (lambda ()
                         (write x)))))
      (fail 'format:w:1)))
(or (string=? (format #f "~10w" #\a) "#\\a       ")
    (fail 'format:w:2))
(or (string=? (format #f "~10@w" #\a) "       #\\a")
    (fail 'format:w:3))

(or (string=? (format #f "~c" #\a) "a")
    (fail 'format:c:1))
(or (string=? (format #f "~c" (integer->char 12))
              (string (integer->char 12)))
    (fail 'format:c:2))

(let ((b (make-bytevector 4)))
  (bytevector-set! b 0 10)
  (bytevector-set! b 1 20)
  (bytevector-set! b 2 30)
  (bytevector-set! b 3 40)
  (or (string=? (format #f "~b" b) "10 20 30 40")
      (fail 'format:b:1))
  (or (string=? (format #f "~B" b) "a 14 1e 28")
      (fail 'format:b:2)))

(or (string=? (format #f "~~~%~%") (string #\~ #\newline #\newline))
    (fail 'format:noarg:1))

(or (string=? (format #f "~x" 32767) "7fff")
    (fail 'format:hex:1))

(or (string=? (format #f "~x" -32767) "-7fff")
    (fail 'format:hex:2))

(or (string=? (format #f "~o" -32767) "-77777")
    (fail 'format:octal:2))

(or (string=? (format #f "~a~?~a" 'foo "~x~o" '(32 32) 'bar) "foo2040bar")
    (fail 'format:recursive:1))
