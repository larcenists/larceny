;;; -*- Mode: Scheme; coding: iso-8859-1 -*-

($$trace "javadot-syntax")

;; Could be changed to ".type" if warranted.
(define javadot-type-suffix
  (make-parameter "javadot-type-suffix" ".class" string?))

;; For dotnet 2.0
(define javadot-generic-suffix
  (make-parameter "javadot-generic-suffix" " ..no such suffix.." string?))

(define (javadot-syntax? symbol)
;; The reader calls javadot-syntax? when deciding to create symbols or
;; javadot symbols.
  (and (symbol? symbol)
       (let* ((string (symbol->string symbol))
              (length (string-length string)))

         (define (leading? char)
           ;; Return #t if the symbol begins with CHAR.
           (and (>= length 1)
                (char=? (string-ref string 0) char)))

         (define (trailing? char)
           ;; Return #t if the symbol ends with CHAR.
           (and (>= length 1)
                (char=? (string-ref string (- length 1)) char)))

         (define (trailing2? penultimate ultimate)
           ;; Return #t if the string ends with the two characters.
           (and (>= length 2)
                (char=? (string-ref string (- length 2)) penultimate)
                (char=? (string-ref string (- length 1)) ultimate)))

         (define (leading-dot?)           (leading? #\.))

         (define (trailing-dollar?)       (trailing? #\$))

         (define (trailing-dot?)          (trailing? #\.))

         (define (trailing-sharp?)        (trailing? #\#))

         (define (trailing-dot-sharp?)    (trailing2? #\. #\#))

         (define (trailing-dollar-sharp?) (trailing2? #\$ #\#))

         (define (dot-dollar?) (and (leading-dot?) (trailing-dollar?)))

         (define (dot-dollar-sharp?) (and (leading-dot?) (trailing-dollar-sharp?)))

         (define (set-dollar-excl?)
           ;; Return #T if the symbol begins with SET-  ends
           ;; with $!, and has a dot somewhere inside.
           ;; These symbols are created by the SETF! macro.
           ;; Kinda gross.
           (and (> length 6)
                (char=? (string-ref string 0) #\s)
                (char=? (string-ref string 1) #\e)
                (char=? (string-ref string 2) #\t)
                (char=? (string-ref string 3) #\-)
                (char=? (string-ref string (- length 2)) #\$)
                (char=? (string-ref string (- length 1)) #\!)))

         (define (set-dollar-sharp-excl?)
           ;; Return #T if the string begins with SET-  and ends
           ;; with $#!
           ;; These strings are created by the SETF! macro.
           ;; Kinda gross.
           (and (> length 7)
                (char=? (string-ref string 0) #\s)
                (char=? (string-ref string 1) #\e)
                (char=? (string-ref string 2) #\t)
                (char=? (string-ref string 3) #\-)
                (char=? (string-ref string (- length 3)) #\$)
                (char=? (string-ref string (- length 2)) #\#)
                (char=? (string-ref string (- length 1)) #\!)))

         (define (set-dot-dollar-excl?)
           ;; Return #T if the symbol begins with SET-.  and ends
           ;; with $!
           ;; These symbols are created by the SETF! macro.
           ;; Kinda gross.
           (and (> length 7)
                (char=? (string-ref string 0) #\s)
                (char=? (string-ref string 1) #\e)
                (char=? (string-ref string 2) #\t)
                (char=? (string-ref string 3) #\-)
                (char=? (string-ref string 4) #\.)
                (char=? (string-ref string (- length 2)) #\$)
                (char=? (string-ref string (- length 1)) #\!)))

         (define (set-dot-dollar-sharp-excl?)
           ;; Return #T if the string begins with SET-.  and ends
           ;; with $#!
           ;; These strings are created by the SETF! macro.
           ;; Kinda gross.
           (and (> length 8)
                (char=? (string-ref string 0) #\s)
                (char=? (string-ref string 1) #\e)
                (char=? (string-ref string 2) #\t)
                (char=? (string-ref string 3) #\-)
                (char=? (string-ref string 4) #\.)
                (char=? (string-ref string (- length 3)) #\$)
                (char=? (string-ref string (- length 2)) #\#)
                (char=? (string-ref string (- length 1)) #\!)))

         (define (trailing-suffix? suffix other)
           (let ((suffix-length (string-length suffix))
                 (other-length (string-length other)))
             (and (> other-length suffix-length)
                  (let loop ((suffix-scan 0)
                             (other-scan (- other-length suffix-length)))
                    (cond ((= suffix-scan suffix-length) #t)
                          ((char=? (char-downcase (string-ref suffix suffix-scan))
                                   (char-downcase (string-ref other other-scan)))
                           (loop (+ suffix-scan 1) (+ other-scan 1)))
                          (else #f))))))

        (define (trailing-dot-class? string)
          (trailing-suffix? (javadot-type-suffix) string))

        (define (trailing-dot-generic? string)
          (trailing-suffix? (javadot-generic-suffix) string))

         (define (embedded-dot?)
           ;; return #T if the `symbol' has embedded dots
           ;; leading and trailing dots are not allowed, however.
           (and (> length 2)
                (not (char=? (string-ref string 0) #\.))
                (not (char=? (string-ref string (- length 1)) #\.))
                (let loop ((scan 1))
                  (cond ((>= scan length) #f)
                        ((char=? (string-ref string scan) #\.) #t)
                        (else (loop (+ scan 1)))))))

         (or ;(dot-dollar?)
             ;(dot-dollar-sharp?)
             ;(set-dot-dollar-excl?)
             ;(set-dot-dollar-sharp-excl?)
             (leading-dot?)
             ;(trailing-dot-sharp?)
             (trailing-dot?)
             ;(and (set-dollar-sharp-excl? text) (embedded-dot? text))
             ;(and (set-dollar-excl? text) (embedded-dot? text))
             ;(and (trailing-dollar-sharp? text) (embedded-dot? text))
             ;(and (trailing-dollar text) (embedded-dot? text))
             ;(trailing-dot-class?)
             (embedded-dot?)
             ))))
