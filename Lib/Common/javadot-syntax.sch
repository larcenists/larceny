;;; -*- Mode: Scheme; coding: iso-8859-1 -*-

($$trace "javadot-syntax")

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

         (define (trailing-dot-class?)
           (and (> length 6)
                (char=? (string-ref string (- length 6)) #\.)
                (char=? (string-ref string (- length 5)) #\c)
                (char=? (string-ref string (- length 4)) #\l)
                (char=? (string-ref string (- length 3)) #\a)
                (char=? (string-ref string (- length 2)) #\s)
                (char=? (string-ref string (- length 1)) #\s)))

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

         (or (dot-dollar?)
             (dot-dollar-sharp?)
             (set-dot-dollar-excl?)
             (set-dot-dollar-sharp-excl?)
             (leading-dot?)
             (trailing-dot-sharp?)
             (trailing-dot?)
             (trailing-dollar-sharp?)
             (trailing-dollar?)
             (trailing-dot-class?)
             (embedded-dot?)))))
