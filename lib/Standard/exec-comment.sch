; Even more interesting comment facilities.
; 2000-09-30 / lth
;
; The #! ... !# comment recognized at the beginning of the file only
; is common in some Scheme systems and allow Scheme programs to be used
; as scripts on Unix systems.  For example, 
;
;  #! /bin/sh
;  larceny -heap my.heap -args "$0" "$@"
;  !#
;
; The facility in this file recognizes that construct, at the beginning 
; of the file, as a comment.
;
; Semantics: the comment is equivalent to exactly one space.
;
; Two complications:
;
; (1) Larceny already recognizes #! as a prefix for special system
; constants (eg #!unspecified).  The conflict is resolved in favor of
; Larceny: if #! is followed by a letter, then the reader assumes that
; it is not the start of a comment but instead the start of a system
; constant name.  In practice, this is good enough.
;
; (2) Two characters of lookahead are required to resolve the conflict
; just described, and the I/O system only provides one character of
; lookahead.  The existing sharp reader therefore cannot be used to
; parse constants introduced by the #! prefix, so there is a reader
; in this file that recognizes those #! constants recognized normally.
; Obviously, the present solution is brittle in the face of changes 
; to Larceny.

(let* ((sharp-reader (readtable-ref #\#))
       (sharp-class (car sharp-reader))
       (sharp-dispatch (cadr sharp-reader))
       (sharp-dispatch-list (caddr sharp-reader))
       (space-reader (readtable-ref #\space))
       (space-dispatch (cadr space-reader))
       (space-dispatch-list (caddr space-reader)))

  (define (read-comment p)
    (let ((c (peek-char p)))
      (if (eqv? c #\!)
          (begin
            (read-char p)
            (let ((c (peek-char p)))
              (if (and (= (port-position p) 2)
                       (not (char-alphabetic? c)))
                  (begin
                    (read-char p)
                    (let loop ((c (read-char p)))
                      (cond ((eof-object? c)
                             (error "EOF in #!...!# comment."))
                            ((char=? c #\!)
                             (let ((c (read-char p)))
                               (if (eqv? c #\#)
                                   #t
                                   (loop c))))
                            (else
                             (loop (read-char p))))))
                  (let ((s (read p)))
                    (case s
                      ((unspecified) '#!unspecified)
                      ((undefined)   '#!undefined)
                      ((null)        '())
                      ((true)        #t)
                      ((false)       #f)
                      (else
                       (error "Unknown #! constant: " s)))))))
          #f)))

  (define (new-sharp-dispatch c p)
    (if (read-comment p)
        (space-dispatch #\space p)
        (sharp-dispatch c p)))

  (define (new-sharp-dispatch-list c p)
    (if (read-comment p)
        (space-dispatch-list #\space p)
        (sharp-dispatch-list c p)))

  (readtable-set! #\# (list sharp-class
                            new-sharp-dispatch
                            new-sharp-dispatch-list))
  'sharp-bang-comments)

; eof
