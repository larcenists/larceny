; Copyright 2007 William D Clinger
;
; $Id$
;
; Tokens for R6RS Scheme as extended by SRFI 38.
;
; This file is case-sensitive.
; It defines scheme_terminals, which is an input to LexGen.
; See makeLexer.sch for an example of its use.

; The scanner generator gives priority to explicit characters,
; so these predicates can include characters that have special
; meaning when they appear within the same regular expression.

(define character-classes
  '((isEOF eof-object?)
    (isChar char?)
    (isCtrlB
     (lambda (c)
       (and (char? c)
            (char=? c (integer->char 2)))))
    (isCtrlC
     (lambda (c)
       (and (char? c)
            (char=? c (integer->char 3)))))
    (isCtrlF
     (lambda (c)
       (and (char? c)
            (char=? c (integer->char 6)))))
    (isCtrlG
     (lambda (c)
       (and (char? c)
            (char=? c (integer->char 7)))))
    (isCtrlP
     (lambda (c)
       (and (char? c)
            (char=? c (integer->char 16)))))
    (isNel
     (lambda (c)
       (and (char? c)
            (char=? c (integer->char #x85)))))
    (isLS
     (lambda (c)
       (and (char? c)
            (char=? c (integer->char #x2028)))))
    (isAscii
     (lambda (c)
       (and (char? c)
            (char<=? c (integer->char 127)))))
    (isAsciiNotVbar
     (lambda (c)
       (and (char? c)
            (char<=? c (integer->char 127))
            (not (char=? c #\|)))))
    (isAsciiNotDoublequote
     (lambda (c)
       (and (char? c)
            (char<=? c (integer->char 127))
            (not (char=? c #\")))))
    (isNotNewline
     (lambda (c)
       (and (char? c)
            (not (char=? c (integer->char 10))))))
    (isZsZlZp
     (lambda (c)
       (and (char? c) (char-whitespace? c))))
    (isNdMcMe
     (lambda (c)
       (and (char? c)
            (let ((cat (char-general-category c)))
              (memq cat '(Nd Mc Me))))))
    (isOtherConstituent
     (lambda (c)
       (and (char? c)
            (> (char->integer c) 127)
            (let ((cat (char-general-category c)))
              (memq cat '(Lu Ll Lt Lm Lo Mn Nl No
                          Pd Pc Po Sc Sm Sk So Co))))))))

; Expands symbols like %a..z into (! #\a #\b ... #\z).
; FIXME: This notation assumes case-sensitive symbols.

(define (expand-ranges spec)
  (cond ((pair? spec)
         (cons (expand-ranges (car spec))
               (expand-ranges (cdr spec))))
        ((and (symbol? spec)
              (let ((s (symbol->string spec)))
                (and (= 5 (string-length s))
                     (char=? (string-ref s 0) #\%)
                     (char=? (string-ref s 2) #\.)
                     (char=? (string-ref s 3) #\.)
                     s)))
         =>
         (lambda (s)
           (let* ((c1 (string-ref s 1))
                  (c2 (string-ref s 4))
                  (n2 (char->integer c2)))
             (do ((i (char->integer c1) (+ i 1))
                  (chars '() (cons (integer->char i) chars)))
                 ((> i n2)
                  (cons '! (reverse chars)))))))
        (else spec)))

; Regular expressions for the lexical tokens of R6RS.
; Lists Ascii characters explicitly.
; Relies on character classes for all non-Ascii characters.

(define scheme_terminals
  (expand-ranges
   '(

     ; The scanner generator treats whitespace specially,
     ; so the most common kinds of <interlexeme space>
     ; are called whitespace here.

     (whitespace (! #\tab #\linefeed #\vtab #\page #\return #\space
                    isNel
                    isZsZlZp
                    (#\; (* isNotNewline))))

     ; #|...|# comments are hand-coded in the scanner.

     (comment (#\# #\|))

     ; #; <datum> comments are hand-coded in the scanner,
     ; which will handle them by calling the parser.

     (commentdatum (#\# #\;))

     ; In Scheme, an end of file generates a token.

     (eofobj isEOF)

     ; #!r6rs is a comment, but implementations may support
     ; similar flags with arbitrary semantics.

;    (miscflag (#\# #\! (! %a..z %A..Z) (* (! %a..z %A..Z %0..9 #\-))))

     (miscflag (#\# #\! #\r #\6 #\r #\s))

     ; Scheme identifiers are complicated.

     (id (! ((! ; constituent
                %a..z %A..Z isOtherConstituent
                ; special initial
                #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                ; inline hex escape
                (#\\ #\x (! %0..9 %a..f %A..F)
                         (* (! %0..9 %a..f %A..F)) #\;)
                ; peculiar prefix
                (#\- #\>))
             (* (! %a..z %A..Z isOtherConstituent
                   #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                   (#\\ #\x (! %0..9 %a..f %A..F)
                            (* (! %0..9 %a..f %A..F)) #\;)
                   %0..9 #\+ #\- #\. #\@ isNdMcMe)))
            ; peculiar identifiers
            #\+ #\- (#\. #\. #\.)
            ))

     (boolean (#\# (! #\t #\T #\f #\F)))

     ; Scheme numbers are complicated.
     ; This simplifies by grouping binary, octal, and hexadecimal.

     (number (! ; decimal

                ((! ((! () (#\# (! #\d #\D)))
                     (! () (#\# (! #\i #\I #\e #\E))))
                    ((! () (#\# (! #\i #\I #\e #\E)))
                     (! () (#\# (! #\d #\D)))))
                 (! ((! ((! () #\+ #\-)
                         (! ((%0..9 (* %0..9) (* #\#))
                             (! ()
                                (#\/ (%0..9 (* %0..9) (* #\#)))))
                            (; <decimal 10>
                             ((! (%0..9 (* %0..9) (* #\#))
                                 (#\. %0..9 (* %0..9) (* #\#))
                                 (%0..9 (* %0..9) #\. (* %0..9) (* #\#))
                                 (%0..9 (* %0..9) (* #\#) #\. (* #\#)))
                              (! ()
                                 ((! #\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
                                  (! () #\+ #\-)
                                  %0..9 (* %0..9))))
                             (! () (#\| %0..9 (* %0..9))))))
                        ((! #\+ #\-)
                         (! (#\n #\a #\n #\. #\0)
                            (#\i #\n #\f #\. #\0))))
                     (! ()
                        (#\@ (! ((! () #\+ #\-)
                                 (! ((%0..9 (* %0..9) (* #\#))
                                     (! ()
                                        (#\/ (%0..9 (* %0..9) (* #\#)))))
                                    (; <decimal 10>
                                     ((! (%0..9 (* %0..9) (* #\#))
                                         (#\. %0..9 (* %0..9) (* #\#))
                                         (%0..9 (* %0..9)
                                                #\. (* %0..9) (* #\#))
                                         (%0..9 (* %0..9) (* #\#) #\. (* #\#)))
                                      (! ()
                                         ((! #\e #\E #\s #\S #\f #\F
                                             #\d #\D #\l #\L)
                                          (! () #\+ #\-)
                                          %0..9 (* %0..9))))
                                     (! () (#\| %0..9 (* %0..9))))))
                                ((! #\+ #\-)
                                 (! (#\n #\a #\n #\. #\0)
                                    (#\i #\n #\f #\. #\0)))))
                        ((! #\+ #\-)
                         (! (! ((%0..9 (* %0..9) (* #\#))
                                (! ()
                                   (#\/ (%0..9 (* %0..9) (* #\#)))))
                               (; <decimal 10>
                                ((! (%0..9 (* %0..9) (* #\#))
                                    (#\. %0..9 (* %0..9) (* #\#))
                                    (%0..9 (* %0..9) #\. (* %0..9) (* #\#))
                                    (%0..9 (* %0..9) (* #\#) #\. (* #\#)))
                                 (! ()
                                    ((! #\e #\E #\s #\S #\f #\F
                                        #\d #\D #\l #\L)
                                     (! () #\+ #\-)
                                     %0..9 (* %0..9))))
                                (! () (#\| %0..9 (* %0..9)))))
                            (! (#\n #\a #\n #\. #\0)
                               (#\i #\n #\f #\. #\0))
                            ())
                         (! #\i #\I))))
                    ((! #\+ #\-)
                     (! (! ((%0..9 (* %0..9) (* #\#))
                            (! ()
                               (#\/ (%0..9 (* %0..9) (* #\#)))))
                           (; <decimal 10>
                            ((! (%0..9 (* %0..9) (* #\#))
                                (#\. %0..9 (* %0..9) (* #\#))
                                (%0..9 (* %0..9) #\. (* %0..9) (* #\#))
                                (%0..9 (* %0..9) (* #\#) #\. (* #\#)))
                             (! ()
                                ((! #\e #\E #\s #\S #\f #\F #\d #\D #\l #\L)
                                 (! () #\+ #\-)
                                 %0..9 (* %0..9))))
                            (! () (#\| %0..9 (* %0..9)))))
                        (! (#\n #\a #\n #\. #\0)
                           (#\i #\n #\f #\. #\0))
                        ())
                     (! #\i #\I))))

                ; binary

                ((! ((#\# (! #\b #\B))
                     (! () (#\# (! #\i #\I #\e #\E))))
                    ((! () (#\# (! #\i #\I #\e #\E)))
                     (#\# (! #\b #\B))))
                 (! ((! ((! () #\+ #\-)
                         (((! %0..1)
                           (* (! %0..1)) (* #\#))
                          (! ()
                             (#\/ ((! %0..1)
                                   (* (! %0..1)) (* #\#))))))
                        ((! #\+ #\-)
                         (! (#\n #\a #\n #\. #\0)
                            (#\i #\n #\f #\. #\0))))
                     (! ()
                        (#\@ (! ((! () #\+ #\-)
                                 (((! %0..1)
                                   (* (! %0..1)) (* #\#))
                                  (! ()
                                     (#\/ ((! %0..1)
                                           (* (! %0..1))
                                           (* #\#))))))
                                ((! #\+ #\-)
                                 (! (#\n #\a #\n #\. #\0)
                                    (#\i #\n #\f #\. #\0)))))
                        ((! #\+ #\-)
                         (! (((! %0..1)
                              (* (! %0..1)) (* #\#))
                             (! ()
                                (#\/ ((! %0..1)
                                      (* (! %0..1)) (* #\#)))))
                            (#\n #\a #\n #\. #\0)
                            (#\i #\n #\f #\. #\0)
                            ())
                         #\i)))
                    ((! #\+ #\-)
                     (! (((! %0..1)
                          (* (! %0..1)) (* #\#))
                         (! ()
                            (#\/ ((! %0..1)
                                  (* (! %0..1)) (* #\#)))))
                        (#\n #\a #\n #\. #\0)
                        (#\i #\n #\f #\. #\0)
                        ())
                     #\i)))

                ; octal

                ((! ((#\# (! #\o #\O))
                     (! () (#\# (! #\i #\I #\e #\E))))
                    ((! () (#\# (! #\i #\I #\e #\E)))
                     (#\# (! #\o #\O))))
                 (! ((! ((! () #\+ #\-)
                         (((! %0..7)
                           (* (! %0..7)) (* #\#))
                          (! ()
                             (#\/ ((! %0..7)
                                   (* (! %0..7)) (* #\#))))))
                        ((! #\+ #\-)
                         (! (#\n #\a #\n #\. #\0)
                            (#\i #\n #\f #\. #\0))))
                     (! ()
                        (#\@ (! ((! () #\+ #\-)
                                 (((! %0..7)
                                   (* (! %0..7)) (* #\#))
                                  (! ()
                                     (#\/ ((! %0..7)
                                           (* (! %0..7))
                                           (* #\#))))))
                                ((! #\+ #\-)
                                 (! (#\n #\a #\n #\. #\0)
                                    (#\i #\n #\f #\. #\0)))))
                        ((! #\+ #\-)
                         (! (((! %0..7)
                              (* (! %0..7)) (* #\#))
                             (! ()
                                (#\/ ((! %0..7)
                                      (* (! %0..7)) (* #\#)))))
                            (#\n #\a #\n #\. #\0)
                            (#\i #\n #\f #\. #\0)
                            ())
                         #\i)))
                    ((! #\+ #\-)
                     (! (((! %0..7)
                          (* (! %0..7)) (* #\#))
                         (! ()
                            (#\/ ((! %0..7)
                                  (* (! %0..7)) (* #\#)))))
                        (#\n #\a #\n #\. #\0)
                        (#\i #\n #\f #\. #\0)
                        ())
                     #\i)))

                ; hexadecimal

                ((! ((#\# (! #\x #\X))
                     (! () (#\# (! #\i #\I #\e #\E))))
                    ((! () (#\# (! #\i #\I #\e #\E)))
                     (#\# (! #\x #\X))))
                 (! ((! ((! () #\+ #\-)
                         (((! %0..9 %a..f %A..F)
                           (* (! %0..9 %a..f %A..F)) (* #\#))
                          (! ()
                             (#\/ ((! %0..9 %a..f %A..F)
                                   (* (! %0..9 %a..f %A..F)) (* #\#))))))
                        ((! #\+ #\-)
                         (! (#\n #\a #\n #\. #\0)
                            (#\i #\n #\f #\. #\0))))
                     (! ()
                        (#\@ (! ((! () #\+ #\-)
                                 (((! %0..9 %a..f %A..F)
                                   (* (! %0..9 %a..f %A..F)) (* #\#))
                                  (! ()
                                     (#\/ ((! %0..9 %a..f %A..F)
                                           (* (! %0..9 %a..f %A..F))
                                           (* #\#))))))
                                ((! #\+ #\-)
                                 (! (#\n #\a #\n #\. #\0)
                                    (#\i #\n #\f #\. #\0)))))
                        ((! #\+ #\-)
                         (! (((! %0..9 %a..f %A..F)
                              (* (! %0..9 %a..f %A..F)) (* #\#))
                             (! ()
                                (#\/ ((! %0..9 %a..f %A..F)
                                      (* (! %0..9 %a..f %A..F)) (* #\#)))))
                            (#\n #\a #\n #\. #\0)
                            (#\i #\n #\f #\. #\0)
                            ())
                         #\i)))
                    ((! #\+ #\-)
                     (! (((! %0..9 %a..f %A..F)
                          (* (! %0..9 %a..f %A..F)) (* #\#))
                         (! ()
                            (#\/ ((! %0..9 %a..f %A..F)
                                  (* (! %0..9 %a..f %A..F)) (* #\#)))))
                        (#\n #\a #\n #\. #\0)
                        (#\i #\n #\f #\. #\0)
                        ())
                     #\i)))))

     ; Explicitly listing #\nul et cetera would increase
     ; the size of the state machine.  In any case, the
     ; scanner has to check for a delimiter following the
     ; character.

     (character (#\#
                 #\\
                 (! (#\x (! %0..9 %a..f %A..F) (* (! %0..9 %a..f %A..F)))
                    ((! %a..z %A..Z) (* (! %a..z %A..Z)))
                    isChar)))

     (string (#\"
              (* (! (! #\linefeed #\return (#\return #\linefeed)
                       isNel (#\return isNel) isLS)
                    (#\\ (! #\a #\b #\t #\n #\v #\f #\r #\" #\\
                            (! #\linefeed #\return (#\return #\linefeed)
                               isNel (#\return isNel) isLS)
                            #\space
                            (#\x (! %0..9 %a..f %A..F)
                                 (* (! %0..9 %a..f %A..F)))))
                    isChar))
              #\"))

     (lparen #\()
     (rparen #\))
     (lbracket #\[)
     (rbracket #\])
     (vecstart (#\# #\())
     (bvecstart (#\# #\v #\u #\8 #\())
     (quote #\')
     (backquote #\`)
     (comma #\,)
     (splicing (#\, #\@))
     (period #\.)
     (syntax (#\# #\'))
     (quasisyntax (#\# #\`))
     (unsyntax (#\# #\,))
     (unsyntaxsplicing (#\# #\, #\@))

     ; Extensions for SRFI 38.

     (sharingdef (#\# %0..9 (* %0..9) #\=))
     (sharinguse (#\# %0..9 (* %0..9) #\#))

     )))