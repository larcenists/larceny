; Copyright 2007 William D Clinger
;
; $Id$
;
; Tokens for R5.94RS Scheme plus Larceny-specific extensions.
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

; Regular expressions for the lexical tokens of R5.93RS.
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

     (miscflag (#\# #\! (! %a..z %A..Z) (* (! %a..z %A..Z %0..9 #\-))))

     ; Scheme identifiers are complicated.

     ; FIXME: This is superseded further down.

;    (id (! ((! ; constituent
;               (! %a..z %A..Z isOtherConstituent)
;               ; special initial
;               (! #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~)
;                ; inline hex escape
;               (#\\ #\x (! %0..9 %a..f %A..F)
;                        (* (! %0..9 %a..f %A..F)) #\;))
;            (* (! (! %a..z %A..Z isOtherConstituent)
;                  (! #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~)
;                  (#\\ #\x (! %0..9 %a..f %A..F)
;                           (* (! %0..9 %a..f %A..F)) #\;)
;                  %0..9 #\+ #\- #\. #\@ isNdMcMe)))
;           ; peculiar identifiers
;           #\+ #\- (#\. #\. #\.)
;           (#\- #\>
;            (* (! (! %a..z %A..Z isOtherConstituent)
;                  (! #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~)
;                  (#\\ #\x (! %0..9 %a..f %A..F) (* (! %0..9 %a..f %A..F)))
;                  %0..9 #\+ #\- #\. #\@ isNdMcMe)))))

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
                         #\i)))
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
                     #\i)))

                ; binary, octal, hexadecimal

                ((! ((#\# (! #\b #\B #\o #\O #\x #\X))
                     (! () (#\# (! #\i #\I #\e #\E))))
                    ((! () (#\# (! #\i #\I #\e #\E)))
                     (#\# (! #\b #\B #\o #\O #\x #\X))))
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
                    (%a..z (* %a..z))
                    isChar)))

     ; FIXME: This is superseded further down.

;    (string (#\"
;             (* (! (! #\linefeed #\return (#\return #\linefeed)
;                      isNel (#\return isNel) isLS)
;                   (#\\ (! #\a #\b #\t #\n #\v #\f #\r #\" #\\
;                           (! #\linefeed #\return (#\return #\linefeed)
;                              isNel (#\return isNel) isLS)
;                           #\space
;                           (#\x (! %0..9 %a..f %A..F)
;                                (* (! %0..9 %a..f %A..F)))))
;                   isChar))
;             #\"))

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

     ; Larceny-specific extensions.

     ; Larceny has allowed identifiers to start with a dot;
     ; both Twobit and the JavaDot notation rely on this.
     ;
     ; Larceny has allowed identifiers to start with an @ sign;
     ; Twobit currently relies on this.
     ;
     ; Larceny has allowed identifiers to start with +: or -:
     ; and Twobit currently relies on this.
     ;
     ; Larceny has allowed identifiers to contain embedded
     ; vertical bars, which are treated as ordinary characters;
     ; Twobit and .fasl files rely on this.
     ;
     ; Larceny has allowed vertical bars at the beginning and
     ; end of a symbol, which escape the entire symbol.
     ; Parts of Common Larceny may rely on this.
     ;
     ; Larceny has also allowed backslashes to escape random
     ; Ascii characters within symbols.
     ;
     ; Larceny has allowed four other peculiar identifiers.
     ;
     ; Larceny v0.93 also allows some MzScheme weirdness
     ; whose purpose I don't understand.

     (id (! ((! ; constituent
                %a..z %A..Z isOtherConstituent
                ; special initial
                #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                ; inline hex escape
                (#\\ #\x (! %0..9 %a..f %A..F)
                         (* (! %0..9 %a..f %A..F)) #\;)
                (#\\ isAscii)                               ; backslash escape
                (#\# #\%)                                ; MzScheme randomness
                #\. #\@ #\|                 ; leading dot or @ or vertical bar
                (#\+ #\:)                                         ; leading +:
                (#\- #\:)                                         ; leading -:
                ; peculiar prefix
                (#\- #\>))
             (* (! %a..z %A..Z isOtherConstituent
                   #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                   (#\\ #\x (! %0..9 %a..f %A..F)
                            (* (! %0..9 %a..f %A..F)) #\;)
                   (#\\ isAscii)                            ; backslash escape
                   (#\# #\%)                             ; MzScheme randomness
                   #\|                     ; embedded or trailing vertical bar
                   %0..9 #\+ #\- #\. #\@ isNdMcMe)))
            ; peculiar identifiers
            #\+ #\- (#\. #\. #\.)
            (#\- #\-)                                                    ;  --
            (#\- #\1 #\+)                                                ; -1+
            (#\1 #\+)                                                    ;  1+
            (#\1 #\-)                                                    ;  1-
            ))

     ; Larceny allows any character to be preceded by a backslash.

     (string (#\"
              (* (! (! #\linefeed #\return (#\return #\linefeed)
                       isNel (#\return isNel) isLS)
                    (#\\ (! #\a #\b #\t #\n #\v #\f #\r #\" #\\
                            (! #\linefeed #\return (#\return #\linefeed)
                               isNel (#\return isNel) isLS)
                            #\space
                            (#\x (! %0..9 %a..f %A..F)
                                 (* (! %0..9 %a..f %A..F)))
                            isChar))                 ; anything can be escaped
                    isChar))
              #\"))

     ; #^B #^C #^F #^G #^P (used in .fasl files)

     (xfaslb (#\# isCtrlB))                                         ; #^B"..."
     (xfaslc (#\# isCtrlC))                              ; #^Cxxxxxxxxxxxxxxxx
     (xfaslf (#\# isCtrlF))                                      ; #^Fxxxxxxxx
     (xfaslg (#\# isCtrlG))                                        ; #^Gsymbol
     (xfaslp (#\# isCtrlP))                                         ; #^P(...)

     ; #! ... !# comments (see lib/Standard/exec-comment.sch)
     ; These are hand-coded in the scanner so the parser doesn't see them.

     (xcomment (#\# #\!))

     ; #.(...) read-time evaluation (see lib/Standard/sharp-dot.sch)

     (xsharpdot (#\# #\.))

     ; #&... (see lib/Standard/box.sch)
     ; Turns into (box ...).

     (xbox (#\# #\&))

     ; #"..." Ascii string syntax of MzScheme

     (xstring (#\# #\" (* (! isAsciiNotDoubleQuote (#\\ #\") (#\\ #\\))) #\"))

     )))