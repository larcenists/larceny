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
    (isNotNewline
     (lambda (c)
       (and (char? c)
            (not (char=? c (integer->char 10)))  ; linefeed
            (not (char=? c (integer->char 13)))  ; return
            (not (char=? c (integer->char 133))) ; Next Line, NEL, #\x85
            (not
             (char=? c (integer->char 8232)))))) ; Line Separator, LS, #\x2028
    (isNotVLineOrBackslash
     (lambda (c)
       (and (char? c)
            (not (char=? c #\|))
            (not (char=? c #\\)))))
    (isLeftDoubleAngleQuote
     (lambda (c)
       (and (char? c)
            (char=? c (integer->char #x00ab)))))
    (isRightDoubleAngleQuote
     (lambda (c)
       (and (char? c)
            (char=? c (integer->char #x00bb)))))
    (isNotRightDoubleAngleQuote
     (lambda (c)
       (and (char? c)
            (not (char=? c (integer->char #x00bb))))))
    (isNotDoublequote
     (lambda (c)
       (and (char? c)
            (char<=? c (integer->char 127))
            (not (char=? c #\")))))
    (isZsZlZp
     (lambda (c)
       (and (char? c) (char-whitespace? c))))
    (isNdMcMe
     (lambda (c)
       (and (char? c)
            (let ((cat (char-general-category c)))
              (memq cat '(Nd Mc Me))))))
    (isZeroWidthSubsequent
     (lambda (c)
       (and (char? c)
            (<= #x200c (char->integer c) #x200d))))
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

     ; #!r6rs is a comment (R6RS only).
     ; #!fold-case and #!no-fold-case are comments (R7RS only).
     ; In both R6RS and R7RS, implementations may support
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

     (boolean (! (#\# (! #\t #\T))
                 (#\# (! #\f #\F))
                 (#\# (! #\t #\T)
                      (! #\r #\R)
                      (! #\u #\U)
                      (! #\e #\E))
                 (#\# (! #\f #\F)
                      (! #\a #\A)
                      (! #\l #\L)
                      (! #\s #\S)
                      (! #\e #\E))))

     ; Scheme numbers are complicated.
     ; This simplifies by grouping binary, octal, and hexadecimal.

     (number (! ; decimal

                ((! ((! () (#\# (! #\d #\D)))
                     (! () (#\# (! #\i #\I #\e #\E))))
                    ((! () (#\# (! #\i #\I #\e #\E)))
                     (! () (#\# (! #\d #\D)))))
                 (! ((! ((! () #\+ #\-)
                         (! ((%0..9 (* %0..9)
                                    (* #\#))                  ; not R7RS
                             (! ()
                                (#\/ (%0..9 (* %0..9)
                                            (* #\#)))))       ; not R7RS
                            (; <decimal 10>
                             ((! (%0..9 (* %0..9)
                                        (* #\#))              ; not R7RS
                                 (#\. %0..9 (* %0..9)
                                            (* #\#))          ; not R7RS
                                 (%0..9 (* %0..9)
                                        #\.
                                        (* %0..9)
                                        (* #\#))              ; not R7RS
                                 (%0..9 (* %0..9)
                                        (* #\#) #\. (* #\#))) ; not R7RS
                              (! ()
                                 ((! #\e #\E
                                     #\s #\S                  ; not R7RS
                                     #\f #\F                  ; not R7RS
                                     #\d #\D                  ; not R7RS
                                     #\l #\L)                 ; not R7RS
                                  (! () #\+ #\-)
                                  %0..9 (* %0..9))))
                             (! ()
                                (#\| %0..9 (* %0..9))))))     ; R6RS only
                        ;; If no exactness or radix prefix
                        ;; is present, then these will look
                        ;; like R7RS peculiar identifiers.
                        ((! #\+ #\-)
                         (! (((! #\n #\N)
                              (! #\a #\A)
                              (! #\n #\N)) #\. #\0)
                            (((! #\i #\I)
                              (! #\n #\N)
                              (! #\f #\F)) #\. #\0))))
                     (! ()
                        (#\@ (! ((! () #\+ #\-)
                                 (! ((%0..9 (* %0..9)
                                     (* #\#))                 ; R5RS/IEEE only
                                     (! ()
                                        (#\/
                                         (%0..9 (* %0..9)
                                                (* #\#)))))   ; R5RS/IEEE only
                                    (; <decimal 10>
                                     ((! (%0..9 (* %0..9)
                                                (* #\#))      ; R5RS/IEEE only
                                         (#\. %0..9 (* %0..9)
                                                    (* #\#))  ; R5RS/IEEE only
                                         (%0..9 (* %0..9)
                                                #\. (* %0..9)
                                                    (* #\#))  ; R5RS/IEEE only
                                         (%0..9 (* %0..9)
                                                (* #\#)       ; R5RS/IEEE only
                                                #\.
                                                (* #\#)))     ; R5RS/IEEE only
                                      (! ()
                                         ((! #\e #\E
                                             #\s #\S #\f #\F  ; not R7RS
                                             #\d #\D #\l #\L) ; not R7RS
                                          (! () #\+ #\-)
                                          %0..9 (* %0..9))))
                                     (! ()
                                        (#\| %0..9 (* %0..9))))))  ; R6RS only
                                ((! #\+ #\-)
                                 (! (((! #\n #\N)
                                      (! #\a #\A)
                                      (! #\n #\N)) #\. #\0)
                                    (((! #\i #\I)
                                      (! #\n #\N)
                                      (! #\f #\F)) #\. #\0)))))
                        ((! #\+ #\-)
                         (! (! ((%0..9 (* %0..9)
                                       (* #\#))               ; not R7RS
                                (! ()
                                   (#\/ (%0..9 (* %0..9)
                                               (* #\#)))))    ; not R7RS
                               (; <decimal 10>
                                ((! (%0..9 (* %0..9)
                                           (* #\#))           ; not R7RS
                                    (#\. %0..9 (* %0..9)
                                               (* #\#))       ; not R7RS
                                    (%0..9 (* %0..9)
                                           #\.
                                           (* %0..9)
                                           (* #\#))           ; not R7RS
                                    (%0..9 (* %0..9)
                                           (* #\#) #\. (* #\#))) ; not R7RS
                                 (! ()
                                    ((! #\e #\E
                                        #\s #\S               ; not R7RS
                                        #\f #\F               ; not R7RS
                                        #\d #\D               ; not R7RS
                                        #\l #\L)              ; not R7RS
                                     (! () #\+ #\-)
                                     %0..9 (* %0..9))))
                                (! () (#\| %0..9 (* %0..9)))))
                            (! (((! #\n #\N)
                                 (! #\a #\A)
                                 (! #\n #\N)) #\. #\0)
                               (((! #\i #\I)
                                 (! #\n #\N)
                                 (! #\f #\F)) #\. #\0))
                            ())
                         (! #\i #\I))))
                    ((! #\+ #\-)
                     (! (! ((%0..9 (* %0..9)
                                   (* #\#))                   ; not R7RS
                            (! ()
                               (#\/ (%0..9 (* %0..9)
                                           (* #\#)))))        ; not R7RS
                           (; <decimal 10>
                            ((! (%0..9 (* %0..9)
                                       (* #\#))               ; not R7RS
                                (#\. %0..9 (* %0..9)
                                           (* #\#))           ; not R7RS
                                (%0..9 (* %0..9)
                                       #\.
                                       (* %0..9)
                                       (* #\#))               ; not R7RS
                                (%0..9 (* %0..9)
                                       (* #\#) #\. (* #\#)))  ; not R7RS
                             (! ()
                                ((! #\e #\E
                                    #\s #\S                   ; not R7RS
                                    #\f #\F                   ; not R7RS
                                    #\d #\D                   ; not R7RS
                                    #\l #\L)                  ; not R7RS
                                 (! () #\+ #\-)
                                 %0..9 (* %0..9))))
                            (! ()
                               (#\| %0..9 (* %0..9)))))       ; R6RS only
                        (! (((! #\n #\N)
                             (! #\a #\A)
                             (! #\n #\N)) #\. #\0)
                           (((! #\i #\I)
                             (! #\n #\N)
                             (! #\f #\F)) #\. #\0))
                        ())
                     (! #\i #\I))))

                ; binary, octal, hexadecimal

                ((! ((#\# (! #\b #\B #\o #\O #\x #\X))
                     (! () (#\# (! #\i #\I #\e #\E))))
                    ((! () (#\# (! #\i #\I #\e #\E)))
                     (#\# (! #\b #\B #\o #\O #\x #\X))))
                 (! ((! ((! () #\+ #\-)
                         (((! %0..9 %a..f %A..F)
                           (* (! %0..9 %a..f %A..F))
                           (* #\#))                           ; not R7RS
                          (! ()
                             (#\/ ((! %0..9 %a..f %A..F)
                                   (* (! %0..9 %a..f %A..F))
                                   (* #\#))))))               ; not R7RS
                        ((! #\+ #\-)
                         (! (((! #\n #\N)
                              (! #\a #\A)
                              (! #\n #\N)) #\. #\0)
                            (((! #\i #\I)
                              (! #\n #\N)
                              (! #\f #\F)) #\. #\0))))
                     (! ()
                        (#\@ (! ((! () #\+ #\-)
                                 (((! %0..9 %a..f %A..F)
                                   (* (! %0..9 %a..f %A..F))
                                   (* #\#))                   ; not R7RS
                                  (! ()
                                     (#\/ ((! %0..9 %a..f %A..F)
                                           (* (! %0..9 %a..f %A..F))
                                           (* #\#))))))       ; not R7RS
                                ((! #\+ #\-)
                                 (! (((! #\n #\N)
                                      (! #\a #\A)
                                      (! #\n #\N)) #\. #\0)
                                    (((! #\i #\I)
                                      (! #\n #\N)
                                      (! #\f #\F)) #\. #\0)))))
                        ((! #\+ #\-)
                         (! (((! %0..9 %a..f %A..F)
                              (* (! %0..9 %a..f %A..F))
                              (* #\#))                        ; not R7RS
                             (! ()
                                (#\/ ((! %0..9 %a..f %A..F)
                                      (* (! %0..9 %a..f %A..F))
                                      (* #\#)))))             ; not R7RS
                            (((! #\n #\N)
                              (! #\a #\A)
                              (! #\n #\N)) #\. #\0)
                            (((! #\i #\I)
                              (! #\n #\N)
                              (! #\f #\F)) #\. #\0)
                            ())
                         (! #\i #\I))))
                    ((! #\+ #\-)
                     (! (((! %0..9 %a..f %A..F)
                          (* (! %0..9 %a..f %A..F))
                             (* #\#))                         ; not R7RS
                         (! ()
                            (#\/ ((! %0..9 %a..f %A..F)
                                  (* (! %0..9 %a..f %A..F))
                                  (* #\#)))))                 ; not R7RS
                        (((! #\n #\N)
                          (! #\a #\A)
                          (! #\n #\N)) #\. #\0)
                        (((! #\i #\I)
                          (! #\n #\N)
                          (! #\f #\F)) #\. #\0)
                        ())
                     (! #\i #\I))))))

     ; Explicitly listing #\nul et cetera would increase
     ; the size of the state machine.  In any case, the
     ; scanner has to check for a delimiter following the
     ; character.

     (character (#\#
                 #\\
                 (! (#\x (! %0..9 %a..f %A..F) (* (! %0..9 %a..f %A..F)))
                    ((! %a..z %A..Z) (* (! %a..z %A..Z)))
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
     (lbracket #\[)                                           ; R6RS only
     (rbracket #\])                                           ; R6RS only
     (vecstart (#\# #\())
     (bvecstart (! (#\# #\v #\u #\8 #\()                      ; R6RS only
                   (#\# #\u #\8 #\()))                        ; R7RS only
     (quote #\')
     (backquote #\`)
     (comma #\,)
     (splicing (#\, #\@))
     (period #\.)
     (syntax (#\# #\'))                                       ; R6RS only
     (quasisyntax (#\# #\`))                                  ; R6RS only
     (unsyntax (#\# #\,))                                     ; R6RS only
     (unsyntaxsplicing (#\# #\, #\@))                         ; R6RS only

     ; Extensions for SRFI 38 and R7RS.

     (sharingdef (#\# %0..9 (* %0..9) #\=))                   ; R7RS only
     (sharinguse (#\# %0..9 (* %0..9) #\#))                   ; R7RS only

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
     ; vertical lines, which are treated as ordinary characters;
     ; Twobit and .fasl files rely on this.
     ;
     ; Larceny has allowed vertical lines at the beginning and
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

     ;; R7RS lexical syntax
     ;;
     ;; <identifier>  ::=  <initial> <subsequent>*
     ;;                 |  <vline> <symbol element>* <vline>
     ;;                 |  <peculiar identifier>
     ;;
     ;; <peculiar>  ::=  <explicit sign>
     ;;               |  <explicit sign> <sign subsequent> <subsequent>*
     ;;               |  <explicit sign> . <dot subsequent> <subsequent>*
     ;;               |  . <dot subsequent> <subsequent>*
     ;;
     ;; <dot subsequent>  ::=  <sign subsequent>
     ;;                     |  .
     ;;
     ;; which is equivalent to
     ;;
     ;; <identifier>  ::=  <identifier-prefix> <subsequent>*
     ;;                 |  <vline> <symbol element>* <vline>
     ;;                 |  <explicit sign>
     ;;
     ;; <identifier-prefix>  ::=  <initial>
     ;;                        |  <explicit sign> <sign subsequent>
     ;;                        |  <explicit sign> . <sign subsequent>
     ;;                        |  <explicit sign> . .
     ;;                        |  . <sign subsequent>
     ;;                        |  . .
     ;;
     ;; For R6RS, we add -> as another <identifier-prefix>.

     (id (! ;; peculiar identifiers of R5RS, R6RS, and R7RS
            #\+
            #\-

            ;; peculiar identifier of R6RS and R7RS
            (#\. #\. #\.)

            ;; peculiar identifiers of Larceny only, for historical reasons

            (#\- #\-)                                                    ;  --
            (#\- #\1 #\+)                                                ; -1+
            (#\1 #\+)                                                    ;  1+
            (#\1 #\-)                                                    ;  1-

            ((! ;; <identifier-prefix> is one of

                ;; <initial>: a letter, special initial, or inline hex escape

                ;; letter
                %a..z %A..Z isOtherConstituent
                ;; special initial
                #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~

                ;; special initial in R7RS (see errata) and Larceny only:
                #\@

                ;; R6RS and Larceny only:
                ;; inline hex escape

                (#\\ #\x (! %0..9 %a..f %A..F)
                         (* (! %0..9 %a..f %A..F)) #\;)

                ;; R7RS and Larceny only:
                ;; <explicit sign> <sign subsequent>
                ;; <explicit sign> . <sign subsequent>
                ;; . <sign subsequent>

                ((! #\+ #\-
                    ((! #\+ #\-) #\.)
                    #\.)
                 ;; <sign subsequent>  ::=  <initial> | <explicit sign> | @
                 (! %a..z %A..Z isOtherConstituent
                    #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                    #\@
                    #\+ #\-))

                ;; R7RS and Larceny only:
                ;; <explicit sign> . .
                ;; . .

                (! (#\+ #\. #\.)
                   (#\- #\. #\.)
                   (#\. #\.))

                ;; R6RS has another peculiar prefix
                (#\- #\>))                                    ; R6RS only

             (* (! ;; <subsequent> is one of
             
                   %a..z %A..Z
                   isOtherConstituent
                   isZeroWidthSubsequent
                   #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~

                   ;; R7RS allows inline hex escapes only with leading vline
                   ;; R6RS and Larceny allow inline hex escapes anywhere

                   (#\\ #\x (! %0..9 %a..f %A..F)
                            (* (! %0..9 %a..f %A..F)) #\;)

                   %0..9
                   isNdMcMe
                   isZeroWidthSubsequent                      ; R7RS only
                   #\+ #\- #\. #\@

                   ;; Larceny only

                   #\|

                   (#\\ isAscii)                            ; backslash escape
                   (#\# #\%))))                          ; MzScheme randomness
            
            ;; R7RS and Larceny only
            ;; <vline> <symbol element>* <vline>

            (#\|

             (* (! isNotVLineOrBackslash
                   
                   (#\\ #\x (! %0..9 %a..f %A..F)
                            (* (! %0..9 %a..f %A..F)) #\;)

                   (#\\ #\a)
                   (#\\ #\b)
                   (#\\ #\t)
                   (#\\ #\n)
                   (#\\ #\r)

                   (#\\ #\\)
                   (#\\ #\")
                   (#\\ #\|)))

             #\|)))

     ; Larceny allows any character to be preceded by a backslash.

     (string (#\"
              (* (! (! #\linefeed #\return (#\return #\linefeed)
                       isNel (#\return isNel) isLS)
                    (#\\ (! #\a #\b #\t #\n #\v #\f #\r #\" #\\
                            isLeftDoubleAngleQuote isRightDoubleAngleQuote
                            (! #\linefeed #\return (#\return #\linefeed)
                               isNel (#\return isNel) isLS)
                            #\space
                            (#\x (! %0..9 %a..f %A..F)
                                 (* (! %0..9 %a..f %A..F)))
                            isChar))                 ; anything can be escaped
                    isNotDoubleQuote))
              #\"))

     ; Same as string except for opening and closing quotes.

     (text (isLeftDoubleAngleQuote
            (* (! (! #\linefeed #\return (#\return #\linefeed)
                     isNel (#\return isNel) isLS)
                  (#\\ (! #\a #\b #\t #\n #\v #\f #\r #\" #\\
                          isLeftDoubleAngleQuote isRightDoubleAngleQuote
                          (! #\linefeed #\return (#\return #\linefeed)
                             isNel (#\return isNel) isLS)
                          #\space
                          (#\x (! %0..9 %a..f %A..F)
                               (* (! %0..9 %a..f %A..F)))
                          isChar))                 ; anything can be escaped
                  isNotRightDoubleAngleQuote))
            isRightDoubleAngleQuote))

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
