; Copyright 2006 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright and permission notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This file contains an implementation of the official Unicode
; word-breaking algorithm, as best I could figure it out from
; Unicode Standard Annex #29, version 7.0.0.
;
; The tables in this file were generated from the
; Unicode Character Database, revision 7.0.0.
; The same goes for the hard-coded constants.
;
; This file does not rely on any lexical syntax for
; non-Ascii characters and strings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Word-breaking as defined by Unicode Standard Annex #29.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Implementation notes.
;
; The string-foldcase, string-downcase, and string-titlecase
; procedures rely on the notion of a word, which is defined
; by Unicode Standard Annex 29.
;
; The string-foldcase and string-downcase procedures rely on
; word boundaries only when they encounter a Greek sigma, so
; their performance should not be greatly affected by the
; performance of the word-breaking algorithm.
;
; The string-titlecase procedure must find all word boundaries,
; but it is typically used on short strings (titles).
;
; Hence the performance of the word-breaking algorithm should
; not matter too much for this reference implementation.
; Word-breaking is more generally useful, however, so I tried
; to make this implementation reasonably efficient.
;
; Word boundaries are defined by 14 different rules in
; Unicode Standard Annex #29, and by GraphemeBreakProperty.txt
; and WordBreakProperty.txt.  See also WordBreakTest.html.
;

; Historical note.
;
; My original implementation of those specifications for Unicode 5.0
; failed 6 of the 494 tests in auxiliary/WordBreakTest.txt, but it
; appeared to me that those tests were inconsistent with the
; word-breaking rules in UAX #29.  John Cowan forwarded my
; bug report to the Unicode experts, and Mark Davis responded
; on 29 May 2007:
;
;   Thanks for following up on this. I think you have found a problem in the
;   formulation of word break, not the test. The intention was to break after a
;   Sep character, as is done in Sentence break. So my previous suggestion was
;   incorrect; instead, what we need is a new rule:
; 
;   *Break after paragraph separators.*
;    WB3a. Sep ÷
;   I'll make a propose to the UTC for this.

; Here is Will's translation of the rules for Unicode 7.0.0
; into a finite state machine that searches forward within a
; string, looking for the next position at which a word break
; is allowed.  The current state consists of an index i into
; the string and a summary of the left context whose rightmost
; character is at index i.  The left context is usually
; determined by the character at index i, but there are four
; complications:
;
;     Extend and Format characters are ignored unless they
;         appear at the beginning of the text (rule WB4)
;         or follow something like a CR LF Newline (UAX #29 6.2).
;     Rule WB7.
;     Rule WB7c
;     Rule WB11.
;
; In the implementation below, the left context ending at i
; is encoded by the following symbols:
;
;   symbol                                              see rule
;   ------                                              --------
;     CR                                                   WB3
;     Hebrew                               WB5, WB6, WB7a, WB7b, WB9, WB13a
;     ALetter                              WB5, WB6,             WB9, WB13a
;     WB7 (left side of rule WB7)                          WB7
;     WB7c (left side of rule WB7c)                        WB7c
;     WB11 (left side of rule WB11)                        WB11
;     Numeric                                         WB8, WB10, WB12, WB13a
;     Katakana
;     ExtendNumLet                                         WB13a, WB13b
;     RegionalIndicator                                    WB13c
;     other (none of the above)                            WB14
;
; Given a string s and an exact integer i (which need not be
; a valid index into s), returns the index of the next character
; that is not part of the word containing the character at i,
; or the length of s if the word containing the character at i
; extends through the end of s.  If i is negative or a valid
; index into s, then the returned value will be greater than i.

(define (string-next-word-break s i)
  (let ((n (string-length s)))

    ; Given a valid index i into s, returns the left context at i.

    (define (left-context i)
      (let* ((c (string-ref s i))
             (cat (char-word-break-category c)))
        (case cat
         ((MidLetter)
          (let ((i-1 (- i 1)))
            (if (and (<= 0 i-1)
                     (memq (left-context i-1) '(ALetter Hebrew)))
                'WB7
                cat)))
         ((MidNum)
          (let ((i-1 (- i 1)))
            (if (and (<= 0 i-1) (eq? (left-context i-1) 'Numeric))
                'WB11
                cat)))
         ((MidNumLet SingleQuote)
          (let ((i-1 (- i 1)))
            (if (<= 0 i-1)
                (case (left-context i-1)
                 ((ALetter Hebrew) 'WB7)
                 ((Numeric) 'WB11)
                 (else cat))
                cat)))
         ((DoubleQuote)
          (let ((i-1 (- i 1)))
            (if (and (<= 0 i-1) (eq? (left-context i-1) 'Hebrew))
                'WB7c
                cat)))
         ((ExtendOrFormat)
          (if (< 0 i)
              (let ((context (left-context (- i 1))))
                (if (memq context '(CR LF Newline))
                    'ExtendOrFormat
                    context))
              'other))
         (else cat))))

    ; Returns the index of the last non-Extend, non-Format
    ; character within (substring s 0 j).  Should not be
    ; called unless such a character exists.

    (define (index-of-previous-non-ignored j)
      (let* ((j1 (- j 1))
             (c (string-ref s j1))
             (cat (char-word-break-category c)))
        (case cat
         ((ExtendOrFormat) (index-of-previous-non-ignored j1))
         (else j1))))

    ; Returns the index of the last character within (substring s 0 j)
    ; that has one of the given word break categories.  Should not be
    ; called unless such a character exists.

    (define (index-of-previous j categories)
      (let* ((j1 (- j 1))
             (c (string-ref s j1))
             (cat (char-word-break-category c)))
        (if (memq cat categories)
            j1
            (index-of-previous j1 categories))))

    ; Given j and the context to the left of (not including) j,
    ; returns the index at the start of the next word
    ; (or before which a word break is permitted).

    (define (loop j context)
      (if (>= j n)
          (case context
           ((WB7)
            (let ((j (index-of-previous n '(MidLetter MidNumLet SingleQuote))))
              (if (< i j) j n)))
           ((WB7c)
            (let ((j (index-of-previous n '(DoubleQuote))))
              (if (< i j) j n)))
           ((WB11)
            (let ((j (index-of-previous n '(MidNum MidNumLet SingleQuote))))
              (if (< i j) j n)))
           (else n))
          (let* ((c (string-ref s j))
                 (cat (char-word-break-category c)))
            (case cat
             ((ExtendOrFormat)
              (case context
               ((CR LF Newline) j)
               (else (loop (+ j 1) context))))
             (else
              (case context
               ((CR)
                (if (char=? c r7rs-linefeed-character)
                    (loop (+ j 1) cat)
                    j))
               ((Hebrew)
                (case cat
                 ((ALetter Hebrew SingleQuote Numeric ExtendNumLet)
                  (loop (+ j 1) cat))
                 ((MidLetter MidNumLet SingleQuote)
                  (loop (+ j 1) 'WB7))
                 ((DoubleQuote)
                  (loop (+ j 1) 'WB7c))
                 (else j)))
               ((ALetter)
                (case cat
                 ((ALetter Hebrew Numeric ExtendNumLet)
                  (loop (+ j 1) cat))
                 ((MidLetter MidNumLet SingleQuote)
                  (loop (+ j 1) 'WB7))
                 (else j)))
               ((WB7)
                (case cat
                 ((ALetter Hebrew) (loop (+ j 1) cat))
                 (else
                  (let ((j2 (index-of-previous
                             j '(MidLetter MidNumLet SingleQuote))))
                    (if (< i j2) j2 j)))))
               ((WB7c)
                (case cat
                 ((Hebrew) (loop (+ j 1) cat))
                 (else
                  (let ((j2 (index-of-previous j '(DoubleQuote))))
                    (if (< i j2) j2 j)))))
               ((WB11)
                (case cat
                 ((Numeric) (loop (+ j 1) cat))
                 (else
                  (let ((j2 (index-of-previous
                             j '(MidNum MidNumLet SingleQuote))))
                    (if (< i j2) j2 j)))))
               ((Numeric)
                (case cat
                 ((Numeric ALetter Hebrew ExtendNumLet)
                  (loop (+ j 1) cat))
                 ((MidNum MidNumLet SingleQuote)
                  (loop (+ j 1) 'WB11))
                 (else j)))
               ((Katakana)
                (case cat
                 ((Katakana ExtendNumLet) (loop (+ j 1) cat))
                 (else j)))
               ((ExtendNumLet)
                (case cat
                 ((ExtendNumLet ALetter Hebrew Numeric Katakana)
                  (loop (+ j 1) cat))
                 (else j)))
               ((RegionalIndicator)
                (case cat
                 ((RegionalIndicator)
                  (loop (+ j 1) cat))
                 (else j)))
               (else j)))))))

    (cond ((< i 0) 0)
          ((<= n i) n)
          (else (loop (+ i 1) (left-context i))))))

; Given a string s and an exact integer i (which need not be
; a valid index into s), returns the index of the character
; that begins the word that contains the character at i.
; If i is a valid index into s, then the returned value
; will be less than or equal to i.
;
; Algorithm:
;     Search backward for a character that is obviously part
;         of a previous word, or for the beginning of the string.
;     Search forward from there.
;     Repeat forward search as necessary.

(define (string-previous-word-break s i)
  (let ((n (string-length s)))

    (define (forwards j)
      (let ((k (string-next-word-break s j)))
        (if (<= i k)
            j
            (forwards k))))

    (if (>= i n)
        n
      (let backwards ((j i))
        (if (< j 0)
            (forwards 0)
            (let* ((c (string-ref s j))
                   (cat (char-word-break-category c)))
              (case cat
               ((other)
                (forwards j))
               (else
                (backwards (- j 1))))))))))

; Given a character, returns its word break property
; as a symbol.  Used by string-next-word-break.
;
; FIXME:  This should be bummed to avoid multiple calls
; to char-general-category.

(define (char-word-break-category c)
  (cond ((char-aletter? c) 'ALetter)
        ((char-midletter? c) 'MidLetter)
        ((char-numeric? c) 'Numeric)
        ((char-midnum? c) 'MidNum)
        ((char-midnumlet? c) 'MidNumLet)
        ((char-extendnumlet? c) 'ExtendNumLet)
        ((char-extend? c) 'ExtendOrFormat)
        ((char-format? c) 'ExtendOrFormat)
        ((char-hebrew? c) 'Hebrew)
        ((char-katakana? c) 'Katakana)
        ((char-regionalindicator? c) 'RegionalIndicator)
        (else
         (let ((sv (char->integer c)))
           (case sv
            ((#x0d) 'CR)
            ((#x0a) 'LF)
            ((#x0b #x0c #x85 #x2028 #x2029) 'Newline)
            ((#x27) 'SingleQuote)
            ((#x22) 'DoubleQuote)
            (else 'other))))))

; FIXME:
; Hard-coding knowledge of Hebrew_Letter into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were computed by
;     (decision-strategy (extract-hebrew))

(define (char-hebrew? c)
  (case (char-general-category c)
   ((Lo)
    (let ((sv (char->integer c)))
      (cond ((<= #xaa sv #x294) #f)
            ((<= #x620 sv #x9f8e) #f)
            ((<= #xfb50 sv #x2a600) #f)
            (else #t))))
   (else #f)))

; FIXME:
; Hard-coding knowledge of Extend into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were computed by
;     (decision-strategy (extract-extend))

(define (char-extend? c)
  (case (char-general-category c)
   ((Me Mn) #t)
   ((Mc)
    (let ((sv (char->integer c)))
      (cond ((<= #x903 sv #x983) #f)
            ((<= #x9bf sv #x9cc) #f)
            ((<= #xa03 sv #xb03) #f)
            ((<= #xb40 sv #xb4c) #f)
            ((<= #xbbf sv #xbcc) #f)
            ((<= #xc01 sv #xcc1) #f)
            ((<= #xcc3 sv #xccb) #f)
            ((<= #xd02 sv #xd03) #f)
            ((<= #xd3f sv #xd4c) #f)
            ((<= #xd82 sv #xd83) #f)
            ((<= #xdd0 sv #xdde) #f)
            ((<= #xdf2 sv #x1cf3) #f)
            ((<= #xa823 sv #x11303) #f)
            ((<= #x1133f sv #x1134d) #f)
            ((<= #x11362 sv #x11363) #f)
            ((<= #x114b1 sv #x114bc) #f)
            ((<= #x114be sv #x114c1) #f)
            ((<= #x115b0 sv #x16f7e) #f)
            ((<= #x1d166 sv #x1d16d) #f)
            (else #t))))
   (else
    (case (char->integer c)
     ((#x200c #x200d) #t)
     ((#xff9e #xff9f) #t)
     (else #f)))))

; FIXME:
; Hard-coding knowledge of Format into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were computed by
;     (decision-strategy (extract-format))

(define (char-format? c)
  (case (char-general-category c)
   ((Cf)
    (case (char->integer c)
     ((#x200b #x200c #x200d) #f)
     (else #t)))
   (else #f)))

; FIXME:
; Hard-coding knowledge of Katakana into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were computed by
;     (decision-strategy (extract-katakana))

(define (char-katakana? c)
  (case (char-general-category c)
   ((Lo)
    (let ((sv (char->integer c)))
      (cond ((<= #xaa sv #x309f) #f)
            ((<= #x3105 sv #x31ba) #f)
            ((<= #x3400 sv #xfefc) #f)
            ((<= #xffa0 sv #x16f50) #f)
            ((<= #x1b001 sv #x2a600) #f)
            (else #t))))
   ((So)
    (let ((sv (char->integer c)))
      (cond ((<= #xa6 sv #x32cf) #f)
            ((<= #x3358 sv #x1f8ad) #f)
            (else #t))))
   (else
    (case (char->integer c)
     ((#x3031 #x3032 #x3033 #x3034 #x3035
       #x309b #x309c
       #x30a0
       #x30fc #x30fd #x30fe
       #xff70)
      #t)
     (else #f)))))

; FIXME:
; Hard-coding knowledge of Aletter into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were computed by
;     (decision-strategy (extract-aletter))

(define (char-aletter? c)
  (if (< (char->integer c) 128)
      (or (and (char<=? #\a c) (char<=? c #\z))
          (and (char<=? #\A c) (char<=? c #\Z)))
      (let ((category (char-general-category c)))
        (case category
         ((Lu Ll Lt)
          #t)
         ((Lo)
          (let ((sv (char->integer c)))
            (cond ((<= #x5d0 sv #x5f2) #f)
                  ((<= #xe01 sv #xedf) #f)
                  ((<= #x1000 sv #x108e) #f)
                  ((<= #x1780 sv #x17dc) #f)
                  ((<= #x1950 sv #x19c7) #f)
                  ((<= #x1a20 sv #x1a54) #f)
                  ((= sv #x3006) #f)
                  ((<= #x3041 sv #x30ff) #f)
                  ((<= #x31f0 sv #x9fcc) #f)
                  ((<= #xa9e0 sv #xa9fe) #f)
                  ((<= #xaa60 sv #xaadc) #f)
                  ((<= #x8c48 sv #xfb4f) #f)
                  ((<= #xff66 sv #xff9d) #f)
                  ((<= #x1b000 sv #x1b001) #f)
                  ((<= #x20000 sv #x2a600) #f)
                  (else #t))))
         ((Lm)
          (let ((sv (char->integer c)))
            (cond ((<= #xe46 sv #xec6) #f)
                  ((= sv #x17d7) #f)
                  ((= sv #x1aa7) #f)
                  ((<= #x3031 sv #x3035) #f)
                  ((<= #x309d sv #x30fe) #f)
                  ((<= #xa9e6 sv #xaadd) #f)
                  ((<= #xff70 sv #xff9f) #f)
                  (else #t))))
         ((Nl)
          (let ((sv (char->integer c)))
            (cond ((<= #x3007 sv #x303a) #f)
                  (else #t))))
         (else
          (let ((sv (char->integer c)))
            (or (= sv #x5f3)
                (<= #x24b6 sv #x24e9)
                (<= #x1f130 sv #x1f149)
                (<= #x1f150 sv #x1f169)
                (<= #x1f170 sv #x1f189))))))))

; FIXME:
; Hard-coding knowledge of MidLetter into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were computed by
;     (decision-strategy (extract-midletter))

(define (char-midletter? c)
  (case (char->integer c)
   ((#x3a #xb7 #x2d7 #xb7 #x5f4 #x2027 #xfe13 #xfe55 #xff1a)
    #t)
   (else #f)))

; FIXME:
; Hard-coding knowledge of MidNum into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were obtained by
; inspection of WordBreakProperty.txt
;
; FIXME: (extract-midnum) returns the union of MidNum with MidNumLet.

(define (char-midnum? c)
  (case (char->integer c)
   ((#x002c #x003b #x037e #x0589 #x060c
     #x060d #x066c #x07f8 #x2044 #xfe10
     #xfe14 #xfe50 #xfe54 #xff0c #xff1b)
    #t)
   (else #f)))

; FIXME:
; Hard-coding knowledge of MidNum into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were computed by
;     (decision-strategy (extract-midnumlet))

(define (char-midnumlet? c)
  (case (char->integer c)
   ((#x2e #x2018 #x2019 #x2024 #xfe52 #xff07 #xff0e)
    #t)
   (else #f)))

; FIXME:
; Hard-coding knowledge of ExtendNumLet into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were computed by
;     (decision-strategy (extract-extendnumlet))

(define (char-extendnumlet? c)
  (eq? 'Pc (char-general-category c)))

; FIXME:
; Hard-coding knowledge of RegionalIndicator into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were obtained by
; inspection of UAX #29.

(define (char-regionalindicator? c)
  (<= #x1f1e6 (char->integer c) #x1f1ff))

