($$trace "unicode2")

; Copyright 2006 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (proto-unicode2)
;
; This file contains an implementation of the official Unicode
; word-breaking algorithm, as best I could figure it out from
; Unicode Standard Annex #29, version 5.0.0.
;
; The tables in this file were generated from the
; Unicode Character Database, revision 5.0.0.
; The same goes for the hard-code constants.
;
; This file does not rely on any lexical syntax for
; non-Ascii characters and strings.

;(library (proto-unicode2)
;  (export
;
;    string-next-word-break
;    string-previous-word-break)
;
;  (import (r6rs base)
;          (r6rs bytevector)
;          (proto-unicode0)
;          (proto-unicode1))

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
; My original implementation of those specifications failed
; 6 of the 494 tests in auxiliary/WordBreakTest.txt, but it
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
;
; Here is Will's translation of those rules (including WB3a)
; into a finite state machine that searches forward within a
; string, looking for the next position at which a word break
; is allowed.  The current state consists of an index i into
; the string and a summary of the left context whose rightmost
; character is at index i.  The left context is usually
; determined by the character at index i, but there are three
; complications:
;
;     Extend and Format characters are ignored unless they
;         follow a separator or the beginning of the text.
;     ALetter followed by MidLetter is treated specially.
;     Numeric followed by MidNum is treated specially.
;
; In the implementation below, the left context ending at i
; is encoded by the following symbols:
;
;     CR
;     Sep (excluding CR)
;     ALetter
;     MidLetter
;     ALetterMidLetter (ALetter followed by MidLetter)
;     Numeric
;     MidNum
;     NumericMidNum (Numeric followed by MidNum)
;     Katakana
;     ExtendNumLet
;     other (none of the above)
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
         ((Sep)
          (if (char=? c #\return) 'CR cat))
         ((MidLetter)
          (let ((i-1 (- i 1)))
            (if (and (<= 0 i-1) (eq? (left-context i-1) 'ALetter))
                'ALetterMidLetter
                cat)))
         ((MidNum)
          (let ((i-1 (- i 1)))
            (if (and (<= 0 i-1) (eq? (left-context i-1) 'Numeric))
                'NumericMidNum
                cat)))
         ((ExtendOrFormat)
          (if (< 0 i) (left-context (- i 1)) 'other))
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

    ; Given j and the context to the left of (not including) j,
    ; returns the index at the start of the next word
    ; (or before which a word break is permitted).

    (define (loop j context)
      (if (>= j n)
          (case context
           ((ALetterMidLetter NumericMidNum)
            (let ((j (index-of-previous-non-ignored n)))
              (if (< i j) j n)))
           (else n))
          (let* ((c (string-ref s j))
                 (cat (char-word-break-category c)))
            (case cat
             ((ExtendOrFormat)
              (case context
               ((CR Sep) j)
               (else (loop (+ j 1) context))))
             (else
              (case context
               ((CR)
                (if (char=? c #\linefeed)
                    (loop (+ j 1) cat)
                    j))
               ((ALetter)
                (case cat
                 ((ALetter Numeric ExtendNumLet)
                  (loop (+ j 1) cat))
                 ((MidLetter)
                  (loop (+ j 1) 'ALetterMidLetter))
                 (else j)))
               ((ALetterMidLetter)
                (case cat
                 ((ALetter) (loop (+ j 1) cat))
                 (else
                  (let ((j2 (index-of-previous-non-ignored j)))
                    (if (< i j2) j2 j)))))
               ((Numeric)
                (case cat
                 ((Numeric ALetter ExtendNumLet)
                  (loop (+ j 1) cat))
                 ((MidNum)
                  (loop (+ j 1) 'NumericMidNum))
                 (else j)))
               ((NumericMidNum)
                (case cat
                 ((Numeric) (loop (+ j 1) cat))
                 (else
                  (let ((j2 (index-of-previous-non-ignored j)))
                    (if (< i j2) j2 j)))))
               ((MidLetter MidNum) j)
               ((Katakana)
                (case cat
                 ((Katakana ExtendNumLet) (loop (+ j 1) cat))
                 (else j)))
               ((ExtendNumLet)
                (case cat
                 ((ExtendNumLet ALetter Numeric Katakana)
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
        ((char-katakana? c) 'Katakana)
        ((char-extendnumlet? c) 'ExtendNumLet)
        ((char-extend? c) 'ExtendOrFormat)
        ((char-format? c) 'ExtendOrFormat)
        ((memv (char->integer c)
               '(#x0a #x0d #x85 #x2028 #x2029))
         ; Sep = { line feed, carriage return, next line,
         ;         line separator, paragraph separator }
         'Sep)
        (else 'other)))

; FIXME:
; Hard-coding knowledge of Control into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were computed by
;     (decision-strategy (extract-control))
;
; FIXME:  This doesn't seem to be used anywhere.

(define (char-control? c)
  (case (char-general-category c)
   ((Zl Zp) #t)
   ((Cc)
    (case (char->integer c)
     ; #\return and #\linefeed are not considered control
     ; characters by the word-breaking algorithm in UAX #29
     ((#x000d #x000a) #f)
     (else #t)))
   ((Cf)
    (case (char->integer c)
     ; ZWNJ and ZWJ are not considered control characters
     ; by the word-breaking algorithm in UAX #29
     ((#x200c #x200d) #f)
     (else #t)))
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
            ((<= #xdf2 sv #xa827) #f)
            ((<= #x1d166 sv #x1d16d) #f)
            (else #t))))
   (else
    (case (char->integer c)
     ((#x200c #x200d) #t)
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
     ((#x200c #x200d) #f)
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
   ((Lm)
    (let ((sv (char->integer c)))
      (cond ((<= #x2b0 sv #x3005) #f)
            ((<= #x303b sv #x309e) #f)
            ((<= #xa015 sv #xa71a) #f)
            (else #t))))
   ((Lo)
    (let ((sv (char->integer c)))
      (cond ((<= #x1bb sv #x309f) #f)
            ((<= #x3105 sv #x31b7) #f)
            ((<= #x3400 sv #xfefc) #f)
            ((<= #xffa0 sv #x2fa1d) #f)
            (else #t))))
   (else
    (case (char->integer c)
     ((#x309b #x309c #x30a0) #t)
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
            (cond ((<= #xe01 sv #xedd) #f)
                  ((<= #x1000 sv #x1055) #f)
                  ((<= #x1780 sv #x17dc) #f)
                  ((<= #x1950 sv #x19c7) #f)
                  ((= sv #x3006) #f)
                  ((<= #x3041 sv #x30ff) #f)
                  ((<= #x31f0 sv #x9fbb) #f)
                  ((<= #xf900 sv #xfa2d) #f)
                  ((<= #xfa70 sv #xfad9) #f)
                  ((<= #xff66 sv #xff9d) #f)
                  ((<= #x20000 sv #x2fa1d) #f)
                  (else #t))))         
         ((Lm)
          (let ((sv (char->integer c)))
            (cond ((<  sv #xe46) #t)
                  ((<= #xe46 sv #xec6) #f)
                  ((=  sv #x17d7) #f)
                  ((<= #x3031 sv #x3035) #f)
                  ((<= #x309d sv #x30fe) #f)
                  ((<= #xff70 sv #xff9f) #f)
                  (else #t))))
         ((Mc)
          (let ((sv (char->integer c)))
            (cond ((<  sv #x9be) #t)
                  ((memv sv '(#x9be #x9d7 #xb3e #xbd7 #xcc2
                              #xd3e #xd57 #xdcf #xddf))
                   #f)
                  ((<= #xb57 sv #xbbe) #f)
                  ((<= #xcd5 sv #xcd6) #f)
                  ((<= #xf3e sv #xf3f) #f)
                  ((<= #x102c sv #x17c8) #f)
                  ((<= #x19b0 sv #x19c9) #f)
                  ((<= #x1b44 sv #xa802) #f)
                  ((<= #x1d165 sv #x1d172) #f)
                  (else #t))))
         ((Nl)
          (let ((sv (char->integer c)))
            (cond ((<= #x3007 sv #x303a) #f)
                  (else #t))))
         (else
          (let ((sv (char->integer c)))
            (or (= sv #x5f3)
                (<= #x24b6 sv #x24e9))))))))

; FIXME:
; Hard-coding knowledge of MidLetter into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were computed by
;     (decision-strategy (extract-midletter))

(define (char-midletter? c)
  (case (char->integer c)
   ((#x27 #x3a #xb7 #x5f4 #x2019 #x2027) #t)
   (else #f)))

; FIXME:
; Hard-coding knowledge of MidNum into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were obtained by
; inspection of WordBreakProperty.txt

(define (char-midnum? c)
  (case (char->integer c)
   ((#x002c #x002e #x003b #x037e #x0589 #x060d #x07f8 #x2044
     #xfe10 #xfe13 #xfe14)
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

;)
