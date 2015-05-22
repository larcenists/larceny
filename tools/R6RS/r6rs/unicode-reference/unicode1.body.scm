;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Procedures that operate on characters.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Case-insensitive comparisons.

(define char-ci=?
  (make-comparison-predicate
   (lambda (c1 c2)
     (char=? (char-foldcase c1) (char-foldcase c2)))))

(define char-ci<?
  (make-comparison-predicate
   (lambda (c1 c2)
     (char<? (char-foldcase c1) (char-foldcase c2)))))

(define char-ci>?
  (make-comparison-predicate
   (lambda (c1 c2)
     (char>? (char-foldcase c1) (char-foldcase c2)))))

(define char-ci<=?
  (make-comparison-predicate
   (lambda (c1 c2)
     (char<=? (char-foldcase c1) (char-foldcase c2)))))

(define char-ci>=?
  (make-comparison-predicate
   (lambda (c1 c2)
     (char>=? (char-foldcase c1) (char-foldcase c2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Simple (character-to-character) case conversions.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char-upcase c)
  (let ((n (char->integer c)))
    (cond ((<= #x61 n #x7a)
           (integer->char (- n #x20)))
          ((< n #xb5)
           c)
          ((<= #xe0 n #xfe)
           (integer->char (- n #x20)))
          ((= n #xb5)
           (integer->char #x039c))
          ((< n #xff)
           c)
          (else
           (let ((probe
                  (if (<= n #xffff)
                      (binary-search-16bit n simple-upcase-chars-16bit)
                      (binary-search n simple-upcase-chars-morebits))))
             (if probe
                 (integer->char
                  (+ n (vector-ref simple-case-adjustments
                                   (bytevector-u8-ref
                                    simple-upcase-adjustments
                                    probe))))
                 c))))))

(define (char-downcase c)
  (let ((n (char->integer c)))
    (cond ((<= #x41 n #x5a)
           (integer->char (+ n #x20)))
          ((< n #xc0)
           c)
          ((<= n #xde)
           (integer->char (+ n #x20)))
          ((< n #xff)
           c)
          (else
           (let ((probe
                  (if (<= n #xffff)
                      (binary-search-16bit n simple-downcase-chars-16bit)
                      (binary-search n simple-downcase-chars-morebits))))
             (if probe
                 (integer->char
                  (- n (vector-ref simple-case-adjustments
                                   (bytevector-u8-ref
                                    simple-downcase-adjustments
                                    probe))))
                 c))))))

; FIXME:
; Hard-coding the exceptional code points into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; The exceptional mappings are computed as follows:
;
; (let ((m-title (extract-simple-titlecase-mappings))
;       (m-upper (extract-simple-uppercase-mappings)))
;   (filter (lambda (m) (not (member m m-upper)))
;           m-title))

(define (char-titlecase c)
  (let ((n (char->integer c)))
    (cond ((< n #x01c4)
           (char-upcase c))
          ((> n #x01f3)
           (char-upcase c))
          (else
           (case n
             ((#x01c4 #x01c5 #x01c6)
              (integer->char #x01c5))
             ((#x01c7 #x01c8 #x01c9)
              (integer->char #x01c8))
             ((#x01ca #x01cb #x01cc)
              (integer->char #x01cb))
             ((#x01f1 #x01f2 #x01f3)
              (integer->char #x01f2))
             (else
              (char-upcase c)))))))

; ASCII characters are treated specially, making them about
; twice as fast as the general case.

(define (char-foldcase c)
  (let ((cp (char->integer c)))
    (if (< cp #xb5)
        (char-downcase c)
        (let ((i (binary-search cp simple-foldcase-exceptions)))
          (if i
              (integer->char (vector-ref simple-foldcase-mappings i))
              (char-downcase c))))))

; Given a character, returns its Unicode general category.
; The tables used to implement this procedure occupy about 12869 bytes.
; About a third of those bytes could be saved by splitting the large
; vector into a bytevector for the 16-bit scalar values and using a
; general vector only for the scalar values greater than 65535.

(define (char-general-category c)
  (let ((n (char->integer c)))
    (vector-ref
     vector-of-general-category-symbols
     (if (< n (bytevector-length
               general-category-indices-for-common-characters))
         (bytevector-u8-ref general-category-indices-for-common-characters n)
         (bytevector-u8-ref general-category-indices-for-all-characters
                       (binary-search-of-vector
                        n
                        vector-of-code-points-with-same-category))))))

; Given a character, returns true if and only if the character
; has the Alphabetic property, which is defined as
; Other_Alphabetic + Lu + Ll + Lt + Lm + Lo + Nl
;
; FIXME:
; Hard-coding knowledge of Other_Alphabetic into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded excluded ranges were computed by
;     (decision-strategy (extract-other-alphabetic))
; and inserted into the code by copy-and-paste
; and a couple of Emacs keyboard macros.

(define (char-alphabetic? c)
  (if (< (char->integer c) 128)
      (or (and (char<=? #\a c) (char<=? c #\z))
          (and (char<=? #\A c) (char<=? c #\Z)))
      (let ((category (char-general-category c)))
        (case category
         ((Lu Ll Lt Lm Lo Nl)
          #t)
         ((Mc)
          (let ((sv (char->integer c)))
            (cond ((<  sv #xf3e) #t)
                  ((<= #xf3e sv #xf3f) #f)
                  ((<= #x1063 sv #x1064) #f)
                  ((<= #x1069 sv #x106d) #f)
                  ((<= #x1087 sv #x109b) #f)
                  ((<= #x1bf2 sv #x1bf3) #f)
                  ((<= #x302e sv #x302f) #f)
                  ((<= #xaa7b sv #xaa7d) #f)
                  ((<= #x1d165 sv #x1d172) #f)
                  ((memv sv
                         '(#x1b44 #x1baa #x1ce1 #xa953 #xa9c0
                           #xabec #x111c0 #x11235 #x1134d #x116b6))
                   #f)
                  (else #t))))
         ((Mn)
          (let ((sv (char->integer c)))
            (cond ((<  sv #x300) #t)
                  ((<= #x300 sv #x344) #f)
                  ((<= #x346 sv #x5af) #f)
                  ((<= #x6df sv #x6e0) #f)
                  ((<= #x6ea sv #x6ec) #f)
                  ((<= #x740 sv #x74a) #f)
                  ((<= #x7eb sv #x7f3) #f)
                  ((<= #x818 sv #x819) #f)
                  ((<= #x82d sv #x85b) #f)
                  ((<= #x8ea sv #x8ef) #f)
                  ((<= #x94d sv #x954) #f)
                  ((<= #xe47 sv #xe4c) #f)
                  ((<= #xec8 sv #xecc) #f)
                  ((<= #xf18 sv #xf39) #f)
                  ((<= #xf82 sv #xf87) #f)
                  ((<= #x1037 sv #x103a) #f)
                  ((<= #x135d sv #x135e) #f)
                  ((<= #x17b4 sv #x17b5) #f)
                  ((<= #x17c9 sv #x180d) #f)
                  ((<= #x1939 sv #x193b) #f)
                  ((<= #x1a75 sv #x1abd) #f)
                  ((<= #x1b6b sv #x1b73) #f)
                  ((<= #x1c36 sv #x1de6) #f)
                  ((<= #x1df5 sv #x2d7f) #f)
                  ((<= #x302a sv #xa66f) #f)
                  ((<= #xa67c sv #xa67d) #f)
                  ((<= #xa6f0 sv #xa80b) #f)
                  ((<= #xa8c4 sv #xa8f1) #f)
                  ((<= #xa92b sv #xa92d) #f)
                  ((<= #xaabf sv #xaac1) #f)
                  ((<= #xfe00 sv #x102e0) #f)
                  ((<= #x10a38 sv #x10ae6) #f)
                  ((<= #x11046 sv #x11081) #f)
                  ((<= #x110b9 sv #x110ba) #f)
                  ((<= #x11133 sv #x11173) #f)
                  ((<= #x112e9 sv #x112ea) #f)
                  ((<= #x11366 sv #x11374) #f)
                  ((<= #x114c2 sv #x114c3) #f)
                  ((<= #x115bf sv #x115c0) #f)
                  ((<= #x116b7 sv #x16af4) #f)
                  ((<= #x16f8f sv #x1bc9d) #f)
                  ((<= #x1d167 sv #xe01ef) #f)
                  ((memv sv
                         '(#x658 #x93c #x9bc #x9cd #xa3c
                           #xa4d #xabc #xacd #xb3c #xb4d
                           #xbcd #xc4d #xcbc #xccd #xd4d
                           #xdca #xe4e #xfc6 #x108d #x1714
                           #x1734 #x1a60 #x1b34 #x1bab #x1be6
                           #xa9b3 #xa9e5 #xaa7c #xaaf6 #xabed
                           #x11236 #x1133c #x1163f))
                   #f)
                  (else #t))))
         (else
          (let ((sv (char->integer c)))
            (or (<= #x24b6 sv #x24e9)
                (<= #x1f130 sv #x1f149)
                (<= #x1f150 sv #x1f169)
                (<= #x1f170 sv #x1f189))))))))

; Given a character, returns true if and only if the character
; has the numeric property, which appears to be true if and only if
; its entry in UnicodeData.txt contains a numeric value in column 8.
;
; FIXME:
; Hard-coding knowledge of Numeric into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were computed by
;     (decision-strategy (extract-numeric))

(define (char-numeric? c)
  (case (char-general-category c)
   ((Nd)
    (cond ((<= #xff10 (char->integer c) #xff19) #f)
          (else #t)))
   (else #f)))

; FIXME:
; The predicate above computes the Numeric property
; that is needed for word-breaking.  The predicate
; that is commented out below computes the more
; inclusive property of having a numeric value,
; which includes characters for fractions et cetera.
;
; Note: the hard-coded excluded ranges were computed by
;     (decision-strategy (map car (extract-numeric-values)))
;
; (define (char-numeric? c)
;   (if (< (char->integer c) 128)
;       (and (char<=? #\0 c) (char<=? c #\9))
;       (case (char-general-category c)
;        ((Nd) #t)
;        ((No) #t)
;        ((Nl) #t)
;        (else
;         (memv (char->integer c)
;               '((#x53c3 #x62fe #x5169 #x96f6 #x516d
;                  #x9678 #x4ec0 #x5efe)))))))

; Given a character, returns true if and only if the character
; has the White_Space property, which is enumerated in PropList.txt.
;
; FIXME:
; Hard-coding the exceptional code points into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.
;
; Note: the hard-coded code points were computed by
;     (decision-strategy (extract-white-space))

(define (char-whitespace? c)
  (let ((k (char->integer c)))
    (if (< k 256)
        (case k
         ((#x09 #x0a #x0b #x0c #x0d #x20 #x85 #xa0) #t)
         (else #f))
        (case (char-general-category c)
         ((Zs Zl Zp) #t)
         (else #f)))))

; Given a character, returns true if and only if the character
; has the Uppercase property, which is defined as
; Other_Uppercase + Lu.  Other_Uppercase is enumerated in PropList.txt.
;
; FIXME:
; Hard-coding knowledge of Other_Uppercase into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.

(define (char-upper-case? c)
  (if (< (char->integer c) 128)
      (and (char<=? #\A c) (char<=? c #\Z))
      (or (eq? 'Lu (char-general-category c))

          ; In Unicode 7.0.0, Other_Uppercase consists of
          ;     2160..216F
          ;     24B6..24CF
          ;     1F130..1F149
          ;     1F150..1F169
          ;     1F170..1F189

          (let ((sv (char->integer c)))
            (cond ((< sv #x2160)
                   #f)
                  ((<= #x2160 sv #x216f)
                   #t)
                  ((<= #x24b6 sv #x24cf)
                   #t)
                  ((<= #x1f130 sv #x1f149)
                   #t)
                  ((<= #x1f150 sv #x1f169)
                   #t)
                  ((<= #x1f170 sv #x1f189)
                   #t)
                  (else #f))))))

; Given a character, returns true if and only if the character
; has the Lowercase property, which is defined as
; Other_Lowercase + Ll.  Other_Lowercase is enumerated in PropList.txt.
;
; FIXME:
; Hard-coding knowledge of Other_Lowercase into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.

(define (char-lower-case? c)
  (if (< (char->integer c) 128)
      (and (char<=? #\a c) (char<=? c #\z))
      (or (eq? 'Ll (char-general-category c))
          (let ((sv (char->integer c)))
            (or (= sv #x00aa)
                (= sv #x00ba)
                (<= #x02b0 sv #x02b8)
                (<= #x02c0 sv #x02c1)
                (<= #x02e0 sv #x02e4)
                (= sv #x0345)
                (= sv #x037a)
                (<= #x1d2c sv #x1d6a)
                (= sv #x1d78)
                (<= #x1d9b sv #x1dbf)
                (= sv #x2071)
                (= sv #x207f)
                (<= #x2090 sv #x209c)
                (<= #x2170 sv #x217f)
                (<= #x24d0 sv #x24e9)
                (<= #x2c7c sv #x2c7d)
                (<= #xa69c sv #xa69d)
                (= sv #xa770)
                (<= #xa7f8 sv #xa7f9)
                (<= #xab5c sv #xab5f))))))

; Given a character, returns true if and only if the character
; is in Unicode general category Lt.

(define (char-title-case? c)
  (eq? 'Lt (char-general-category c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Unicode general properties.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The symbols that represent Unicode general properties.
; There are 30 of these.
; This table occupies about 128 bytes, not counting
; the space occupied by the symbols themselves.

(define vector-of-general-category-symbols
  '#(
     ; Letter: uppercase, lowercase, titlecase, modifier, other
     Lu Ll Lt Lm Lo

     ; Mark: nonspacing, spacing combining, enclosing
     Mn Mc Me

     ; Number: decimal digit, letter, other
     Nd Nl No

     ; Punctuation: connector, dash, open, close,
     ;     initial quote, final quote, other
     Pc Pd Ps Pe Pi Pf Po

     ; Symbol: math, currency, modifier, other
     Sm Sc Sk So

     ; Separator: space, line, paragraph
     Zs Zl Zp

     ; Other: control, format, surrogate, private use, not assigned
     Cc Cf Cs Co Cn))

; Given a symbol that appears in the vector above,
; returns its index within the vector.
; Used only for initialization, so it needn't be fast.

(define (general-category-symbol->index sym)
  (let ((n (vector-length vector-of-general-category-symbols)))
    (do ((i 0 (+ i 1)))
        ((or (= i n)
             (eq? sym (vector-ref vector-of-general-category-symbols i)))
         (if (= i n)
             (error "Unrecognized Unicode general category" sym)
             i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The following tables were generated from
; UnicodeData.txt, CaseFolding.txt,
; SpecialCasing.txt, PropList.txt,
; WordBreakProperty.txt, and CompositionExclusions.txt.
; Use parseUCD.sch to regenerate these tables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following array of bytes, together with the vector below it,
; implements an indirect mapping from all Unicode scalar values to
; indices into vector-of-general-category-symbols.
;
; This table contains 3499 entries.

(define general-category-indices-for-all-characters
  (u8-list->bytevector
   (map
    general-category-symbol->index
    '(
      Cc Zs Po Sc Po Ps Pe Po Sm Po Pd Po Nd Po Sm Po 
      Lu Ps Po Pe Sk Pc Sk Ll Ps Sm Pe Sm Cc Zs Po Sc 
      So Po Sk So Lo Pi Sm Cf So Sk So Sm No Sk Ll Po 
      Sk No Lo Pf No Po Lu Sm Lu Ll Sm Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lo Lu Ll Lo 
      Lu Lt Ll Lu Lt Ll Lu Lt Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Lt Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lo Ll Lm Sk 
      Lm Sk Lm Sk Lm Sk Lm Sk Mn Lu Ll Lu Ll Lm Sk Lu 
      Ll Cn Lm Ll Po Lu Cn Sk Lu Po Lu Cn Lu Cn Lu Ll 
      Lu Cn Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Sm Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll So Mn Me Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Cn Lu Cn Lm Po Cn Ll Cn Po Pd Cn So Sc Cn 
      Mn Pd Mn Po Mn Po Mn Po Mn Cn Lo Cn Lo Po Cn Cf 
      Sm Po Sc Po So Mn Po Cf Cn Po Lo Lm Lo Mn Nd Po 
      Lo Mn Lo Po Lo Mn Cf So Mn Lm Mn So Mn Lo Nd Lo 
      So Lo Po Cn Cf Lo Mn Lo Mn Cn Lo Mn Lo Cn Nd Lo 
      Mn Lm So Po Lm Cn Lo Mn Lm Mn Lm Mn Lm Mn Cn Po 
      Cn Lo Mn Cn Po Cn Lo Cn Mn Mc Lo Mn Mc Mn Lo Mc 
      Mn Mc Mn Mc Lo Mn Lo Mn Po Nd Po Lm Lo Mn Mc Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Mn Lo Mc Mn 
      Cn Mc Cn Mc Mn Lo Cn Mc Cn Lo Cn Lo Mn Cn Nd Lo 
      Sc No So Sc Cn Mn Mc Cn Lo Cn Lo Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Lo Cn Mn Cn Mc Mn Cn Mn Cn Mn Cn Mn 
      Cn Lo Cn Lo Cn Nd Mn Lo Mn Cn Mn Mc Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Mn Lo Mc Mn Cn Mn Mc 
      Cn Mc Mn Cn Lo Cn Lo Mn Cn Nd Po Sc Cn Mn Mc Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Mn Lo Mc Mn 
      Mc Mn Cn Mc Cn Mc Mn Cn Mn Mc Cn Lo Cn Lo Mn Cn 
      Nd So Lo No Cn Mn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Mc Mn Mc Cn Mc Cn 
      Mc Mn Cn Lo Cn Mc Cn Nd No So Sc So Cn Mn Mc Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Mn Mc Cn Mn Cn Mn Cn 
      Mn Cn Lo Cn Lo Mn Cn Nd Cn No So Cn Mn Mc Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Mn Lo Mc Mn Mc Cn Mn 
      Mc Cn Mc Mn Cn Mc Cn Lo Cn Lo Mn Cn Nd Cn Lo Cn 
      Mn Mc Cn Lo Cn Lo Cn Lo Cn Lo Mc Mn Cn Mc Cn Mc 
      Mn Lo Cn Mc Cn Lo Mn Cn Nd No Cn So Lo Cn Mc Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Mn Cn Mc Mn Cn Mn 
      Cn Mc Cn Nd Cn Mc Po Cn Lo Mn Lo Mn Cn Sc Lo Lm 
      Mn Po Nd Po Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Mn Lo Mn Cn 
      Mn Lo Cn Lo Cn Lm Cn Mn Cn Nd Cn Lo Cn Lo So Po 
      So Po So Mn So Nd No So Mn So Mn So Mn Ps Pe Ps 
      Pe Mc Lo Cn Lo Cn Mn Mc Mn Po Mn Lo Mn Cn Mn Cn 
      So Mn So Cn So Po So Po Cn Lo Mc Mn Mc Mn Mc Mn 
      Mc Mn Lo Nd Po Lo Mc Mn Lo Mn Lo Mc Lo Mc Lo Mn 
      Lo Mn Mc Mn Mc Mn Lo Mc Nd Mc Mn So Lu Cn Lu Cn 
      Lu Cn Lo Po Lm Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Mn Po No Cn Lo So Cn Lo Cn Pd Lo 
      Po Lo Zs Lo Ps Pe Cn Lo Po Nl Lo Cn Lo Cn Lo Mn 
      Cn Lo Mn Po Cn Lo Mn Cn Lo Cn Lo Cn Mn Cn Lo Mn 
      Mc Mn Mc Mn Mc Mn Po Lm Po Sc Lo Mn Cn Nd Cn No 
      Cn Po Pd Po Mn Cf Cn Nd Cn Lo Lm Lo Cn Lo Mn Lo 
      Cn Lo Cn Lo Cn Mn Mc Mn Mc Cn Mc Mn Mc Mn Cn So 
      Cn Po Nd Lo Cn Lo Cn Lo Cn Mc Lo Mc Cn Nd No Cn 
      So Lo Mn Mc Mn Cn Po Lo Mc Mn Mc Mn Cn Mn Mc Mn 
      Mc Mn Mc Mn Cn Mn Nd Cn Nd Cn Po Lm Po Cn Mn Me 
      Cn Mn Mc Lo Mn Mc Mn Mc Mn Mc Mn Mc Lo Cn Nd Po 
      So Mn So Cn Mn Mc Lo Mc Mn Mc Mn Mc Mn Lo Nd Lo 
      Mn Mc Mn Mc Mn Mc Mn Mc Cn Po Lo Mc Mn Mc Mn Cn 
      Po Nd Cn Lo Nd Lo Lm Po Cn Po Cn Mn Po Mn Mc Mn 
      Lo Mn Lo Mc Mn Lo Cn Mn Cn Ll Lm Ll Lm Ll Lm Mn 
      Cn Mn Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Cn Lu Cn Ll 
      Lu Ll Lu Ll Cn Lu Cn Ll Cn Lu Cn Lu Cn Lu Cn Lu 
      Ll Lu Ll Cn Ll Lt Ll Lt Ll Lt Ll Cn Ll Lu Lt Sk 
      Ll Sk Ll Cn Ll Lu Lt Sk Ll Cn Ll Lu Cn Sk Ll Lu 
      Sk Cn Ll Cn Ll Lu Lt Sk Cn Zs Cf Pd Po Pi Pf Ps 
      Pi Pf Ps Pi Po Zl Zp Cf Zs Po Pi Pf Po Pc Po Sm 
      Ps Pe Po Sm Po Pc Po Zs Cf Cn Cf No Lm Cn No Sm 
      Ps Pe Lm No Sm Ps Pe Cn Lm Cn Sc Cn Mn Me Mn Me 
      Mn Cn So Lu So Lu So Ll Lu Ll Lu Ll So Lu So Sm 
      Lu So Lu So Lu So Lu So Lu So Ll Lu Ll Lo Ll So 
      Ll Lu Sm Lu Ll So Sm So Ll So No Nl Lu Ll Nl No 
      Cn Sm So Sm So Sm So Sm So Sm So Sm So Sm So Sm 
      So Sm So Sm So Ps Pe Ps Pe So Sm So Ps Pe So Sm 
      So Sm So Sm So Cn So Cn So Cn No So No So Sm So 
      Sm So Sm So Sm So Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe 
      Ps Pe Ps Pe No So Sm Ps Pe Sm Ps Pe Ps Pe Ps Pe 
      Ps Pe Ps Pe Sm So Sm Ps Pe Ps Pe Ps Pe Ps Pe Ps 
      Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Sm Ps Pe 
      Ps Pe Sm Ps Pe Sm So Sm So Sm So Cn So Cn So Cn 
      So Cn So Cn Lu Cn Ll Cn Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lm Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll So Lu Ll 
      Lu Ll Mn Lu Ll Cn Po No Po Ll Cn Ll Cn Ll Cn Lo 
      Cn Lm Po Cn Mn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Mn Po Pi Pf Pi Pf Po Pi Pf 
      Po Pi Pf Po Pd Po Pd Po Pi Pf Po Pi Pf Ps Pe Ps 
      Pe Ps Pe Ps Pe Po Lm Po Pd Po Pd Po Ps Cn So Cn 
      So Cn So Cn So Cn Zs Po So Lm Lo Nl Ps Pe Ps Pe 
      Ps Pe Ps Pe Ps Pe So Ps Pe Ps Pe Ps Pe Ps Pe Pd 
      Ps Pe So Nl Mn Mc Pd Lm So Nl Lm Lo Po So Cn Lo 
      Cn Mn Sk Lm Lo Pd Lo Po Lm Lo Cn Lo Cn Lo Cn So 
      No So Lo Cn So Cn Lo So Cn No So No So No So No 
      So No So Cn So Lo Cn So Lo Cn Lo Lm Lo Cn So Cn 
      Lo Lm Po Lo Lm Po Lo Nd Lo Cn Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lo Mn Me Po Mn Po Lm Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lm Cn Mn Lo Nl 
      Mn Po Cn Sk Lm Sk Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lm Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lm Sk Lu Ll Lu Ll Cn Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Cn Lu Cn Lo Lm Ll Lo Mn Lo Mn Lo Mn Lo Mc 
      Mn Mc So Cn No So Sc So Cn Lo Po Cn Mc Lo Mc Mn 
      Cn Po Nd Cn Mn Lo Po Lo Cn Nd Lo Mn Po Lo Mn Mc 
      Cn Po Lo Cn Mn Mc Lo Mn Mc Mn Mc Mn Mc Po Cn Lm 
      Nd Cn Po Lo Mn Lm Lo Nd Lo Cn Lo Mn Mc Mn Mc Mn 
      Cn Lo Mn Lo Mn Mc Cn Nd Cn Po Lo Lm Lo So Lo Mc 
      Mn Mc Lo Mn Lo Mn Lo Mn Lo Mn Lo Mn Lo Cn Lo Lm 
      Po Lo Mc Mn Mc Po Lo Lm Mc Mn Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Ll Sk Lm Cn Ll Cn Lo Mc Mn Mc Mn 
      Mc Po Mc Mn Cn Nd Cn Lo Cn Lo Cn Lo Cn Cs Co Lo 
      Cn Lo Cn Ll Cn Ll Cn Lo Mn Lo Sm Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Sk Cn Lo Pe Ps Cn Lo Cn Lo Cn 
      Lo Sc So Cn Mn Po Ps Pe Po Cn Mn Cn Po Pd Pc Ps 
      Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Po 
      Ps Pe Po Pc Po Cn Po Pd Ps Pe Ps Pe Ps Pe Po Sm 
      Pd Sm Cn Po Sc Po Cn Lo Cn Lo Cn Cf Cn Po Sc Po 
      Ps Pe Po Sm Po Pd Po Nd Po Sm Po Lu Ps Po Pe Sk 
      Pc Sk Ll Ps Sm Pe Sm Ps Pe Po Ps Pe Po Lo Lm Lo 
      Lm Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Sc Sm Sk So Sc 
      Cn So Sm So Cn Cf So Cn Lo Cn Lo Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Lo Cn Po Cn No Cn So Nl No So No So 
      Cn So Cn So Cn So Mn Cn Lo Cn Lo Cn Mn No Cn Lo 
      No Cn Lo Nl Lo Nl Cn Lo Mn Cn Lo Cn Po Lo Cn Lo 
      Po Nl Cn Lu Ll Lo Cn Nd Cn Lo Cn Lo Cn Po Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Po No Lo So No Lo Cn No Cn Lo No Cn Po Lo Cn 
      Po Cn Lo Cn Lo Cn Lo Mn Cn Mn Cn Mn Lo Cn Lo Cn 
      Lo Cn Mn Cn Mn No Cn Po Cn Lo No Po Lo No Cn Lo 
      So Lo Mn Cn No Po Cn Lo Cn Po Lo Cn No Lo Cn No 
      Lo Cn Po Cn No Cn Lo Cn No Cn Mc Mn Mc Lo Mn Po 
      Cn No Nd Cn Mn Mc Lo Mc Mn Mc Mn Po Cf Po Cn Lo 
      Cn Nd Cn Mn Lo Mn Mc Mn Cn Nd Po Cn Lo Mn Po Lo 
      Cn Mn Mc Lo Mc Mn Mc Lo Po Cn Po Cn Nd Lo Cn No 
      Cn Lo Cn Lo Mc Mn Mc Mn Mc Mn Po Cn Lo Mn Mc Mn 
      Cn Nd Cn Mn Mc Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn 
      Lo Cn Mn Lo Mc Mn Mc Cn Mc Cn Mc Cn Mc Cn Lo Mc 
      Cn Mn Cn Mn Cn Lo Mc Mn Mc Mn Mc Mn Mc Mn Lo Po 
      Lo Cn Nd Cn Lo Mc Mn Cn Mc Mn Mc Mn Po Cn Lo Mc 
      Mn Mc Mn Mc Mn Po Lo Cn Nd Cn Lo Mn Mc Mn Mc Mn 
      Mc Mn Cn Nd Cn Lu Ll Nd No Cn Lo Cn Lo Cn Lo Cn 
      Nl Cn Po Cn Lo Cn Lo Cn Lo Cn Nd Cn Po Cn Lo Cn 
      Mn Po Cn Lo Mn Po So Lm Po So Cn Nd Cn No Cn Lo 
      Cn Lo Cn Lo Cn Lo Mc Cn Mn Lm Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn So Mn Po Cf Cn So Cn So Cn So Mc 
      Mn So Mc Cf Mn So Mn So Mn So Cn So Mn So Cn So 
      Cn No Cn Lu Ll Lu Ll Cn Ll Lu Ll Lu Cn Lu Cn Lu 
      Cn Lu Cn Lu Cn Lu Ll Cn Ll Cn Ll Cn Ll Lu Ll Lu 
      Cn Lu Cn Lu Cn Lu Cn Ll Lu Cn Lu Cn Lu Cn Lu Cn 
      Lu Cn Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Cn 
      Lu Sm Ll Sm Ll Lu Sm Ll Sm Ll Lu Sm Ll Sm Ll Lu 
      Sm Ll Sm Ll Lu Sm Ll Sm Ll Lu Ll Cn Nd Cn Lo Cn 
      No Mn Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Sm Cn So Cn So Cn So Cn So Cn So 
      Cn So Cn No Cn So Cn So Cn So Cn So Cn So Cn So 
      Cn So Cn So Cn So Cn So Cn So Cn So Cn So Cn So 
      Cn So Cn So Cn So Cn So Cn So Cn So Cn So Cn So 
      Cn So Cn So Cn So Cn So Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Cf Cn Cf Cn Mn Cn Co Cn Co Cn ))))

; The following vector of exact integers represents the
; Unicode scalar values whose Unicode general category
; is different from the Unicode scalar value immediately
; less than it.
;
; This table contains 3499 entries.

(define vector-of-code-points-with-same-category
  '#(
     #x0 #x20 #x21 #x24 #x25 #x28 #x29 #x2a 
     #x2b #x2c #x2d #x2e #x30 #x3a #x3c #x3f 
     #x41 #x5b #x5c #x5d #x5e #x5f #x60 #x61 
     #x7b #x7c #x7d #x7e #x7f #xa0 #xa1 #xa2 
     #xa6 #xa7 #xa8 #xa9 #xaa #xab #xac #xad 
     #xae #xaf #xb0 #xb1 #xb2 #xb4 #xb5 #xb6 
     #xb8 #xb9 #xba #xbb #xbc #xbf #xc0 #xd7 
     #xd8 #xdf #xf7 #xf8 #x100 #x101 #x102 #x103 
     #x104 #x105 #x106 #x107 #x108 #x109 #x10a #x10b 
     #x10c #x10d #x10e #x10f #x110 #x111 #x112 #x113 
     #x114 #x115 #x116 #x117 #x118 #x119 #x11a #x11b 
     #x11c #x11d #x11e #x11f #x120 #x121 #x122 #x123 
     #x124 #x125 #x126 #x127 #x128 #x129 #x12a #x12b 
     #x12c #x12d #x12e #x12f #x130 #x131 #x132 #x133 
     #x134 #x135 #x136 #x137 #x139 #x13a #x13b #x13c 
     #x13d #x13e #x13f #x140 #x141 #x142 #x143 #x144 
     #x145 #x146 #x147 #x148 #x14a #x14b #x14c #x14d 
     #x14e #x14f #x150 #x151 #x152 #x153 #x154 #x155 
     #x156 #x157 #x158 #x159 #x15a #x15b #x15c #x15d 
     #x15e #x15f #x160 #x161 #x162 #x163 #x164 #x165 
     #x166 #x167 #x168 #x169 #x16a #x16b #x16c #x16d 
     #x16e #x16f #x170 #x171 #x172 #x173 #x174 #x175 
     #x176 #x177 #x178 #x17a #x17b #x17c #x17d #x17e 
     #x181 #x183 #x184 #x185 #x186 #x188 #x189 #x18c 
     #x18e #x192 #x193 #x195 #x196 #x199 #x19c #x19e 
     #x19f #x1a1 #x1a2 #x1a3 #x1a4 #x1a5 #x1a6 #x1a8 
     #x1a9 #x1aa #x1ac #x1ad #x1ae #x1b0 #x1b1 #x1b4 
     #x1b5 #x1b6 #x1b7 #x1b9 #x1bb #x1bc #x1bd #x1c0 
     #x1c4 #x1c5 #x1c6 #x1c7 #x1c8 #x1c9 #x1ca #x1cb 
     #x1cc #x1cd #x1ce #x1cf #x1d0 #x1d1 #x1d2 #x1d3 
     #x1d4 #x1d5 #x1d6 #x1d7 #x1d8 #x1d9 #x1da #x1db 
     #x1dc #x1de #x1df #x1e0 #x1e1 #x1e2 #x1e3 #x1e4 
     #x1e5 #x1e6 #x1e7 #x1e8 #x1e9 #x1ea #x1eb #x1ec 
     #x1ed #x1ee #x1ef #x1f1 #x1f2 #x1f3 #x1f4 #x1f5 
     #x1f6 #x1f9 #x1fa #x1fb #x1fc #x1fd #x1fe #x1ff 
     #x200 #x201 #x202 #x203 #x204 #x205 #x206 #x207 
     #x208 #x209 #x20a #x20b #x20c #x20d #x20e #x20f 
     #x210 #x211 #x212 #x213 #x214 #x215 #x216 #x217 
     #x218 #x219 #x21a #x21b #x21c #x21d #x21e #x21f 
     #x220 #x221 #x222 #x223 #x224 #x225 #x226 #x227 
     #x228 #x229 #x22a #x22b #x22c #x22d #x22e #x22f 
     #x230 #x231 #x232 #x233 #x23a #x23c #x23d #x23f 
     #x241 #x242 #x243 #x247 #x248 #x249 #x24a #x24b 
     #x24c #x24d #x24e #x24f #x294 #x295 #x2b0 #x2c2 
     #x2c6 #x2d2 #x2e0 #x2e5 #x2ec #x2ed #x2ee #x2ef 
     #x300 #x370 #x371 #x372 #x373 #x374 #x375 #x376 
     #x377 #x378 #x37a #x37b #x37e #x37f #x380 #x384 
     #x386 #x387 #x388 #x38b #x38c #x38d #x38e #x390 
     #x391 #x3a2 #x3a3 #x3ac #x3cf #x3d0 #x3d2 #x3d5 
     #x3d8 #x3d9 #x3da #x3db #x3dc #x3dd #x3de #x3df 
     #x3e0 #x3e1 #x3e2 #x3e3 #x3e4 #x3e5 #x3e6 #x3e7 
     #x3e8 #x3e9 #x3ea #x3eb #x3ec #x3ed #x3ee #x3ef 
     #x3f4 #x3f5 #x3f6 #x3f7 #x3f8 #x3f9 #x3fb #x3fd 
     #x430 #x460 #x461 #x462 #x463 #x464 #x465 #x466 
     #x467 #x468 #x469 #x46a #x46b #x46c #x46d #x46e 
     #x46f #x470 #x471 #x472 #x473 #x474 #x475 #x476 
     #x477 #x478 #x479 #x47a #x47b #x47c #x47d #x47e 
     #x47f #x480 #x481 #x482 #x483 #x488 #x48a #x48b 
     #x48c #x48d #x48e #x48f #x490 #x491 #x492 #x493 
     #x494 #x495 #x496 #x497 #x498 #x499 #x49a #x49b 
     #x49c #x49d #x49e #x49f #x4a0 #x4a1 #x4a2 #x4a3 
     #x4a4 #x4a5 #x4a6 #x4a7 #x4a8 #x4a9 #x4aa #x4ab 
     #x4ac #x4ad #x4ae #x4af #x4b0 #x4b1 #x4b2 #x4b3 
     #x4b4 #x4b5 #x4b6 #x4b7 #x4b8 #x4b9 #x4ba #x4bb 
     #x4bc #x4bd #x4be #x4bf #x4c0 #x4c2 #x4c3 #x4c4 
     #x4c5 #x4c6 #x4c7 #x4c8 #x4c9 #x4ca #x4cb #x4cc 
     #x4cd #x4ce #x4d0 #x4d1 #x4d2 #x4d3 #x4d4 #x4d5 
     #x4d6 #x4d7 #x4d8 #x4d9 #x4da #x4db #x4dc #x4dd 
     #x4de #x4df #x4e0 #x4e1 #x4e2 #x4e3 #x4e4 #x4e5 
     #x4e6 #x4e7 #x4e8 #x4e9 #x4ea #x4eb #x4ec #x4ed 
     #x4ee #x4ef #x4f0 #x4f1 #x4f2 #x4f3 #x4f4 #x4f5 
     #x4f6 #x4f7 #x4f8 #x4f9 #x4fa #x4fb #x4fc #x4fd 
     #x4fe #x4ff #x500 #x501 #x502 #x503 #x504 #x505 
     #x506 #x507 #x508 #x509 #x50a #x50b #x50c #x50d 
     #x50e #x50f #x510 #x511 #x512 #x513 #x514 #x515 
     #x516 #x517 #x518 #x519 #x51a #x51b #x51c #x51d 
     #x51e #x51f #x520 #x521 #x522 #x523 #x524 #x525 
     #x526 #x527 #x528 #x529 #x52a #x52b #x52c #x52d 
     #x52e #x52f #x530 #x531 #x557 #x559 #x55a #x560 
     #x561 #x588 #x589 #x58a #x58b #x58d #x58f #x590 
     #x591 #x5be #x5bf #x5c0 #x5c1 #x5c3 #x5c4 #x5c6 
     #x5c7 #x5c8 #x5d0 #x5eb #x5f0 #x5f3 #x5f5 #x600 
     #x606 #x609 #x60b #x60c #x60e #x610 #x61b #x61c 
     #x61d #x61e #x620 #x640 #x641 #x64b #x660 #x66a 
     #x66e #x670 #x671 #x6d4 #x6d5 #x6d6 #x6dd #x6de 
     #x6df #x6e5 #x6e7 #x6e9 #x6ea #x6ee #x6f0 #x6fa 
     #x6fd #x6ff #x700 #x70e #x70f #x710 #x711 #x712 
     #x730 #x74b #x74d #x7a6 #x7b1 #x7b2 #x7c0 #x7ca 
     #x7eb #x7f4 #x7f6 #x7f7 #x7fa #x7fb #x800 #x816 
     #x81a #x81b #x824 #x825 #x828 #x829 #x82e #x830 
     #x83f #x840 #x859 #x85c #x85e #x85f #x8a0 #x8b3 
     #x8e4 #x903 #x904 #x93a #x93b #x93c #x93d #x93e 
     #x941 #x949 #x94d #x94e #x950 #x951 #x958 #x962 
     #x964 #x966 #x970 #x971 #x972 #x981 #x982 #x984 
     #x985 #x98d #x98f #x991 #x993 #x9a9 #x9aa #x9b1 
     #x9b2 #x9b3 #x9b6 #x9ba #x9bc #x9bd #x9be #x9c1 
     #x9c5 #x9c7 #x9c9 #x9cb #x9cd #x9ce #x9cf #x9d7 
     #x9d8 #x9dc #x9de #x9df #x9e2 #x9e4 #x9e6 #x9f0 
     #x9f2 #x9f4 #x9fa #x9fb #x9fc #xa01 #xa03 #xa04 
     #xa05 #xa0b #xa0f #xa11 #xa13 #xa29 #xa2a #xa31 
     #xa32 #xa34 #xa35 #xa37 #xa38 #xa3a #xa3c #xa3d 
     #xa3e #xa41 #xa43 #xa47 #xa49 #xa4b #xa4e #xa51 
     #xa52 #xa59 #xa5d #xa5e #xa5f #xa66 #xa70 #xa72 
     #xa75 #xa76 #xa81 #xa83 #xa84 #xa85 #xa8e #xa8f 
     #xa92 #xa93 #xaa9 #xaaa #xab1 #xab2 #xab4 #xab5 
     #xaba #xabc #xabd #xabe #xac1 #xac6 #xac7 #xac9 
     #xaca #xacb #xacd #xace #xad0 #xad1 #xae0 #xae2 
     #xae4 #xae6 #xaf0 #xaf1 #xaf2 #xb01 #xb02 #xb04 
     #xb05 #xb0d #xb0f #xb11 #xb13 #xb29 #xb2a #xb31 
     #xb32 #xb34 #xb35 #xb3a #xb3c #xb3d #xb3e #xb3f 
     #xb40 #xb41 #xb45 #xb47 #xb49 #xb4b #xb4d #xb4e 
     #xb56 #xb57 #xb58 #xb5c #xb5e #xb5f #xb62 #xb64 
     #xb66 #xb70 #xb71 #xb72 #xb78 #xb82 #xb83 #xb84 
     #xb85 #xb8b #xb8e #xb91 #xb92 #xb96 #xb99 #xb9b 
     #xb9c #xb9d #xb9e #xba0 #xba3 #xba5 #xba8 #xbab 
     #xbae #xbba #xbbe #xbc0 #xbc1 #xbc3 #xbc6 #xbc9 
     #xbca #xbcd #xbce #xbd0 #xbd1 #xbd7 #xbd8 #xbe6 
     #xbf0 #xbf3 #xbf9 #xbfa #xbfb #xc00 #xc01 #xc04 
     #xc05 #xc0d #xc0e #xc11 #xc12 #xc29 #xc2a #xc3a 
     #xc3d #xc3e #xc41 #xc45 #xc46 #xc49 #xc4a #xc4e 
     #xc55 #xc57 #xc58 #xc5a #xc60 #xc62 #xc64 #xc66 
     #xc70 #xc78 #xc7f #xc80 #xc81 #xc82 #xc84 #xc85 
     #xc8d #xc8e #xc91 #xc92 #xca9 #xcaa #xcb4 #xcb5 
     #xcba #xcbc #xcbd #xcbe #xcbf #xcc0 #xcc5 #xcc6 
     #xcc7 #xcc9 #xcca #xccc #xcce #xcd5 #xcd7 #xcde 
     #xcdf #xce0 #xce2 #xce4 #xce6 #xcf0 #xcf1 #xcf3 
     #xd01 #xd02 #xd04 #xd05 #xd0d #xd0e #xd11 #xd12 
     #xd3b #xd3d #xd3e #xd41 #xd45 #xd46 #xd49 #xd4a 
     #xd4d #xd4e #xd4f #xd57 #xd58 #xd60 #xd62 #xd64 
     #xd66 #xd70 #xd76 #xd79 #xd7a #xd80 #xd82 #xd84 
     #xd85 #xd97 #xd9a #xdb2 #xdb3 #xdbc #xdbd #xdbe 
     #xdc0 #xdc7 #xdca #xdcb #xdcf #xdd2 #xdd5 #xdd6 
     #xdd7 #xdd8 #xde0 #xde6 #xdf0 #xdf2 #xdf4 #xdf5 
     #xe01 #xe31 #xe32 #xe34 #xe3b #xe3f #xe40 #xe46 
     #xe47 #xe4f #xe50 #xe5a #xe5c #xe81 #xe83 #xe84 
     #xe85 #xe87 #xe89 #xe8a #xe8b #xe8d #xe8e #xe94 
     #xe98 #xe99 #xea0 #xea1 #xea4 #xea5 #xea6 #xea7 
     #xea8 #xeaa #xeac #xead #xeb1 #xeb2 #xeb4 #xeba 
     #xebb #xebd #xebe #xec0 #xec5 #xec6 #xec7 #xec8 
     #xece #xed0 #xeda #xedc #xee0 #xf00 #xf01 #xf04 
     #xf13 #xf14 #xf15 #xf18 #xf1a #xf20 #xf2a #xf34 
     #xf35 #xf36 #xf37 #xf38 #xf39 #xf3a #xf3b #xf3c 
     #xf3d #xf3e #xf40 #xf48 #xf49 #xf6d #xf71 #xf7f 
     #xf80 #xf85 #xf86 #xf88 #xf8d #xf98 #xf99 #xfbd 
     #xfbe #xfc6 #xfc7 #xfcd #xfce #xfd0 #xfd5 #xfd9 
     #xfdb #x1000 #x102b #x102d #x1031 #x1032 #x1038 #x1039 
     #x103b #x103d #x103f #x1040 #x104a #x1050 #x1056 #x1058 
     #x105a #x105e #x1061 #x1062 #x1065 #x1067 #x106e #x1071 
     #x1075 #x1082 #x1083 #x1085 #x1087 #x108d #x108e #x108f 
     #x1090 #x109a #x109d #x109e #x10a0 #x10c6 #x10c7 #x10c8 
     #x10cd #x10ce #x10d0 #x10fb #x10fc #x10fd #x1249 #x124a 
     #x124e #x1250 #x1257 #x1258 #x1259 #x125a #x125e #x1260 
     #x1289 #x128a #x128e #x1290 #x12b1 #x12b2 #x12b6 #x12b8 
     #x12bf #x12c0 #x12c1 #x12c2 #x12c6 #x12c8 #x12d7 #x12d8 
     #x1311 #x1312 #x1316 #x1318 #x135b #x135d #x1360 #x1369 
     #x137d #x1380 #x1390 #x139a #x13a0 #x13f5 #x1400 #x1401 
     #x166d #x166f #x1680 #x1681 #x169b #x169c #x169d #x16a0 
     #x16eb #x16ee #x16f1 #x16f9 #x1700 #x170d #x170e #x1712 
     #x1715 #x1720 #x1732 #x1735 #x1737 #x1740 #x1752 #x1754 
     #x1760 #x176d #x176e #x1771 #x1772 #x1774 #x1780 #x17b4 
     #x17b6 #x17b7 #x17be #x17c6 #x17c7 #x17c9 #x17d4 #x17d7 
     #x17d8 #x17db #x17dc #x17dd #x17de #x17e0 #x17ea #x17f0 
     #x17fa #x1800 #x1806 #x1807 #x180b #x180e #x180f #x1810 
     #x181a #x1820 #x1843 #x1844 #x1878 #x1880 #x18a9 #x18aa 
     #x18ab #x18b0 #x18f6 #x1900 #x191f #x1920 #x1923 #x1927 
     #x1929 #x192c #x1930 #x1932 #x1933 #x1939 #x193c #x1940 
     #x1941 #x1944 #x1946 #x1950 #x196e #x1970 #x1975 #x1980 
     #x19ac #x19b0 #x19c1 #x19c8 #x19ca #x19d0 #x19da #x19db 
     #x19de #x1a00 #x1a17 #x1a19 #x1a1b #x1a1c #x1a1e #x1a20 
     #x1a55 #x1a56 #x1a57 #x1a58 #x1a5f #x1a60 #x1a61 #x1a62 
     #x1a63 #x1a65 #x1a6d #x1a73 #x1a7d #x1a7f #x1a80 #x1a8a 
     #x1a90 #x1a9a #x1aa0 #x1aa7 #x1aa8 #x1aae #x1ab0 #x1abe 
     #x1abf #x1b00 #x1b04 #x1b05 #x1b34 #x1b35 #x1b36 #x1b3b 
     #x1b3c #x1b3d #x1b42 #x1b43 #x1b45 #x1b4c #x1b50 #x1b5a 
     #x1b61 #x1b6b #x1b74 #x1b7d #x1b80 #x1b82 #x1b83 #x1ba1 
     #x1ba2 #x1ba6 #x1ba8 #x1baa #x1bab #x1bae #x1bb0 #x1bba 
     #x1be6 #x1be7 #x1be8 #x1bea #x1bed #x1bee #x1bef #x1bf2 
     #x1bf4 #x1bfc #x1c00 #x1c24 #x1c2c #x1c34 #x1c36 #x1c38 
     #x1c3b #x1c40 #x1c4a #x1c4d #x1c50 #x1c5a #x1c78 #x1c7e 
     #x1c80 #x1cc0 #x1cc8 #x1cd0 #x1cd3 #x1cd4 #x1ce1 #x1ce2 
     #x1ce9 #x1ced #x1cee #x1cf2 #x1cf4 #x1cf5 #x1cf7 #x1cf8 
     #x1cfa #x1d00 #x1d2c #x1d6b #x1d78 #x1d79 #x1d9b #x1dc0 
     #x1df6 #x1dfc #x1e00 #x1e01 #x1e02 #x1e03 #x1e04 #x1e05 
     #x1e06 #x1e07 #x1e08 #x1e09 #x1e0a #x1e0b #x1e0c #x1e0d 
     #x1e0e #x1e0f #x1e10 #x1e11 #x1e12 #x1e13 #x1e14 #x1e15 
     #x1e16 #x1e17 #x1e18 #x1e19 #x1e1a #x1e1b #x1e1c #x1e1d 
     #x1e1e #x1e1f #x1e20 #x1e21 #x1e22 #x1e23 #x1e24 #x1e25 
     #x1e26 #x1e27 #x1e28 #x1e29 #x1e2a #x1e2b #x1e2c #x1e2d 
     #x1e2e #x1e2f #x1e30 #x1e31 #x1e32 #x1e33 #x1e34 #x1e35 
     #x1e36 #x1e37 #x1e38 #x1e39 #x1e3a #x1e3b #x1e3c #x1e3d 
     #x1e3e #x1e3f #x1e40 #x1e41 #x1e42 #x1e43 #x1e44 #x1e45 
     #x1e46 #x1e47 #x1e48 #x1e49 #x1e4a #x1e4b #x1e4c #x1e4d 
     #x1e4e #x1e4f #x1e50 #x1e51 #x1e52 #x1e53 #x1e54 #x1e55 
     #x1e56 #x1e57 #x1e58 #x1e59 #x1e5a #x1e5b #x1e5c #x1e5d 
     #x1e5e #x1e5f #x1e60 #x1e61 #x1e62 #x1e63 #x1e64 #x1e65 
     #x1e66 #x1e67 #x1e68 #x1e69 #x1e6a #x1e6b #x1e6c #x1e6d 
     #x1e6e #x1e6f #x1e70 #x1e71 #x1e72 #x1e73 #x1e74 #x1e75 
     #x1e76 #x1e77 #x1e78 #x1e79 #x1e7a #x1e7b #x1e7c #x1e7d 
     #x1e7e #x1e7f #x1e80 #x1e81 #x1e82 #x1e83 #x1e84 #x1e85 
     #x1e86 #x1e87 #x1e88 #x1e89 #x1e8a #x1e8b #x1e8c #x1e8d 
     #x1e8e #x1e8f #x1e90 #x1e91 #x1e92 #x1e93 #x1e94 #x1e95 
     #x1e9e #x1e9f #x1ea0 #x1ea1 #x1ea2 #x1ea3 #x1ea4 #x1ea5 
     #x1ea6 #x1ea7 #x1ea8 #x1ea9 #x1eaa #x1eab #x1eac #x1ead 
     #x1eae #x1eaf #x1eb0 #x1eb1 #x1eb2 #x1eb3 #x1eb4 #x1eb5 
     #x1eb6 #x1eb7 #x1eb8 #x1eb9 #x1eba #x1ebb #x1ebc #x1ebd 
     #x1ebe #x1ebf #x1ec0 #x1ec1 #x1ec2 #x1ec3 #x1ec4 #x1ec5 
     #x1ec6 #x1ec7 #x1ec8 #x1ec9 #x1eca #x1ecb #x1ecc #x1ecd 
     #x1ece #x1ecf #x1ed0 #x1ed1 #x1ed2 #x1ed3 #x1ed4 #x1ed5 
     #x1ed6 #x1ed7 #x1ed8 #x1ed9 #x1eda #x1edb #x1edc #x1edd 
     #x1ede #x1edf #x1ee0 #x1ee1 #x1ee2 #x1ee3 #x1ee4 #x1ee5 
     #x1ee6 #x1ee7 #x1ee8 #x1ee9 #x1eea #x1eeb #x1eec #x1eed 
     #x1eee #x1eef #x1ef0 #x1ef1 #x1ef2 #x1ef3 #x1ef4 #x1ef5 
     #x1ef6 #x1ef7 #x1ef8 #x1ef9 #x1efa #x1efb #x1efc #x1efd 
     #x1efe #x1eff #x1f08 #x1f10 #x1f16 #x1f18 #x1f1e #x1f20 
     #x1f28 #x1f30 #x1f38 #x1f40 #x1f46 #x1f48 #x1f4e #x1f50 
     #x1f58 #x1f59 #x1f5a #x1f5b #x1f5c #x1f5d #x1f5e #x1f5f 
     #x1f60 #x1f68 #x1f70 #x1f7e #x1f80 #x1f88 #x1f90 #x1f98 
     #x1fa0 #x1fa8 #x1fb0 #x1fb5 #x1fb6 #x1fb8 #x1fbc #x1fbd 
     #x1fbe #x1fbf #x1fc2 #x1fc5 #x1fc6 #x1fc8 #x1fcc #x1fcd 
     #x1fd0 #x1fd4 #x1fd6 #x1fd8 #x1fdc #x1fdd #x1fe0 #x1fe8 
     #x1fed #x1ff0 #x1ff2 #x1ff5 #x1ff6 #x1ff8 #x1ffc #x1ffd 
     #x1fff #x2000 #x200b #x2010 #x2016 #x2018 #x2019 #x201a 
     #x201b #x201d #x201e #x201f #x2020 #x2028 #x2029 #x202a 
     #x202f #x2030 #x2039 #x203a #x203b #x203f #x2041 #x2044 
     #x2045 #x2046 #x2047 #x2052 #x2053 #x2054 #x2055 #x205f 
     #x2060 #x2065 #x2066 #x2070 #x2071 #x2072 #x2074 #x207a 
     #x207d #x207e #x207f #x2080 #x208a #x208d #x208e #x208f 
     #x2090 #x209d #x20a0 #x20be #x20d0 #x20dd #x20e1 #x20e2 
     #x20e5 #x20f1 #x2100 #x2102 #x2103 #x2107 #x2108 #x210a 
     #x210b #x210e #x2110 #x2113 #x2114 #x2115 #x2116 #x2118 
     #x2119 #x211e #x2124 #x2125 #x2126 #x2127 #x2128 #x2129 
     #x212a #x212e #x212f #x2130 #x2134 #x2135 #x2139 #x213a 
     #x213c #x213e #x2140 #x2145 #x2146 #x214a #x214b #x214c 
     #x214e #x214f #x2150 #x2160 #x2183 #x2184 #x2185 #x2189 
     #x218a #x2190 #x2195 #x219a #x219c #x21a0 #x21a1 #x21a3 
     #x21a4 #x21a6 #x21a7 #x21ae #x21af #x21ce #x21d0 #x21d2 
     #x21d3 #x21d4 #x21d5 #x21f4 #x2300 #x2308 #x2309 #x230a 
     #x230b #x230c #x2320 #x2322 #x2329 #x232a #x232b #x237c 
     #x237d #x239b #x23b4 #x23dc #x23e2 #x23fb #x2400 #x2427 
     #x2440 #x244b #x2460 #x249c #x24ea #x2500 #x25b7 #x25b8 
     #x25c1 #x25c2 #x25f8 #x2600 #x266f #x2670 #x2768 #x2769 
     #x276a #x276b #x276c #x276d #x276e #x276f #x2770 #x2771 
     #x2772 #x2773 #x2774 #x2775 #x2776 #x2794 #x27c0 #x27c5 
     #x27c6 #x27c7 #x27e6 #x27e7 #x27e8 #x27e9 #x27ea #x27eb 
     #x27ec #x27ed #x27ee #x27ef #x27f0 #x2800 #x2900 #x2983 
     #x2984 #x2985 #x2986 #x2987 #x2988 #x2989 #x298a #x298b 
     #x298c #x298d #x298e #x298f #x2990 #x2991 #x2992 #x2993 
     #x2994 #x2995 #x2996 #x2997 #x2998 #x2999 #x29d8 #x29d9 
     #x29da #x29db #x29dc #x29fc #x29fd #x29fe #x2b00 #x2b30 
     #x2b45 #x2b47 #x2b4d #x2b74 #x2b76 #x2b96 #x2b98 #x2bba 
     #x2bbd #x2bc9 #x2bca #x2bd2 #x2c00 #x2c2f #x2c30 #x2c5f 
     #x2c60 #x2c61 #x2c62 #x2c65 #x2c67 #x2c68 #x2c69 #x2c6a 
     #x2c6b #x2c6c #x2c6d #x2c71 #x2c72 #x2c73 #x2c75 #x2c76 
     #x2c7c #x2c7e #x2c81 #x2c82 #x2c83 #x2c84 #x2c85 #x2c86 
     #x2c87 #x2c88 #x2c89 #x2c8a #x2c8b #x2c8c #x2c8d #x2c8e 
     #x2c8f #x2c90 #x2c91 #x2c92 #x2c93 #x2c94 #x2c95 #x2c96 
     #x2c97 #x2c98 #x2c99 #x2c9a #x2c9b #x2c9c #x2c9d #x2c9e 
     #x2c9f #x2ca0 #x2ca1 #x2ca2 #x2ca3 #x2ca4 #x2ca5 #x2ca6 
     #x2ca7 #x2ca8 #x2ca9 #x2caa #x2cab #x2cac #x2cad #x2cae 
     #x2caf #x2cb0 #x2cb1 #x2cb2 #x2cb3 #x2cb4 #x2cb5 #x2cb6 
     #x2cb7 #x2cb8 #x2cb9 #x2cba #x2cbb #x2cbc #x2cbd #x2cbe 
     #x2cbf #x2cc0 #x2cc1 #x2cc2 #x2cc3 #x2cc4 #x2cc5 #x2cc6 
     #x2cc7 #x2cc8 #x2cc9 #x2cca #x2ccb #x2ccc #x2ccd #x2cce 
     #x2ccf #x2cd0 #x2cd1 #x2cd2 #x2cd3 #x2cd4 #x2cd5 #x2cd6 
     #x2cd7 #x2cd8 #x2cd9 #x2cda #x2cdb #x2cdc #x2cdd #x2cde 
     #x2cdf #x2ce0 #x2ce1 #x2ce2 #x2ce3 #x2ce5 #x2ceb #x2cec 
     #x2ced #x2cee #x2cef #x2cf2 #x2cf3 #x2cf4 #x2cf9 #x2cfd 
     #x2cfe #x2d00 #x2d26 #x2d27 #x2d28 #x2d2d #x2d2e #x2d30 
     #x2d68 #x2d6f #x2d70 #x2d71 #x2d7f #x2d80 #x2d97 #x2da0 
     #x2da7 #x2da8 #x2daf #x2db0 #x2db7 #x2db8 #x2dbf #x2dc0 
     #x2dc7 #x2dc8 #x2dcf #x2dd0 #x2dd7 #x2dd8 #x2ddf #x2de0 
     #x2e00 #x2e02 #x2e03 #x2e04 #x2e05 #x2e06 #x2e09 #x2e0a 
     #x2e0b #x2e0c #x2e0d #x2e0e #x2e17 #x2e18 #x2e1a #x2e1b 
     #x2e1c #x2e1d #x2e1e #x2e20 #x2e21 #x2e22 #x2e23 #x2e24 
     #x2e25 #x2e26 #x2e27 #x2e28 #x2e29 #x2e2a #x2e2f #x2e30 
     #x2e3a #x2e3c #x2e40 #x2e41 #x2e42 #x2e43 #x2e80 #x2e9a 
     #x2e9b #x2ef4 #x2f00 #x2fd6 #x2ff0 #x2ffc #x3000 #x3001 
     #x3004 #x3005 #x3006 #x3007 #x3008 #x3009 #x300a #x300b 
     #x300c #x300d #x300e #x300f #x3010 #x3011 #x3012 #x3014 
     #x3015 #x3016 #x3017 #x3018 #x3019 #x301a #x301b #x301c 
     #x301d #x301e #x3020 #x3021 #x302a #x302e #x3030 #x3031 
     #x3036 #x3038 #x303b #x303c #x303d #x303e #x3040 #x3041 
     #x3097 #x3099 #x309b #x309d #x309f #x30a0 #x30a1 #x30fb 
     #x30fc #x30ff #x3100 #x3105 #x312e #x3131 #x318f #x3190 
     #x3192 #x3196 #x31a0 #x31bb #x31c0 #x31e4 #x31f0 #x3200 
     #x321f #x3220 #x322a #x3248 #x3250 #x3251 #x3260 #x3280 
     #x328a #x32b1 #x32c0 #x32ff #x3300 #x3400 #x4db6 #x4dc0 
     #x4e00 #x9fcd #xa000 #xa015 #xa016 #xa48d #xa490 #xa4c7 
     #xa4d0 #xa4f8 #xa4fe #xa500 #xa60c #xa60d #xa610 #xa620 
     #xa62a #xa62c #xa640 #xa641 #xa642 #xa643 #xa644 #xa645 
     #xa646 #xa647 #xa648 #xa649 #xa64a #xa64b #xa64c #xa64d 
     #xa64e #xa64f #xa650 #xa651 #xa652 #xa653 #xa654 #xa655 
     #xa656 #xa657 #xa658 #xa659 #xa65a #xa65b #xa65c #xa65d 
     #xa65e #xa65f #xa660 #xa661 #xa662 #xa663 #xa664 #xa665 
     #xa666 #xa667 #xa668 #xa669 #xa66a #xa66b #xa66c #xa66d 
     #xa66e #xa66f #xa670 #xa673 #xa674 #xa67e #xa67f #xa680 
     #xa681 #xa682 #xa683 #xa684 #xa685 #xa686 #xa687 #xa688 
     #xa689 #xa68a #xa68b #xa68c #xa68d #xa68e #xa68f #xa690 
     #xa691 #xa692 #xa693 #xa694 #xa695 #xa696 #xa697 #xa698 
     #xa699 #xa69a #xa69b #xa69c #xa69e #xa69f #xa6a0 #xa6e6 
     #xa6f0 #xa6f2 #xa6f8 #xa700 #xa717 #xa720 #xa722 #xa723 
     #xa724 #xa725 #xa726 #xa727 #xa728 #xa729 #xa72a #xa72b 
     #xa72c #xa72d #xa72e #xa72f #xa732 #xa733 #xa734 #xa735 
     #xa736 #xa737 #xa738 #xa739 #xa73a #xa73b #xa73c #xa73d 
     #xa73e #xa73f #xa740 #xa741 #xa742 #xa743 #xa744 #xa745 
     #xa746 #xa747 #xa748 #xa749 #xa74a #xa74b #xa74c #xa74d 
     #xa74e #xa74f #xa750 #xa751 #xa752 #xa753 #xa754 #xa755 
     #xa756 #xa757 #xa758 #xa759 #xa75a #xa75b #xa75c #xa75d 
     #xa75e #xa75f #xa760 #xa761 #xa762 #xa763 #xa764 #xa765 
     #xa766 #xa767 #xa768 #xa769 #xa76a #xa76b #xa76c #xa76d 
     #xa76e #xa76f #xa770 #xa771 #xa779 #xa77a #xa77b #xa77c 
     #xa77d #xa77f #xa780 #xa781 #xa782 #xa783 #xa784 #xa785 
     #xa786 #xa787 #xa788 #xa789 #xa78b #xa78c #xa78d #xa78e 
     #xa78f #xa790 #xa791 #xa792 #xa793 #xa796 #xa797 #xa798 
     #xa799 #xa79a #xa79b #xa79c #xa79d #xa79e #xa79f #xa7a0 
     #xa7a1 #xa7a2 #xa7a3 #xa7a4 #xa7a5 #xa7a6 #xa7a7 #xa7a8 
     #xa7a9 #xa7aa #xa7ae #xa7b0 #xa7b2 #xa7f7 #xa7f8 #xa7fa 
     #xa7fb #xa802 #xa803 #xa806 #xa807 #xa80b #xa80c #xa823 
     #xa825 #xa827 #xa828 #xa82c #xa830 #xa836 #xa838 #xa839 
     #xa83a #xa840 #xa874 #xa878 #xa880 #xa882 #xa8b4 #xa8c4 
     #xa8c5 #xa8ce #xa8d0 #xa8da #xa8e0 #xa8f2 #xa8f8 #xa8fb 
     #xa8fc #xa900 #xa90a #xa926 #xa92e #xa930 #xa947 #xa952 
     #xa954 #xa95f #xa960 #xa97d #xa980 #xa983 #xa984 #xa9b3 
     #xa9b4 #xa9b6 #xa9ba #xa9bc #xa9bd #xa9c1 #xa9ce #xa9cf 
     #xa9d0 #xa9da #xa9de #xa9e0 #xa9e5 #xa9e6 #xa9e7 #xa9f0 
     #xa9fa #xa9ff #xaa00 #xaa29 #xaa2f #xaa31 #xaa33 #xaa35 
     #xaa37 #xaa40 #xaa43 #xaa44 #xaa4c #xaa4d #xaa4e #xaa50 
     #xaa5a #xaa5c #xaa60 #xaa70 #xaa71 #xaa77 #xaa7a #xaa7b 
     #xaa7c #xaa7d #xaa7e #xaab0 #xaab1 #xaab2 #xaab5 #xaab7 
     #xaab9 #xaabe #xaac0 #xaac1 #xaac2 #xaac3 #xaadb #xaadd 
     #xaade #xaae0 #xaaeb #xaaec #xaaee #xaaf0 #xaaf2 #xaaf3 
     #xaaf5 #xaaf6 #xaaf7 #xab01 #xab07 #xab09 #xab0f #xab11 
     #xab17 #xab20 #xab27 #xab28 #xab2f #xab30 #xab5b #xab5c 
     #xab60 #xab64 #xab66 #xabc0 #xabe3 #xabe5 #xabe6 #xabe8 
     #xabe9 #xabeb #xabec #xabed #xabee #xabf0 #xabfa #xac00 
     #xd7a4 #xd7b0 #xd7c7 #xd7cb #xd7fc #xd800 #xe000 #xf900 
     #xfa6e #xfa70 #xfada #xfb00 #xfb07 #xfb13 #xfb18 #xfb1d 
     #xfb1e #xfb1f #xfb29 #xfb2a #xfb37 #xfb38 #xfb3d #xfb3e 
     #xfb3f #xfb40 #xfb42 #xfb43 #xfb45 #xfb46 #xfbb2 #xfbc2 
     #xfbd3 #xfd3e #xfd3f #xfd40 #xfd50 #xfd90 #xfd92 #xfdc8 
     #xfdf0 #xfdfc #xfdfd #xfdfe #xfe00 #xfe10 #xfe17 #xfe18 
     #xfe19 #xfe1a #xfe20 #xfe2e #xfe30 #xfe31 #xfe33 #xfe35 
     #xfe36 #xfe37 #xfe38 #xfe39 #xfe3a #xfe3b #xfe3c #xfe3d 
     #xfe3e #xfe3f #xfe40 #xfe41 #xfe42 #xfe43 #xfe44 #xfe45 
     #xfe47 #xfe48 #xfe49 #xfe4d #xfe50 #xfe53 #xfe54 #xfe58 
     #xfe59 #xfe5a #xfe5b #xfe5c #xfe5d #xfe5e #xfe5f #xfe62 
     #xfe63 #xfe64 #xfe67 #xfe68 #xfe69 #xfe6a #xfe6c #xfe70 
     #xfe75 #xfe76 #xfefd #xfeff #xff00 #xff01 #xff04 #xff05 
     #xff08 #xff09 #xff0a #xff0b #xff0c #xff0d #xff0e #xff10 
     #xff1a #xff1c #xff1f #xff21 #xff3b #xff3c #xff3d #xff3e 
     #xff3f #xff40 #xff41 #xff5b #xff5c #xff5d #xff5e #xff5f 
     #xff60 #xff61 #xff62 #xff63 #xff64 #xff66 #xff70 #xff71 
     #xff9e #xffa0 #xffbf #xffc2 #xffc8 #xffca #xffd0 #xffd2 
     #xffd8 #xffda #xffdd #xffe0 #xffe2 #xffe3 #xffe4 #xffe5 
     #xffe7 #xffe8 #xffe9 #xffed #xffef #xfff9 #xfffc #xfffe 
     #x10000 #x1000c #x1000d #x10027 #x10028 #x1003b #x1003c #x1003e 
     #x1003f #x1004e #x10050 #x1005e #x10080 #x100fb #x10100 #x10103 
     #x10107 #x10134 #x10137 #x10140 #x10175 #x10179 #x1018a #x1018c 
     #x1018d #x10190 #x1019c #x101a0 #x101a1 #x101d0 #x101fd #x101fe 
     #x10280 #x1029d #x102a0 #x102d1 #x102e0 #x102e1 #x102fc #x10300 
     #x10320 #x10324 #x10330 #x10341 #x10342 #x1034a #x1034b #x10350 
     #x10376 #x1037b #x10380 #x1039e #x1039f #x103a0 #x103c4 #x103c8 
     #x103d0 #x103d1 #x103d6 #x10400 #x10428 #x10450 #x1049e #x104a0 
     #x104aa #x10500 #x10528 #x10530 #x10564 #x1056f #x10570 #x10600 
     #x10737 #x10740 #x10756 #x10760 #x10768 #x10800 #x10806 #x10808 
     #x10809 #x1080a #x10836 #x10837 #x10839 #x1083c #x1083d #x1083f 
     #x10856 #x10857 #x10858 #x10860 #x10877 #x10879 #x10880 #x1089f 
     #x108a7 #x108b0 #x10900 #x10916 #x1091c #x1091f #x10920 #x1093a 
     #x1093f #x10940 #x10980 #x109b8 #x109be #x109c0 #x10a00 #x10a01 
     #x10a04 #x10a05 #x10a07 #x10a0c #x10a10 #x10a14 #x10a15 #x10a18 
     #x10a19 #x10a34 #x10a38 #x10a3b #x10a3f #x10a40 #x10a48 #x10a50 
     #x10a59 #x10a60 #x10a7d #x10a7f #x10a80 #x10a9d #x10aa0 #x10ac0 
     #x10ac8 #x10ac9 #x10ae5 #x10ae7 #x10aeb #x10af0 #x10af7 #x10b00 
     #x10b36 #x10b39 #x10b40 #x10b56 #x10b58 #x10b60 #x10b73 #x10b78 
     #x10b80 #x10b92 #x10b99 #x10b9d #x10ba9 #x10bb0 #x10c00 #x10c49 
     #x10e60 #x10e7f #x11000 #x11001 #x11002 #x11003 #x11038 #x11047 
     #x1104e #x11052 #x11066 #x11070 #x1107f #x11082 #x11083 #x110b0 
     #x110b3 #x110b7 #x110b9 #x110bb #x110bd #x110be #x110c2 #x110d0 
     #x110e9 #x110f0 #x110fa #x11100 #x11103 #x11127 #x1112c #x1112d 
     #x11135 #x11136 #x11140 #x11144 #x11150 #x11173 #x11174 #x11176 
     #x11177 #x11180 #x11182 #x11183 #x111b3 #x111b6 #x111bf #x111c1 
     #x111c5 #x111c9 #x111cd #x111ce #x111d0 #x111da #x111db #x111e1 
     #x111f5 #x11200 #x11212 #x11213 #x1122c #x1122f #x11232 #x11234 
     #x11235 #x11236 #x11238 #x1123e #x112b0 #x112df #x112e0 #x112e3 
     #x112eb #x112f0 #x112fa #x11301 #x11302 #x11304 #x11305 #x1130d 
     #x1130f #x11311 #x11313 #x11329 #x1132a #x11331 #x11332 #x11334 
     #x11335 #x1133a #x1133c #x1133d #x1133e #x11340 #x11341 #x11345 
     #x11347 #x11349 #x1134b #x1134e #x11357 #x11358 #x1135d #x11362 
     #x11364 #x11366 #x1136d #x11370 #x11375 #x11480 #x114b0 #x114b3 
     #x114b9 #x114ba #x114bb #x114bf #x114c1 #x114c2 #x114c4 #x114c6 
     #x114c7 #x114c8 #x114d0 #x114da #x11580 #x115af #x115b2 #x115b6 
     #x115b8 #x115bc #x115be #x115bf #x115c1 #x115ca #x11600 #x11630 
     #x11633 #x1163b #x1163d #x1163e #x1163f #x11641 #x11644 #x11645 
     #x11650 #x1165a #x11680 #x116ab #x116ac #x116ad #x116ae #x116b0 
     #x116b6 #x116b7 #x116b8 #x116c0 #x116ca #x118a0 #x118c0 #x118e0 
     #x118ea #x118f3 #x118ff #x11900 #x11ac0 #x11af9 #x12000 #x12399 
     #x12400 #x1246f #x12470 #x12475 #x13000 #x1342f #x16800 #x16a39 
     #x16a40 #x16a5f #x16a60 #x16a6a #x16a6e #x16a70 #x16ad0 #x16aee 
     #x16af0 #x16af5 #x16af6 #x16b00 #x16b30 #x16b37 #x16b3c #x16b40 
     #x16b44 #x16b45 #x16b46 #x16b50 #x16b5a #x16b5b #x16b62 #x16b63 
     #x16b78 #x16b7d #x16b90 #x16f00 #x16f45 #x16f50 #x16f51 #x16f7f 
     #x16f8f #x16f93 #x16fa0 #x1b000 #x1b002 #x1bc00 #x1bc6b #x1bc70 
     #x1bc7d #x1bc80 #x1bc89 #x1bc90 #x1bc9a #x1bc9c #x1bc9d #x1bc9f 
     #x1bca0 #x1bca4 #x1d000 #x1d0f6 #x1d100 #x1d127 #x1d129 #x1d165 
     #x1d167 #x1d16a #x1d16d #x1d173 #x1d17b #x1d183 #x1d185 #x1d18c 
     #x1d1aa #x1d1ae #x1d1de #x1d200 #x1d242 #x1d245 #x1d246 #x1d300 
     #x1d357 #x1d360 #x1d372 #x1d400 #x1d41a #x1d434 #x1d44e #x1d455 
     #x1d456 #x1d468 #x1d482 #x1d49c #x1d49d #x1d49e #x1d4a0 #x1d4a2 
     #x1d4a3 #x1d4a5 #x1d4a7 #x1d4a9 #x1d4ad #x1d4ae #x1d4b6 #x1d4ba 
     #x1d4bb #x1d4bc #x1d4bd #x1d4c4 #x1d4c5 #x1d4d0 #x1d4ea #x1d504 
     #x1d506 #x1d507 #x1d50b #x1d50d #x1d515 #x1d516 #x1d51d #x1d51e 
     #x1d538 #x1d53a #x1d53b #x1d53f #x1d540 #x1d545 #x1d546 #x1d547 
     #x1d54a #x1d551 #x1d552 #x1d56c #x1d586 #x1d5a0 #x1d5ba #x1d5d4 
     #x1d5ee #x1d608 #x1d622 #x1d63c #x1d656 #x1d670 #x1d68a #x1d6a6 
     #x1d6a8 #x1d6c1 #x1d6c2 #x1d6db #x1d6dc #x1d6e2 #x1d6fb #x1d6fc 
     #x1d715 #x1d716 #x1d71c #x1d735 #x1d736 #x1d74f #x1d750 #x1d756 
     #x1d76f #x1d770 #x1d789 #x1d78a #x1d790 #x1d7a9 #x1d7aa #x1d7c3 
     #x1d7c4 #x1d7ca #x1d7cb #x1d7cc #x1d7ce #x1d800 #x1e800 #x1e8c5 
     #x1e8c7 #x1e8d0 #x1e8d7 #x1ee00 #x1ee04 #x1ee05 #x1ee20 #x1ee21 
     #x1ee23 #x1ee24 #x1ee25 #x1ee27 #x1ee28 #x1ee29 #x1ee33 #x1ee34 
     #x1ee38 #x1ee39 #x1ee3a #x1ee3b #x1ee3c #x1ee42 #x1ee43 #x1ee47 
     #x1ee48 #x1ee49 #x1ee4a #x1ee4b #x1ee4c #x1ee4d #x1ee50 #x1ee51 
     #x1ee53 #x1ee54 #x1ee55 #x1ee57 #x1ee58 #x1ee59 #x1ee5a #x1ee5b 
     #x1ee5c #x1ee5d #x1ee5e #x1ee5f #x1ee60 #x1ee61 #x1ee63 #x1ee64 
     #x1ee65 #x1ee67 #x1ee6b #x1ee6c #x1ee73 #x1ee74 #x1ee78 #x1ee79 
     #x1ee7d #x1ee7e #x1ee7f #x1ee80 #x1ee8a #x1ee8b #x1ee9c #x1eea1 
     #x1eea4 #x1eea5 #x1eeaa #x1eeab #x1eebc #x1eef0 #x1eef2 #x1f000 
     #x1f02c #x1f030 #x1f094 #x1f0a0 #x1f0af #x1f0b1 #x1f0c0 #x1f0c1 
     #x1f0d0 #x1f0d1 #x1f0f6 #x1f100 #x1f10d #x1f110 #x1f12f #x1f130 
     #x1f16c #x1f170 #x1f19b #x1f1e6 #x1f203 #x1f210 #x1f23b #x1f240 
     #x1f249 #x1f250 #x1f252 #x1f300 #x1f32d #x1f330 #x1f37e #x1f380 
     #x1f3cf #x1f3d4 #x1f3f8 #x1f400 #x1f4ff #x1f500 #x1f54b #x1f550 
     #x1f57a #x1f57b #x1f5a4 #x1f5a5 #x1f643 #x1f645 #x1f6d0 #x1f6e0 
     #x1f6ed #x1f6f0 #x1f6f4 #x1f700 #x1f774 #x1f780 #x1f7d5 #x1f800 
     #x1f80c #x1f810 #x1f848 #x1f850 #x1f85a #x1f860 #x1f888 #x1f890 
     #x1f8ae #x20000 #x2a6d7 #x2a700 #x2b735 #x2b740 #x2b81e #x2f800 
     #x2fa1e #xe0001 #xe0002 #xe0020 #xe0080 #xe0100 #xe01f0 #xf0000 
     #xffffe #x100000 #x10fffe ))

; The following vector contains the general category for
; characters whose Unicode scalar value is less than 256.
;
; This table contains 256 entries.

(define general-category-indices-for-common-characters
  (u8-list->bytevector
   (map
    general-category-symbol->index
    '(
       Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc 
       Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc 
       Zs Po Po Po Sc Po Po Po Ps Pe Po Sm Po Pd Po Po 
       Nd Nd Nd Nd Nd Nd Nd Nd Nd Nd Po Po Sm Sm Sm Po 
       Po Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu 
       Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Ps Po Pe Sk Pc 
       Sk Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll 
       Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ps Sm Pe Sm Cc 
       Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc 
       Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc 
       Zs Po Sc Sc Sc Sc So Po Sk So Lo Pi Sm Cf So Sk 
       So Sm No No Sk Ll Po Po Sk No Lo Pf No No No Po 
       Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu 
       Lu Lu Lu Lu Lu Lu Lu Sm Lu Lu Lu Lu Lu Lu Lu Ll 
       Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll 
       Ll Ll Ll Ll Ll Ll Ll Sm Ll Ll Ll Ll Ll Ll Ll Ll ))))

; The code below is commented out because it is
; redundant with the table above.  The code
; below remains because it might be useful in
; case-insensitive systems that don't read the
; table above correctly.

;(define general-category-indices-for-common-characters
;  (do ((i 0 (+ i 1))
;       (bv (make-bytevector 256)))
;      ((= i 256)
;       bv)
;    (bytevector-set! bv
;                     i
;                     (general-category-symbol->index
;                      (char-general-category
;                       (integer->char i))))))

; This vector contains the numerical adjustments to make
; when converting a character from one case to another.
; For conversions to uppercase or titlecase, add the
; adjustment contained in this vector.
; For conversions to lowercase, subtract the adjustment
; contained in this vector.
;
; This table contains 85 elements.

(define simple-case-adjustments
  '#(
     #x-2a2b #x-2a28 #x-1c60 #x-1c25 #x-12c #x-e8 #x-db #x-da 
     #x-d9 #x-d6 #x-d5 #x-d3 #x-d2 #x-d1 #x-cf #x-ce 
     #x-cd #x-cb #x-ca #x-74 #x-60 #x-56 #x-50 #x-4f 
     #x-47 #x-45 #x-40 #x-3f #x-3e #x-3b #x-39 #x-36 
     #x-30 #x-2f #x-28 #x-26 #x-25 #x-20 #x-1f #x-1c 
     #x-1a #x-10 #x-f #x-8 #x-2 #x-1 #x7 #x8 
     #x9 #x38 #x3c #x4a #x54 #x56 #x61 #x64 
     #x70 #x79 #x7e #x80 #x82 #xa3 #xc3 #xc7 
     #x2e7 #xee6 #x1d5d #x1dbf #x2046 #x20bf #x29e7 #x29f7 
     #x29fd #x2a1c #x2a1e #x2a1f #x2a3f #x8a04 #xa512 #xa528 
     #xa52a #xa541 #xa544 #xa54b #xa54f ))

; This bytevector uses two bytes per code point
; to list all 16-bit code points, in increasing order,
; that have a simple uppercase mapping.
;
; This table contains 2056 elements.

(define simple-upcase-chars-16bit
  '#u8(
        #x0 #x61 #x0 #x62 #x0 #x63 #x0 #x64 
        #x0 #x65 #x0 #x66 #x0 #x67 #x0 #x68 
        #x0 #x69 #x0 #x6a #x0 #x6b #x0 #x6c 
        #x0 #x6d #x0 #x6e #x0 #x6f #x0 #x70 
        #x0 #x71 #x0 #x72 #x0 #x73 #x0 #x74 
        #x0 #x75 #x0 #x76 #x0 #x77 #x0 #x78 
        #x0 #x79 #x0 #x7a #x0 #xb5 #x0 #xe0 
        #x0 #xe1 #x0 #xe2 #x0 #xe3 #x0 #xe4 
        #x0 #xe5 #x0 #xe6 #x0 #xe7 #x0 #xe8 
        #x0 #xe9 #x0 #xea #x0 #xeb #x0 #xec 
        #x0 #xed #x0 #xee #x0 #xef #x0 #xf0 
        #x0 #xf1 #x0 #xf2 #x0 #xf3 #x0 #xf4 
        #x0 #xf5 #x0 #xf6 #x0 #xf8 #x0 #xf9 
        #x0 #xfa #x0 #xfb #x0 #xfc #x0 #xfd 
        #x0 #xfe #x0 #xff #x1 #x1 #x1 #x3 
        #x1 #x5 #x1 #x7 #x1 #x9 #x1 #xb 
        #x1 #xd #x1 #xf #x1 #x11 #x1 #x13 
        #x1 #x15 #x1 #x17 #x1 #x19 #x1 #x1b 
        #x1 #x1d #x1 #x1f #x1 #x21 #x1 #x23 
        #x1 #x25 #x1 #x27 #x1 #x29 #x1 #x2b 
        #x1 #x2d #x1 #x2f #x1 #x31 #x1 #x33 
        #x1 #x35 #x1 #x37 #x1 #x3a #x1 #x3c 
        #x1 #x3e #x1 #x40 #x1 #x42 #x1 #x44 
        #x1 #x46 #x1 #x48 #x1 #x4b #x1 #x4d 
        #x1 #x4f #x1 #x51 #x1 #x53 #x1 #x55 
        #x1 #x57 #x1 #x59 #x1 #x5b #x1 #x5d 
        #x1 #x5f #x1 #x61 #x1 #x63 #x1 #x65 
        #x1 #x67 #x1 #x69 #x1 #x6b #x1 #x6d 
        #x1 #x6f #x1 #x71 #x1 #x73 #x1 #x75 
        #x1 #x77 #x1 #x7a #x1 #x7c #x1 #x7e 
        #x1 #x7f #x1 #x80 #x1 #x83 #x1 #x85 
        #x1 #x88 #x1 #x8c #x1 #x92 #x1 #x95 
        #x1 #x99 #x1 #x9a #x1 #x9e #x1 #xa1 
        #x1 #xa3 #x1 #xa5 #x1 #xa8 #x1 #xad 
        #x1 #xb0 #x1 #xb4 #x1 #xb6 #x1 #xb9 
        #x1 #xbd #x1 #xbf #x1 #xc5 #x1 #xc6 
        #x1 #xc8 #x1 #xc9 #x1 #xcb #x1 #xcc 
        #x1 #xce #x1 #xd0 #x1 #xd2 #x1 #xd4 
        #x1 #xd6 #x1 #xd8 #x1 #xda #x1 #xdc 
        #x1 #xdd #x1 #xdf #x1 #xe1 #x1 #xe3 
        #x1 #xe5 #x1 #xe7 #x1 #xe9 #x1 #xeb 
        #x1 #xed #x1 #xef #x1 #xf2 #x1 #xf3 
        #x1 #xf5 #x1 #xf9 #x1 #xfb #x1 #xfd 
        #x1 #xff #x2 #x1 #x2 #x3 #x2 #x5 
        #x2 #x7 #x2 #x9 #x2 #xb #x2 #xd 
        #x2 #xf #x2 #x11 #x2 #x13 #x2 #x15 
        #x2 #x17 #x2 #x19 #x2 #x1b #x2 #x1d 
        #x2 #x1f #x2 #x23 #x2 #x25 #x2 #x27 
        #x2 #x29 #x2 #x2b #x2 #x2d #x2 #x2f 
        #x2 #x31 #x2 #x33 #x2 #x3c #x2 #x3f 
        #x2 #x40 #x2 #x42 #x2 #x47 #x2 #x49 
        #x2 #x4b #x2 #x4d #x2 #x4f #x2 #x50 
        #x2 #x51 #x2 #x52 #x2 #x53 #x2 #x54 
        #x2 #x56 #x2 #x57 #x2 #x59 #x2 #x5b 
        #x2 #x5c #x2 #x60 #x2 #x61 #x2 #x63 
        #x2 #x65 #x2 #x66 #x2 #x68 #x2 #x69 
        #x2 #x6b #x2 #x6c #x2 #x6f #x2 #x71 
        #x2 #x72 #x2 #x75 #x2 #x7d #x2 #x80 
        #x2 #x83 #x2 #x87 #x2 #x88 #x2 #x89 
        #x2 #x8a #x2 #x8b #x2 #x8c #x2 #x92 
        #x2 #x9e #x3 #x45 #x3 #x71 #x3 #x73 
        #x3 #x77 #x3 #x7b #x3 #x7c #x3 #x7d 
        #x3 #xac #x3 #xad #x3 #xae #x3 #xaf 
        #x3 #xb1 #x3 #xb2 #x3 #xb3 #x3 #xb4 
        #x3 #xb5 #x3 #xb6 #x3 #xb7 #x3 #xb8 
        #x3 #xb9 #x3 #xba #x3 #xbb #x3 #xbc 
        #x3 #xbd #x3 #xbe #x3 #xbf #x3 #xc0 
        #x3 #xc1 #x3 #xc2 #x3 #xc3 #x3 #xc4 
        #x3 #xc5 #x3 #xc6 #x3 #xc7 #x3 #xc8 
        #x3 #xc9 #x3 #xca #x3 #xcb #x3 #xcc 
        #x3 #xcd #x3 #xce #x3 #xd0 #x3 #xd1 
        #x3 #xd5 #x3 #xd6 #x3 #xd7 #x3 #xd9 
        #x3 #xdb #x3 #xdd #x3 #xdf #x3 #xe1 
        #x3 #xe3 #x3 #xe5 #x3 #xe7 #x3 #xe9 
        #x3 #xeb #x3 #xed #x3 #xef #x3 #xf0 
        #x3 #xf1 #x3 #xf2 #x3 #xf3 #x3 #xf5 
        #x3 #xf8 #x3 #xfb #x4 #x30 #x4 #x31 
        #x4 #x32 #x4 #x33 #x4 #x34 #x4 #x35 
        #x4 #x36 #x4 #x37 #x4 #x38 #x4 #x39 
        #x4 #x3a #x4 #x3b #x4 #x3c #x4 #x3d 
        #x4 #x3e #x4 #x3f #x4 #x40 #x4 #x41 
        #x4 #x42 #x4 #x43 #x4 #x44 #x4 #x45 
        #x4 #x46 #x4 #x47 #x4 #x48 #x4 #x49 
        #x4 #x4a #x4 #x4b #x4 #x4c #x4 #x4d 
        #x4 #x4e #x4 #x4f #x4 #x50 #x4 #x51 
        #x4 #x52 #x4 #x53 #x4 #x54 #x4 #x55 
        #x4 #x56 #x4 #x57 #x4 #x58 #x4 #x59 
        #x4 #x5a #x4 #x5b #x4 #x5c #x4 #x5d 
        #x4 #x5e #x4 #x5f #x4 #x61 #x4 #x63 
        #x4 #x65 #x4 #x67 #x4 #x69 #x4 #x6b 
        #x4 #x6d #x4 #x6f #x4 #x71 #x4 #x73 
        #x4 #x75 #x4 #x77 #x4 #x79 #x4 #x7b 
        #x4 #x7d #x4 #x7f #x4 #x81 #x4 #x8b 
        #x4 #x8d #x4 #x8f #x4 #x91 #x4 #x93 
        #x4 #x95 #x4 #x97 #x4 #x99 #x4 #x9b 
        #x4 #x9d #x4 #x9f #x4 #xa1 #x4 #xa3 
        #x4 #xa5 #x4 #xa7 #x4 #xa9 #x4 #xab 
        #x4 #xad #x4 #xaf #x4 #xb1 #x4 #xb3 
        #x4 #xb5 #x4 #xb7 #x4 #xb9 #x4 #xbb 
        #x4 #xbd #x4 #xbf #x4 #xc2 #x4 #xc4 
        #x4 #xc6 #x4 #xc8 #x4 #xca #x4 #xcc 
        #x4 #xce #x4 #xcf #x4 #xd1 #x4 #xd3 
        #x4 #xd5 #x4 #xd7 #x4 #xd9 #x4 #xdb 
        #x4 #xdd #x4 #xdf #x4 #xe1 #x4 #xe3 
        #x4 #xe5 #x4 #xe7 #x4 #xe9 #x4 #xeb 
        #x4 #xed #x4 #xef #x4 #xf1 #x4 #xf3 
        #x4 #xf5 #x4 #xf7 #x4 #xf9 #x4 #xfb 
        #x4 #xfd #x4 #xff #x5 #x1 #x5 #x3 
        #x5 #x5 #x5 #x7 #x5 #x9 #x5 #xb 
        #x5 #xd #x5 #xf #x5 #x11 #x5 #x13 
        #x5 #x15 #x5 #x17 #x5 #x19 #x5 #x1b 
        #x5 #x1d #x5 #x1f #x5 #x21 #x5 #x23 
        #x5 #x25 #x5 #x27 #x5 #x29 #x5 #x2b 
        #x5 #x2d #x5 #x2f #x5 #x61 #x5 #x62 
        #x5 #x63 #x5 #x64 #x5 #x65 #x5 #x66 
        #x5 #x67 #x5 #x68 #x5 #x69 #x5 #x6a 
        #x5 #x6b #x5 #x6c #x5 #x6d #x5 #x6e 
        #x5 #x6f #x5 #x70 #x5 #x71 #x5 #x72 
        #x5 #x73 #x5 #x74 #x5 #x75 #x5 #x76 
        #x5 #x77 #x5 #x78 #x5 #x79 #x5 #x7a 
        #x5 #x7b #x5 #x7c #x5 #x7d #x5 #x7e 
        #x5 #x7f #x5 #x80 #x5 #x81 #x5 #x82 
        #x5 #x83 #x5 #x84 #x5 #x85 #x5 #x86 
        #x1d #x79 #x1d #x7d #x1e #x1 #x1e #x3 
        #x1e #x5 #x1e #x7 #x1e #x9 #x1e #xb 
        #x1e #xd #x1e #xf #x1e #x11 #x1e #x13 
        #x1e #x15 #x1e #x17 #x1e #x19 #x1e #x1b 
        #x1e #x1d #x1e #x1f #x1e #x21 #x1e #x23 
        #x1e #x25 #x1e #x27 #x1e #x29 #x1e #x2b 
        #x1e #x2d #x1e #x2f #x1e #x31 #x1e #x33 
        #x1e #x35 #x1e #x37 #x1e #x39 #x1e #x3b 
        #x1e #x3d #x1e #x3f #x1e #x41 #x1e #x43 
        #x1e #x45 #x1e #x47 #x1e #x49 #x1e #x4b 
        #x1e #x4d #x1e #x4f #x1e #x51 #x1e #x53 
        #x1e #x55 #x1e #x57 #x1e #x59 #x1e #x5b 
        #x1e #x5d #x1e #x5f #x1e #x61 #x1e #x63 
        #x1e #x65 #x1e #x67 #x1e #x69 #x1e #x6b 
        #x1e #x6d #x1e #x6f #x1e #x71 #x1e #x73 
        #x1e #x75 #x1e #x77 #x1e #x79 #x1e #x7b 
        #x1e #x7d #x1e #x7f #x1e #x81 #x1e #x83 
        #x1e #x85 #x1e #x87 #x1e #x89 #x1e #x8b 
        #x1e #x8d #x1e #x8f #x1e #x91 #x1e #x93 
        #x1e #x95 #x1e #x9b #x1e #xa1 #x1e #xa3 
        #x1e #xa5 #x1e #xa7 #x1e #xa9 #x1e #xab 
        #x1e #xad #x1e #xaf #x1e #xb1 #x1e #xb3 
        #x1e #xb5 #x1e #xb7 #x1e #xb9 #x1e #xbb 
        #x1e #xbd #x1e #xbf #x1e #xc1 #x1e #xc3 
        #x1e #xc5 #x1e #xc7 #x1e #xc9 #x1e #xcb 
        #x1e #xcd #x1e #xcf #x1e #xd1 #x1e #xd3 
        #x1e #xd5 #x1e #xd7 #x1e #xd9 #x1e #xdb 
        #x1e #xdd #x1e #xdf #x1e #xe1 #x1e #xe3 
        #x1e #xe5 #x1e #xe7 #x1e #xe9 #x1e #xeb 
        #x1e #xed #x1e #xef #x1e #xf1 #x1e #xf3 
        #x1e #xf5 #x1e #xf7 #x1e #xf9 #x1e #xfb 
        #x1e #xfd #x1e #xff #x1f #x0 #x1f #x1 
        #x1f #x2 #x1f #x3 #x1f #x4 #x1f #x5 
        #x1f #x6 #x1f #x7 #x1f #x10 #x1f #x11 
        #x1f #x12 #x1f #x13 #x1f #x14 #x1f #x15 
        #x1f #x20 #x1f #x21 #x1f #x22 #x1f #x23 
        #x1f #x24 #x1f #x25 #x1f #x26 #x1f #x27 
        #x1f #x30 #x1f #x31 #x1f #x32 #x1f #x33 
        #x1f #x34 #x1f #x35 #x1f #x36 #x1f #x37 
        #x1f #x40 #x1f #x41 #x1f #x42 #x1f #x43 
        #x1f #x44 #x1f #x45 #x1f #x51 #x1f #x53 
        #x1f #x55 #x1f #x57 #x1f #x60 #x1f #x61 
        #x1f #x62 #x1f #x63 #x1f #x64 #x1f #x65 
        #x1f #x66 #x1f #x67 #x1f #x70 #x1f #x71 
        #x1f #x72 #x1f #x73 #x1f #x74 #x1f #x75 
        #x1f #x76 #x1f #x77 #x1f #x78 #x1f #x79 
        #x1f #x7a #x1f #x7b #x1f #x7c #x1f #x7d 
        #x1f #x80 #x1f #x81 #x1f #x82 #x1f #x83 
        #x1f #x84 #x1f #x85 #x1f #x86 #x1f #x87 
        #x1f #x90 #x1f #x91 #x1f #x92 #x1f #x93 
        #x1f #x94 #x1f #x95 #x1f #x96 #x1f #x97 
        #x1f #xa0 #x1f #xa1 #x1f #xa2 #x1f #xa3 
        #x1f #xa4 #x1f #xa5 #x1f #xa6 #x1f #xa7 
        #x1f #xb0 #x1f #xb1 #x1f #xb3 #x1f #xbe 
        #x1f #xc3 #x1f #xd0 #x1f #xd1 #x1f #xe0 
        #x1f #xe1 #x1f #xe5 #x1f #xf3 #x21 #x4e 
        #x21 #x70 #x21 #x71 #x21 #x72 #x21 #x73 
        #x21 #x74 #x21 #x75 #x21 #x76 #x21 #x77 
        #x21 #x78 #x21 #x79 #x21 #x7a #x21 #x7b 
        #x21 #x7c #x21 #x7d #x21 #x7e #x21 #x7f 
        #x21 #x84 #x24 #xd0 #x24 #xd1 #x24 #xd2 
        #x24 #xd3 #x24 #xd4 #x24 #xd5 #x24 #xd6 
        #x24 #xd7 #x24 #xd8 #x24 #xd9 #x24 #xda 
        #x24 #xdb #x24 #xdc #x24 #xdd #x24 #xde 
        #x24 #xdf #x24 #xe0 #x24 #xe1 #x24 #xe2 
        #x24 #xe3 #x24 #xe4 #x24 #xe5 #x24 #xe6 
        #x24 #xe7 #x24 #xe8 #x24 #xe9 #x2c #x30 
        #x2c #x31 #x2c #x32 #x2c #x33 #x2c #x34 
        #x2c #x35 #x2c #x36 #x2c #x37 #x2c #x38 
        #x2c #x39 #x2c #x3a #x2c #x3b #x2c #x3c 
        #x2c #x3d #x2c #x3e #x2c #x3f #x2c #x40 
        #x2c #x41 #x2c #x42 #x2c #x43 #x2c #x44 
        #x2c #x45 #x2c #x46 #x2c #x47 #x2c #x48 
        #x2c #x49 #x2c #x4a #x2c #x4b #x2c #x4c 
        #x2c #x4d #x2c #x4e #x2c #x4f #x2c #x50 
        #x2c #x51 #x2c #x52 #x2c #x53 #x2c #x54 
        #x2c #x55 #x2c #x56 #x2c #x57 #x2c #x58 
        #x2c #x59 #x2c #x5a #x2c #x5b #x2c #x5c 
        #x2c #x5d #x2c #x5e #x2c #x61 #x2c #x65 
        #x2c #x66 #x2c #x68 #x2c #x6a #x2c #x6c 
        #x2c #x73 #x2c #x76 #x2c #x81 #x2c #x83 
        #x2c #x85 #x2c #x87 #x2c #x89 #x2c #x8b 
        #x2c #x8d #x2c #x8f #x2c #x91 #x2c #x93 
        #x2c #x95 #x2c #x97 #x2c #x99 #x2c #x9b 
        #x2c #x9d #x2c #x9f #x2c #xa1 #x2c #xa3 
        #x2c #xa5 #x2c #xa7 #x2c #xa9 #x2c #xab 
        #x2c #xad #x2c #xaf #x2c #xb1 #x2c #xb3 
        #x2c #xb5 #x2c #xb7 #x2c #xb9 #x2c #xbb 
        #x2c #xbd #x2c #xbf #x2c #xc1 #x2c #xc3 
        #x2c #xc5 #x2c #xc7 #x2c #xc9 #x2c #xcb 
        #x2c #xcd #x2c #xcf #x2c #xd1 #x2c #xd3 
        #x2c #xd5 #x2c #xd7 #x2c #xd9 #x2c #xdb 
        #x2c #xdd #x2c #xdf #x2c #xe1 #x2c #xe3 
        #x2c #xec #x2c #xee #x2c #xf3 #x2d #x0 
        #x2d #x1 #x2d #x2 #x2d #x3 #x2d #x4 
        #x2d #x5 #x2d #x6 #x2d #x7 #x2d #x8 
        #x2d #x9 #x2d #xa #x2d #xb #x2d #xc 
        #x2d #xd #x2d #xe #x2d #xf #x2d #x10 
        #x2d #x11 #x2d #x12 #x2d #x13 #x2d #x14 
        #x2d #x15 #x2d #x16 #x2d #x17 #x2d #x18 
        #x2d #x19 #x2d #x1a #x2d #x1b #x2d #x1c 
        #x2d #x1d #x2d #x1e #x2d #x1f #x2d #x20 
        #x2d #x21 #x2d #x22 #x2d #x23 #x2d #x24 
        #x2d #x25 #x2d #x27 #x2d #x2d #xa6 #x41 
        #xa6 #x43 #xa6 #x45 #xa6 #x47 #xa6 #x49 
        #xa6 #x4b #xa6 #x4d #xa6 #x4f #xa6 #x51 
        #xa6 #x53 #xa6 #x55 #xa6 #x57 #xa6 #x59 
        #xa6 #x5b #xa6 #x5d #xa6 #x5f #xa6 #x61 
        #xa6 #x63 #xa6 #x65 #xa6 #x67 #xa6 #x69 
        #xa6 #x6b #xa6 #x6d #xa6 #x81 #xa6 #x83 
        #xa6 #x85 #xa6 #x87 #xa6 #x89 #xa6 #x8b 
        #xa6 #x8d #xa6 #x8f #xa6 #x91 #xa6 #x93 
        #xa6 #x95 #xa6 #x97 #xa6 #x99 #xa6 #x9b 
        #xa7 #x23 #xa7 #x25 #xa7 #x27 #xa7 #x29 
        #xa7 #x2b #xa7 #x2d #xa7 #x2f #xa7 #x33 
        #xa7 #x35 #xa7 #x37 #xa7 #x39 #xa7 #x3b 
        #xa7 #x3d #xa7 #x3f #xa7 #x41 #xa7 #x43 
        #xa7 #x45 #xa7 #x47 #xa7 #x49 #xa7 #x4b 
        #xa7 #x4d #xa7 #x4f #xa7 #x51 #xa7 #x53 
        #xa7 #x55 #xa7 #x57 #xa7 #x59 #xa7 #x5b 
        #xa7 #x5d #xa7 #x5f #xa7 #x61 #xa7 #x63 
        #xa7 #x65 #xa7 #x67 #xa7 #x69 #xa7 #x6b 
        #xa7 #x6d #xa7 #x6f #xa7 #x7a #xa7 #x7c 
        #xa7 #x7f #xa7 #x81 #xa7 #x83 #xa7 #x85 
        #xa7 #x87 #xa7 #x8c #xa7 #x91 #xa7 #x93 
        #xa7 #x97 #xa7 #x99 #xa7 #x9b #xa7 #x9d 
        #xa7 #x9f #xa7 #xa1 #xa7 #xa3 #xa7 #xa5 
        #xa7 #xa7 #xa7 #xa9 #xff #x41 #xff #x42 
        #xff #x43 #xff #x44 #xff #x45 #xff #x46 
        #xff #x47 #xff #x48 #xff #x49 #xff #x4a 
        #xff #x4b #xff #x4c #xff #x4d #xff #x4e 
        #xff #x4f #xff #x50 #xff #x51 #xff #x52 
        #xff #x53 #xff #x54 #xff #x55 #xff #x56 
        #xff #x57 #xff #x58 #xff #x59 #xff #x5a ))

; This vector contains all other code points,
; in increasing order, that have a simple
; uppercase mapping.
;
; This table contains 72 elements.

(define simple-upcase-chars-morebits
  '#(
     #x10428 #x10429 #x1042a #x1042b #x1042c #x1042d #x1042e #x1042f 
     #x10430 #x10431 #x10432 #x10433 #x10434 #x10435 #x10436 #x10437 
     #x10438 #x10439 #x1043a #x1043b #x1043c #x1043d #x1043e #x1043f 
     #x10440 #x10441 #x10442 #x10443 #x10444 #x10445 #x10446 #x10447 
     #x10448 #x10449 #x1044a #x1044b #x1044c #x1044d #x1044e #x1044f 
     #x118c0 #x118c1 #x118c2 #x118c3 #x118c4 #x118c5 #x118c6 #x118c7 
     #x118c8 #x118c9 #x118ca #x118cb #x118cc #x118cd #x118ce #x118cf 
     #x118d0 #x118d1 #x118d2 #x118d3 #x118d4 #x118d5 #x118d6 #x118d7 
     #x118d8 #x118d9 #x118da #x118db #x118dc #x118dd #x118de #x118df ))

; This bytevector uses two bytes per code point
; to list all 16-bit code points, in increasing order,
; that have a simple lowercase mapping.
;
; This table contains 2040 elements.

(define simple-downcase-chars-16bit
  '#u8(
        #x0 #x41 #x0 #x42 #x0 #x43 #x0 #x44 
        #x0 #x45 #x0 #x46 #x0 #x47 #x0 #x48 
        #x0 #x49 #x0 #x4a #x0 #x4b #x0 #x4c 
        #x0 #x4d #x0 #x4e #x0 #x4f #x0 #x50 
        #x0 #x51 #x0 #x52 #x0 #x53 #x0 #x54 
        #x0 #x55 #x0 #x56 #x0 #x57 #x0 #x58 
        #x0 #x59 #x0 #x5a #x0 #xc0 #x0 #xc1 
        #x0 #xc2 #x0 #xc3 #x0 #xc4 #x0 #xc5 
        #x0 #xc6 #x0 #xc7 #x0 #xc8 #x0 #xc9 
        #x0 #xca #x0 #xcb #x0 #xcc #x0 #xcd 
        #x0 #xce #x0 #xcf #x0 #xd0 #x0 #xd1 
        #x0 #xd2 #x0 #xd3 #x0 #xd4 #x0 #xd5 
        #x0 #xd6 #x0 #xd8 #x0 #xd9 #x0 #xda 
        #x0 #xdb #x0 #xdc #x0 #xdd #x0 #xde 
        #x1 #x0 #x1 #x2 #x1 #x4 #x1 #x6 
        #x1 #x8 #x1 #xa #x1 #xc #x1 #xe 
        #x1 #x10 #x1 #x12 #x1 #x14 #x1 #x16 
        #x1 #x18 #x1 #x1a #x1 #x1c #x1 #x1e 
        #x1 #x20 #x1 #x22 #x1 #x24 #x1 #x26 
        #x1 #x28 #x1 #x2a #x1 #x2c #x1 #x2e 
        #x1 #x30 #x1 #x32 #x1 #x34 #x1 #x36 
        #x1 #x39 #x1 #x3b #x1 #x3d #x1 #x3f 
        #x1 #x41 #x1 #x43 #x1 #x45 #x1 #x47 
        #x1 #x4a #x1 #x4c #x1 #x4e #x1 #x50 
        #x1 #x52 #x1 #x54 #x1 #x56 #x1 #x58 
        #x1 #x5a #x1 #x5c #x1 #x5e #x1 #x60 
        #x1 #x62 #x1 #x64 #x1 #x66 #x1 #x68 
        #x1 #x6a #x1 #x6c #x1 #x6e #x1 #x70 
        #x1 #x72 #x1 #x74 #x1 #x76 #x1 #x78 
        #x1 #x79 #x1 #x7b #x1 #x7d #x1 #x81 
        #x1 #x82 #x1 #x84 #x1 #x86 #x1 #x87 
        #x1 #x89 #x1 #x8a #x1 #x8b #x1 #x8e 
        #x1 #x8f #x1 #x90 #x1 #x91 #x1 #x93 
        #x1 #x94 #x1 #x96 #x1 #x97 #x1 #x98 
        #x1 #x9c #x1 #x9d #x1 #x9f #x1 #xa0 
        #x1 #xa2 #x1 #xa4 #x1 #xa6 #x1 #xa7 
        #x1 #xa9 #x1 #xac #x1 #xae #x1 #xaf 
        #x1 #xb1 #x1 #xb2 #x1 #xb3 #x1 #xb5 
        #x1 #xb7 #x1 #xb8 #x1 #xbc #x1 #xc4 
        #x1 #xc5 #x1 #xc7 #x1 #xc8 #x1 #xca 
        #x1 #xcb #x1 #xcd #x1 #xcf #x1 #xd1 
        #x1 #xd3 #x1 #xd5 #x1 #xd7 #x1 #xd9 
        #x1 #xdb #x1 #xde #x1 #xe0 #x1 #xe2 
        #x1 #xe4 #x1 #xe6 #x1 #xe8 #x1 #xea 
        #x1 #xec #x1 #xee #x1 #xf1 #x1 #xf2 
        #x1 #xf4 #x1 #xf6 #x1 #xf7 #x1 #xf8 
        #x1 #xfa #x1 #xfc #x1 #xfe #x2 #x0 
        #x2 #x2 #x2 #x4 #x2 #x6 #x2 #x8 
        #x2 #xa #x2 #xc #x2 #xe #x2 #x10 
        #x2 #x12 #x2 #x14 #x2 #x16 #x2 #x18 
        #x2 #x1a #x2 #x1c #x2 #x1e #x2 #x20 
        #x2 #x22 #x2 #x24 #x2 #x26 #x2 #x28 
        #x2 #x2a #x2 #x2c #x2 #x2e #x2 #x30 
        #x2 #x32 #x2 #x3a #x2 #x3b #x2 #x3d 
        #x2 #x3e #x2 #x41 #x2 #x43 #x2 #x44 
        #x2 #x45 #x2 #x46 #x2 #x48 #x2 #x4a 
        #x2 #x4c #x2 #x4e #x3 #x70 #x3 #x72 
        #x3 #x76 #x3 #x7f #x3 #x86 #x3 #x88 
        #x3 #x89 #x3 #x8a #x3 #x8c #x3 #x8e 
        #x3 #x8f #x3 #x91 #x3 #x92 #x3 #x93 
        #x3 #x94 #x3 #x95 #x3 #x96 #x3 #x97 
        #x3 #x98 #x3 #x99 #x3 #x9a #x3 #x9b 
        #x3 #x9c #x3 #x9d #x3 #x9e #x3 #x9f 
        #x3 #xa0 #x3 #xa1 #x3 #xa3 #x3 #xa4 
        #x3 #xa5 #x3 #xa6 #x3 #xa7 #x3 #xa8 
        #x3 #xa9 #x3 #xaa #x3 #xab #x3 #xcf 
        #x3 #xd8 #x3 #xda #x3 #xdc #x3 #xde 
        #x3 #xe0 #x3 #xe2 #x3 #xe4 #x3 #xe6 
        #x3 #xe8 #x3 #xea #x3 #xec #x3 #xee 
        #x3 #xf4 #x3 #xf7 #x3 #xf9 #x3 #xfa 
        #x3 #xfd #x3 #xfe #x3 #xff #x4 #x0 
        #x4 #x1 #x4 #x2 #x4 #x3 #x4 #x4 
        #x4 #x5 #x4 #x6 #x4 #x7 #x4 #x8 
        #x4 #x9 #x4 #xa #x4 #xb #x4 #xc 
        #x4 #xd #x4 #xe #x4 #xf #x4 #x10 
        #x4 #x11 #x4 #x12 #x4 #x13 #x4 #x14 
        #x4 #x15 #x4 #x16 #x4 #x17 #x4 #x18 
        #x4 #x19 #x4 #x1a #x4 #x1b #x4 #x1c 
        #x4 #x1d #x4 #x1e #x4 #x1f #x4 #x20 
        #x4 #x21 #x4 #x22 #x4 #x23 #x4 #x24 
        #x4 #x25 #x4 #x26 #x4 #x27 #x4 #x28 
        #x4 #x29 #x4 #x2a #x4 #x2b #x4 #x2c 
        #x4 #x2d #x4 #x2e #x4 #x2f #x4 #x60 
        #x4 #x62 #x4 #x64 #x4 #x66 #x4 #x68 
        #x4 #x6a #x4 #x6c #x4 #x6e #x4 #x70 
        #x4 #x72 #x4 #x74 #x4 #x76 #x4 #x78 
        #x4 #x7a #x4 #x7c #x4 #x7e #x4 #x80 
        #x4 #x8a #x4 #x8c #x4 #x8e #x4 #x90 
        #x4 #x92 #x4 #x94 #x4 #x96 #x4 #x98 
        #x4 #x9a #x4 #x9c #x4 #x9e #x4 #xa0 
        #x4 #xa2 #x4 #xa4 #x4 #xa6 #x4 #xa8 
        #x4 #xaa #x4 #xac #x4 #xae #x4 #xb0 
        #x4 #xb2 #x4 #xb4 #x4 #xb6 #x4 #xb8 
        #x4 #xba #x4 #xbc #x4 #xbe #x4 #xc0 
        #x4 #xc1 #x4 #xc3 #x4 #xc5 #x4 #xc7 
        #x4 #xc9 #x4 #xcb #x4 #xcd #x4 #xd0 
        #x4 #xd2 #x4 #xd4 #x4 #xd6 #x4 #xd8 
        #x4 #xda #x4 #xdc #x4 #xde #x4 #xe0 
        #x4 #xe2 #x4 #xe4 #x4 #xe6 #x4 #xe8 
        #x4 #xea #x4 #xec #x4 #xee #x4 #xf0 
        #x4 #xf2 #x4 #xf4 #x4 #xf6 #x4 #xf8 
        #x4 #xfa #x4 #xfc #x4 #xfe #x5 #x0 
        #x5 #x2 #x5 #x4 #x5 #x6 #x5 #x8 
        #x5 #xa #x5 #xc #x5 #xe #x5 #x10 
        #x5 #x12 #x5 #x14 #x5 #x16 #x5 #x18 
        #x5 #x1a #x5 #x1c #x5 #x1e #x5 #x20 
        #x5 #x22 #x5 #x24 #x5 #x26 #x5 #x28 
        #x5 #x2a #x5 #x2c #x5 #x2e #x5 #x31 
        #x5 #x32 #x5 #x33 #x5 #x34 #x5 #x35 
        #x5 #x36 #x5 #x37 #x5 #x38 #x5 #x39 
        #x5 #x3a #x5 #x3b #x5 #x3c #x5 #x3d 
        #x5 #x3e #x5 #x3f #x5 #x40 #x5 #x41 
        #x5 #x42 #x5 #x43 #x5 #x44 #x5 #x45 
        #x5 #x46 #x5 #x47 #x5 #x48 #x5 #x49 
        #x5 #x4a #x5 #x4b #x5 #x4c #x5 #x4d 
        #x5 #x4e #x5 #x4f #x5 #x50 #x5 #x51 
        #x5 #x52 #x5 #x53 #x5 #x54 #x5 #x55 
        #x5 #x56 #x10 #xa0 #x10 #xa1 #x10 #xa2 
        #x10 #xa3 #x10 #xa4 #x10 #xa5 #x10 #xa6 
        #x10 #xa7 #x10 #xa8 #x10 #xa9 #x10 #xaa 
        #x10 #xab #x10 #xac #x10 #xad #x10 #xae 
        #x10 #xaf #x10 #xb0 #x10 #xb1 #x10 #xb2 
        #x10 #xb3 #x10 #xb4 #x10 #xb5 #x10 #xb6 
        #x10 #xb7 #x10 #xb8 #x10 #xb9 #x10 #xba 
        #x10 #xbb #x10 #xbc #x10 #xbd #x10 #xbe 
        #x10 #xbf #x10 #xc0 #x10 #xc1 #x10 #xc2 
        #x10 #xc3 #x10 #xc4 #x10 #xc5 #x10 #xc7 
        #x10 #xcd #x1e #x0 #x1e #x2 #x1e #x4 
        #x1e #x6 #x1e #x8 #x1e #xa #x1e #xc 
        #x1e #xe #x1e #x10 #x1e #x12 #x1e #x14 
        #x1e #x16 #x1e #x18 #x1e #x1a #x1e #x1c 
        #x1e #x1e #x1e #x20 #x1e #x22 #x1e #x24 
        #x1e #x26 #x1e #x28 #x1e #x2a #x1e #x2c 
        #x1e #x2e #x1e #x30 #x1e #x32 #x1e #x34 
        #x1e #x36 #x1e #x38 #x1e #x3a #x1e #x3c 
        #x1e #x3e #x1e #x40 #x1e #x42 #x1e #x44 
        #x1e #x46 #x1e #x48 #x1e #x4a #x1e #x4c 
        #x1e #x4e #x1e #x50 #x1e #x52 #x1e #x54 
        #x1e #x56 #x1e #x58 #x1e #x5a #x1e #x5c 
        #x1e #x5e #x1e #x60 #x1e #x62 #x1e #x64 
        #x1e #x66 #x1e #x68 #x1e #x6a #x1e #x6c 
        #x1e #x6e #x1e #x70 #x1e #x72 #x1e #x74 
        #x1e #x76 #x1e #x78 #x1e #x7a #x1e #x7c 
        #x1e #x7e #x1e #x80 #x1e #x82 #x1e #x84 
        #x1e #x86 #x1e #x88 #x1e #x8a #x1e #x8c 
        #x1e #x8e #x1e #x90 #x1e #x92 #x1e #x94 
        #x1e #x9e #x1e #xa0 #x1e #xa2 #x1e #xa4 
        #x1e #xa6 #x1e #xa8 #x1e #xaa #x1e #xac 
        #x1e #xae #x1e #xb0 #x1e #xb2 #x1e #xb4 
        #x1e #xb6 #x1e #xb8 #x1e #xba #x1e #xbc 
        #x1e #xbe #x1e #xc0 #x1e #xc2 #x1e #xc4 
        #x1e #xc6 #x1e #xc8 #x1e #xca #x1e #xcc 
        #x1e #xce #x1e #xd0 #x1e #xd2 #x1e #xd4 
        #x1e #xd6 #x1e #xd8 #x1e #xda #x1e #xdc 
        #x1e #xde #x1e #xe0 #x1e #xe2 #x1e #xe4 
        #x1e #xe6 #x1e #xe8 #x1e #xea #x1e #xec 
        #x1e #xee #x1e #xf0 #x1e #xf2 #x1e #xf4 
        #x1e #xf6 #x1e #xf8 #x1e #xfa #x1e #xfc 
        #x1e #xfe #x1f #x8 #x1f #x9 #x1f #xa 
        #x1f #xb #x1f #xc #x1f #xd #x1f #xe 
        #x1f #xf #x1f #x18 #x1f #x19 #x1f #x1a 
        #x1f #x1b #x1f #x1c #x1f #x1d #x1f #x28 
        #x1f #x29 #x1f #x2a #x1f #x2b #x1f #x2c 
        #x1f #x2d #x1f #x2e #x1f #x2f #x1f #x38 
        #x1f #x39 #x1f #x3a #x1f #x3b #x1f #x3c 
        #x1f #x3d #x1f #x3e #x1f #x3f #x1f #x48 
        #x1f #x49 #x1f #x4a #x1f #x4b #x1f #x4c 
        #x1f #x4d #x1f #x59 #x1f #x5b #x1f #x5d 
        #x1f #x5f #x1f #x68 #x1f #x69 #x1f #x6a 
        #x1f #x6b #x1f #x6c #x1f #x6d #x1f #x6e 
        #x1f #x6f #x1f #x88 #x1f #x89 #x1f #x8a 
        #x1f #x8b #x1f #x8c #x1f #x8d #x1f #x8e 
        #x1f #x8f #x1f #x98 #x1f #x99 #x1f #x9a 
        #x1f #x9b #x1f #x9c #x1f #x9d #x1f #x9e 
        #x1f #x9f #x1f #xa8 #x1f #xa9 #x1f #xaa 
        #x1f #xab #x1f #xac #x1f #xad #x1f #xae 
        #x1f #xaf #x1f #xb8 #x1f #xb9 #x1f #xba 
        #x1f #xbb #x1f #xbc #x1f #xc8 #x1f #xc9 
        #x1f #xca #x1f #xcb #x1f #xcc #x1f #xd8 
        #x1f #xd9 #x1f #xda #x1f #xdb #x1f #xe8 
        #x1f #xe9 #x1f #xea #x1f #xeb #x1f #xec 
        #x1f #xf8 #x1f #xf9 #x1f #xfa #x1f #xfb 
        #x1f #xfc #x21 #x26 #x21 #x2a #x21 #x2b 
        #x21 #x32 #x21 #x60 #x21 #x61 #x21 #x62 
        #x21 #x63 #x21 #x64 #x21 #x65 #x21 #x66 
        #x21 #x67 #x21 #x68 #x21 #x69 #x21 #x6a 
        #x21 #x6b #x21 #x6c #x21 #x6d #x21 #x6e 
        #x21 #x6f #x21 #x83 #x24 #xb6 #x24 #xb7 
        #x24 #xb8 #x24 #xb9 #x24 #xba #x24 #xbb 
        #x24 #xbc #x24 #xbd #x24 #xbe #x24 #xbf 
        #x24 #xc0 #x24 #xc1 #x24 #xc2 #x24 #xc3 
        #x24 #xc4 #x24 #xc5 #x24 #xc6 #x24 #xc7 
        #x24 #xc8 #x24 #xc9 #x24 #xca #x24 #xcb 
        #x24 #xcc #x24 #xcd #x24 #xce #x24 #xcf 
        #x2c #x0 #x2c #x1 #x2c #x2 #x2c #x3 
        #x2c #x4 #x2c #x5 #x2c #x6 #x2c #x7 
        #x2c #x8 #x2c #x9 #x2c #xa #x2c #xb 
        #x2c #xc #x2c #xd #x2c #xe #x2c #xf 
        #x2c #x10 #x2c #x11 #x2c #x12 #x2c #x13 
        #x2c #x14 #x2c #x15 #x2c #x16 #x2c #x17 
        #x2c #x18 #x2c #x19 #x2c #x1a #x2c #x1b 
        #x2c #x1c #x2c #x1d #x2c #x1e #x2c #x1f 
        #x2c #x20 #x2c #x21 #x2c #x22 #x2c #x23 
        #x2c #x24 #x2c #x25 #x2c #x26 #x2c #x27 
        #x2c #x28 #x2c #x29 #x2c #x2a #x2c #x2b 
        #x2c #x2c #x2c #x2d #x2c #x2e #x2c #x60 
        #x2c #x62 #x2c #x63 #x2c #x64 #x2c #x67 
        #x2c #x69 #x2c #x6b #x2c #x6d #x2c #x6e 
        #x2c #x6f #x2c #x70 #x2c #x72 #x2c #x75 
        #x2c #x7e #x2c #x7f #x2c #x80 #x2c #x82 
        #x2c #x84 #x2c #x86 #x2c #x88 #x2c #x8a 
        #x2c #x8c #x2c #x8e #x2c #x90 #x2c #x92 
        #x2c #x94 #x2c #x96 #x2c #x98 #x2c #x9a 
        #x2c #x9c #x2c #x9e #x2c #xa0 #x2c #xa2 
        #x2c #xa4 #x2c #xa6 #x2c #xa8 #x2c #xaa 
        #x2c #xac #x2c #xae #x2c #xb0 #x2c #xb2 
        #x2c #xb4 #x2c #xb6 #x2c #xb8 #x2c #xba 
        #x2c #xbc #x2c #xbe #x2c #xc0 #x2c #xc2 
        #x2c #xc4 #x2c #xc6 #x2c #xc8 #x2c #xca 
        #x2c #xcc #x2c #xce #x2c #xd0 #x2c #xd2 
        #x2c #xd4 #x2c #xd6 #x2c #xd8 #x2c #xda 
        #x2c #xdc #x2c #xde #x2c #xe0 #x2c #xe2 
        #x2c #xeb #x2c #xed #x2c #xf2 #xa6 #x40 
        #xa6 #x42 #xa6 #x44 #xa6 #x46 #xa6 #x48 
        #xa6 #x4a #xa6 #x4c #xa6 #x4e #xa6 #x50 
        #xa6 #x52 #xa6 #x54 #xa6 #x56 #xa6 #x58 
        #xa6 #x5a #xa6 #x5c #xa6 #x5e #xa6 #x60 
        #xa6 #x62 #xa6 #x64 #xa6 #x66 #xa6 #x68 
        #xa6 #x6a #xa6 #x6c #xa6 #x80 #xa6 #x82 
        #xa6 #x84 #xa6 #x86 #xa6 #x88 #xa6 #x8a 
        #xa6 #x8c #xa6 #x8e #xa6 #x90 #xa6 #x92 
        #xa6 #x94 #xa6 #x96 #xa6 #x98 #xa6 #x9a 
        #xa7 #x22 #xa7 #x24 #xa7 #x26 #xa7 #x28 
        #xa7 #x2a #xa7 #x2c #xa7 #x2e #xa7 #x32 
        #xa7 #x34 #xa7 #x36 #xa7 #x38 #xa7 #x3a 
        #xa7 #x3c #xa7 #x3e #xa7 #x40 #xa7 #x42 
        #xa7 #x44 #xa7 #x46 #xa7 #x48 #xa7 #x4a 
        #xa7 #x4c #xa7 #x4e #xa7 #x50 #xa7 #x52 
        #xa7 #x54 #xa7 #x56 #xa7 #x58 #xa7 #x5a 
        #xa7 #x5c #xa7 #x5e #xa7 #x60 #xa7 #x62 
        #xa7 #x64 #xa7 #x66 #xa7 #x68 #xa7 #x6a 
        #xa7 #x6c #xa7 #x6e #xa7 #x79 #xa7 #x7b 
        #xa7 #x7d #xa7 #x7e #xa7 #x80 #xa7 #x82 
        #xa7 #x84 #xa7 #x86 #xa7 #x8b #xa7 #x8d 
        #xa7 #x90 #xa7 #x92 #xa7 #x96 #xa7 #x98 
        #xa7 #x9a #xa7 #x9c #xa7 #x9e #xa7 #xa0 
        #xa7 #xa2 #xa7 #xa4 #xa7 #xa6 #xa7 #xa8 
        #xa7 #xaa #xa7 #xab #xa7 #xac #xa7 #xad 
        #xa7 #xb0 #xa7 #xb1 #xff #x21 #xff #x22 
        #xff #x23 #xff #x24 #xff #x25 #xff #x26 
        #xff #x27 #xff #x28 #xff #x29 #xff #x2a 
        #xff #x2b #xff #x2c #xff #x2d #xff #x2e 
        #xff #x2f #xff #x30 #xff #x31 #xff #x32 
        #xff #x33 #xff #x34 #xff #x35 #xff #x36 
        #xff #x37 #xff #x38 #xff #x39 #xff #x3a ))

; This vector contains all other code points,
; in increasing order, that have a simple
; lowercase mapping.
;
; This table contains 72 elements.

(define simple-downcase-chars-morebits
  '#(
     #x10400 #x10401 #x10402 #x10403 #x10404 #x10405 #x10406 #x10407 
     #x10408 #x10409 #x1040a #x1040b #x1040c #x1040d #x1040e #x1040f 
     #x10410 #x10411 #x10412 #x10413 #x10414 #x10415 #x10416 #x10417 
     #x10418 #x10419 #x1041a #x1041b #x1041c #x1041d #x1041e #x1041f 
     #x10420 #x10421 #x10422 #x10423 #x10424 #x10425 #x10426 #x10427 
     #x118a0 #x118a1 #x118a2 #x118a3 #x118a4 #x118a5 #x118a6 #x118a7 
     #x118a8 #x118a9 #x118aa #x118ab #x118ac #x118ad #x118ae #x118af 
     #x118b0 #x118b1 #x118b2 #x118b3 #x118b4 #x118b5 #x118b6 #x118b7 
     #x118b8 #x118b9 #x118ba #x118bb #x118bc #x118bd #x118be #x118bf ))

; The bytes of this bytevector are indexes into
; the simple-case-adjustments vector, and correspond
; to the code points in simple-upcase-chars-16bit
; followed by those in simple-upcase-chars-morebits.
;
; This table contains 1100 elements.

(define simple-upcase-adjustments
  '#u8(
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x40 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x39 #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x5 #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x4 #x3e #x2d #x2d #x2d #x2d #x2d #x36 
        #x2d #x3d #x3c #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x31 #x2d #x2c 
        #x2d #x2c #x2d #x2c #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x17 #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2c 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x4c 
        #x4c #x2d #x2d #x2d #x2d #x2d #x2d #x4b 
        #x49 #x4a #xc #xf #x10 #x10 #x12 #x11 
        #x54 #x10 #x53 #xe #x4f #x52 #xd #xb 
        #x47 #x51 #xb #x48 #xa #x9 #x46 #x7 
        #x7 #x50 #x7 #x19 #x8 #x8 #x18 #x6 
        #x4e #x34 #x2d #x2d #x2d #x3c #x3c #x3c 
        #x23 #x24 #x24 #x24 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x26 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x1a 
        #x1b #x1b #x1c #x1e #x21 #x1f #x2b #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x15 #x16 #x2e #x13 #x14 
        #x2d #x2d #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x16 #x16 #x16 #x16 #x16 #x16 
        #x16 #x16 #x16 #x16 #x16 #x16 #x16 #x16 
        #x16 #x16 #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2a #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x4d #x41 #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x1d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x33 #x33 #x35 #x35 #x35 #x35 
        #x37 #x37 #x3b #x3b #x38 #x38 #x3a #x3a 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x30 #x3 #x30 #x2f #x2f #x2f 
        #x2f #x2e #x30 #x27 #x29 #x29 #x29 #x29 
        #x29 #x29 #x29 #x29 #x29 #x29 #x29 #x29 
        #x29 #x29 #x29 #x29 #x2d #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x2d #x0 
        #x1 #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2 #x2 #x2 #x2 #x2 
        #x2 #x2 #x2 #x2 #x2 #x2 #x2 #x2 
        #x2 #x2 #x2 #x2 #x2 #x2 #x2 #x2 
        #x2 #x2 #x2 #x2 #x2 #x2 #x2 #x2 
        #x2 #x2 #x2 #x2 #x2 #x2 #x2 #x2 
        #x2 #x2 #x2 #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x22 #x22 #x22 #x22 
        #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 
        #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 
        #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 
        #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 
        #x22 #x22 #x22 #x22 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 ))

; The bytes of this bytevector are indexes into
; the simple-case-adjustments vector, and correspond
; to the code points in simple-downcase-chars-16bit
; followed by those in simple-downcase-chars-morebits.
;
; This table contains 1092 elements.

(define simple-downcase-adjustments
  '#u8(
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x3f #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x39 #x2d #x2d #x2d #xc 
        #x2d #x2d #xf #x2d #x10 #x10 #x2d #x17 
        #x12 #x11 #x2d #x10 #xe #xb #xd #x2d 
        #xb #xa #x9 #x2d #x2d #x2d #x7 #x2d 
        #x7 #x2d #x7 #x2d #x8 #x8 #x2d #x2d 
        #x6 #x2d #x2d #x2c #x2d #x2c #x2d #x2c 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2c #x2d #x2d #x36 #x31 #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x3c #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x0 #x2d #x3d 
        #x1 #x2d #x3e #x19 #x18 #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x13 #x23 #x24 
        #x24 #x24 #x1a #x1b #x1b #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x2b 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x32 #x2d #x2e #x2d 
        #x3c #x3c #x3c #x16 #x16 #x16 #x16 #x16 
        #x16 #x16 #x16 #x16 #x16 #x16 #x16 #x16 
        #x16 #x16 #x16 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2a 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x2 #x2 #x2 
        #x2 #x2 #x2 #x2 #x2 #x2 #x2 #x2 
        #x2 #x2 #x2 #x2 #x2 #x2 #x2 #x2 
        #x2 #x2 #x2 #x2 #x2 #x2 #x2 #x2 
        #x2 #x2 #x2 #x2 #x2 #x2 #x2 #x2 
        #x2 #x2 #x2 #x2 #x2 #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x43 #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x2f #x2f #x2f #x2f #x2f 
        #x2f #x2f #x2f #x33 #x33 #x30 #x35 #x35 
        #x35 #x35 #x30 #x2f #x2f #x37 #x37 #x2f 
        #x2f #x38 #x38 #x2e #x3b #x3b #x3a #x3a 
        #x30 #x42 #x45 #x44 #x27 #x29 #x29 #x29 
        #x29 #x29 #x29 #x29 #x29 #x29 #x29 #x29 
        #x29 #x29 #x29 #x29 #x29 #x2d #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 
        #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x2d 
        #x47 #x41 #x46 #x2d #x2d #x2d #x49 #x48 
        #x4b #x4a #x2d #x2d #x4c #x4c #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x4d #x2d #x2d #x2d #x2d #x2d #x2d #x4f 
        #x2d #x2d #x2d #x2d #x2d #x2d #x2d #x2d 
        #x2d #x2d #x2d #x2d #x52 #x54 #x53 #x51 
        #x4e #x50 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x22 #x22 #x22 #x22 
        #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 
        #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 
        #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 
        #x22 #x22 #x22 #x22 #x22 #x22 #x22 #x22 
        #x22 #x22 #x22 #x22 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 #x25 #x25 #x25 #x25 
        #x25 #x25 #x25 #x25 ))

; The scalar values in this vector fold to the
; scalar values in the simple-foldcase-mappings
; vector under simple case folding.  All other
; scalar values fold to their downcased values.
;
; Each of those tables contains 223 elements.

(define simple-foldcase-exceptions
  '#(
        #xb5 #x17f #x345 #x370 #x372 #x376 #x37f #x3c2 
        #x3cf #x3d0 #x3d1 #x3d5 #x3d6 #x3f0 #x3f1 #x3f5 
        #x514 #x516 #x518 #x51a #x51c #x51e #x520 #x522 
        #x524 #x526 #x528 #x52a #x52c #x52e #x10c7 #x10cd 
        #x1e9b #x1e9e #x1efa #x1efc #x1efe #x1fbe #x2c6d #x2c6e 
        #x2c6f #x2c70 #x2c72 #x2c7e #x2c7f #x2ceb #x2ced #x2cf2 
        #xa640 #xa642 #xa644 #xa646 #xa648 #xa64a #xa64c #xa64e 
        #xa650 #xa652 #xa654 #xa656 #xa658 #xa65a #xa65c #xa65e 
        #xa660 #xa662 #xa664 #xa666 #xa668 #xa66a #xa66c #xa680 
        #xa682 #xa684 #xa686 #xa688 #xa68a #xa68c #xa68e #xa690 
        #xa692 #xa694 #xa696 #xa698 #xa69a #xa722 #xa724 #xa726 
        #xa728 #xa72a #xa72c #xa72e #xa732 #xa734 #xa736 #xa738 
        #xa73a #xa73c #xa73e #xa740 #xa742 #xa744 #xa746 #xa748 
        #xa74a #xa74c #xa74e #xa750 #xa752 #xa754 #xa756 #xa758 
        #xa75a #xa75c #xa75e #xa760 #xa762 #xa764 #xa766 #xa768 
        #xa76a #xa76c #xa76e #xa779 #xa77b #xa77d #xa77e #xa780 
        #xa782 #xa784 #xa786 #xa78b #xa78d #xa790 #xa792 #xa796 
        #xa798 #xa79a #xa79c #xa79e #xa7a0 #xa7a2 #xa7a4 #xa7a6 
        #xa7a8 #xa7aa #xa7ab #xa7ac #xa7ad #xa7b0 #xa7b1 #x10400 
        #x10401 #x10402 #x10403 #x10404 #x10405 #x10406 #x10407 #x10408 
        #x10409 #x1040a #x1040b #x1040c #x1040d #x1040e #x1040f #x10410 
        #x10411 #x10412 #x10413 #x10414 #x10415 #x10416 #x10417 #x10418 
        #x10419 #x1041a #x1041b #x1041c #x1041d #x1041e #x1041f #x10420 
        #x10421 #x10422 #x10423 #x10424 #x10425 #x10426 #x10427 #x118a0 
        #x118a1 #x118a2 #x118a3 #x118a4 #x118a5 #x118a6 #x118a7 #x118a8 
        #x118a9 #x118aa #x118ab #x118ac #x118ad #x118ae #x118af #x118b0 
        #x118b1 #x118b2 #x118b3 #x118b4 #x118b5 #x118b6 #x118b7 #x118b8 
        #x118b9 #x118ba #x118bb #x118bc #x118bd #x118be #x118bf ))

(define simple-foldcase-mappings
  '#(
        #x03BC #x0073 #x03B9 #x0371 #x0373 #x0377 #x03F3 #x03C3 
        #x03D7 #x03B2 #x03B8 #x03C6 #x03C0 #x03BA #x03C1 #x03B5 
        #x0515 #x0517 #x0519 #x051B #x051D #x051F #x0521 #x0523 
        #x0525 #x0527 #x0529 #x052B #x052D #x052F #x2D27 #x2D2D 
        #x1E61 #x00DF #x1EFB #x1EFD #x1EFF #x03B9 #x0251 #x0271 
        #x0250 #x0252 #x2C73 #x023F #x0240 #x2CEC #x2CEE #x2CF3 
        #xA641 #xA643 #xA645 #xA647 #xA649 #xA64B #xA64D #xA64F 
        #xA651 #xA653 #xA655 #xA657 #xA659 #xA65B #xA65D #xA65F 
        #xA661 #xA663 #xA665 #xA667 #xA669 #xA66B #xA66D #xA681 
        #xA683 #xA685 #xA687 #xA689 #xA68B #xA68D #xA68F #xA691 
        #xA693 #xA695 #xA697 #xA699 #xA69B #xA723 #xA725 #xA727 
        #xA729 #xA72B #xA72D #xA72F #xA733 #xA735 #xA737 #xA739 
        #xA73B #xA73D #xA73F #xA741 #xA743 #xA745 #xA747 #xA749 
        #xA74B #xA74D #xA74F #xA751 #xA753 #xA755 #xA757 #xA759 
        #xA75B #xA75D #xA75F #xA761 #xA763 #xA765 #xA767 #xA769 
        #xA76B #xA76D #xA76F #xA77A #xA77C #x1D79 #xA77F #xA781 
        #xA783 #xA785 #xA787 #xA78C #x0265 #xA791 #xA793 #xA797 
        #xA799 #xA79B #xA79D #xA79F #xA7A1 #xA7A3 #xA7A5 #xA7A7 
        #xA7A9 #x0266 #x025C #x0261 #x026C #x029E #x0287 #x10428 
        #x10429 #x1042A #x1042B #x1042C #x1042D #x1042E #x1042F #x10430 
        #x10431 #x10432 #x10433 #x10434 #x10435 #x10436 #x10437 #x10438 
        #x10439 #x1043A #x1043B #x1043C #x1043D #x1043E #x1043F #x10440 
        #x10441 #x10442 #x10443 #x10444 #x10445 #x10446 #x10447 #x10448 
        #x10449 #x1044A #x1044B #x1044C #x1044D #x1044E #x1044F #x118C0 
        #x118C1 #x118C2 #x118C3 #x118C4 #x118C5 #x118C6 #x118C7 #x118C8 
        #x118C9 #x118CA #x118CB #x118CC #x118CD #x118CE #x118CF #x118D0 
        #x118D1 #x118D2 #x118D3 #x118D4 #x118D5 #x118D6 #x118D7 #x118D8 
        #x118D9 #x118DA #x118DB #x118DC #x118DD #x118DE #x118DF ))

