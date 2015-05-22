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
; This file contains all of the code for R6RS library section 1.2
; except for the four normalization procedures, which are in
; (proto-unicode4).  In other words, this file contains the
; case-conversion and case-sensitive comparison operations on
; strings.
;
; The tables in this file were generated from the
; Unicode Character Database, revision 7.0.0.
;
; This file does not rely on any lexical syntax for
; non-Ascii characters and strings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Procedures that operate on strings.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Case-insensitive comparisons.

(define (string-ci=? s1 s2)
  (string=? (string-foldcase s1) (string-foldcase s2)))

(define (string-ci<? s1 s2)
  (string<? (string-foldcase s1) (string-foldcase s2)))

(define (string-ci>? s1 s2)
  (string>? (string-foldcase s1) (string-foldcase s2)))

(define (string-ci<=? s1 s2)
  (string<=? (string-foldcase s1) (string-foldcase s2)))

(define (string-ci>=? s1 s2)
  (string>=? (string-foldcase s1) (string-foldcase s2)))

(define (string-upcase s)
  (let* ((n (string-length s))
         (s2 (make-string n)))
    ; For when character-to-character mappings suffice.
    (define (fast i)
      (if (< i n)
          (let* ((c (string-ref s i))
                 (cp (char->integer c)))
            (if (< cp #x00df)
                (begin (string-set! s2 i (char-upcase c))
                       (fast (+ i 1)))
                (let ((probe (binary-search-16bit cp special-case-chars)))
                  (if probe
                      (let ((c2 (vector-ref special-uppercase-mapping probe)))
                        (if (char? c2)
                            (begin (string-set! s2 i c2)
                                   (fast (+ i 1)))
                            (slow-caser s
                                        i
                                        (list (substring s2 0 i))
                                        i
                                        char-upcase
                                        special-uppercase-mapping)))
                      (begin (string-set! s2 i (char-upcase c))
                             (fast (+ i 1)))))))
          s2))
    (fast 0)))

(define (string-downcase s)
  (let* ((n (string-length s))
         (s2 (make-string n)))
    ; For when character-to-character mappings suffice.
    (define (fast i)
      (if (< i n)
          (let* ((c (string-ref s i))
                 (cp (char->integer c)))
            (if (< cp #x00df)
                (begin (string-set! s2 i (char-downcase c))
                       (fast (+ i 1)))
                (let ((probe (binary-search-16bit cp special-case-chars)))
                  (if probe
                      (let ((c2 (vector-ref special-lowercase-mapping probe)))
                        (if (char? c2)
                            ; Special handling of Greek final sigma
                            ; when converting to lower case.
                            (if (and (char? c2)
                                     (= (char->integer c2)
                                        #x03c2))
                                ; Is the sigma the last letter of a word?
                                (let ((c3 (if (final-cased? s i)
                                              c2
                                              (integer->char #x03c3))))
                                  (string-set! s2 i c3)
                                  (fast (+ i 1)))
                                (begin (string-set! s2 i c2)
                                       (fast (+ i 1))))
                            ; String length must grow.
                            (slow-caser s
                                        i
                                        (list (substring s2 0 i))
                                        i
                                        char-downcase
                                        special-lowercase-mapping)))
                      (begin (string-set! s2 i (char-downcase c))
                             (fast (+ i 1)))))))
          s2))
    (fast 0)))

; From Section 3.13 of the Unicode 7.0.0 standard:
;
;     toTitlecase(X):  Find the word boundaries in X according to
;     Unicode Standard Annex #29, "Unicode Text Segmentation."
;     For each word boundary, find the first cased character F
;     following the word boundary.  If F exists, map F to
;     Titlecase_Mapping(F); then map all characters C between F
;     and the following word boundary to Lowercase_Mapping(C).

(define (string-titlecase s)
  (let ((n (string-length s)))

    (define (next-title-cased s i)
      (let ((j (string-next-word-break s i)))
        (define (loop j)
          (cond ((= j n) n)
                ((cased? (string-ref s j)) j)
                (else (loop (+ j 1)))))
        (loop j)))

    ;; iF is the index of the next cased character F to be converted
    ;; to title case.  chars is a list of characters and strings.

    (define (loop i iF chars)

      (cond ((= i n)

             ; Concatenate the characters and strings.
             (let* ((n2 (do ((mapped chars (cdr mapped))
                             (n2 0
                                 (+ n2 (if (char? (car mapped))
                                           1
                                           (string-length (car mapped))))))
                            ((null? mapped) n2)))
                    (s2 (make-string n2)))
               (define (loop i mapped)
                 (if (null? mapped)
                     s2
                     (let ((c2 (car mapped)))
                       (if (char? c2)
                           (let ((i1 (- i 1)))
                             (string-set! s2 i1 c2)
                             (loop i1 (cdr mapped)))
                           (do ((j (- (string-length c2) 1) (- j 1))
                                (i (- i 1) (- i 1)))
                               ((< j 0)
                                (loop i (cdr mapped)))
                             (string-set! s2 i (string-ref c2 j)))))))
               (loop n2 chars)))

            (else
             (let* ((c (string-ref s i))
                    (cp (char->integer c)))
               (let ((probe (if (< cp #x00df)
                                #f
                                (binary-search-16bit cp special-case-chars))))
                 (if (= i iF)
                     (let ((x (if probe
                                  (vector-ref special-titlecase-mapping probe)
                                  (char-titlecase c)))
                           (iF (next-title-cased s i)))
                       (loop (+ i 1) iF (cons x chars)))
                     (let ((x (if probe
                                  (vector-ref special-lowercase-mapping probe)
                                  (char-downcase c))))
                       (loop (+ i 1) iF (cons x chars)))))))))

    (loop 0 (next-title-cased s -1) '())))

; Returns the case-folded version of a string.
; If the string is already case-folded, then it may be returned.

(define (string-foldcase s)
  (string-foldcase-fast s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Help procedures and tables (not part of R6RS)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a string s, returns a string that is string=? to
; (string-foldcase s), but the result may be eq? to s.

(define (string-foldcase-fast s)
  (let ((n (string-length s)))

    ; All characters of s before index i are ASCII,
    ; and none are upper case.  If i is the length
    ; of s, then s can be returned as the result.

    (define (foldcase-without-allocating i)
      (if (= i n)
          s
          (let ((c (string-ref s i)))
            (if (or (and (<= (char->integer c) #x7f)
                         (not (char-upper-case? c)))
                    (and (char=? c (char-downcase c))
                         (not (binary-search (char->integer c)
                                             full-foldcase-exceptions))))
                (foldcase-without-allocating (+ i 1))
                (foldcase-medium (make-string n) 0)))))

    ; All characters of s before index i fold to a single
    ; character, and their folded versions have been stored
    ; into the corresponding elements of s2.
    ; If i is the length of s, then s2 is the result.

    (define (foldcase-medium s2 i)
      (if (= i n)
          s2
          (let ((c (string-ref s i)))
            (if (<= (char->integer c) #x7f)
                (begin (string-set! s2 i (char-downcase c))
                       (foldcase-medium s2 (+ i 1)))
                (let ((j (binary-search (char->integer c)
                                        full-foldcase-exceptions)))
                  (if j
                      (let ((c2 (vector-ref full-foldcase-mappings j)))
                        (if (char? c2)
                            (begin (string-set! s2 i c2)
                                   (foldcase-medium s2 (+ i 1)))
                            (string-foldcase-slow s)))
                      (begin (string-set! s2 i (char-downcase c))
                             (foldcase-medium s2 (+ i 1)))))))))

    ; General case: the result may be longer than the original.

    (define (string-foldcase-slow s)
      (define (loop i chars)
        (if (= i n)
            (list->string (reverse chars))
            (let ((c (string-ref s i)))
              (if (<= (char->integer c) #x7f)
                  (loop (+ i 1) (cons (char-downcase c) chars))
                  (let ((j (binary-search (char->integer c)
                                          full-foldcase-exceptions)))
                    (if j
                        (let ((s3 (vector-ref full-foldcase-mappings j)))
                          (loop (+ i 1)
                                (append (reverse (string->list s3)) chars)))
                        (loop (+ i 1)
                              (cons (char-downcase c) chars))))))))
      (loop 0 '()))

    (foldcase-without-allocating 0)))

; Given:
;
;     s:       the string whose case is being converted
;     i:       an exact integer index into or past s
;     mapped:  a list of characters and strings
;     n2:      an exact integer
;     caser:   a simple case mapping
;     table:   a table of special mappings
;
; such that:
;
;     0 <= i <= (string-length s)
;     (substring s 0 i) maps to (concatenation (reverse mapped))
;     n2 = (string-length (concatenation (reverse mapped)))
;     caser is char-downcase, char-upcase, or char-titlecase
;     table is one of
;         special-lowercase-mapping
;         special-uppercase-mapping
;         special-titlecase-mapping
;
; where (concatenation things)
;     = (apply string-append
;              (map (lambda (x) (if (char? x) (string x) x))
;                   things))
;
; Returns: the cased version of s.

(define (slow-caser s i mapped n2 caser table)
  (if (< i (string-length s))
      (let* ((c (string-ref s i))
             (cp (char->integer c)))
        (if (< cp #x00df)
            (slow-caser s
                        (+ i 1)
                        (cons (caser c) mapped)
                        (+ n2 1)
                        caser
                        table)
            (let ((probe (binary-search-16bit cp special-case-chars)))
              (if probe
                  (let ((c2 (vector-ref table probe)))
                    ; Special handling of Greek final sigma
                    ; when converting to lower case.
                    (if (and (char? c2)
                             (= (char->integer c2)
                                #x03c2))
                        ; Is the sigma the last letter of a word?
                        (let ((c3 (if (final-cased? s i)
                                      c2
                                      (integer->char #x03c3))))
                          (slow-caser s
                                      (+ i 1)
                                      (cons c3 mapped)
                                      (+ n2 1)
                                      caser
                                      table))
                        (slow-caser s
                                    (+ i 1)
                                    (cons c2 mapped)
                                    (+ n2
                                       (if (char? c2)
                                           1
                                           (string-length c2)))
                                    caser
                                    table)))
                  (slow-caser s
                              (+ i 1)
                              (cons (caser c) mapped)
                              (+ n2 1)
                              caser
                              table)))))
      (let ((s2 (make-string n2)))
        (define (loop i mapped)
          (if (null? mapped)
              s2
              (let ((c2 (car mapped)))
                (if (char? c2)
                    (let ((i1 (- i 1)))
                      (string-set! s2 i1 c2)
                      (loop i1 (cdr mapped)))
                    (do ((j (- (string-length c2) 1) (- j 1))
                         (i (- i 1) (- i 1)))
                        ((< j 0)
                         (loop (+ i 1) (cdr mapped)))
                      (string-set! s2 i (string-ref c2 j)))))))
        (loop n2 mapped))))

; Given a string s and an index i into s,
; returns #t if and only if C = (string-ref s i) is
; a Final_Cased casing context:
;
; Within the closest word boundaries containing C,
; there is a cased letter before C, and there is
; no cased letter after C.
;
; Note:  A character is cased if and only if
; it is uppercase, lowercase, or titlecase.
; That is not the same as Lu + Ll + Lt.

(define (cased? c)
  (or (char-lower-case? c)
      (char-upper-case? c)
      (eq? 'Lt (char-general-category c))))

(define (final-cased? s i)
  (and (not (cased-after? s i))
       (cased-before? s i)))

(define (cased-before? s i)
  (let* ((k i)
         (i (string-previous-word-break s i)))
    (define (loop j)
      (if (= j k)
          #f
          (let ((c (string-ref s j)))
            (or (cased? c)
                (loop (+ j 1))))))
    (loop i)))

(define (cased-after? s i)
  (let ((k (string-next-word-break s i)))
    (define (loop j)
      (if (= j k)
          #f
          (let ((c (string-ref s j)))
            (or (cased? c)
                (loop (+ j 1))))))
    (loop (+ i 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The following tables were generated from
; UnicodeData.txt, CaseFolding.txt,
; SpecialCasing.txt, PropList.txt,
; WordBreakProperty.txt, and CompositionExclusions.txt.
; Use parseUCD.sch to regenerate these tables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This bytevector uses two bytes per code point
; to list 16-bit code points, in increasing order,
; that have anything other than a simple case mapping.
;
; The locale-dependent mappings are not in this table.
;
; This table contains 210 elements.

(define special-case-chars
  '#u8(
        #x0 #xdf #x1 #x30 #x1 #x49 #x1 #xf0 
        #x3 #x90 #x3 #xa3 #x3 #xb0 #x3 #xc3 
        #x5 #x87 #x1e #x96 #x1e #x97 #x1e #x98 
        #x1e #x99 #x1e #x9a #x1f #x50 #x1f #x52 
        #x1f #x54 #x1f #x56 #x1f #x80 #x1f #x81 
        #x1f #x82 #x1f #x83 #x1f #x84 #x1f #x85 
        #x1f #x86 #x1f #x87 #x1f #x88 #x1f #x89 
        #x1f #x8a #x1f #x8b #x1f #x8c #x1f #x8d 
        #x1f #x8e #x1f #x8f #x1f #x90 #x1f #x91 
        #x1f #x92 #x1f #x93 #x1f #x94 #x1f #x95 
        #x1f #x96 #x1f #x97 #x1f #x98 #x1f #x99 
        #x1f #x9a #x1f #x9b #x1f #x9c #x1f #x9d 
        #x1f #x9e #x1f #x9f #x1f #xa0 #x1f #xa1 
        #x1f #xa2 #x1f #xa3 #x1f #xa4 #x1f #xa5 
        #x1f #xa6 #x1f #xa7 #x1f #xa8 #x1f #xa9 
        #x1f #xaa #x1f #xab #x1f #xac #x1f #xad 
        #x1f #xae #x1f #xaf #x1f #xb2 #x1f #xb3 
        #x1f #xb4 #x1f #xb6 #x1f #xb7 #x1f #xbc 
        #x1f #xc2 #x1f #xc3 #x1f #xc4 #x1f #xc6 
        #x1f #xc7 #x1f #xcc #x1f #xd2 #x1f #xd3 
        #x1f #xd6 #x1f #xd7 #x1f #xe2 #x1f #xe3 
        #x1f #xe4 #x1f #xe6 #x1f #xe7 #x1f #xf2 
        #x1f #xf3 #x1f #xf4 #x1f #xf6 #x1f #xf7 
        #x1f #xfc #xfb #x0 #xfb #x1 #xfb #x2 
        #xfb #x3 #xfb #x4 #xfb #x5 #xfb #x6 
        #xfb #x13 #xfb #x14 #xfb #x15 #xfb #x16 
        #xfb #x17 ))

; Each code point in special-case-chars maps to the
; character or string contained in the following tables.
;
; Each of these tables contains 105 elements, not counting
; the strings that are the elements themselves.

(define special-lowercase-mapping
  (let ((str (lambda args
               (if (= 1 (length args))
                   (integer->char (car args))
                   (apply string (map integer->char args))))))
    (vector
     (str #xdf)
     (str #x69 #x307)
     (str #x149)
     (str #x1f0)
     (str #x390)
     (str #x3c2)
     (str #x3b0)
     (str #x3c2)
     (str #x587)
     (str #x1e96)
     (str #x1e97)
     (str #x1e98)
     (str #x1e99)
     (str #x1e9a)
     (str #x1f50)
     (str #x1f52)
     (str #x1f54)
     (str #x1f56)
     (str #x1f80)
     (str #x1f81)
     (str #x1f82)
     (str #x1f83)
     (str #x1f84)
     (str #x1f85)
     (str #x1f86)
     (str #x1f87)
     (str #x1f80)
     (str #x1f81)
     (str #x1f82)
     (str #x1f83)
     (str #x1f84)
     (str #x1f85)
     (str #x1f86)
     (str #x1f87)
     (str #x1f90)
     (str #x1f91)
     (str #x1f92)
     (str #x1f93)
     (str #x1f94)
     (str #x1f95)
     (str #x1f96)
     (str #x1f97)
     (str #x1f90)
     (str #x1f91)
     (str #x1f92)
     (str #x1f93)
     (str #x1f94)
     (str #x1f95)
     (str #x1f96)
     (str #x1f97)
     (str #x1fa0)
     (str #x1fa1)
     (str #x1fa2)
     (str #x1fa3)
     (str #x1fa4)
     (str #x1fa5)
     (str #x1fa6)
     (str #x1fa7)
     (str #x1fa0)
     (str #x1fa1)
     (str #x1fa2)
     (str #x1fa3)
     (str #x1fa4)
     (str #x1fa5)
     (str #x1fa6)
     (str #x1fa7)
     (str #x1fb2)
     (str #x1fb3)
     (str #x1fb4)
     (str #x1fb6)
     (str #x1fb7)
     (str #x1fb3)
     (str #x1fc2)
     (str #x1fc3)
     (str #x1fc4)
     (str #x1fc6)
     (str #x1fc7)
     (str #x1fc3)
     (str #x1fd2)
     (str #x1fd3)
     (str #x1fd6)
     (str #x1fd7)
     (str #x1fe2)
     (str #x1fe3)
     (str #x1fe4)
     (str #x1fe6)
     (str #x1fe7)
     (str #x1ff2)
     (str #x1ff3)
     (str #x1ff4)
     (str #x1ff6)
     (str #x1ff7)
     (str #x1ff3)
     (str #xfb00)
     (str #xfb01)
     (str #xfb02)
     (str #xfb03)
     (str #xfb04)
     (str #xfb05)
     (str #xfb06)
     (str #xfb13)
     (str #xfb14)
     (str #xfb15)
     (str #xfb16)
     (str #xfb17)
)))

(define special-titlecase-mapping
  (let ((str (lambda args
               (if (= 1 (length args))
                   (integer->char (car args))
                   (apply string (map integer->char args))))))
    (vector
     (str #x53 #x73)
     (str #x130)
     (str #x2bc #x4e)
     (str #x4a #x30c)
     (str #x399 #x308 #x301)
     (str #x3a3)
     (str #x3a5 #x308 #x301)
     (str #x3a3)
     (str #x535 #x582)
     (str #x48 #x331)
     (str #x54 #x308)
     (str #x57 #x30a)
     (str #x59 #x30a)
     (str #x41 #x2be)
     (str #x3a5 #x313)
     (str #x3a5 #x313 #x300)
     (str #x3a5 #x313 #x301)
     (str #x3a5 #x313 #x342)
     (str #x1f88)
     (str #x1f89)
     (str #x1f8a)
     (str #x1f8b)
     (str #x1f8c)
     (str #x1f8d)
     (str #x1f8e)
     (str #x1f8f)
     (str #x1f88)
     (str #x1f89)
     (str #x1f8a)
     (str #x1f8b)
     (str #x1f8c)
     (str #x1f8d)
     (str #x1f8e)
     (str #x1f8f)
     (str #x1f98)
     (str #x1f99)
     (str #x1f9a)
     (str #x1f9b)
     (str #x1f9c)
     (str #x1f9d)
     (str #x1f9e)
     (str #x1f9f)
     (str #x1f98)
     (str #x1f99)
     (str #x1f9a)
     (str #x1f9b)
     (str #x1f9c)
     (str #x1f9d)
     (str #x1f9e)
     (str #x1f9f)
     (str #x1fa8)
     (str #x1fa9)
     (str #x1faa)
     (str #x1fab)
     (str #x1fac)
     (str #x1fad)
     (str #x1fae)
     (str #x1faf)
     (str #x1fa8)
     (str #x1fa9)
     (str #x1faa)
     (str #x1fab)
     (str #x1fac)
     (str #x1fad)
     (str #x1fae)
     (str #x1faf)
     (str #x1fba #x345)
     (str #x1fbc)
     (str #x386 #x345)
     (str #x391 #x342)
     (str #x391 #x342 #x345)
     (str #x1fbc)
     (str #x1fca #x345)
     (str #x1fcc)
     (str #x389 #x345)
     (str #x397 #x342)
     (str #x397 #x342 #x345)
     (str #x1fcc)
     (str #x399 #x308 #x300)
     (str #x399 #x308 #x301)
     (str #x399 #x342)
     (str #x399 #x308 #x342)
     (str #x3a5 #x308 #x300)
     (str #x3a5 #x308 #x301)
     (str #x3a1 #x313)
     (str #x3a5 #x342)
     (str #x3a5 #x308 #x342)
     (str #x1ffa #x345)
     (str #x1ffc)
     (str #x38f #x345)
     (str #x3a9 #x342)
     (str #x3a9 #x342 #x345)
     (str #x1ffc)
     (str #x46 #x66)
     (str #x46 #x69)
     (str #x46 #x6c)
     (str #x46 #x66 #x69)
     (str #x46 #x66 #x6c)
     (str #x53 #x74)
     (str #x53 #x74)
     (str #x544 #x576)
     (str #x544 #x565)
     (str #x544 #x56b)
     (str #x54e #x576)
     (str #x544 #x56d)
)))

(define special-uppercase-mapping
  (let ((str (lambda args
               (if (= 1 (length args))
                   (integer->char (car args))
                   (apply string (map integer->char args))))))
    (vector
     (str #x53 #x53)
     (str #x130)
     (str #x2bc #x4e)
     (str #x4a #x30c)
     (str #x399 #x308 #x301)
     (str #x3a3)
     (str #x3a5 #x308 #x301)
     (str #x3a3)
     (str #x535 #x552)
     (str #x48 #x331)
     (str #x54 #x308)
     (str #x57 #x30a)
     (str #x59 #x30a)
     (str #x41 #x2be)
     (str #x3a5 #x313)
     (str #x3a5 #x313 #x300)
     (str #x3a5 #x313 #x301)
     (str #x3a5 #x313 #x342)
     (str #x1f08 #x399)
     (str #x1f09 #x399)
     (str #x1f0a #x399)
     (str #x1f0b #x399)
     (str #x1f0c #x399)
     (str #x1f0d #x399)
     (str #x1f0e #x399)
     (str #x1f0f #x399)
     (str #x1f08 #x399)
     (str #x1f09 #x399)
     (str #x1f0a #x399)
     (str #x1f0b #x399)
     (str #x1f0c #x399)
     (str #x1f0d #x399)
     (str #x1f0e #x399)
     (str #x1f0f #x399)
     (str #x1f28 #x399)
     (str #x1f29 #x399)
     (str #x1f2a #x399)
     (str #x1f2b #x399)
     (str #x1f2c #x399)
     (str #x1f2d #x399)
     (str #x1f2e #x399)
     (str #x1f2f #x399)
     (str #x1f28 #x399)
     (str #x1f29 #x399)
     (str #x1f2a #x399)
     (str #x1f2b #x399)
     (str #x1f2c #x399)
     (str #x1f2d #x399)
     (str #x1f2e #x399)
     (str #x1f2f #x399)
     (str #x1f68 #x399)
     (str #x1f69 #x399)
     (str #x1f6a #x399)
     (str #x1f6b #x399)
     (str #x1f6c #x399)
     (str #x1f6d #x399)
     (str #x1f6e #x399)
     (str #x1f6f #x399)
     (str #x1f68 #x399)
     (str #x1f69 #x399)
     (str #x1f6a #x399)
     (str #x1f6b #x399)
     (str #x1f6c #x399)
     (str #x1f6d #x399)
     (str #x1f6e #x399)
     (str #x1f6f #x399)
     (str #x1fba #x399)
     (str #x391 #x399)
     (str #x386 #x399)
     (str #x391 #x342)
     (str #x391 #x342 #x399)
     (str #x391 #x399)
     (str #x1fca #x399)
     (str #x397 #x399)
     (str #x389 #x399)
     (str #x397 #x342)
     (str #x397 #x342 #x399)
     (str #x397 #x399)
     (str #x399 #x308 #x300)
     (str #x399 #x308 #x301)
     (str #x399 #x342)
     (str #x399 #x308 #x342)
     (str #x3a5 #x308 #x300)
     (str #x3a5 #x308 #x301)
     (str #x3a1 #x313)
     (str #x3a5 #x342)
     (str #x3a5 #x308 #x342)
     (str #x1ffa #x399)
     (str #x3a9 #x399)
     (str #x38f #x399)
     (str #x3a9 #x342)
     (str #x3a9 #x342 #x399)
     (str #x3a9 #x399)
     (str #x46 #x46)
     (str #x46 #x49)
     (str #x46 #x4c)
     (str #x46 #x46 #x49)
     (str #x46 #x46 #x4c)
     (str #x53 #x54)
     (str #x53 #x54)
     (str #x544 #x546)
     (str #x544 #x535)
     (str #x544 #x53b)
     (str #x54e #x546)
     (str #x544 #x53d)
)))

; Under full case folding, the scalar values
; in this vector fold to the characters and strings
; in the full-foldcase-mappings vector.
; All other scalar values fold to their (simple)
; downcased values.
;
; Each of those tables contains 326 elements.

(define full-foldcase-exceptions
  '#(
        #xb5 #xdf #x130 #x149 #x17f #x1f0 #x345 #x370 
        #x372 #x376 #x37f #x390 #x3b0 #x3c2 #x3cf #x3d0 
        #x3d1 #x3d5 #x3d6 #x3f0 #x3f1 #x3f5 #x514 #x516 
        #x518 #x51a #x51c #x51e #x520 #x522 #x524 #x526 
        #x528 #x52a #x52c #x52e #x587 #x10c7 #x10cd #x1e96 
        #x1e97 #x1e98 #x1e99 #x1e9a #x1e9b #x1e9e #x1efa #x1efc 
        #x1efe #x1f50 #x1f52 #x1f54 #x1f56 #x1f80 #x1f81 #x1f82 
        #x1f83 #x1f84 #x1f85 #x1f86 #x1f87 #x1f88 #x1f89 #x1f8a 
        #x1f8b #x1f8c #x1f8d #x1f8e #x1f8f #x1f90 #x1f91 #x1f92 
        #x1f93 #x1f94 #x1f95 #x1f96 #x1f97 #x1f98 #x1f99 #x1f9a 
        #x1f9b #x1f9c #x1f9d #x1f9e #x1f9f #x1fa0 #x1fa1 #x1fa2 
        #x1fa3 #x1fa4 #x1fa5 #x1fa6 #x1fa7 #x1fa8 #x1fa9 #x1faa 
        #x1fab #x1fac #x1fad #x1fae #x1faf #x1fb2 #x1fb3 #x1fb4 
        #x1fb6 #x1fb7 #x1fbc #x1fbe #x1fc2 #x1fc3 #x1fc4 #x1fc6 
        #x1fc7 #x1fcc #x1fd2 #x1fd3 #x1fd6 #x1fd7 #x1fe2 #x1fe3 
        #x1fe4 #x1fe6 #x1fe7 #x1ff2 #x1ff3 #x1ff4 #x1ff6 #x1ff7 
        #x1ffc #x2c6d #x2c6e #x2c6f #x2c70 #x2c72 #x2c7e #x2c7f 
        #x2ceb #x2ced #x2cf2 #xa640 #xa642 #xa644 #xa646 #xa648 
        #xa64a #xa64c #xa64e #xa650 #xa652 #xa654 #xa656 #xa658 
        #xa65a #xa65c #xa65e #xa660 #xa662 #xa664 #xa666 #xa668 
        #xa66a #xa66c #xa680 #xa682 #xa684 #xa686 #xa688 #xa68a 
        #xa68c #xa68e #xa690 #xa692 #xa694 #xa696 #xa698 #xa69a 
        #xa722 #xa724 #xa726 #xa728 #xa72a #xa72c #xa72e #xa732 
        #xa734 #xa736 #xa738 #xa73a #xa73c #xa73e #xa740 #xa742 
        #xa744 #xa746 #xa748 #xa74a #xa74c #xa74e #xa750 #xa752 
        #xa754 #xa756 #xa758 #xa75a #xa75c #xa75e #xa760 #xa762 
        #xa764 #xa766 #xa768 #xa76a #xa76c #xa76e #xa779 #xa77b 
        #xa77d #xa77e #xa780 #xa782 #xa784 #xa786 #xa78b #xa78d 
        #xa790 #xa792 #xa796 #xa798 #xa79a #xa79c #xa79e #xa7a0 
        #xa7a2 #xa7a4 #xa7a6 #xa7a8 #xa7aa #xa7ab #xa7ac #xa7ad 
        #xa7b0 #xa7b1 #xfb00 #xfb01 #xfb02 #xfb03 #xfb04 #xfb05 
        #xfb06 #xfb13 #xfb14 #xfb15 #xfb16 #xfb17 #x10400 #x10401 
        #x10402 #x10403 #x10404 #x10405 #x10406 #x10407 #x10408 #x10409 
        #x1040a #x1040b #x1040c #x1040d #x1040e #x1040f #x10410 #x10411 
        #x10412 #x10413 #x10414 #x10415 #x10416 #x10417 #x10418 #x10419 
        #x1041a #x1041b #x1041c #x1041d #x1041e #x1041f #x10420 #x10421 
        #x10422 #x10423 #x10424 #x10425 #x10426 #x10427 #x118a0 #x118a1 
        #x118a2 #x118a3 #x118a4 #x118a5 #x118a6 #x118a7 #x118a8 #x118a9 
        #x118aa #x118ab #x118ac #x118ad #x118ae #x118af #x118b0 #x118b1 
        #x118b2 #x118b3 #x118b4 #x118b5 #x118b6 #x118b7 #x118b8 #x118b9 
        #x118ba #x118bb #x118bc #x118bd #x118be #x118bf ))

(define full-foldcase-mappings
  (let ((str (lambda args
               (if (= 1 (length args))
                   (integer->char (car args))
                   (apply string (map integer->char args))))))
    (vector
        (str #x3bc)
        (str #x73 #x73)
        (str #x69 #x307)
        (str #x2bc #x6e)
        (str #x73)
        (str #x6a #x30c)
        (str #x3b9)
        (str #x371)
        (str #x373)
        (str #x377)
        (str #x3f3)
        (str #x3b9 #x308 #x301)
        (str #x3c5 #x308 #x301)
        (str #x3c3)
        (str #x3d7)
        (str #x3b2)
        (str #x3b8)
        (str #x3c6)
        (str #x3c0)
        (str #x3ba)
        (str #x3c1)
        (str #x3b5)
        (str #x515)
        (str #x517)
        (str #x519)
        (str #x51b)
        (str #x51d)
        (str #x51f)
        (str #x521)
        (str #x523)
        (str #x525)
        (str #x527)
        (str #x529)
        (str #x52b)
        (str #x52d)
        (str #x52f)
        (str #x565 #x582)
        (str #x2d27)
        (str #x2d2d)
        (str #x68 #x331)
        (str #x74 #x308)
        (str #x77 #x30a)
        (str #x79 #x30a)
        (str #x61 #x2be)
        (str #x1e61)
        (str #x73 #x73)
        (str #x1efb)
        (str #x1efd)
        (str #x1eff)
        (str #x3c5 #x313)
        (str #x3c5 #x313 #x300)
        (str #x3c5 #x313 #x301)
        (str #x3c5 #x313 #x342)
        (str #x1f00 #x3b9)
        (str #x1f01 #x3b9)
        (str #x1f02 #x3b9)
        (str #x1f03 #x3b9)
        (str #x1f04 #x3b9)
        (str #x1f05 #x3b9)
        (str #x1f06 #x3b9)
        (str #x1f07 #x3b9)
        (str #x1f00 #x3b9)
        (str #x1f01 #x3b9)
        (str #x1f02 #x3b9)
        (str #x1f03 #x3b9)
        (str #x1f04 #x3b9)
        (str #x1f05 #x3b9)
        (str #x1f06 #x3b9)
        (str #x1f07 #x3b9)
        (str #x1f20 #x3b9)
        (str #x1f21 #x3b9)
        (str #x1f22 #x3b9)
        (str #x1f23 #x3b9)
        (str #x1f24 #x3b9)
        (str #x1f25 #x3b9)
        (str #x1f26 #x3b9)
        (str #x1f27 #x3b9)
        (str #x1f20 #x3b9)
        (str #x1f21 #x3b9)
        (str #x1f22 #x3b9)
        (str #x1f23 #x3b9)
        (str #x1f24 #x3b9)
        (str #x1f25 #x3b9)
        (str #x1f26 #x3b9)
        (str #x1f27 #x3b9)
        (str #x1f60 #x3b9)
        (str #x1f61 #x3b9)
        (str #x1f62 #x3b9)
        (str #x1f63 #x3b9)
        (str #x1f64 #x3b9)
        (str #x1f65 #x3b9)
        (str #x1f66 #x3b9)
        (str #x1f67 #x3b9)
        (str #x1f60 #x3b9)
        (str #x1f61 #x3b9)
        (str #x1f62 #x3b9)
        (str #x1f63 #x3b9)
        (str #x1f64 #x3b9)
        (str #x1f65 #x3b9)
        (str #x1f66 #x3b9)
        (str #x1f67 #x3b9)
        (str #x1f70 #x3b9)
        (str #x3b1 #x3b9)
        (str #x3ac #x3b9)
        (str #x3b1 #x342)
        (str #x3b1 #x342 #x3b9)
        (str #x3b1 #x3b9)
        (str #x3b9)
        (str #x1f74 #x3b9)
        (str #x3b7 #x3b9)
        (str #x3ae #x3b9)
        (str #x3b7 #x342)
        (str #x3b7 #x342 #x3b9)
        (str #x3b7 #x3b9)
        (str #x3b9 #x308 #x300)
        (str #x3b9 #x308 #x301)
        (str #x3b9 #x342)
        (str #x3b9 #x308 #x342)
        (str #x3c5 #x308 #x300)
        (str #x3c5 #x308 #x301)
        (str #x3c1 #x313)
        (str #x3c5 #x342)
        (str #x3c5 #x308 #x342)
        (str #x1f7c #x3b9)
        (str #x3c9 #x3b9)
        (str #x3ce #x3b9)
        (str #x3c9 #x342)
        (str #x3c9 #x342 #x3b9)
        (str #x3c9 #x3b9)
        (str #x251)
        (str #x271)
        (str #x250)
        (str #x252)
        (str #x2c73)
        (str #x23f)
        (str #x240)
        (str #x2cec)
        (str #x2cee)
        (str #x2cf3)
        (str #xa641)
        (str #xa643)
        (str #xa645)
        (str #xa647)
        (str #xa649)
        (str #xa64b)
        (str #xa64d)
        (str #xa64f)
        (str #xa651)
        (str #xa653)
        (str #xa655)
        (str #xa657)
        (str #xa659)
        (str #xa65b)
        (str #xa65d)
        (str #xa65f)
        (str #xa661)
        (str #xa663)
        (str #xa665)
        (str #xa667)
        (str #xa669)
        (str #xa66b)
        (str #xa66d)
        (str #xa681)
        (str #xa683)
        (str #xa685)
        (str #xa687)
        (str #xa689)
        (str #xa68b)
        (str #xa68d)
        (str #xa68f)
        (str #xa691)
        (str #xa693)
        (str #xa695)
        (str #xa697)
        (str #xa699)
        (str #xa69b)
        (str #xa723)
        (str #xa725)
        (str #xa727)
        (str #xa729)
        (str #xa72b)
        (str #xa72d)
        (str #xa72f)
        (str #xa733)
        (str #xa735)
        (str #xa737)
        (str #xa739)
        (str #xa73b)
        (str #xa73d)
        (str #xa73f)
        (str #xa741)
        (str #xa743)
        (str #xa745)
        (str #xa747)
        (str #xa749)
        (str #xa74b)
        (str #xa74d)
        (str #xa74f)
        (str #xa751)
        (str #xa753)
        (str #xa755)
        (str #xa757)
        (str #xa759)
        (str #xa75b)
        (str #xa75d)
        (str #xa75f)
        (str #xa761)
        (str #xa763)
        (str #xa765)
        (str #xa767)
        (str #xa769)
        (str #xa76b)
        (str #xa76d)
        (str #xa76f)
        (str #xa77a)
        (str #xa77c)
        (str #x1d79)
        (str #xa77f)
        (str #xa781)
        (str #xa783)
        (str #xa785)
        (str #xa787)
        (str #xa78c)
        (str #x265)
        (str #xa791)
        (str #xa793)
        (str #xa797)
        (str #xa799)
        (str #xa79b)
        (str #xa79d)
        (str #xa79f)
        (str #xa7a1)
        (str #xa7a3)
        (str #xa7a5)
        (str #xa7a7)
        (str #xa7a9)
        (str #x266)
        (str #x25c)
        (str #x261)
        (str #x26c)
        (str #x29e)
        (str #x287)
        (str #x66 #x66)
        (str #x66 #x69)
        (str #x66 #x6c)
        (str #x66 #x66 #x69)
        (str #x66 #x66 #x6c)
        (str #x73 #x74)
        (str #x73 #x74)
        (str #x574 #x576)
        (str #x574 #x565)
        (str #x574 #x56b)
        (str #x57e #x576)
        (str #x574 #x56d)
        (str #x10428)
        (str #x10429)
        (str #x1042a)
        (str #x1042b)
        (str #x1042c)
        (str #x1042d)
        (str #x1042e)
        (str #x1042f)
        (str #x10430)
        (str #x10431)
        (str #x10432)
        (str #x10433)
        (str #x10434)
        (str #x10435)
        (str #x10436)
        (str #x10437)
        (str #x10438)
        (str #x10439)
        (str #x1043a)
        (str #x1043b)
        (str #x1043c)
        (str #x1043d)
        (str #x1043e)
        (str #x1043f)
        (str #x10440)
        (str #x10441)
        (str #x10442)
        (str #x10443)
        (str #x10444)
        (str #x10445)
        (str #x10446)
        (str #x10447)
        (str #x10448)
        (str #x10449)
        (str #x1044a)
        (str #x1044b)
        (str #x1044c)
        (str #x1044d)
        (str #x1044e)
        (str #x1044f)
        (str #x118c0)
        (str #x118c1)
        (str #x118c2)
        (str #x118c3)
        (str #x118c4)
        (str #x118c5)
        (str #x118c6)
        (str #x118c7)
        (str #x118c8)
        (str #x118c9)
        (str #x118ca)
        (str #x118cb)
        (str #x118cc)
        (str #x118cd)
        (str #x118ce)
        (str #x118cf)
        (str #x118d0)
        (str #x118d1)
        (str #x118d2)
        (str #x118d3)
        (str #x118d4)
        (str #x118d5)
        (str #x118d6)
        (str #x118d7)
        (str #x118d8)
        (str #x118d9)
        (str #x118da)
        (str #x118db)
        (str #x118dc)
        (str #x118dd)
        (str #x118de)
        (str #x118df))))

