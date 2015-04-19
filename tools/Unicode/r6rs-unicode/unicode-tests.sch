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
; Basic tests of (r6rs unicode) procedures,
; mostly taken from the R6RS examples.
;
; Usage:
;
; > (load "unicode-tests.sch")
;
; This is an R6RS top-level program, but
; it does not rely upon any non-R5RS lexical syntax,
; and can be converted into an R5RS program by removing
; the import declaration.
;
; During the transition to R6RS, an R5RS-compatible equal?
; procedure might not use Unicode-compatible versions of the
; character and string comparisons, so we define an extended
; version of equal? here.

(import (rnrs base)
        (rnrs control)
        (rnrs lists)
        (rnrs io simple)
        (local unicode))

(define (unicode-equal? x y)
  (or (and (char? x) (char? y) (char=? x y))
      (and (string? x) (string? y) (string=? x y))
      (equal? x y)))

(define-syntax unicode-test
  (syntax-rules (=> error)
   ((unicode-test name0 name exp => result exit)
    (begin
     (display 'name0) (display 'name) (display "...") (newline)
     (if (not (unicode-equal? exp result))
         (begin (display "*****BUG*****")
                (newline)
                (display "Failed test ")
                (display 'name0)
                (display 'name)
                (display ":")
                (newline)
                (write 'exp)
                (newline)
                (exit #f)))))))

(begin
 (define es-zed (integer->char #x00df))
 (define final-sigma (integer->char #x03c2))
 (define lower-sigma (integer->char #x03c3))
 (define upper-sigma (integer->char #x03a3))
 (define upper-chi (integer->char #x03a7))
 (define upper-alpha (integer->char #x0391))
 (define upper-omicron (integer->char #x039f))
 (define lower-chi (integer->char #x03c7))
 (define lower-alpha (integer->char #x03b1))
 (define lower-omicron (integer->char #x03bf))
 (define strasse (string #\S #\t #\r #\a es-zed #\e))
 (define upper-chaos (string upper-chi upper-alpha upper-omicron upper-sigma))
 (define final-chaos (string lower-chi lower-alpha lower-omicron final-sigma))
 (define lower-chaos (string lower-chi lower-alpha lower-omicron lower-sigma))
)

(define (basic-unicode-char-tests)
  (call-with-current-continuation
   (lambda (exit)
     (let-syntax ((test (syntax-rules (=> error)
                         ((test name exp => result)
                          (unicode-test char- name exp => result exit)))))

       (let ()

         ; Given a unary predicate on characters, returns a sorted
         ; list of all characters that satisfy the predicate.

         (define (filter-all-chars p?)
           (do ((i 0 (+ i 1))
                (chars '()
                       (if (and (not (<= #xd800 i #xdfff))
                                (p? (integer->char i)))
                           (cons (integer->char i) chars)
                           chars)))
               ((= i #x110000)
                (reverse chars))))

         ; Given a list of characters, prints its length and returns 0.

         (define (report chars n)
           (display "  ")
           (display (length chars))
           (display " characters")
           (if (not (= n (length chars)))
               (begin (display " but expected ")
                      (write n)
                      (display " in Unicode 7.0.0")))
           (newline)
           0)

         (test type1 (integer->char 32) => #\space)
         (test type2 (char->integer (integer->char 5000)) => 5000)
         ;(test type3 (integer->char #xd800) => error)

         (test comp1 (char<? #\z es-zed) => #t)
         (test comp2 (char<? #\z #\Z) => #f)
         (test comp3 (char-ci<? #\z #\Z) => #f)
         (test comp4 (char-ci=? #\z #\Z) => #t)
         (test comp5 (char-ci=? final-sigma lower-sigma) => #t)

         (test case1 (char-upcase #\i) => #\I)
         (test case2 (char-downcase #\i) => #\i)
         (test case3 (char-titlecase #\i) => #\I)
         (test case4 (char-foldcase #\i) => #\i)

         (test case5 (char-upcase es-zed) => es-zed)
         (test case6 (char-downcase es-zed) => es-zed)
         (test case7 (char-titlecase es-zed) => es-zed)
         (test case8 (char-foldcase es-zed) => es-zed)

         (test case9 (char-upcase upper-sigma) => upper-sigma)
         (test case10 (char-downcase upper-sigma) => lower-sigma)
         (test case11 (char-titlecase upper-sigma) => upper-sigma)
         (test case12 (char-foldcase upper-sigma) => lower-sigma)

         (test case13 (char-upcase final-sigma) => upper-sigma)
         (test case14 (char-downcase final-sigma) => final-sigma)
         (test case15 (char-titlecase final-sigma) => upper-sigma)
         (test case16 (char-foldcase final-sigma) => lower-sigma)

         (test cat1 (char-general-category #\a) => 'Ll)
         (test cat2 (char-general-category #\space) => 'Zs)
         (test cat3 (char-general-category (integer->char #x10FFFF)) => 'Cn)

         (test alpha1 (char-alphabetic? #\a) => #t)
         (test numer1 (char-numeric? #\1) => #t)
         (test white1 (char-whitespace? #\space) => #t)
         (test white2 (char-whitespace? (integer->char #x00A0)) => #t)
         (test upper1 (char-upper-case? upper-sigma) => #t)
         (test lower1 (char-lower-case? lower-sigma) => #t)
         (test lower2 (char-lower-case? (integer->char #x00AA)) => #t)
         (test title1 (char-title-case? #\I) => #f)
         (test title2 (char-title-case? (integer->char #x01C5)) => #t)

         (test excluded
               (do ((i 128 (+ i 1))
                    (excluded '()
                     (if (and (not (<= #xd800 i #xdfff))
                              (memq (char-general-category (integer->char i))
                                  '(Ps Pe Pi Pf Zs Zp Zl Cc Cf)))
                         (cons i excluded)
                       excluded)))
                   ((= i #x110000)
                    (reverse excluded)))
               => excluded-code-points-above-127)

         (test upcase
               (filter-all-chars (lambda (c) (char-upcase c) #f))
               => '())

         (test downcase
               (filter-all-chars (lambda (c) (char-downcase c) #f))
               => '())

         (test titlecase
               (filter-all-chars (lambda (c) (char-titlecase c) #f))
               => '())

         (test foldcase
               (filter-all-chars (lambda (c) (char-foldcase c) #f))
               => '())

         (test general-category
               (report (filter-all-chars (lambda (c)
                                           (char-general-category c)))
                       1112064)
               => 0)

         (test alphabetic?
               (report (filter-all-chars char-alphabetic?) 104077)
               => 0)

         (test numeric?
               (report (filter-all-chars char-numeric?) 530)
               => 0)

         (test whitespace?
               (report (filter-all-chars char-whitespace?) 25)
               => 0)

         (test upper-case?
               (report (filter-all-chars char-upper-case?) 1610)
               => 0)

         (test lower-case?
               (report (filter-all-chars char-lower-case?) 2030)
               => 0)

         (test title-case?
               (report (filter-all-chars char-title-case?) 31)
               => 0)

)))))


(define (basic-unicode-string-tests)
  (call-with-current-continuation
   (lambda (exit)
     (let-syntax ((test (syntax-rules (=> error)
                         ((test name exp => result)
                          (unicode-test string- name exp => result exit)))))

       (test scomp1 (string<? "z" (string es-zed)) => #t)
       (test scomp2 (string<? "z" "zz") => #t)
       (test scomp3 (string<? "z" "Z") => #f)
       (test scomp4 (string=? strasse "Strasse") => #f)

       (test sup1 (string-upcase "Hi") => "HI")
       (test sdown1 (string-downcase "Hi") => "hi")
       (test sfold1 (string-foldcase "Hi") => "hi")

       (test sup2  (string-upcase strasse) => "STRASSE")
       (test sdown2 (string-downcase strasse)
                    => (string-append "s" (substring strasse 1 6)))
       (test sfold2 (string-foldcase strasse) => "strasse")
       (test sdown3 (string-downcase "STRASSE")  => "strasse")

       (test chaos1 (string-upcase upper-chaos) => upper-chaos)
       (test chaos2 (string-downcase (string upper-sigma))
                    => (string lower-sigma))
       (test chaos3 (string-downcase upper-chaos) => final-chaos)
       (test chaos4 (string-downcase (string-append upper-chaos
                                                    (string upper-sigma)))
                    => (string-append (substring lower-chaos 0 3)
                                      (string lower-sigma final-sigma)))
       (test chaos5 (string-downcase (string-append upper-chaos
                                                    (string #\space
                                                            upper-sigma)))
                    => (string-append final-chaos
                                      (string #\space lower-sigma)))
       (test chaos6 (string-foldcase (string-append upper-chaos
                                                    (string upper-sigma)))
                    => (string-append lower-chaos
                                      (string lower-sigma)))
       (test chaos7 (string-upcase final-chaos) => upper-chaos)
       (test chaos8 (string-upcase lower-chaos) => upper-chaos)

       (test stitle1 (string-titlecase "kNock KNoCK") => "Knock Knock")
       (test stitle2 (string-titlecase "who's there?") => "Who's There?")
       (test stitle3 (string-titlecase "r6rs") => "R6rs")
       (test stitle4 (string-titlecase "R6RS") => "R6rs")

       (test norm1 (string-normalize-nfd (string #\xE9))
                   => (string #\x65 #\x301))
       (test norm2 (string-normalize-nfc (string #\xE9))
                   => (string #\xE9))
       (test norm3 (string-normalize-nfd (string #\x65 #\x301))
                   => (string #\x65 #\x301))
       (test norm4 (string-normalize-nfc (string #\x65 #\x301))
                   => (string #\xE9))

       (test sci1 (string-ci<? "z" "Z") => #f)
       (test sci2 (string-ci=? "z" "Z") => #t)
       (test sci3 (string-ci=? strasse "Strasse") => #t)
       (test sci4 (string-ci=? strasse "STRASSE") => #t)
       (test sci5 (string-ci=? upper-chaos lower-chaos) => #t)

))))


; SRFI 77 listed all code points above 127 in Unicode 4.1 whose
; Unicode general category is Ps, Pe, Pi, Pf, Zs, Zp, Zl, Cc, or Cf.
;
; Two code points (#\x23b4 and #\x23b5) were dropped from that list
; in Unicode 5.0, and there have been quite a few more changes since
; then.  The following list was generated by the reference
; implementation, so it isn't a real test of that implementation.

(define excluded-code-points-above-127
  '(
    #x80 #x81 #x82 #x83 #x84 #x85 #x86 #x87
    #x88 #x89 #x8a #x8b #x8c #x8d #x8e #x8f
    #x90 #x91 #x92 #x93 #x94 #x95 #x96 #x97
    #x98 #x99 #x9a #x9b #x9c #x9d #x9e #x9f
    #xa0 #xab #xad #xbb #x600 #x601 #x602 #x603
    #x604 #x605 #x61c #x6dd #x70f #xf3a #xf3b #xf3c
    #xf3d #x1680 #x169b #x169c #x180e #x2000 #x2001 #x2002
    #x2003 #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200a
    #x200b #x200c #x200d #x200e #x200f #x2018 #x2019 #x201a
    #x201b #x201c #x201d #x201e #x201f #x2028 #x2029 #x202a
    #x202b #x202c #x202d #x202e #x202f #x2039 #x203a #x2045
    #x2046 #x205f #x2060 #x2061 #x2062 #x2063 #x2064 #x2066
    #x2067 #x2068 #x2069 #x206a #x206b #x206c #x206d #x206e
    #x206f #x207d #x207e #x208d #x208e #x2308 #x2309 #x230a
    #x230b #x2329 #x232a #x2768 #x2769 #x276a #x276b #x276c
    #x276d #x276e #x276f #x2770 #x2771 #x2772 #x2773 #x2774
    #x2775 #x27c5 #x27c6 #x27e6 #x27e7 #x27e8 #x27e9 #x27ea
    #x27eb #x27ec #x27ed #x27ee #x27ef #x2983 #x2984 #x2985
    #x2986 #x2987 #x2988 #x2989 #x298a #x298b #x298c #x298d
    #x298e #x298f #x2990 #x2991 #x2992 #x2993 #x2994 #x2995
    #x2996 #x2997 #x2998 #x29d8 #x29d9 #x29da #x29db #x29fc
    #x29fd #x2e02 #x2e03 #x2e04 #x2e05 #x2e09 #x2e0a #x2e0c
    #x2e0d #x2e1c #x2e1d #x2e20 #x2e21 #x2e22 #x2e23 #x2e24
    #x2e25 #x2e26 #x2e27 #x2e28 #x2e29 #x2e42 #x3000 #x3008
    #x3009 #x300a #x300b #x300c #x300d #x300e #x300f #x3010
    #x3011 #x3014 #x3015 #x3016 #x3017 #x3018 #x3019 #x301a
    #x301b #x301d #x301e #x301f #xfd3e #xfd3f #xfe17 #xfe18
    #xfe35 #xfe36 #xfe37 #xfe38 #xfe39 #xfe3a #xfe3b #xfe3c
    #xfe3d #xfe3e #xfe3f #xfe40 #xfe41 #xfe42 #xfe43 #xfe44
    #xfe47 #xfe48 #xfe59 #xfe5a #xfe5b #xfe5c #xfe5d #xfe5e
    #xfeff #xff08 #xff09 #xff3b #xff3d #xff5b #xff5d #xff5f
    #xff60 #xff62 #xff63 #xfff9 #xfffa #xfffb #x110bd #x1bca0
    #x1bca1 #x1bca2 #x1bca3 #x1d173 #x1d174 #x1d175 #x1d176 #x1d177
    #x1d178 #x1d179 #x1d17a #xe0001 #xe0020 #xe0021 #xe0022 #xe0023
    #xe0024 #xe0025 #xe0026 #xe0027 #xe0028 #xe0029 #xe002a #xe002b
    #xe002c #xe002d #xe002e #xe002f #xe0030 #xe0031 #xe0032 #xe0033
    #xe0034 #xe0035 #xe0036 #xe0037 #xe0038 #xe0039 #xe003a #xe003b
    #xe003c #xe003d #xe003e #xe003f #xe0040 #xe0041 #xe0042 #xe0043
    #xe0044 #xe0045 #xe0046 #xe0047 #xe0048 #xe0049 #xe004a #xe004b
    #xe004c #xe004d #xe004e #xe004f #xe0050 #xe0051 #xe0052 #xe0053
    #xe0054 #xe0055 #xe0056 #xe0057 #xe0058 #xe0059 #xe005a #xe005b
    #xe005c #xe005d #xe005e #xe005f #xe0060 #xe0061 #xe0062 #xe0063
    #xe0064 #xe0065 #xe0066 #xe0067 #xe0068 #xe0069 #xe006a #xe006b
    #xe006c #xe006d #xe006e #xe006f #xe0070 #xe0071 #xe0072 #xe0073
    #xe0074 #xe0075 #xe0076 #xe0077 #xe0078 #xe0079 #xe007a #xe007b
    #xe007c #xe007d #xe007e #xe007f))

(basic-unicode-char-tests)
(basic-unicode-string-tests)
