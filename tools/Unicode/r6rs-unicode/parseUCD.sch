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
; Parsers for UCD File Format.
;
; Typical usage:
;     (read-unicode-files)
;     (write-unicode-tables "temp.sch")
;
; The code in temp.sch must then be copied by hand into the
; appropriate unicode*.sch files.
;
; Uses filter, and perhaps a few more non-R5RS procedures.
; Assumes the target system has bytevector operations.
;
; The Unicode files that are needed are:
;     UnicodeData.txt
;     CaseFolding.txt
;     SpecialCasing.txt
;     CompositionExclusions.txt
;     PropList.txt
;     auxiliary/GraphemeBreakProperty.txt
;     auxiliary/WordBreakProperty.txt
; These files can be found at http://www.unicode.org/Public/UNIDATA/
;
; The Larceny format for a line of parsed data is
; a vector
;     whose element 0 is an exact integer or pair of exact integers
;         specifying a code point or range (inclusive)
;     whose remaining elements are strings of ASCII characters

; Given a string naming a file that is in UCD File Format,
; returns a list of the Larceny format for the lines of parsed data
; in the file.

(define (parseUCD-file filename)
  (display "Parsing ")
  (display filename)
  (display "...")
  (newline)
  (call-with-input-file
   filename
   parseUCD-port))

; Given an input port for UCD File Format,
; reads and parses the lines of data in the port,
; returning a list of the Larceny format for the lines of parsed data.

(define (parseUCD-port in)
  (let loop ((lines '()))
    (let ((x (parseUCD-line in)))
      (if (eof-object? x)
          (reverse lines)
          (loop (cons x lines))))))

; Reads and parses a single line from the given input port.
; Returns an end-of-file object or
; a vector of parsed data in the Larceny format described above.

(define (parseUCD-line in)
  (consume-whitespace! in)
  (let ((code-point (parseUCD-field in)))
    (cond ((eof-object? code-point)
           code-point)
          ((not code-point)
           (parseUCD-line in))
          (else
           (let loop ((fields '()))
             (let ((x (parseUCD-field in)))
               (if x
                   (loop (cons x fields))
                   (let ((result (apply vector
                                        (parse-code-point code-point)
                                        (reverse fields))))
                     ; hack for parsing ranges in UnicodeData.txt
                     (if (and (> (vector-length result) 1)
                              (let* ((name (vector-ref result 1))
                                     (n (string-length name)))
                                (and (> n 8)
                                     (string-ci=? (substring name (- n 8) n)
                                                  ", First>")
                                     (string=? (substring name 0 1) "<"))))
                         (let* ((name (vector-ref result 1))
                                (n (string-length name))
                                (result2 (parseUCD-line in)))
                           (if  (and (> (vector-length result2) 1)
                                     (let* ((name2 (vector-ref result2 1))
                                            (n2 (string-length name2)))
                                       (and (> n2 7)
                                            (= (- n 1) n2)
                                            (string=?
                                             (substring name 0 (- n 8))
                                             (substring name2 0 (- n 8)))
                                            (string-ci=?
                                             (substring name2 (- n 8) n2)
                                             ", Last>"))))
                                (let ((cp1 (vector-ref result 0))
                                      (cp2 (vector-ref result2 0)))
                                  (vector-set! result 0 (list cp1 cp2))
                                  result)
                                (error "Bad UnicodeData.txt range syntax: "
                                       name (vector-ref result2 1))))
                         result)))))))))

; Reads and parses a single field from the given input port.
; Returns an end-of-file object, #f, or a string of ASCII characters.

(define (parseUCD-field in)
  (consume-whitespace! in)
  (let loop ((chars '()))
    (let ((c (peek-char in)))
      (cond ((eof-object? c)
             (if (null? chars)
                 c
                 (canonical-string (list->string (reverse chars)))))
            ((char=? c #\;)
             (read-char in)
             (canonical-string (list->string (reverse chars))))
            ((char=? c #\newline)
             (if (null? chars)
                 (begin (read-char in) #f)
                 (canonical-string (list->string (reverse chars)))))
;           ((char-whitespace? c)
;            (consume-whitespace! in)
;            (if (null? chars)
;                #f
;                (canonical-string (list->string (reverse chars)))))
            (else
             (read-char in)
             (loop (cons c chars)))))))

; Given a string specifying a code point or range of code points,
; returns an exact integer or pair of exact integers or #f.

(define (parse-code-point str)
  ;(display str)
  ;(newline)
  (let* ((chars (string->list str))
         (range (memv #\. chars))
         (spaces (memv #\space chars)))
    (if range
        (list (parse-code-point
               (substring str 0 (- (string-length str) (length range))))
              (parse-code-point
               (list->string (cddr range))))
        (if (and spaces (not (eq? chars spaces)))
            (parse-code-point
             (substring str 0 (- (string-length str) (length spaces))))
            (string->number str 16)))))

; Returns the canonical string equal to the given string.

(define (canonical-string str)
  (let ((probe (member str canonical-canonical-strings)))
    (if probe
        (car probe)
        str)))

(define canonical-canonical-strings
  '("Lu" "Ll" "Lt" "Lm" "Lo"
    "Mn" "Mc" "Me"
    "Nd" "Nl" "No"
    "Pc" "Pd" "Ps" "Pe" "Pi" "Pf" "Po"
    "Sm" "Sc" "Sk" "So"
    "Zs" "Zl" "Zp"
    "Cc" "Cf" "Cs" "Co" "Cn"))

; Consumes whitespace from the given input port.
; Does not consume the #\newline at the end of a comment line.

(define (consume-whitespace! in)
  (let ((c (peek-char in)))
    (cond ((eof-object? c)
           #f)
          ((char=? c #\newline)
           #f)
          ((char-whitespace? c)
           (read-char in)
           (consume-whitespace! in))
          ((char=? c #\#)
           (consume-line! in))
          (else #f))))

; Consumes the current line from the given input port.

(define (consume-line! in)
  (let ((c (peek-char in)))
    (cond ((eof-object? c)
           #f)
          ((char=? c #\newline)
           #f)
          (else
           (read-char in)
           (consume-line! in)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Reads the standard file from a specified directory,
; which defaults to the current working directory.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define unicode-data '())            ; 17710 entries (Unicode 5.0.0)
(define case-folding '())            ;  1039 entries
(define special-casing '())          ;   119 entries
(define composition-exclusions '())  ;    81 entries
(define prop-list '())               ;   896 entries
(define grapheme-break-property '()) ;
(define word-break-property '())     ;   468 entries

(define (read-unicode-files . rest)
  (if (null? rest)
      (read-unicode-files "")
      (let ((dir (car rest)))
        (set! unicode-data
              (parseUCD-file (string-append dir "UnicodeData.txt")))
        (set! case-folding
              (parseUCD-file (string-append dir "CaseFolding.txt")))
        (set! special-casing
              (parseUCD-file (string-append dir "SpecialCasing.txt")))
        (set! composition-exclusions
              (parseUCD-file (string-append dir "CompositionExclusions.txt")))
        (set! prop-list
              (parseUCD-file (string-append dir "PropList.txt")))
        (set! grapheme-break-property
              (parseUCD-file (string-append dir "GraphemeBreakProperty.txt")))
        (set! word-break-property
              (parseUCD-file (string-append dir "WordBreakProperty.txt")))
        #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The proposed R6RS procedures that require Unicode information
; are:
;
; char-upcase
; char-downcase
; char-titlecase
; char-foldcase
;
; char-general-category
; char-alphabetic?
; char-numberic?
; char-whitespace?
; char-upper-case?
; char-lower-case?
; char-title-case?
;
; string-upcase
; string-downcase
; string-titlecase
; string-foldcase
;
; string-normalize-nfd
; string-normalize-nfkd
; string-normalize-nfc
; string-normalize-nfkc
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given the Larceny format for a line of parsed data
; taken from UnicodeData.txt,
; returns the Unicode general category as a symbol.

(define (extract-general-category data)
  (if (> (vector-length data) 2)
      (string->symbol (vector-ref data 2))
      (error "Bad data to extract-general-category" data)))

; Given an exact integer that corresponds to a Unicode scalar value,
; returns its Unicode general category as a symbol.

(define (get-general-category n)
  (define (data-scalar-value data)
    (let ((sv (vector-ref data 0)))
      (if (pair? sv) (car sv) sv)))
  (let loop ((previous (car unicode-data))
             (rest (cdr unicode-data)))
    (cond ((null? rest)
           (extract-general-category previous))
          ((>= n (data-scalar-value (car rest)))
           (loop (car rest) (cdr rest)))
          (else
           (extract-general-category previous)))))

; Given the Larceny format for a line of parsed data
; taken from UnicodeData.txt,
; returns the simple uppercase mapping as a string.

(define (extract-simple-uppercase-mapping data)
  (if (> (vector-length data) 12)
      (vector-ref data 12)
      ""))

; Given the Larceny format for a line of parsed data
; taken from UnicodeData.txt,
; returns the simple lowercase mapping as a string.

(define (extract-simple-lowercase-mapping data)
  (if (> (vector-length data) 13)
      (vector-ref data 13)
      ""))

; Given the Larceny format for a line of parsed data
; taken from UnicodeData.txt,
; returns the simple titlecase mapping as a string.

(define (extract-simple-titlecase-mapping data)
  (if (> (vector-length data) 14)
      (vector-ref data 14)
      ""))

; Returns a list of lists of the form (code-point category)
; where code-point is an exact integer, category is a string,
; and all code-points from this one to the next share the
; same Unicode general category.

(define (general-category-intervals)
  (define unassigned-category (string->symbol "Cn"))
  (let loop ((n 0)
             (prior-cp -1)                   ; previous code point
             (previous unassigned-category)  ; previous category
             (data unicode-data)
             (intervals '()))
    (cond ((null? data)
           (reverse (cons (list (+ prior-cp 1) unassigned-category)
                          intervals)))
          ((pair? (vector-ref (car data) 0))
           (cond ((and (not (= (+ prior-cp 1) (car (vector-ref (car data) 0))))
                       (not (eq? previous unassigned-category)))
                  ; We have a gap in the code points.
                  (loop (+ n 1)
                        (+ prior-cp 1)
                        unassigned-category
                        data
                        (cons (list (+ prior-cp 1) unassigned-category)
                              intervals)))
                 ((eq? previous (extract-general-category (car data)))
                  (let* ((datum-as-list (vector->list (car data)))
                         (cp1 (cadr (car datum-as-list))))
                    (loop n
                          (- cp1 1)
                          previous
                          (cons (list->vector (cons cp1 (cdr datum-as-list)))
                                (cdr data))
                          intervals)))
                 (else
                  (let* ((datum (car data))
                         (datum-as-list (vector->list datum))
                         (cp1 (car (car datum-as-list)))
                         (cp2 (cadr (car datum-as-list)))
                         (datum1 (list->vector (cons cp1
                                                     (cdr datum-as-list))))
                         (datum2 (list->vector (cons (list (+ cp1 1) cp2)
                                                     (cdr datum-as-list)))))
                    (loop n
                       prior-cp
                       previous
                       (cons datum1 (cons datum2 (cdr data)))
                       intervals)))))
          ((and (not (= (+ prior-cp 1) (vector-ref (car data) 0)))
                (not (eq? previous unassigned-category)))
           ; We have a gap in the code points.
           (loop (+ n 1)
                 (+ prior-cp 1)
                 unassigned-category
                 data
                 (cons (list (+ prior-cp 1) unassigned-category)
                       intervals)))
          ((eq? previous (extract-general-category (car data)))
           (loop n (vector-ref (car data) 0) previous (cdr data) intervals))
          (else
           (let ((code-point (vector-ref (car data) 0)))
             ;(display (number->string code-point 16))
             ;(display " ")
             (let ((category (extract-general-category (car data))))
               ;(write category)
               ;(newline)
               (loop (+ n 1)
                     (vector-ref (car data) 0)
                     category
                     (cdr data)
                     (cons (list code-point category) intervals))))))))

; Given the Larceny format for a line of parsed data
; taken from UnicodeData.txt,
; returns the Decomposition_Type and Decomposition_Mapping
; as a string.

(define (extract-decomposition-type data)
  (if (> (vector-length data) 5)
      (vector-ref data 5)
      ""))

; Given the Larceny format for a line of parsed data
; taken from UnicodeData.txt,
; returns the Canonical_Combining_Class
; as a string.

(define (extract-combining-class data)
  (if (> (vector-length data) 3)
      (vector-ref data 3)
      ""))

; Given the Larceny format for a line of parsed data
; taken from UnicodeData.txt,
; returns the numeric value as a string.

(define (extract-numeric-value data)
  (if (> (vector-length data) 8)
      (vector-ref data 8)
      ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Returns a list of lists of the form (code-point0 code-point1)
; where both code-point0 and code-point1 are strings of hex digits
; representing a Unicode scalar value,
; and code-point1 is f applied to the Larceny format
; for a line of parsed data.

(define (extract-from-unicode f)
  (let* ((x (filter (lambda (data)
                      (> (string-length (f data)) 0))
                    unicode-data)))
    (map (lambda (data)
           (let ((cp (vector-ref data 0)))
             (if (pair? cp)
                 (list (map (lambda (cp) (number->string cp 16))
                            cp)
                       (f data))
                 (list (number->string (vector-ref data 0) 16)
                       (f data)))))
         x)))

; Returns a list of lists of the form (code-point0 code-point1)
; where both code-point0 and code-point1 are strings of hex digits
; representing a Unicode scalar value, and code-point1 is
; the simple uppercase mapping of code-point0,
; the simple lowercase mapping of code-point0,
; the simple titlecase mapping of code-point0,
; the decomposition type, or
; the combining class.

(define (extract-simple-uppercase-mappings)
  (extract-from-unicode extract-simple-uppercase-mapping))

(define (extract-simple-lowercase-mappings)
  (extract-from-unicode extract-simple-lowercase-mapping))

(define (extract-simple-titlecase-mappings)
  (extract-from-unicode extract-simple-titlecase-mapping))

(define (extract-decomposition-types)
  (extract-from-unicode extract-decomposition-type))

(define (extract-combining-classes)
  (filter (lambda (x)
            (not (string=? (cadr x) "0")))
          (extract-from-unicode extract-combining-class)))

(define (extract-canonical-compositions)
  (define excluded
    (map (lambda (x) (vector-ref x 0))
         composition-exclusions))
  (filter (lambda (x)
            (and (not (char=? #\< (string-ref (cadr x) 0)))
                 (> (string-length (cadr x)) 5)
                 (not (memv (string->number (string-append "#x" (car x)))
                            excluded))))
          (extract-decomposition-types)))

(define (extract-numeric-values)
  (extract-from-unicode extract-numeric-value))

; Returns a list whose elements are of the forms
;     code-point0
;     (code-point0 code-point1)
; where both code-point0 and code-point1 are strings of hex digits
; representing a Unicode scalar value that has the named property
; according to the specified database (prop-list or word-break-property).

(define (extract-from-database database propname)
  (let* ((n (string-length propname))
         (x (filter (lambda (data)
                      (let ((s (vector-ref data 1)))
                        (and (<= n (string-length s))
                             (string=? propname (substring s 0 n)))))
                    database)))
    (map (lambda (data)
           (let ((cp (vector-ref data 0)))
             (if (pair? cp)
                 (map (lambda (cp) (number->string cp 16))
                      cp)
                 (number->string (vector-ref data 0) 16))))
         x)))

(define (extract-other-alphabetic)
  (extract-from-database prop-list "Other_Alphabetic"))

(define (extract-white-space)
  (extract-from-database prop-list "White_Space"))

(define (extract-control)
  (extract-from-database grapheme-break-property "Control"))

(define (extract-hebrew)
  (extract-from-database word-break-property "Hebrew_Letter"))

(define (extract-newline)
  (extract-from-database word-break-property "Newline"))

(define (extract-extend)
  (extract-from-database word-break-property "Extend"))

(define (extract-format)
  (extract-from-database word-break-property "Format"))

(define (extract-katakana)
  (extract-from-database word-break-property "Katakana"))

(define (extract-aletter)
  (extract-from-database word-break-property "ALetter"))

(define (extract-midletter)
  (extract-from-database word-break-property "MidLetter"))

(define (extract-midnum)
  (extract-from-database word-break-property "MidNum"))

(define (extract-midnumlet)
  (extract-from-database word-break-property "MidNumLet"))

(define (extract-numeric)
  (extract-from-database word-break-property "Numeric"))

(define (extract-extendnumlet)
  (extract-from-database word-break-property "ExtendNumLet"))

; Returns a list of lists of the form (code-point0 code-point1)
; where both code-point0 and code-point1 are strings of hex digits
; representing a Unicode scalar value, and code-point1 is
; the simple case folding of code-point0.

(define (extract-simple-foldcase-exceptions)
  (define (mapping v)
    (list->string
     (map integer->char
          (parse-code-points (vector-ref v 2)))))
  (let* ((simple-mappings
          (filter (lambda (v)
                    (member (vector-ref v 1) '("C" "S")))
                  case-folding))
         (exceptions
          (filter (lambda (v)
                    (not (string=?
                          (string
                           (char-downcase (integer->char (vector-ref v 0))))
                          (mapping v))))
                  simple-mappings)))
    (map (lambda (v)
           (list (number->string (vector-ref v 0) 16)
                 (vector-ref v 2)))
         exceptions)))

; Returns a list of lists of the form (code-point mapping)
; where code-point is a string of hex digits representing
; a Unicode scalar value, and mapping is the full case
; folding of code-point.

(define (extract-full-foldcase-exceptions)
  (define (mapping v)
    (list->string
     (map integer->char
          (parse-code-points (vector-ref v 2)))))
  (let* ((full-mappings
          (filter (lambda (v)
                    (member (vector-ref v 1) '("C" "F")))
                  case-folding))
         (exceptions
          (filter (lambda (v)
                    (not (string=?
                          (string
                           (char-downcase (integer->char (vector-ref v 0))))
                          (mapping v))))
                  full-mappings)))
    (map (lambda (v)
           (list (number->string (vector-ref v 0) 16)
                 (mapping v)))
         exceptions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Output of tables for unicode.sch and normalization.sch
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a file name, writes the tables to a file of that name.

(define (write-unicode-tables filename)
  (call-with-output-file
   filename
   (lambda (out)
     (define (block-comment)
       (display (make-string 64 #\;) out)
       (newline out)
       (display ";" out)
       (newline out)
       (display "; The following tables were generated from" out)
       (newline out)
       (display "; UnicodeData.txt, CaseFolding.txt," out)
       (newline out)
       (display "; SpecialCasing.txt, PropList.txt," out)
       (newline out)
       (display "; WordBreakProperty.txt, and CompositionExclusions.txt." out)
       (newline out)
       (display "; Use parseUCD.sch to regenerate these tables." out)
       (newline out)
       (display ";" out)
       (newline out)
       (display (make-string 64 #\;) out)
       (newline out)
       (newline out))
     (block-comment)
     (write-category-tables out)
     (write-casing-tables out)
     (block-comment)
     (write-special-casing-tables out)
     (block-comment)
     (write-normalization-tables out))))

(define (write-category-tables out)
  (define c1
    "; The following vector contains the general category for")
  (define c2
    "; characters whose Unicode scalar value is less than ")
  (define s1
    "; The following array of bytes, together with the vector below it,")
  (define s2
    "; implements an indirect mapping from all Unicode scalar values to")
  (define s3
    "; indices into vector-of-general-category-symbols.")
  (define t1
    "; The following vector of exact integers represents the")
  (define t2
    "; Unicode scalar values whose Unicode general category")
  (define t3
    "; is different from the Unicode scalar value immediately")
  (define t4
    "; less than it.")
  (define number-of-common-characters 256)

  (let* ((intervals (general-category-intervals))
         (code-points (map car intervals))
         (categories (map cadr intervals)))

    (display s1 out)
    (newline out)
    (display s2 out)
    (newline out)
    (display s3 out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (length categories) out)
    (display " entries." out)
    (newline out)
    (newline out)
    (display "(define general-category-indices-for-all-characters" out)
    (newline out)
    (display "  (u8-list->bytevector" out)
    (newline out)
    (display "   (map" out)
    (newline out)
    (display "    general-category-symbol->index" out)
    (newline out)
    (display "    '(" out)

    (do ((categories categories (cdr categories))
         (n 0 (+ n 1)))
        ((null? categories))
      (if (zero? (modulo n 16))
          (begin (newline out) (display "      " out)))
      (display (car categories) out)
      (display " " out))
    (display "))))" out)
    (newline out)
    (newline out)

    (display t1 out)
    (newline out)
    (display t2 out)
    (newline out)
    (display t3 out)
    (newline out)
    (display t4 out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (length code-points) out)
    (display " entries." out)
    (newline out)
    (newline out)

    (display "(define vector-of-code-points-with-same-category" out)
    (newline out)
    (display "  '#(" out)
    (do ((code-points code-points
                      (cdr code-points))
         (n 0 (+ n 1)))
        ((null? code-points))
      (if (zero? (modulo n 8))
          (begin (newline out) (display "     " out)))
      (display "#x" out)
      (display (number->string (car code-points) 16) out)
      (display " " out))
    (display "))" out)
    (newline out)
    (newline out)

    (display c1 out)
    (newline out)
    (display c2 out)
    (write number-of-common-characters out)
    (display "." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display number-of-common-characters out)
    (display " entries." out)
    (newline out)
    (newline out)
    (display "(define general-category-indices-for-common-characters" out)
    (newline out)
    (display "  (u8-list->bytevector" out)
    (newline out)
    (display "   (map" out)
    (newline out)
    (display "    general-category-symbol->index" out)
    (newline out)
    (display "    '(" out)

    (do ((n 0 (+ n 1)))
        ((>= n number-of-common-characters))
      (if (zero? (modulo n 16))
          (begin (newline out) (display "       " out)))
      (display (get-general-category n) out)
      (display " " out))
    (display "))))" out)
    (newline out)
    (newline out)

    (display "; The code below is commented out because it is" out)
    (newline out)
    (display "; redundant with the table above.  The code" out)
    (newline out)
    (display "; below remains because it might be useful in" out)
    (newline out)
    (display "; case-insensitive systems that don't read the" out)
    (newline out)
    (display "; table above correctly." out)
    (newline out)
    (newline out)

    (display ";(define general-category-indices-for-common-characters" out)
    (newline out)
    (display ";  (do ((i 0 (+ i 1))" out)
    (newline out)
    (display ";       (bv (make-bytevector " out)
    (display number-of-common-characters out)
    (display ")))" out)
    (newline out)
    (display ";      ((= i " out)
    (display number-of-common-characters out)
    (display ")" out)
    (newline out)
    (display ";       bv)" out)
    (newline out)
    (display ";    (bytevector-set! bv" out)
    (newline out)
    (display ";                     i" out)
    (newline out)
    (display ";                     (general-category-symbol->index" out)
    (newline out)
    (display ";                      (char-general-category" out)
    (newline out)
    (display ";                       (integer->char i))))))" out)
    (newline out)
    (newline out)))

; Just a simple insertion sort on lists.

(define (mysort <= xs)
  (define (insert x xs)
    (if (or (null? xs)
            (<= x (car xs)))
        (cons x xs)
        (cons (car xs)
              (insert x (cdr xs)))))
  (define (sort xs)
    (if (null? xs)
        '()
        (insert (car xs)
                (sort (cdr xs)))))
  (sort xs))

(define (write-casing-tables out)

  (let* ((simple-upcase-mappings (extract-simple-uppercase-mappings))
         (simple-downcase-mappings (extract-simple-lowercase-mappings))
         (adjustments '())
         (adjustments-vector '#())
         (is16bit? (lambda (mapping) (< (string-length (car mapping)) 5)))
         (not16bit? (lambda (mapping) (>= (string-length (car mapping)) 5)))
         (simple-upcase-mappings-16bit
          (filter is16bit? simple-upcase-mappings))
         (simple-downcase-mappings-16bit
          (filter is16bit? simple-downcase-mappings))
         (simple-upcase-mappings-morebits
          (filter not16bit? simple-upcase-mappings))
         (simple-downcase-mappings-morebits
          (filter not16bit? simple-downcase-mappings))
         (special-case-mappings
          (mysort
           (lambda (x y) (<= (vector-ref x 0) (vector-ref y 0)))
           (filter (lambda (x)
                     (or (< (vector-length x) 5)
                         (string-ci=? (vector-ref x 4) "Final_Sigma")
                         (string-ci=? (vector-ref x 4) "Not_Final_Sigma")))
                   (cons

                    ; FIXME: is this a bug?
                    ; This is commented out within SpecialCasing.txt,
                    ; but treating it as a special case simplifies the
                    ; common case for string-downcase.

                    '#(#x03C3 "03C2" "03A3" "03A3" "Final_Sigma")
                    special-casing))))
         (simple-foldcase-exceptions (extract-simple-foldcase-exceptions)))

    ; Given a list of pairs of exact integers that map
    ; code points to code points, adds the difference
    ; between the two code points to the list of adjustments
    ; if it isn't there already.

    (define (compute-distinct-adjustments! mappings)
      (for-each (lambda (mapping)
                  (let ((adjustment (- (string->number
                                        (string-append "#x" (cadr mapping)))
                                       (string->number
                                        (string-append "#x" (car mapping))))))
                    (if (not (memv adjustment adjustments))
                        (set! adjustments
                              (cons adjustment adjustments)))))
                mappings))

    ; Given a 16-bit exact integer code point, returns its low 8 bits.

    (define (lo-bits k) (remainder k 256))

    ; Given a 16-bit exact integer code point, returns its high 8 bits.

    (define (hi-bits k) (quotient k 256))

    ; Display definition of the str procedure, which is used to
    ; initialize the tables for special case mappings.

    (define (display-str)
      (display "  (let ((str (lambda args" out)
      (newline out)
      (display "               (if (= 1 (length args))" out)
      (newline out)
      (display "                   (integer->char (car args))" out)
      (newline out)
      (display "                   (apply string (map integer->char args))))))"
                                   out)
      (newline out))

    ; Compute all adjustments that are used for simple case mappings.

    (compute-distinct-adjustments! simple-downcase-mappings)
    (set! adjustments (map - adjustments))
    (compute-distinct-adjustments! simple-upcase-mappings)

    ; Make sure we can represent an adjustment by a single byte.

    (if (>= (length adjustments) 256)
        (error "Too many simple case adjustments."))

    (set! adjustments (mysort <= adjustments))
    (set! adjustments-vector (list->vector adjustments))

    (display "; This vector contains the numerical adjustments to make" out)
    (newline out)
    (display "; when converting a character from one case to another." out)
    (newline out)
    (display "; For conversions to uppercase or titlecase, add the" out)
    (newline out)
    (display "; adjustment contained in this vector." out)
    (newline out)
    (display "; For conversions to lowercase, subtract the adjustment" out)
    (newline out)
    (display "; contained in this vector." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (length adjustments) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define simple-case-adjustments" out)
    (newline out)
    (display "  '#(" out)
    (do ((adjustments adjustments (cdr adjustments))
         (n 0 (+ n 1)))
        ((null? adjustments))
      (if (zero? (modulo n 8))
          (begin (newline out) (display "     " out)))
      (display "#x" out)
      (display (number->string (car adjustments) 16) out)
      (display " " out))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; This bytevector uses two bytes per code point" out)
    (newline out)
    (display "; to list all 16-bit code points, in increasing order," out)
    (newline out)
    (display "; that have a simple uppercase mapping." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (* 2 (length simple-upcase-mappings-16bit)) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define simple-upcase-chars-16bit" out)
    (newline out)
    (display "  '#vu8(" out)
    (do ((mappings simple-upcase-mappings-16bit
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 4))
          (begin (newline out) (display "        " out)))
      (let* ((cp0 (string->number (string-append "#x" (car (car mappings)))))
             (cp0-hi (number->string (hi-bits cp0) 16))
             (cp0-lo (number->string (lo-bits cp0) 16)))
        (display "#x" out)
        (display cp0-hi out)
        (display " #x" out)
        (display cp0-lo out)
        (display " " out)))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; This vector contains all other code points," out)
    (newline out)
    (display "; in increasing order, that have a simple" out)
    (newline out)
    (display "; uppercase mapping." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (length simple-upcase-mappings-morebits) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define simple-upcase-chars-morebits" out)
    (newline out)
    (display "  '#(" out)
    (do ((mappings simple-upcase-mappings-morebits
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 8))
          (begin (newline out) (display "     " out)))
      (display "#x" out)
      (display (car (car mappings)) out)
      (display " " out))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; This bytevector uses two bytes per code point" out)
    (newline out)
    (display "; to list all 16-bit code points, in increasing order," out)
    (newline out)
    (display "; that have a simple lowercase mapping." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (* 2 (length simple-downcase-mappings-16bit)) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define simple-downcase-chars-16bit" out)
    (newline out)
    (display "  '#vu8(" out)
    (do ((mappings simple-downcase-mappings-16bit
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 4))
          (begin (newline out) (display "        " out)))
      (let* ((cp0 (string->number (string-append "#x" (car (car mappings)))))
             (cp0-hi (number->string (hi-bits cp0) 16))
             (cp0-lo (number->string (lo-bits cp0) 16)))
        (display "#x" out)
        (display cp0-hi out)
        (display " #x" out)
        (display cp0-lo out)
        (display " " out)))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; This vector contains all other code points," out)
    (newline out)
    (display "; in increasing order, that have a simple" out)
    (newline out)
    (display "; lowercase mapping." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (length simple-downcase-mappings-morebits) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define simple-downcase-chars-morebits" out)
    (newline out)
    (display "  '#(" out)
    (do ((mappings simple-downcase-mappings-morebits
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 8))
          (begin (newline out) (display "     " out)))
      (display "#x" out)
      (display (car (car mappings)) out)
      (display " " out))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; The bytes of this bytevector are indexes into" out)
    (newline out)
    (display "; the simple-case-adjustments vector, and correspond" out)
    (newline out)
    (display "; to the code points in simple-upcase-chars-16bit" out)
    (newline out)
    (display "; followed by those in simple-upcase-chars-morebits." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (length simple-upcase-mappings) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define simple-upcase-adjustments" out)
    (newline out)
    (display "  '#vu8(" out)
    (do ((mappings simple-upcase-mappings
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 8))
          (begin (newline out) (display "        " out)))
      (let* ((mapping (car mappings))
             (cp0 (string->number (string-append "#x" (car mapping))))
             (cp1 (string->number (string-append "#x" (cadr mapping))))
             (i (binary-search-of-vector (- cp1 cp0) adjustments-vector)))
        (display "#x" out)
        (display (number->string i 16) out)
        (display " " out)))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; The bytes of this bytevector are indexes into" out)
    (newline out)
    (display "; the simple-case-adjustments vector, and correspond" out)
    (newline out)
    (display "; to the code points in simple-downcase-chars-16bit" out)
    (newline out)
    (display "; followed by those in simple-downcase-chars-morebits." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (length simple-downcase-mappings) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define simple-downcase-adjustments" out)
    (newline out)
    (display "  '#vu8(" out)
    (do ((mappings simple-downcase-mappings
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 8))
          (begin (newline out) (display "        " out)))
      (let* ((mapping (car mappings))
             (cp0 (string->number (string-append "#x" (car mapping))))
             (cp1 (string->number (string-append "#x" (cadr mapping))))
             (i (binary-search-of-vector (- cp0 cp1) adjustments-vector)))
        (display "#x" out)
        (display (number->string i 16) out)
        (display " " out)))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; The scalar values in this vector fold to the" out)
    (newline out)
    (display "; scalar values in the simple-foldcase-mappings" out)
    (newline out)
    (display "; vector under simple case folding.  All other" out)
    (newline out)
    (display "; scalar values fold to their downcased values." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; Each of those tables contains " out)
    (display (length simple-foldcase-exceptions) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define simple-foldcase-exceptions" out)
    (newline out)
    (display "  '#(" out)
    (do ((mappings simple-foldcase-exceptions
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 8))
          (begin (newline out) (display "        " out)))
      (let* ((mapping (car mappings))
             (sv (car mapping)))
        (display "#x" out)
        (display sv out)
        (display " " out)))
    (display "))" out)
    (newline out)
    (newline out)

    (display "(define simple-foldcase-mappings" out)
    (newline out)
    (display "  '#(" out)
    (do ((mappings simple-foldcase-exceptions
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 8))
          (begin (newline out) (display "        " out)))
      (let* ((mapping (car mappings))
             (sv (cadr mapping)))
        (display "#x" out)
        (display sv out)
        (display " " out)))
    (display "))" out)
    (newline out)
    (newline out)))

(define (write-special-casing-tables out)

  (let* ((special-case-mappings
          (mysort
           (lambda (x y) (<= (vector-ref x 0) (vector-ref y 0)))
           (filter (lambda (x)
                     (or (< (vector-length x) 5)
                         (string-ci=? (vector-ref x 4) "Final_Sigma")
                         (string-ci=? (vector-ref x 4) "Not_Final_Sigma")))
                   (cons

                    ; This is commented out within SpecialCasing.txt,
                    ; but treating it as a special case simplifies the
                    ; common case for string-downcase.

                    '#(#x03C3 "03C2" "03A3" "03A3" "Final_Sigma")
                    special-casing))))

         (full-foldcase-exceptions (extract-full-foldcase-exceptions)))

    ; Given a 16-bit exact integer code point, returns its low 8 bits.

    (define (lo-bits k) (remainder k 256))

    ; Given a 16-bit exact integer code point, returns its high 8 bits.

    (define (hi-bits k) (quotient k 256))

    ; Display definition of the str procedure, which is used to
    ; initialize the tables for special case mappings.

    (define (display-str)
      (display "  (let ((str (lambda args" out)
      (newline out)
      (display "               (if (= 1 (length args))" out)
      (newline out)
      (display "                   (integer->char (car args))" out)
      (newline out)
      (display "                   (apply string (map integer->char args))))))"
                                   out)
      (newline out))

    (display "; This bytevector uses two bytes per code point" out)
    (newline out)
    (display "; to list 16-bit code points, in increasing order," out)
    (newline out)
    (display "; that have anything other than a simple case mapping." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; The locale-dependent mappings are not in this table." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (* 2 (length special-case-mappings)) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define special-case-chars" out)
    (newline out)
    (display "  '#vu8(" out)
    (do ((mappings special-case-mappings
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 4))
          (begin (newline out) (display "        " out)))
      (let* ((cp0 (vector-ref (car mappings) 0))
             (cp0-hi (number->string (hi-bits cp0) 16))
             (cp0-lo (number->string (lo-bits cp0) 16)))
        (display "#x" out)
        (display cp0-hi out)
        (display " #x" out)
        (display cp0-lo out)
        (display " " out)))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; Each code point in special-case-chars maps to the" out)
    (newline out)
    (display "; character or string contained in the following tables." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; Each of these tables contains " out)
    (display (length special-case-mappings) out)
    (display " elements, not counting" out)
    (newline out)
    (display "; the strings that are the elements themselves." out)
    (newline out)
    (newline out)

    (display "(define special-lowercase-mapping" out)
    (newline out)
    (display-str)
    (display "    (vector" out)
    (newline out)
    (for-each (lambda (mapping)
                (let ((code-points
                       (parse-code-points (vector-ref mapping 1))))
                  (display "     (str" out)
                  (for-each (lambda (cp)
                              (display " #x" out)
                              (display (number->string cp 16) out))
                            code-points)
                  (display ")" out)
                  (newline out)))
              special-case-mappings)
    (display ")))" out)
    (newline out)
    (newline out)

    (display "(define special-titlecase-mapping" out)
    (newline out)
    (display-str)
    (display "    (vector" out)
    (newline out)
    (for-each (lambda (mapping)
                (let ((code-points
                       (parse-code-points (vector-ref mapping 2))))
                  (display "     (str" out)
                  (for-each (lambda (cp)
                              (display " #x" out)
                              (display (number->string cp 16) out))
                            code-points)
                  (display ")" out)
                  (newline out)))
              special-case-mappings)
    (display ")))" out)
    (newline out)
    (newline out)

    (display "(define special-uppercase-mapping" out)
    (newline out)
    (display-str)
    (display "    (vector" out)
    (newline out)
    (for-each (lambda (mapping)
                (let ((code-points
                       (parse-code-points (vector-ref mapping 3))))
                  (display "     (str" out)
                  (for-each (lambda (cp)
                              (display " #x" out)
                              (display (number->string cp 16) out))
                            code-points)
                  (display ")" out)
                  (newline out)))
              special-case-mappings)
    (display ")))" out)
    (newline out)
    (newline out)

    (display "; Under full case folding, the scalar values" out)
    (newline out)
    (display "; in this vector fold to the characters and strings" out)
    (newline out)
    (display "; in the full-foldcase-mappings vector." out)
    (newline out)
    (display "; All other scalar values fold to their (simple)" out)
    (newline out)
    (display "; downcased values." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; Each of those tables contains " out)
    (display (length full-foldcase-exceptions) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define full-foldcase-exceptions" out)
    (newline out)
    (display "  '#(" out)
    (do ((mappings full-foldcase-exceptions
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 8))
          (begin (newline out) (display "        " out)))
      (let* ((mapping (car mappings))
             (sv (car mapping)))
        (display "#x" out)
        (display sv out)
        (display " " out)))
    (display "))" out)
    (newline out)
    (newline out)

    (display "(define full-foldcase-mappings" out)
    (newline out)
    (display-str)
    (display "    (vector" out)
    (do ((mappings full-foldcase-exceptions
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (newline out)
      (display "        (str" out)
      (let* ((mapping (car mappings))
             (svs (map char->integer
                       (string->list (cadr mapping)))))
        (for-each (lambda (sv)
                    (display " #x" out)
                    (display (number->string sv 16) out))
                  svs))
      (display ")" out))
    (display ")))" out)
    (newline out)
    (newline out)

    (unspecified)))


(define (write-normalization-tables out)

  ; Just a simple insertion sort on lists.

  (define (mysort <= xs)
    (define (insert x xs)
      (if (or (null? xs)
              (<= x (car xs)))
          (cons x xs)
          (cons (car xs)
                (insert x (cdr xs)))))
    (define (sort xs)
      (if (null? xs)
          '()
          (insert (car xs)
                  (sort (cdr xs)))))
    (sort xs))

  (let* ((combining-class-mappings (extract-combining-classes))
         (decomposition-mappings (extract-decomposition-types))
         (decomposition-mappings-16bit
          (filter (lambda (mapping)
                    (< (string-length (car mapping)) 5))
                  decomposition-mappings))
         (decomposition-mappings-morebits
          (filter (lambda (mapping)
                    (>= (string-length (car mapping)) 5))
                  decomposition-mappings))
         (decomposition-mappings-sequences
          (apply append
                 (map parse-code-points-maybe-tagged
                      (map cadr decomposition-mappings))))

         (canonical-compositions (extract-canonical-compositions))
         (hex2num (lambda (s) (string->number (string-append "#x" s))))
         (compositions
          (map (lambda (composition)
                 (list (hex2num (car composition))
                       (parse-code-points (cadr composition))))
               canonical-compositions))
         (mappings (map (lambda (x) (list (cadr x) (car x))) compositions))
         (ignored
          (for-each (lambda (mapping)
                      (define (error msg mapping)
                        (display msg)
                        (write mapping)
                        (newline))
                      (if (or (not (= 2 (length (car mapping))))
                              (not (<= 0 (caar mapping) 65536))
                              (not (<= 0 (cadar mapping) 65536))
                              (not (<= 0 (cadr mapping) 65536)))
                          (error "Unexpectedly complex canonical composition: "
                                 mapping)))
                    mappings))
         (modifiers (mysort >= (map cadar mappings)))
         (modifiers (do ((modifiers modifiers (cdr modifiers))
                         (unique '()
                                 (if (and (not (null? (cdr modifiers)))
                                          (= (car modifiers) (cadr modifiers)))
                                     unique
                                     (cons (car modifiers) unique))))
                        ((null? modifiers)
                         unique)))
         (two-level (map (lambda (modifier)
                           (filter (lambda (mapping)
                                     (= modifier (cadar mapping)))
                                   mappings))
                         modifiers)))


    ; Given a 16-bit exact integer code point, returns its low 8 bits.

    (define (lo-bits k) (remainder k 256))

    ; Given a 16-bit exact integer code point, returns its high 8 bits.

    (define (hi-bits k) (quotient k 256))

    (display "; This vector contains all code points," out)
    (newline out)
    (display "; in increasing order, that have a nonzero" out)
    (newline out)
    (display "; combining class." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (length combining-class-mappings) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define combining-class-is-nonzero" out)
    (newline out)
    (display "  '#(" out)
    (do ((mappings combining-class-mappings
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 8))
          (begin (newline out) (display "     " out)))
      (display "#x" out)
      (display (car (car mappings)) out)
      (display " " out))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; This bytevector contains the combining classes" out)
    (newline out)
    (display "; for the code points in the above vector." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (length combining-class-mappings) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define combining-class-values" out)
    (newline out)
    (display "  '#vu8(" out)
    (do ((mappings combining-class-mappings
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 8))
          (begin (newline out) (display "        " out)))
      (display (cadr (car mappings)) out)
      (display " " out))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; This bytevector uses two bytes per code point" out)
    (newline out)
    (display "; to list 16-bit code points, in increasing order," out)
    (newline out)
    (display "; that have a canonical or compatibility decomposition." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (* 2 (length decomposition-mappings-16bit)) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define decomposition-chars-16bit" out)
    (newline out)
    (display "  '#vu8(" out)
    (do ((mappings decomposition-mappings-16bit
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 4))
          (begin (newline out) (display "        " out)))
      (let* ((cp0 (string->number (string-append "#x" (car (car mappings)))))
             (cp0-hi (number->string (hi-bits cp0) 16))
             (cp0-lo (number->string (lo-bits cp0) 16)))
        (display "#x" out)
        (display cp0-hi out)
        (display " #x" out)
        (display cp0-lo out)
        (display " " out)))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; This vector contains all other code points," out)
    (newline out)
    (display "; in increasing order, that have a canonical" out)
    (newline out)
    (display "; or compatibility decomposition." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (length decomposition-mappings-morebits) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define decomposition-chars-morebits" out)
    (newline out)
    (display "  '#(" out)
    (do ((mappings decomposition-mappings-morebits
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 8))
          (begin (newline out) (display "     " out)))
      (display "#x" out)
      (display (car (car mappings)) out)
      (display " " out))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; This bytevector uses two bytes per index to list" out)
    (newline out)
    (display "; the starting indexes into decomposition-sequences" out)
    (newline out)
    (display "; of the canonical or compatibility decompositions" out)
    (newline out)
    (display "; for the code points in the above two tables." out)
    (newline out)
    (display "; If the index is for a compatibility decomposition," out)
    (newline out)
    (display "; then the high-order bit of the high-order (first)" out)
    (newline out)
    (display "; byte is set." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (* 2 (+ 1 (length decomposition-mappings))) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define decomposition-indexes" out)
    (newline out)
    (display "  '#vu8(" out)
    (do ((mappings decomposition-mappings
                   (cdr mappings))
         (n 0 (+ n 1))
         (i 0 (+ i (length (parse-code-points-maybe-tagged
                            (cadr (car mappings)))))))
        ((null? mappings))
      (if (zero? (modulo n 4))
          (begin (newline out) (display "        " out)))
      (let* ((code-points-as-string (cadr (car mappings)))
             (index (if (char=? #\< (string-ref code-points-as-string 0))
                        (+ i 32768)
                        i))
             (index-hi (number->string (hi-bits index) 16))
             (index-lo (number->string (lo-bits index) 16)))
        (display "#x" out)
        (display index-hi out)
        (display " #x" out)
        (display index-lo out)
        (display " " out)))
    (let* ((index (length decomposition-mappings-sequences))
           (index-hi (number->string (hi-bits index) 16))
           (index-lo (number->string (lo-bits index) 16)))
      (display "#x" out)
      (display index-hi out)
      (display " #x" out)
      (display index-lo out))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; This vector contains sequences of code points" out)
    (newline out)
    (display "; for canonical and compatibility decompositions." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (length decomposition-mappings-sequences) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define decomposition-sequences" out)
    (newline out)
    (display "  '#(" out)
    (do ((mappings decomposition-mappings-sequences
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 8))
          (begin (newline out) (display "     " out)))
      (display "#x" out)
      (display (number->string (car mappings) 16) out)
      (display " " out))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; This vector contains all Unicode scalar values" out)
    (newline out)
    (display "; that can compose with a previous scalar value" out)
    (newline out)
    (display "; under canonical composition." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (length modifiers) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define composition-modifiers" out)
    (newline out)
    (display "  '#(" out)
    (do ((mappings modifiers
                   (cdr mappings))
         (n 0 (+ n 1)))
        ((null? mappings))
      (if (zero? (modulo n 4))
          (begin (newline out) (display "     " out)))
      (let* ((cp (car mappings))
             (hx (number->string cp 16)))
        (display "#x" out)
        (display hx out)
        (display " " out)))
    (display "))" out)
    (newline out)
    (newline out)

    (display "; This vector encodes all canonical compositions." out)
    (newline out)
    (display "; Each element corresponds to the corresponding" out)
    (newline out)
    (display "; element of composition-modifiers, and consists" out)
    (newline out)
    (display "; of a list of two vectors." out)
    (newline out)
    (display "; The first vector contains the scalar values" out)
    (newline out)
    (display "; that, when followed by the corresponding modifier," out)
    (newline out)
    (display "; compose to form the corresponding scalar value" out)
    (newline out)
    (display "; in the second vector." out)
    (newline out)
    (display ";" out)
    (newline out)
    (display "; This table contains " out)
    (display (length modifiers) out)
    (display " elements." out)
    (newline out)
    (display "; The vectors within it contain " out)
    (display (* 2 (length mappings)) out)
    (display " elements." out)
    (newline out)
    (newline out)

    (display "(define canonical-compositions" out)
    (newline out)
    (display "  '#(" out)
    (newline out)
    (for-each (lambda (mappings0)
                (define mappings
                  (mysort (lambda (mapping1 mapping2)
                            (<= (caar mapping1) (caar mapping2)))
                          mappings0))
                (display "     (#(" out)
                (do ((mappings (map caar mappings)
                               (cdr mappings))
                     (n 0 (+ n 1)))
                    ((null? mappings))
                  (if (zero? (modulo n 4))
                      (begin (newline out) (display "        " out)))
                  (let* ((cp (car mappings))
                         (hx (number->string cp 16)))
                    (display "#x" out)
                    (display hx out)
                    (display " " out)))
                (display ")" out)
                (newline out)
                (display "      #(" out)
                (do ((mappings (map cadr mappings)
                               (cdr mappings))
                     (n 0 (+ n 1)))
                    ((null? mappings))
                  (if (zero? (modulo n 4))
                      (begin (newline out) (display "        " out)))
                  (let* ((cp (car mappings))
                         (hx (number->string cp 16)))
                    (display "#x" out)
                    (display hx out)
                    (display " " out)))
                (display "))" out)
                (newline out))
              two-level)
    (display "))" out)
    (newline out)
    (newline out)

    (unspecified)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Utilities.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given an exact integer key and a vector of exact integers
; in strictly increasing order, returns the largest i such
; that element i of the vector is less than or equal to key,
; or -1 if key is less than every element of the vector.

(define (binary-search-of-vector key vec)

  ; Loop invariants:
  ; 0 <= i < j <= (vector-length vec)
  ; vec[i] <= key
  ; if j < (vector-length vec), then key < vec[j]

  (define (loop i j)
    (let ((mid (quotient (+ i j) 2)))
      (cond ((= i mid)
             mid)
            ((<= (vector-ref vec mid) key)
             (loop mid j))
            (else
             (loop i mid)))))

  (let ((hi (vector-length vec)))
    (if (or (= hi 0) (< key (vector-ref vec 0)))
        -1
        (loop 0 hi))))

; Given a string of code points, in hex, separated by spaces,
; returns a list of code points as integers.

(define (parse-code-points s)
  (let ((n (string-length s)))
    ; Returns a list of the code points at index i and following.
    (define (loop i)
      (cond ((= i n) '())
            ((char=? (string-ref s i) #\space)
             (loop (+ i 1)))
            (else
             (let* ((j (next-terminator i))
                    (digits (substring s i j))
                    (cp (string->number (string-append "#x" digits))))
               (cons cp (loop j))))))
    ; Returns the index of the next space, or end of string.
    (define (next-terminator i)
      (cond ((= i n) i)
            ((char=? (string-ref s i) #\space)
             i)
            (else
             (next-terminator (+ i 1)))))
    (loop 0)))

; Given a string of code points, in hex, separated by spaces,
; and possibly preceded by a tag within angle brackets,
; returns a list of code points as integers.

(define (parse-code-points-maybe-tagged s)
  (if (or (zero? (string-length s))
          (not (char=? #\< (string-ref s 0))))
      (parse-code-points s)
      (parse-code-points
       (list->string (cdr (memv #\> (string->list s)))))))

