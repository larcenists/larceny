;;; FIXME: This should be defined at the R5RS level.

;;; FIXME: The char-numeric? procedure returns true for #\x066b and #\x066c,
;;; but those aren't digits.  From UnicodeData.txt version 7.0.0:
;;;
;;; 066B;ARABIC DECIMAL SEPARATOR;Po;0;AN;;;;;N;;;;;
;;; 066C;ARABIC THOUSANDS SEPARATOR;Po;0;AN;;;;;N;;;;;
;;;
;;; Those lines are the same in version 5.0.0, so this bug has been
;;; present in Larceny for a long time.

(define (digit-value c)
  (if (char-numeric? c)
      (if (char<=? #\0 c #\9)
          (- (char->integer c) (char->integer #\0))
          (let loop ((sv (char->integer c))
                     (zeros *unicode-zero-characters*))
            (cond ((null? zeros)
                   ;; this can't happen
                   0)
                  ((<= (car zeros) sv)
                   (- sv (car zeros)))
                  (else
                   (loop sv (cdr zeros))))))
      #f))

;;; FIXME:  Copied, pasted, and edited from a browser window at
;;; http://www.fileformat.info/info/unicode/category/Nd/list.htm

(define *unicode-zero-characters*
  (reverse
   (map char->integer
        '(#\x0030   ; DIGIT ZERO
          #\x0660   ; ARABIC-INDIC DIGIT ZERO
          #\x06F0   ; EXTENDED ARABIC-INDIC DIGIT ZERO
          #\x07C0   ; NKO DIGIT ZERO
          #\x0966   ; DEVANAGARI DIGIT ZERO
          #\x09E6   ; BENGALI DIGIT ZERO
          #\x0A66   ; GURMUKHI DIGIT ZERO
          #\x0AE6   ; GUJARATI DIGIT ZERO
          #\x0B66   ; ORIYA DIGIT ZERO
          #\x0BE6   ; TAMIL DIGIT ZERO
          #\x0C66   ; TELUGU DIGIT ZERO
          #\x0CE6   ; KANNADA DIGIT ZERO
          #\x0D66   ; MALAYALAM DIGIT ZERO
          #\x0DE6   ; SINHALA LITH DIGIT ZERO
          #\x0E50   ; THAI DIGIT ZERO
          #\x0ED0   ; LAO DIGIT ZERO
          #\x0F20   ; TIBETAN DIGIT ZERO
          #\x1040   ; MYANMAR DIGIT ZERO
          #\x1090   ; MYANMAR SHAN DIGIT ZERO
          #\x17E0   ; KHMER DIGIT ZERO
          #\x1810   ; MONGOLIAN DIGIT ZERO
          #\x1946   ; LIMBU DIGIT ZERO
          #\x19D0   ; NEW TAI LUE DIGIT ZERO
          #\x1A80   ; TAI THAM HORA DIGIT ZERO
          #\x1A90   ; TAI THAM THAM DIGIT ZERO
          #\x1B50   ; BALINESE DIGIT ZERO
          #\x1BB0   ; SUNDANESE DIGIT ZERO
          #\x1C40   ; LEPCHA DIGIT ZERO
          #\x1C50   ; OL CHIKI DIGIT ZERO
          #\xA620   ; VAI DIGIT ZERO
          #\xA8D0   ; SAURASHTRA DIGIT ZERO
          #\xA900   ; KAYAH LI DIGIT ZERO
          #\xA9D0   ; JAVANESE DIGIT ZERO
          #\xA9F0   ; MYANMAR TAI LAING DIGIT ZERO
          #\xAA50   ; CHAM DIGIT ZERO
          #\xABF0   ; MEETEI MAYEK DIGIT ZERO
          #\xFF10   ; FULLWIDTH DIGIT ZERO
          #\x104A0  ; OSMANYA DIGIT ZERO
          #\x11066  ; BRAHMI DIGIT ZERO
          #\x110F0  ; SORA SOMPENG DIGIT ZERO
          #\x11136  ; CHAKMA DIGIT ZERO
          #\x111D0  ; SHARADA DIGIT ZERO
          #\x112F0  ; KHUDAWADI DIGIT ZERO
          #\x114D0  ; TIRHUTA DIGIT ZERO
          #\x11650  ; MODI DIGIT ZERO
          #\x116C0  ; TAKRI DIGIT ZERO
          #\x118E0  ; WARANG CITI DIGIT ZERO
          #\x16A60  ; MRO DIGIT ZERO
          #\x16B50  ; PAHAWH HMONG DIGIT ZERO
          #\x1D7CE  ; MATHEMATICAL BOLD DIGIT ZERO
          #\x1D7D8  ; MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO
          #\x1D7E2  ; MATHEMATICAL SANS-SERIF DIGIT ZERO
          #\x1D7EC  ; MATHEMATICAL SANS-SERIF BOLD DIGIT ZERO
          #\x1D7F6  ; MATHEMATICAL MONOSPACE DIGIT ZERO
          ))))
