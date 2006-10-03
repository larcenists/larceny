; Useful string processing procedures
; 2002-03-18 / lth

; Find the first place in the string containing the char.
;
; (string-index "abra" #\a) => 0
; (string-index "abra" #\r) => 2
; (string-index "abra" #\c) => #f

(define (string-index s c)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (cond ((= i len) #f)
            ((char=? c (string-ref s i)) i)
            (else (loop (+ i 1)))))))

; Split a string into runs for which the predicate tests true.
;
; (string-split "" char?) => ()
; (string-split "abc" char?) => ("abc")
; (string-split "a12b34cd56e" char-numeric?) => ("12" "34" "56")

(define (string-split s constituent?)
  (let ((limit (string-length s)))
    (let loop ((i 0) (words '()))
      (cond ((>= i limit) 
             (reverse words))
            ((constituent? (string-ref s i))
             (let ((start i))
               (let loop2 ((i (+ i 1)))
                 (if (and (< i limit) (constituent? (string-ref s i)))
                     (loop2 (+ i 1))
                     (loop (+ i 1) (cons (substring s start i) words))))))
            (else
             (loop (+ i 1) words))))))
  
; Search the string for the first occurence of a substring, optionally
; starting the search from some position in the string.  Return the
; position where the substring is found or #f if not found.
;
; O(n*m) time, but not gratuitously dumb.

(define (string-search s subs . rest)
  (let* ((start (if (null? rest) 0 (car rest)))
	 (ls    (string-length s))
	 (lu    (string-length subs))
	 (limit (- ls lu)))
    (let loop ((i start))
      (cond ((> i limit)
	     #f)
	    ((do ((j i (+ j 1))
		  (k 0 (+ k 1)))
		 ((or (= k lu)
		      (not (char=? (string-ref subs k) (string-ref s j))))
		  (= k lu)))
	     i)
	    (else 
	     (loop (+ i 1)))))))

; Old name for same function

(define substring-match string-search)

; Return a fresh string where the first occurence of the string 'match'
; in the string 'x' is replaced by the string 'new'.

(define (substring-match-replace x match new)
  (let ((i (substring-match x match)))
    (if (not i)
        x
        (string-append (substring x 0 i)
                       new
                       (substring x 
                                  (+ i (string-length match)) 
                                  (string-length x))))))

; Thought experiment: string-quasiquote
;   #`"abc,{expr}cde" => (string-append "abc" (->string expr) "cde")
; eg
;   #`"abc,{(+ 1 2)}cde" => "abc3cde"
; This is like a limited but convenient FORMAT, or like string substitution
; in many shell languages.  Eg
;   (let ((x 10))
;     #`"this is x now: ,{x}")
;
; So: a STRING-QUASIQUOTE macro (easy enough) and a reader syntax.

; eof

