; Copyright Lightship Software, Incorporated.
;
; $Id$
;
; Larceny -- Scheme reader.
;
; install-reader takes no arguments
;
; install-reader assigns the following variables:
;
;       read
;       readtable-ref
;       readtable-set!
;
; The read procedure takes one argument, a port, and returns an
; object read from that port. On end of file the reader returns the
; eof-object.
;
; The readtable-ref procedure takes one argument, a character, and returns
; a list of three elements: the car is the fixnum character class of the
; argument, the cadr is the dispatch procedure for the argument, and the
; caddr is the dispatch procedure used when scanning a list.
;
; The readtable-set! procedure takes two arguments, a character and a list
; such as is returned by the readtable-ref procedure, and changes the
; character's character class and dispatch procedures.

($$trace "reader")

;; If #t, symbols beginning with : are self-quoting.
(define recognize-keywords? (make-parameter "recognize-keywords?" #f))

(define recognize-javadot-symbols? (make-parameter "recognize-javadot-symbols?" #f boolean?))
(define case-sensitive? (make-parameter "case-sensitive?" #f boolean?))
(define read-square-bracket-as-paren (make-parameter "read-square-bracket-as-paren" #f boolean?))

(define install-reader
  (lambda ()
    (letrec
      (
       (tyipeek io/peek-char)
       (tyinext io/peek-next-char)  ; == (begin (tyi p) (tyipeek p))
       (tyi     io/read-char)

       ; The dispatch vector first used by the reader.
       ; Entry 0 is for end of file.
       ; Entry n+1 is for the character whose Ascii code is n.
 
       (read-dispatch-vec
         (make-vector 256 (undefined)))
 
       ; The dispatch vector used for reading elements of a list.
       ; Entries are as described above for read-dispatch-vec.
 
       (read-list-vec
         (make-vector 256 (undefined)))
 
       ; The character syntax table used to parse atoms.
       ; Each character's entry is an integer x = a0 + 2*a1 + 4*a2 where
       ;
       ;     a0 is 1 iff the character is a whitespace character.
       ;     a1 is 1 iff the character is a separator (delimiter).
       ;     a2 is 1 iff the character is a digit.
 
       (character-syntax-table (make-bytevector 256))
 
       ; Equivalent procedures for what are macros in the MacScheme version.
       ; These come from "reader0.sch", but as procedures, they have to
       ; be defined inside this letrec.

       (whitespace?
	(lambda (x)
	  (not (zero? (logand 1 (bytevector-ref 
				 character-syntax-table
				 (char->integer x)))))))

       (separator?
	(lambda (x)
	  (not (zero? (logand 2 (bytevector-ref
				 character-syntax-table
				 (char->integer x)))))))

       (digit?
	(lambda (x)
	  (not (zero? (logand 4 (bytevector-ref
				 character-syntax-table
				 (char->integer x)))))))
 
       ;****************************************************************
 
 
       ; The main internal reader routine.
       ; The character c has already been consumed from the input file.
 
       (read-dispatch
        (lambda (c p)
          (cond ((char? c)
                 ((vector-ref read-dispatch-vec (char->integer c))
                  c p))
                ((eof-object? c) (read-eof p))
                (else
                 (error "read: Error on input port " p)
		 #t))))
 
       ; Reads rest of list.
       ; c has already been consumed from the input file.
 
       (read-list
        (lambda (match c p)
          (cond ((char? c)
                 ((vector-ref read-list-vec (char->integer c)) match c p))
                ((eof-object? c)
                 (read-unexpected-eof p))
                (else
                 (error "read: Error on input port " p)
		 #t))))
 
       ; Read a list element and then read the rest of the list.
       ; c, the first character of the list element, has been consumed.
 
       (read-list-element
         (lambda (match c p)
           (let ((first (read-dispatch c p)))
             (cons first (read-list match (tyi p) p)))))

       ; The dot has been consumed.
 
       (read-dotted-pair-tail
        (lambda (match p)
          (let ((c (tyipeek p)))
            (if (and (char? c) (whitespace? c))
                (let ((tail (read-dispatch (tyi p) p)))
                  (flush-whitespace-until-rparen match (tyi p) p)
                  tail)
                (read-list-element match #\. p)))))

       (flush-whitespace-until-rparen
        (lambda (match c p)
          (if (char? c)
              (cond ((char=? c match) '())
                    ((whitespace? c) (flush-whitespace-until-rparen match (tyi p) p))
                    ((char=? c #\*)
                     (begin
                      (flush-comment p)
                      (flush-whitespace-until-rparen match (tyi p) p)))
                    (else (dotted-pair-error p)))
              (dotted-pair-error p))))

       (dotted-pair-error
        (lambda (p)
          (error "Malformed dotted pair on input port " p)
	  #t))
 
       ; Opening double quote has been consumed.
       ; When first called, c is the first character of the string and has been
       ;   consumed.
       ; Collects characters tail recursively, and then reverses.
       ;
       ; FIXME: conses a lot.

       (read-string
         (lambda (c p l)
           (cond ((not (char? c))
		  (read-unexpected-eof p))
                 ((char=? c #\")
		  (list->string (reverse l)))
                 ((char=? c #\\)
		  (let ((c (tyi p)))
		    (cond ((eq? c #\n) 
			   (read-string (tyi p) p (cons #\newline l)))
			  ((eq? c #\t)
			   (read-string (tyi p) p (cons #\tab l)))
			  ((char? c)
			   (read-string (tyi p) p (cons c l)))
			  (else
			   (error "Unexpected end-of-file.")
			   #t))))
                 (else
                  (read-string (tyi p) p (cons c l))))))

       ; Opening vertical bar has been consumed.
       ; When first called, c is the first character of the symbol and has been
       ;   consumed.
       ; Collects characters tail recursively, and then reverses.
       ;
       ; FIXME: conses a lot.

       (read-escaped-symbol
         (lambda (c p l)
           (cond ((not (char? c))
                  (read-unexpected-eof p))
                 ((char=? c #\|)
                  (string->symbol (list->string (reverse l))))
                 ((char=? c #\\)
                  (let ((c (tyi p)))
                    (cond ((eq? c #\n)
                           (read-escaped-symbol (tyi p) p (cons #\newline l)))
                          ((eq? c #\t)
                           (read-escaped-symbol (tyi p) p (cons #\tab l)))
                          ((char? c)
                           (read-escaped-symbol (tyi p) p (cons c l)))
                          (else
                           (error "Unexpected end-of-file.")
                           #t))))
                 (else
                  (read-escaped-symbol (tyi p) p (cons c l))))))

       ; Reads a symbol.  This skips the attempt to parse a symbol as a number.
       ; Collects characters tail-recursively, and then reverses and interns.
       ; c has not been consumed by the input file.
 
;       (read-symbol
;         (lambda (c p l)
;           (if (or (not (char? c)) (separator? c))
;               (string->symbol (string-downcase! (list->string (reverse l))))
;               (begin (tyi p)
;                      (read-symbol (tyipeek p) p (cons c l))))))

       ; The following implementation of read-symbol cuts storage 
       ; allocation on one reader benchmark by 30% but run-time by only 4%.
       ;
       ; Allocation can be cut further by having one buffer string per
       ; invocation of read, and by using a version of string->symbol
       ; that does not copy its argument.

       (read-symbol
        (lambda (c p l)
          (let* ((char-downcase (if (case-sensitive?)
                                    (lambda (x) x)
                                    char-downcase))
                 (read-symbol2 (make-read-symbol2 char-downcase))
                 (s (make-string 16)))  ; seems like a good length.
                                        ; Initial hackery to be compatible with read-symbol consumers.
            (define (loop i tail)
              (if (pair? tail)
                  (begin (string-set! s i (char-downcase (car tail)))
                         (loop (+ i 1) (cdr tail)))
                  (read-symbol2 c p s i (string-length s))))

            (loop 0 l))))

       (make-read-symbol2
        (lambda (char-downcase)
          (letrec
              ((rs2
                (lambda (c p s n k)
                  (cond ((not (char? c)) (string->symbol (substring s 0 n)))
                        ((separator? c)  (string->symbol (substring s 0 n)))
                        ((< n k)
                         (string-set! s n (char-downcase c))
                         (rs2 (tyinext p) p s (+ n 1) k))
                        (else
                         (let ((s (string-append s s)))
                           (string-set! s n (char-downcase c))
                           (rs2 (tyinext p) p s (+ n 1) (+ k k))))))))
            rs2)))

       ; Similar to read-symbol, but reads a number or symbol.
       ; If it parses as a number, it's a number.  Otherwise it's a symbol.
       ; c has not been consumed.
       ;
       ; FIXME: conses a lot.

       (read-atom
        (lambda (c p l)
          (let ((string-downcase! (if (case-sensitive?)
                                      (lambda (x) x)
                                      string-downcase!)))
            (if (or (not (char? c)) (separator? c))
                (let* ((r (reverse l))
                       (x (parse-number r)))
                  (if x
                      x
                      (let ((x (string->symbol
                                (string-downcase! (list->string r)))))
                        (if (not (peculiar-identifier? x))
                            (warn peculiar-id-message x))
                        (if (and (recognize-javadot-symbols?)
                                 (javadot-syntax? x))
                            (symbol->javadot-symbol x)
                            x))))
                (read-atom (tyinext p) p (cons c l))))))

       (parse-prefixed-number
        (lambda (p prefix)
          (parse-number-loop (tyipeek p) p (list prefix #\#))))
       
       ; FIXME: Conses a lot?

       (parse-number-loop
        (lambda (c p l)
          (if (or (not (char? c)) (separator? c))
              (let ((x (parse-number (reverse l))))
                (if (number? x)
                    x
                    (begin (error "Illegal number syntax "
				  (list->string (reverse l)))
			   #t)))
	      (parse-number-loop (tyinext p) p (cons c l)))))
 
       (peculiar-identifier?
        (lambda (x)
          (or (memq x '(+ - -- -1+ ... 1+ 1-))
              (let ((s (symbol->string x)))
                (and (>= (string-length s) 2)
                     (char=? (string-ref s 0) #\-)
                     (char=? (string-ref s 1) #\>))))))
       
       (peculiar-id-message
        (string-append "Illegal or unsupported number syntax"
                       (make-string 1 #\newline)
                       "    (Will be interpreted as a symbol)"))
       
       ; Flushes characters until it reads a carriage return, line feed,
       ; or end of file character.
 
       (flush-comment
         (lambda (p)
           (let ((c (tyi p)))
             (if (char? c)                                ; EOF
		 (let ((c (char->integer c)))
		   (cond ((eq? c 13) #f)                  ; CR
			 ((eq? c 10) #f)                  ; LF
			 (else (flush-comment p))))))))
 
       (flush-comment-and-read
        (lambda (p)

	  (define (cont)
	    (read-dispatch (tyi p) p))
	    
          (let ((c (tyi p)))
	    (if (not (char? c))                             ; EOF
		(cont)
		(let ((c (char->integer c)))
		  (cond ((eq? c 13) (cont))                 ; CR
			((eq? c 10) (cont))                 ; LF
			(else (flush-comment-and-read p))))))))
 
       (flush-comment-and-read-list
         (lambda (match p)

           (define (cont)
             (read-list match (tyi p) p))

           (let ((c (tyi p)))
            (if (not (char? c))                             ; EOF
                (cont)
                (let ((c (char->integer c)))
                  (cond ((eq? c 13) (cont))                 ; CR
                        ((eq? c 10) (cont))                 ; LF
                        (else (flush-comment-and-read-list match p))))))))

       ; Handles end of file.
 
       (read-eof
         (lambda (p)
           (eof-object)))
 
       ; Handles end of file encountered within a list or string etc.
 
       (read-unexpected-eof
         (lambda (p)
           (error "Unexpected end of file encountered during read on port " p)
	   #t))
 
       ; Miscellaneous help functions.
 
; Commented out because it doesn't handle the ISO character set
;       (char-downcase
;         (lambda (c)
;           (cond ((char>? c #\Z) c)
;                 ((char<? c #\A) c)
;                 (else (integer->char (+ 32 (char->integer c)))))))
 
; Commented out because it doesn't handle the ISO character set
;       (char-alphabetic?
;        (lambda (c)
;          (let ((c (char-downcase c)))
;            (cond ((char>? c #\z) #f)
;                  ((char<? c #\a) #f)
;                  (else #t)))))
       
       (warn
        (lambda (msg . args)
          (if #f ; (issue-warnings)
              (begin (newline)
                     (display "WARNING: ")
                     (display msg)
                     (newline)
                     (for-each (lambda (arg) (write arg) (newline))
                               args)))))
 
       ;****************************************************************
 
       ; Routines that appear in the dispatch tables.
 
       (read-illegal
         (lambda (c p)
           (error "Illegal character in input to read"
                  c)
           #t))

       (read-list-illegal
         (lambda (match c p)
           (error "Illegal character in input to read"
                  c)
           #t))

       ; The eq? test here is supposed to make a fast read-blanks look
       ; that does not interfere with the read-table logic.  In v0.27
       ; it slows the reader down, presumably because the compiler does
       ; not optimize the loop very well.  Revisit later. (FIXME)

       (read-dispatch-whitespace
        (lambda (x p)
          (let ((c (tyi p)))
            (cond ((eq? x c)
		   (read-dispatch-whitespace x p))
		  ((char? c)
		   ((vector-ref read-dispatch-vec (char->integer c)) c p))
                  ((eof-object? c)
                   (read-eof p))
                  (else 
                   (read-dispatch c p)))))) ; let read-dispatch handle error
 
       (read-dispatch-extra-paren
         (lambda (c p)
           (newline)
           ;; used to be "\; ...",
           ;; but Petite Chez yields a reader error on it
           (display "; Extra closing delimiter found in input: ")
           (write c)
           (newline)
           (read-dispatch-whitespace c p)))
 
       (read-dispatch-symbol-starter
         (lambda (c p)
           (let ((sym (read-symbol (tyipeek p) p (cons c '()))))
             (if (and (recognize-javadot-symbols?)
                      (javadot-syntax? sym))
                 (symbol->javadot-symbol sym)
                 sym))))

       (read-dispatch-parse
         (lambda (c p)
           (read-atom (tyipeek p) p (cons c '()))))
 
       (read-dispatch-reserved
         (lambda (c p)
           (error "Reserved delimiter found in input"
                  c)
	   #t))
 
       ; See comments preceding read-dispatch-whitespace (above).

       (read-list-whitespace
        (lambda (match x p)
          (let ((c (tyi p)))
            (cond ((eq? x c)
                   (read-list-whitespace match x p))
                  ((char? c)
                   ((vector-ref read-list-vec (char->integer c)) match c p))
                  ((eof-object? c)
                   (read-unexpected-eof p))
                  (else
                   (read-list match c p))))))       ; let read-list handle the error

       (read-list-dot
         (lambda (match c p)
           (read-dotted-pair-tail match p)))

       (read-long-comment
        (lambda (c p)
          (define (read-long-comment-loop depth c-1)
            (let ((c (tyi p)))
              (cond ((eof-object? c)   (read-unexpected-eof p))
                    ((not (char? c))   (read-list #\) c p))
                    ((not (char? c-1)) (read-long-comment-loop depth c))
                    ((and (char=? c-1 #\|)
                          (char=? c #\#))
                     (if (zero? depth)
                         (read-dispatch (tyi p) p)
                         (read-long-comment-loop (- depth 1) c)))
                    ((and (char=? c-1 #\#) (char=? c #\|))
                     (read-long-comment-loop (+ depth 1) #f))
                    (else (read-long-comment-loop depth c)))))
          (read-long-comment-loop 0 #f)))

       (read-sharp
        (lambda (c p)
          (let ((c (let ((c (tyi p)))
                     (if (char? c)
                         (char-downcase c)
                         (read-unexpected-eof p)))))
            (cond ((char=? c #\t) #t)
                  ((char=? c #\f) (not #t))
                  ((char=? c #\\)
                   (let ((c (tyipeek p)))
                     (cond ((not (char? c))
			    (read-unexpected-eof p))
                           ((char-alphabetic? c)
			    ;; This is horrifically expensive.
                            (let ((x (read-symbol c p '())))
                              (cond ((eq? x 'space)
				     (integer->char **space**))
				    ((eq? x 'newline)
				     (integer->char **newline**))
				    ((eq? x 'tab)
				     (integer->char **tab**))
				    ((eq? x 'return)
				     (integer->char **carriage-return**))
				    ((eq? x 'linefeed)
				     (integer->char **linefeed**))
				    ((eq? x 'page)
				     (integer->char **form-feed**))
				    ((eq? x 'backspace)
				     (integer->char **backspace**))
				    ((= (string-length (symbol->string x)) 1)
				     c)
                                    ;; MzScheme convention:  #\uXX is unicode char where XX is hex code point.
                                    ((and (>= (string-length (symbol->string x)) 1)
                                          (char=? (string-ref (symbol->string x) 0) #\u))
                                     (let ((string (symbol->string x)))
                                       (integer->char (string->number (substring string 1 (string-length string)) 16))))
				    (else
				     (error "Malformed #\\ syntax: " x)
				     #t))))
                           (else
                            (begin (tyi p) c)))))
                  ;; MzScheme randomness
                  ((char=? c #\%)
                   (read-symbol (tyipeek p) p (cons #\# (cons c '()))))
                  ((char=? c #\')
                   (list 'syntax (read-dispatch (tyi p) p)))
                  ((char=? c #\")
                   (list 'ansi-string (read-string (tyi p) p '())))
                  ((char=? c #\`)
                   (list 'quasisyntax (read-dispatch (tyi p) p)))
                  ((char=? c #\,)
                   (list 'unsyntax (read-dispatch (tyi p) p)))
                  ((char=? c #\|)
                   (read-long-comment c p))
                  ((char=? c #\r)
                   (let ((c (tyipeek p)))
                     (cond ((not (char? c))
                            (read-unexpected-eof p))
                           ((char=? c #\x)
                            (tyi p);; discard the x
                            (let ((c (tyipeek p)))
                              (cond ((not (char? c))
                                     (read-unexpected-eof p))
                                    ((char=? c #\")
                                     (tyi p) ;; discard the "
                                     (list 'string->regexp (read-string (tyi p) p '())))
                                    (else (error "Malformed #rx syntax: " c)))))
                           (else
                            (error "Malformed #r syntax: " c)))))

                  ((char=? c #\!)
                   (let ((x (read-symbol (tyipeek p) p '())))
                     (case x
                       ((null) '())
                       ((false) #f)
                       ((true)  #t)
                       ((unspecified) (unspecified))
		       ((undefined) (undefined))
                       ; ((fasl) **fasl**)
                       (else  
			(error "Malformed #! syntax" x)
			#t))))
                  ((char=? c #\()
                   (list->vector (read-list #\) (tyi p) p)))
                  ;; Control-B is used for bytevectors by compile-file.
		  ;; The syntax is #^B"..."
                  ((char=? c (integer->char 2))
                   (tyi p) ; consume double quote
                   (let ((s (read-string (tyi p) p '())))
                     (typetag-set! s sys$tag.bytevector-typetag)
                     (sys$codevector-iflush s)
                     s))
                  ;; Control-C is used for compnum constants by compile-file.
                  ;; The syntax is #^Cxxxxxxxxxxxxxxxx where each x is a byte
                  ;; value. The native byte ordering is used.
                  ((char=? c (integer->char 3))
                   (let ((f (make-bytevector 20)))
                     (do ((i 4 (+ i 1)))
                         ((= i 20)
                          (typetag-set! f sys$tag.compnum-typetag)
                          f)
                       (bytevector-set! f i (char->integer (tyi p))))))
                  ;; Control-F is used for flonum constants by compile-file.
                  ;; The syntax is #^Fxxxxxxxx where each x is a byte value.
                  ;; The native byte ordering is used.
                  ((char=? c (integer->char 6))
                   (let ((f (make-bytevector 12)))
                     (do ((i 4 (+ i 1)))
                         ((= i 12)
                          (typetag-set! f sys$tag.flonum-typetag)
                          f)
                       (bytevector-set! f i (char->integer (tyi p))))))
                  ;; Control-P is used for procedures by compile-file.
                  ;; The syntax is #^P(...)
                  ((char=? c (integer->char 16))
                   (tyi p) ; consume left paren
                   (list->procedure (read-list #\) (tyi p) p)))
                  ;; Control-G is used for global references by compile-file.
                  ;; The syntax is #^Gsymbol; it evaluates to a value which
                  ;; is a global cell.
                  ((char=? c (integer->char 7))
                   ((global-name-resolver) (read-symbol (tyipeek p) p '())))
                  ((char=? c #\e)
                   (parse-prefixed-number p #\e))
                  ((char=? c #\i)
                   (parse-prefixed-number p #\i))
                  ((char=? c #\x)
                   (parse-prefixed-number p #\x))
                  ((char=? c #\d)
                   (parse-prefixed-number p #\d))
                  ((char=? c #\o)
                   (parse-prefixed-number p #\o))
                  ((char=? c #\b)
                   (parse-prefixed-number p #\b))
                  (else 
		   (error "Malformed # syntax" c)
		   #t)))))
 
       (read-list-reserved read-dispatch-reserved)
 
      ; End of letrec bindings.
 
      )
 
      ;****************************************************************
 
 
      ; Initialization for character-syntax-table.
 
      (do ((i 255 (- i 1)))
          ((< i 0) '())
	(bytevector-set! character-syntax-table i 0))

      (for-each (lambda (c)
		  (bytevector-set! 
		   character-syntax-table
		   c
		   (logior 3 (bytevector-ref character-syntax-table c))))
       '(32      ;space
	 13      ;carriage return
	 10      ;line feed
	 9      ;tab
	 12      ;form feed
	 ))


      ; Illegal characters are separators so they will be caught when the next
      ; item is read.  End of file is also a separator.
 
      (do ((c 0 (+ 1 c)))
          ((> c 31) '())
	(bytevector-set!
	 character-syntax-table
	 c
	 (logior 2 (bytevector-ref character-syntax-table c))))

      (for-each (lambda (c)
		  (bytevector-set!
		   character-syntax-table
		   (char->integer c)
		   (logior 2 (bytevector-ref character-syntax-table 
					     (char->integer c)))))
		(list #\( #\) #\[ #\] #\{ #\} #\; #\\))

      (for-each (lambda (c)
              (bytevector-set!
                character-syntax-table
                (char->integer c)
                (logior 4 (bytevector-ref character-syntax-table
					  (char->integer c)))))
            (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

      ;****************************************************************
 
 
      ; Initialization for read-dispatch-vec.

      (do ((i 255 (- i 1)))
          ((< i 0) '())
          (vector-set! read-dispatch-vec i read-illegal))
 
      ; Whitespace handlers.
 
      (for-each (lambda (c)
		  (vector-set! read-dispatch-vec c read-dispatch-whitespace))
		'(32      ;space
		  13      ;carriage return
		  10      ;line feed
		  9      ;tab
		  12      ;form feed
		  ))
 
      ; Symbol starters.
      
      (do ((c 128 (+ 1 c)))
          ((= c 256))
	(vector-set! read-dispatch-vec c read-dispatch-symbol-starter))
 
      (do ((i 0 (+ i 1)))
	  ((= i 256))
	(if (char-alphabetic? (integer->char i))
	    (vector-set! read-dispatch-vec i read-dispatch-symbol-starter)))
 
      (for-each (lambda (c)
              (vector-set! read-dispatch-vec
                           (char->integer c)
                           read-dispatch-symbol-starter))
            (list #\!
                  #\$
                  #\%
                  #\&
                  #\*
                  #\/
                  #\:
                  #\<
                  #\=
                  #\>
                  #\?
                  #\@
                  #\\
                  #\^
                  #\_
                  ;; #\|
                  #\~))

      ; Possible symbol starters that require further parsing.
 
      (for-each (lambda (c)
              (vector-set! read-dispatch-vec (char->integer c)
			   read-dispatch-parse))
            (list #\+
                  #\-
                  #\.
                  #\0
                  #\1
                  #\2
                  #\3
                  #\4
                  #\5
                  #\6
                  #\7
                  #\8
                  #\9))
 
      ; Special characters.
 
      (vector-set! read-dispatch-vec                  ;double quote
                   (char->integer #\")
                   (lambda (c p)
                     (read-string (tyi p) p '())))

      (vector-set! read-dispatch-vec                  ;vertical bar
                   (char->integer #\|)
                   (lambda (c p)
                     (read-escaped-symbol (tyi p) p '())))

      (vector-set! read-dispatch-vec                  ;sharp sign
                   (char->integer #\#)
                   read-sharp)
 
      (vector-set! read-dispatch-vec                  ;quote
                   (char->integer #\')
                   (lambda (c p)
                     (list 'quote (read-dispatch (tyi p) p))))

      (vector-set! read-dispatch-vec                  ;left parenthesis
                   (char->integer #\()
                   (lambda (c p)
                     (read-list #\) (tyi p) p)))

      (vector-set! read-dispatch-vec                  ;right parenthesis
                   (char->integer #\))
                   read-dispatch-extra-paren)
 
      (vector-set! read-dispatch-vec                  ;comma
                   (char->integer #\,)
                   (lambda (c p)
                     (let ((c (tyi p)))
                       (if (and (char? c) (char=? c #\@))
                           (list 'unquote-splicing (read-dispatch (tyi p) p))
                           (list 'unquote (read-dispatch c p))))))
 
      (vector-set! read-dispatch-vec                  ;semicolon
                   (char->integer #\;)
                   (lambda (c p)
                     (flush-comment-and-read p)))

      (vector-set! read-dispatch-vec                  ;backquote
                   (char->integer #\`)
                   (lambda (c p)
                     (list 'quasiquote (read-dispatch (tyi p) p))))

      ; For MzScheme
      (vector-set! read-dispatch-vec
                   (char->integer #\[)
                   (lambda (c p)
                     (if (read-square-bracket-as-paren)
                         (read-list #\] (tyi p) p)
                         (read-dispatch-reserved c p))))

      (vector-set! read-dispatch-vec
                   (char->integer #\])
                   (lambda (c p)
                     (if (read-square-bracket-as-paren)
                         (read-dispatch-extra-paren c p)
                         (read-dispatch-reserved c p))))

      ; Reserved delimiters.
 
      (for-each (lambda (c)
              (vector-set! read-dispatch-vec (char->integer c)
                           read-dispatch-reserved))
            (list #\{
                  #\}))
 
      ;*****************************************************
 
 
      ; Initialization for read-list-vec.
 
      (do ((i 255 (- i 1)))
          ((< i 0) '())
        (vector-set! read-list-vec i read-list-illegal))

      ; Whitespace handlers.
 
      (for-each (lambda (c)
		  (vector-set! read-list-vec c read-list-whitespace))
            '(32      ;space
              13      ;carriage return
              10      ;line feed
               9      ;tab
              12      ;form feed
             ))
 
      ; Symbol starters.
      
      (do ((c 128 (+ 1 c)))
          ((= c 255))
	(vector-set! read-list-vec c read-list-element))
 
      (do ((i 0 (+ i 1)))
	  ((= i 256))
	(if (char-alphabetic? (integer->char i))
	    (vector-set! read-list-vec i read-list-element)))

      (for-each (lambda (c)
		  (vector-set! read-list-vec (char->integer c)
			       read-list-element))
            (list #\!
                  #\$
                  #\%
                  #\&
                  #\*
                  #\/
                  #\:
                  #\<
                  #\=
                  #\>
                  #\?
                  #\@
                  #\\
                  #\^
                  #\_
                  ;; #\|
                  #\~))
 
      ; Possible symbol starters that require further parsing.
 
      (for-each (lambda (c)
		  (vector-set! read-list-vec (char->integer c)
			       read-list-element))
            (list #\+
                  #\-
                  #\0
                  #\1
                  #\2
                  #\3
                  #\4
                  #\5
                  #\6
                  #\7
                  #\8
                  #\9))
 
      ; Special characters.
 
      (vector-set! read-list-vec                      ;double quote
                   (char->integer #\")
                   read-list-element)

      (vector-set! read-list-vec                      ;vertical bar
                   (char->integer #\|)
                   read-list-element)

      (vector-set! read-list-vec                      ;sharp sign
                   (char->integer #\#)
                   read-list-element)
 
      (vector-set! read-list-vec                      ;quote
                   (char->integer #\')
                   read-list-element)
 
      (vector-set! read-list-vec                      ;left parenthesis
                   (char->integer #\()
                   read-list-element)

      (vector-set! read-list-vec        ;left square bracket
                   (char->integer #\[)
                   (lambda (match c p)
                     (if (read-square-bracket-as-paren)
                         (read-list-element match c p)
                         (read-list-reserved c p))))

      (vector-set! read-list-vec                      ;right parenthesis
                   (char->integer #\))
                   (lambda (match c p)
                     (if (char=? match c)
                         '()
                         (error "Wrong closing delimiter." c))))

      (vector-set! read-list-vec                      ;right bracket
                   (char->integer #\])
                   (lambda (match c p)
                     (if (char=? match c)
                         '()
                         (error "Wrong closing delimiter." c))))

      (vector-set! read-list-vec                      ;comma
                   (char->integer #\,)
                   read-list-element)
 
      (vector-set! read-list-vec                      ;dot
                   (char->integer #\.)
                   read-list-dot)
 
      (vector-set! read-list-vec                      ;semicolon
                   (char->integer #\;)
                   (lambda (match c p)
                     (flush-comment-and-read-list match p)))

      (vector-set! read-list-vec                      ;backquote
                   (char->integer #\`)
                   read-list-element)
 
      ; Reserved delimiters.
 
      (for-each (lambda (c)
                  (vector-set! read-list-vec (char->integer c)
                               read-list-reserved))
            (list

                  #\{
                  #\}))
 
      ;*****************************************************
 
 
      ; Assign variables.

      (set! read
            (lambda p
              (let ((p (if (pair? p)
                           (car p)
                           (current-input-port))))
                (read-dispatch (tyi p) p))))

      (set! readtable-ref
            (lambda (char)
              (let ((char (char->integer char)))
                (list (bytevector-ref character-syntax-table char)
                      (vector-ref read-dispatch-vec char)
                      (vector-ref read-list-vec char)))))
 
      (set! readtable-set!
            (lambda (char l)
              (let ((char (char->integer char)))
                (bytevector-set! character-syntax-table char (car l))
                (vector-set! read-dispatch-vec char (cadr l))
                (vector-set! read-list-vec char (caddr l))
                #t)))
 
      #t
 
    ; End of body of letrec, lambda, define.

    )))

; eof
