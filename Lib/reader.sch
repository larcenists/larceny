; Lib/reader.sch
; Larceny -- Scheme reader
;
; $Id: reader.sch,v 1.7 1997/07/07 20:52:12 lth Exp lth $
;
; Original code Copyright Lightship Software.
; Extensive modifications by Lars T. Hansen.
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
       ; These come from "reader0.sch", but a procedures, they have to
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
        (lambda (c p)
          (cond ((char? c)
                 ((vector-ref read-list-vec (char->integer c)) c p))
                ((eof-object? c)
                 (read-unexpected-eof p))
                (else
                 (error "read: Error on input port " p)
		 #t))))
 
       ; Read a list element and then read the rest of the list.
       ; c, the first character of the list element, has been consumed.
 
       (read-list-element
         (lambda (c p)
           (let ((first (read-dispatch c p)))
             (cons first (read-list (tyi p) p)))))
 
       ; The dot has been consumed.
 
       (read-dotted-pair-tail
        (lambda (p)
          (let ((c (tyipeek p)))
            (if (and (char? c) (whitespace? c))
                (let ((tail (read-dispatch (tyi p) p)))
                  (flush-whitespace-until-rparen (tyi p) p)
                  tail)
                (read-list-element #\. p)))))

       (flush-whitespace-until-rparen
        (lambda (c p)
          (if (char? c)
              (cond ((char=? c #\)) '())
                    ((whitespace? c) (flush-whitespace-until-rparen (tyi p) p))
                    ((char=? c #\*)
                     (begin
                      (flush-comment p)
                      (flush-whitespace-until-rparen (tyi p) p)))
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
 
       ; Reads a symbol.  This skips the attempt to parse a symbol as a number.
       ; Collects characters tail-recursively, and then reverses and interns.
       ; c has not been consumed by the input file.
 
;       (read-symbol
;         (lambda (c p l)
;           (if (or (not (char? c)) (separator? c))
;               (string->symbol (string-downcase (list->string (reverse l))))
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
	  (let ((s (make-string 16)))   ; seems like a good length.
	    ; Initial hackery to be compatible with read-symbol consumers.
	    (if (null? l)
		(read-symbol2 c p s 0 (string-length s))
		(begin (string-set! s 0 (char-downcase (car l)))
		       (read-symbol2 c p s 1 (string-length s)))))))

       (read-symbol2
	(lambda (c p s n k)
	  (cond ((not (char? c)) (string->symbol (substring s 0 n)))
		((separator? c)  (string->symbol (substring s 0 n)))
		((< n k)
		 (string-set! s n (char-downcase c))
		 (read-symbol2 (tyinext p) p s (+ n 1) k))
		(else
		 (let ((s (string-append s s)))
		   (string-set! s n (char-downcase c))
		   (read-symbol2 (tyinext p) p s (+ n 1) (+ k k)))))))

       ; Similar to read-symbol, but reads a number or symbol.
       ; If it parses as a number, it's a number.  Otherwise it's a symbol.
       ; c has not been consumed.
       ;
       ; FIXME: conses a lot.

       (read-atom
        (lambda (c p l)
          (if (or (not (char? c)) (separator? c))
              (let* ((r (reverse l))
		     (x (parse-number r)))
                (if x
                    x
                    (let ((x (string->symbol
			      (string-downcase (list->string r)))))
                      (if (not (peculiar-identifier? x))
                          (warn peculiar-id-message x))
                      x)))
	      (read-atom (tyinext p) p (cons c l)))))
       
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
         (lambda (p)

	   (define (cont)
	     (read-list (tyi p) p))

           (let ((c (tyi p)))
            (if (not (char? c))                             ; EOF
		(cont)
		(let ((c (char->integer c)))
		  (cond ((eq? c 13) (cont))                 ; CR
			((eq? c 10) (cont))                 ; LF
			(else (flush-comment-and-read-list p))))))))
 
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
 
       (string-downcase
        (letrec ((loop (lambda (s i)
                         (if (< i 0)
                             s
                             (let ((x (bytevector-like-ref s i)))
                               (cond
                                ((> x (char->integer #\Z)) #f)
                                ((< x (char->integer #\A)) #f)
                                (else (bytevector-like-set! s i (+ x 32))))
                               (loop s (- i 1)))))))
          (lambda (s)
            (loop s (- (string-length s) 1)))))
 
       (char-downcase
         (lambda (c)
           (cond ((char>? c #\Z) c)
                 ((char<? c #\A) c)
                 (else (integer->char (+ 32 (char->integer c)))))))
 
       (char-alphabetic?
        (lambda (c)
          (let ((c (char-downcase c)))
            (cond ((char>? c #\z) #f)
                  ((char<? c #\a) #f)
                  (else #t)))))
       
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
           (display "\; Extra right parenthesis found in input")
           (newline)
           (read-dispatch-whitespace c p)))
 
       (read-dispatch-symbol-starter
         (lambda (c p)
           (read-symbol (tyipeek p) p (cons c '()))))
 
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
        (lambda (x p)
          (let ((c (tyi p)))
            (cond ((eq? x c)
		   (read-list-whitespace x p))
		  ((char? c)
		   ((vector-ref read-list-vec (char->integer c)) c p))
                  ((eof-object? c)
                   (read-unexpected-eof p))
                  (else
                   (read-list c p))))))       ; let read-list handle the error
 
       (read-list-dot
         (lambda (c p)
           (read-dotted-pair-tail p)))
 
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
				     (integer->char 32))
				    ((eq? x 'newline)
				     (integer->char **newline**))
				    ((eq? x 'tab)
				     (integer->char 9))
				    ((eq? x 'return)
				     (integer->char 13))
				    ((eq? x 'linefeed)
				     (integer->char 10))
				    ((eq? x 'page)
				     (integer->char 12))
				    ((eq? x 'backspace)
				     (integer->char 8))
				    ((= (string-length (symbol->string x)) 1)
				     c)
				    (else
				     (error "Malformed #\\ syntax: " x)
				     #t))))
                           (else
			    (begin (tyi p) c)))))
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
                   (list->vector (read-list (tyi p) p)))
                  ;; Control-B is used for bytevectors by compile-file.
		  ;; The syntax is #^B"..."
                  ((char=? c (integer->char 2))
                   (tyi p) ; consume double quote
		   (let ((s (read-string (tyi p) p '())))
		     (typetag-set! s sys$tag.bytevector-typetag)
		     (sys$codevector-iflush s)
		     s))
		  ;; Control-P is used for procedures by compile-file.
		  ;; The syntax is #^P(...)
		  ((char=? c (integer->char 16))
		   (tyi p) ; consume left paren
		   (list->procedure (read-list (tyi p) p)))
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
          ((= c 255))
          (vector-set! read-dispatch-vec c read-dispatch-symbol-starter))
 
      (do ((c (char->integer #\a) (+ 1 c)))
          ((> c (char->integer #\z)))
          (vector-set! read-dispatch-vec c read-dispatch-symbol-starter))
 
      (do ((c (char->integer #\A) (+ 1 c)))
          ((> c (char->integer #\Z)))
          (vector-set! read-dispatch-vec c read-dispatch-symbol-starter))
 
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
                  #\|
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
                     (read-list (tyi p) p)))
 
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
 
      ; Reserved delimiters.
 
      (for-each (lambda (c)
              (vector-set! read-dispatch-vec (char->integer c)
			   read-dispatch-reserved))
            (list #\[
                  #\]
                  #\{
                  #\}))
 
      ;*****************************************************
 
 
      ; Initialization for read-list-vec.
 
      (do ((i 255 (- i 1)))
          ((< i 0) '())
          (vector-set! read-list-vec i read-illegal))
 
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
 
      (do ((c (char->integer #\a) (+ 1 c)))
          ((> c (char->integer #\z)))
          (vector-set! read-list-vec c read-list-element))
 
      (do ((c (char->integer #\A) (+ 1 c)))
          ((> c (char->integer #\Z)))
          (vector-set! read-list-vec c read-list-element))
 
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
                  #\|
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
 
      (vector-set! read-list-vec                      ;sharp sign
                   (char->integer #\#)
                   read-list-element)
 
      (vector-set! read-list-vec                      ;quote
                   (char->integer #\')
                   read-list-element)
 
      (vector-set! read-list-vec                      ;left parenthesis
                   (char->integer #\()
                   read-list-element)
 
      (vector-set! read-list-vec                      ;right parenthesis
                   (char->integer #\))
                   (lambda (c p) '()))
 
      (vector-set! read-list-vec                      ;comma
                   (char->integer #\,)
                   read-list-element)
 
      (vector-set! read-list-vec                      ;dot
                   (char->integer #\.)
                   read-list-dot)
 
      (vector-set! read-list-vec                      ;semicolon
                   (char->integer #\;)
                   (lambda (c p)
                     (flush-comment-and-read-list p)))
 
      (vector-set! read-list-vec                      ;backquote
                   (char->integer #\`)
                   read-list-element)
 
      ; Reserved delimiters.
 
      (for-each (lambda (c)
		  (vector-set! read-list-vec (char->integer c)
			       read-list-reserved))
            (list #\[
                  #\]
                  #\{
                  #\}))
 
      ;*****************************************************
 
 
      ; Assign variables.

      (set! read
	    (lambda p
	      (let ((p (if (not (null? p))
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
