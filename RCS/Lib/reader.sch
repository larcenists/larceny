; Copyright Lightship Software.
;
; $Id$
;
; Scheme reader.                        17 April 1990
; Modified for the new system by lth.   16 January 1992
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
; value of the global variable **eof**.
;
; The readtable-ref procedure takes one argument, a character, and returns
; a list of three elements: the car is the fixnum character class of the
; argument, the cadr is the dispatch procedure for the argument, and the
; caddr is the dispatch procedure used when scanning a list.
;
; The readtable-set! procedure takes two arguments, a character and a list
; such as is returned by the readtable-ref procedure, and changes the
; character's character class and dispatch procedures.

(begin (display "$Id$")
       (newline))

; Some rudimentary compatibility stuff (aka "patches")

(define tyipeek peek-char)
(define tyi read-char)
(define (ascii s) (string-ref s 0))

(define install-reader
  (lambda ()
;    (optimize space)
;    (define parsenumber parse-number)
    (letrec
      (
 
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
				 (logand (char->integer x) 255)))))))

       (separator?
	(lambda (x)
	  (not (zero? (logand 2 (bytevector-ref
				 character-syntax-table
				 (logand (char->integer x) 255)))))))
 
       (digit?
	(lambda (x)
	  (not (zero? (logand 4 (bytevector-ref
				 character-syntax-table
				 (logand (char->integer x) 255)))))))
 
       (double-quote?
	(lambda (x)
	  (char=? x #\")))
 
       (backslash?
	(lambda (x)
	  (char=? x #\\)))
 
       ;****************************************************************
 
 
       ; The main internal reader routine.
       ; The character c has already been consumed from the input file.
 
       (read-dispatch
        (lambda (c p)
;          (optimize speed)
          (cond ((char? c)
                 ((vector-ref read-dispatch-vec (logand (char->integer c) 255))
                  c p))
                ((or (eq? c -1) (eof-object? c)) (read-eof p))
                (else
                 (error "Error on input" p)))))
 
       ; Reads rest of list.
       ; c has already been consumed from the input file.
 
       (read-list
        (lambda (c p)
;          (optimize speed)
          (cond ((char? c)
                 ((vector-ref read-list-vec (logand (char->integer c) 255)) c p))
                ((or (eq? c -1) (eof-object? c))
                 (read-unexpected-eof p))
                (else
                 (error "Error on input" p)))))
 
       ; Read a list element and then read the rest of the list.
       ; c, the first character of the list element, has been consumed.
 
       (read-list-element
         (lambda (c p)
;           (optimize speed)
           (let ((first (read-dispatch c p)))
             (cons first (read-list (tyi p) p)))))
 
       ; The dot has been consumed.
 
       (read-dotted-pair-tail
        (lambda (p)
;          (optimize speed)
          (let ((c (tyipeek p)))
            (if (and (char? c) (whitespace? c))
                (let ((tail (read-dispatch (tyi p) p)))
                  (flush-whitespace-until-rparen (tyi p) p)
                  tail)
                (read-list-element (ascii ".") p)))))
 
       (flush-whitespace-until-rparen
        (lambda (c p)
          (if (char? c)
              (cond ((char=? c (ascii ")")) '())
                    ((whitespace? c) (flush-whitespace-until-rparen (tyi p) p))
                    ((char=? c (ascii "*"))
                     (begin
                      (flush-comment p)
                      (flush-whitespace-until-rparen (tyi p) p)))
                    (else (dotted-pair-error p)))
              (dotted-pair-error p))))
       
       (dotted-pair-error
        (lambda (p)
          (error "Malformed dotted pair in input" p)))
 
       ; Opening double quote has been consumed.
       ; When first called, c is the first character of the string and has been
       ;   consumed.
       ; Collects characters tail recursively, and then reverses.
 
       (read-string
         (lambda (c p l)
;           (optimize speed)
           (cond ((not (char? c))   (read-unexpected-eof p))
                 ((double-quote? c) (list->string (reverse l)))
                 ((backslash? c)    (let ((c (tyi p)))
                                      (read-string (tyi p) p (cons c l))))
                 (else              (read-string (tyi p) p (cons c l))))))
 
       ; Reads a symbol.  This skips the attempt to parse a symbol as a number.
       ; Collects characters tail-recursively, and then reverses and interns.
       ; c has not been consumed by the input file.
 
       (read-symbol
         (lambda (c p l)
;           (optimize speed)
           (if (or (not (char? c)) (separator? c))
               (string->symbol (string-downcase (list->string (reverse l))))
               (begin (tyi p)
                      (read-symbol (tyipeek p) p (cons c l))))))
 
       ; Similar to read-symbol, but reads a number or symbol.
       ; If it parses as a number, it's a number.  Otherwise it's a symbol.
       ; c has not been consumed.

       (read-atom
        (lambda (c p l)
;          (optimize speed)
          (if (or (not (char? c)) (separator? c))
              (let ((x (begin (display "at 2 ") (write l) (write (reverse l)) (newline)
			      (parse-number (reverse l)))))
                (if x
                    x
                    (let ((x (string->symbol
                              (string-downcase
                               (list->string (reverse l))))))
                      (if (not (peculiar-identifier? x))
                          (warn peculiar-id-message x))
                      x)))
              (begin (tyi p)
                     (read-atom (tyipeek p) p (cons c l))))))
       
       (parse-prefixed-number
        (lambda (p prefix)
;          (optimize space)
          (parse-number-loop (tyipeek p) p (list prefix (ascii "#")))))
       
       (parse-number-loop
        (lambda (c p l)
;          (optimize space)
          (if (or (not (char? c)) (separator? c))
              (let ((x (begin (display "at 1 ") (write l) (newline)
			      (parse-number (reverse l)))))
                (if (number? x)
                    x
                    (error "Illegal number syntax" (list->string (reverse l)))))
              (begin (tyi p)
                     (parse-number-loop (tyipeek p) p (cons c l))))))
 
;       (parse-number parsenumber)
       
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
       
       ; Flushes characters until it reads a carriage return, line feed, or end
       ; of file character.
 
       (flush-comment
         (lambda (p)
;           (optimize speed)
           (let ((c (tyi p)))
             (if (or (not (char? c))
                     (memq (char->integer c) '(13 10)))     ; CR, LF, EOF
                 '()
                 (flush-comment p)))))
 
       (flush-comment-and-read
        (lambda (p)
;          (optimize speed)
          (let ((c (tyi p)))
            (if (or (not (char? c))
                    (memq (char->integer c) '(13 10)))      ; CR, LF, EOF
                (read-dispatch (tyi p) p)
                (flush-comment-and-read p)))))
 
       (flush-comment-and-read-list
         (lambda (p)
;           (optimize speed)
           (let ((c (tyi p)))
            (if (or (not (char? c))
                    (memq (char->integer c) '(13 10)))      ; CR, LF, EOF
                (read-list (tyi p) p)
                (flush-comment-and-read-list p)))))
 
       ; Handles end of file.
 
       (read-eof
         (lambda (p)
           **eof**))
 
       ; Handles end of file encountered within a list or string etc.
 
       (read-unexpected-eof
         (lambda (p)
           (error "Unexpected end of file encountered during read")))
 
       ; Miscellaneous help functions.
 
       (string-downcase
        (letrec ((loop (lambda (s i)
;                         (optimize speed)
                         (if (< i 0)
                             s
                             (let ((x (bytevector-like-ref s i)))
                               (cond
                                ((> x (char->integer (ascii "Z"))) #f)
                                ((< x (char->integer (ascii "A"))) #f)
                                (else (bytevector-like-set! s i (+ x 32))))
                               (loop s (- i 1)))))))
          (lambda (s)
;            (optimize speed)
            (loop s (- (string-length s) 1)))))
 
       (char-downcase
         (lambda (c)
;           (optimize speed)
           (cond ((char>? c (ascii "Z")) c)
                 ((char<? c (ascii "A")) c)
                 (else (integer->char (+ 32 (char->integer c)))))))
 
       (char-alphabetic?
        (lambda (c)
;          (optimize speed)
          (let ((c (char-downcase c)))
            (cond ((char>? c (ascii "z")) #f)
                  ((char<? c (ascii "a")) #f)
                  (else #t)))))
       
       (warn
        (lambda (msg . args)
;          (optimize space)
          (if (issue-warnings)
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
                  c)))
 
       (read-dispatch-whitespace
        (lambda (c p)
;          (optimize speed)
          (let ((c (tyi p)))
            (cond ((char? c)
                   ((vector-ref read-dispatch-vec (logand (char->integer c) 255)) c p))
                  ((or (eq? c -1) (eof-object? c))
                   (read-eof p))
                  (else 
                   (read-dispatch c p))))))   ; let read-dispatch handle the error
 
       (read-dispatch-extra-paren
         (lambda (c p)
           (newline)
           (display "\; Extra right parenthesis found in input")
           (newline)
           (read-dispatch-whitespace c p)))
 
       (read-dispatch-symbol-starter
         (lambda (c p)
;           (optimize speed)
           (read-symbol (tyipeek p) p (cons c '()))))
 
       (read-dispatch-parse
         (lambda (c p)
;           (optimize speed)
           (read-atom (tyipeek p) p (cons c '()))))
 
       (read-dispatch-reserved
         (lambda (c p)
           (error "Reserved delimiter found in input"
                  c)))
 
       (read-list-whitespace
        (lambda (c p)
;          (optimize speed)
          (let ((c (tyi p)))
            (cond ((char? c)
                   ((vector-ref read-list-vec (logand (char->integer c) 255)) c p))
                  ((or (eq? c -1) (eof-object? c))
                   (read-unexpected-eof p))
                  (else
                   (read-list c p))))))          ; let read-list handle the error
 
       (read-list-dot
         (lambda (c p)
;           (optimize speed)
           (read-dotted-pair-tail p)))
 
       (read-sharp
        (lambda (c p)
;          (optimize speed)
          (let ((c (let ((c (tyi p)))
                     (if (char? c)
                         (char-downcase c)
                         (read-unexpected-eof p)))))
            (cond ((char=? c (ascii "t")) #t)
                  ((char=? c (ascii "f")) (not #t))
                  ((char=? c (ascii "\\ "))
                   (let ((c (tyipeek p)))
                     (cond ((not (char? c)) (read-unexpected-eof p))
                           ((char-alphabetic? c)
                            (let ((x (read-symbol c p '())))
                              (case x
                                ((space) (ascii " "))
                                ((newline) (integer->char **newline**))
                                ((tab) (integer->char 9))
                                ((return) (integer->char 13))
                                ((linefeed) (integer->char 10))
                                ((page) (integer->char 12))
                                ((backspace) (integer->char 8))
                                (else (if (= (string-length (symbol->string x)) 1)
                                          c
                                          (error "Malformed #\\ syntax" x))))))
                           (else (begin (tyi p) c)))))
                  ((char=? c (ascii "!"))
                   (let ((x (read-symbol (tyipeek p) p '())))
                     (case x
                       ((null) '())
                       ((false) #f)
                       ((true)  #t)
                       ((unspecified) **unspecified**)
                       ((fasl) **fasl**)
                       (else  (error "Malformed #! syntax" x)))))
                  ((char=? c (ascii "("))
                   (list->vector (read-list (tyi p) p)))
                  ; control-B is used for bytevectors by compile-file
                  ((char=? c (integer->char 2))
                   (tyi p) ; consume double quote
                   (typetag-set! (read-string (tyi p) p '()) 
				 sys$tag.bytevector-typetag))
                  ((char=? c (ascii "e"))
                   (parse-prefixed-number p (ascii "e")))
                  ((char=? c (ascii "i"))
                   (parse-prefixed-number p (ascii "i")))
                  ((char=? c (ascii "x"))
                   (parse-prefixed-number p (ascii "x")))
                  ((char=? c (ascii "d"))
                   (parse-prefixed-number p (ascii "d")))
                  ((char=? c (ascii "o"))
                   (parse-prefixed-number p (ascii "o")))
                  ((char=? c (ascii "b"))
                   (parse-prefixed-number p (ascii "b")))
                  (else (error "Malformed # syntax"
                               c))))))
 
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
            (list (ascii "(")
                  (ascii ")")
                  (ascii "[")
                  (ascii "]")
                  (ascii "{")
                  (ascii "}")
                  (ascii ";")
                  (ascii "\"")
	     ))

      (for-each (lambda (c)
              (bytevector-set!
                character-syntax-table
                (char->integer c)
                (logior 4 (bytevector-ref character-syntax-table (char->integer c)))))
            (list (ascii "0")
                  (ascii "1")
                  (ascii "2")
                  (ascii "3")
                  (ascii "4")
                  (ascii "5")
                  (ascii "6")
                  (ascii "7")
                  (ascii "8")
                  (ascii "9")))

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
 
      (do ((c (char->integer (ascii "a")) (+ 1 c)))
          ((> c (char->integer (ascii "z"))))
          (vector-set! read-dispatch-vec c read-dispatch-symbol-starter))
 
      (do ((c (char->integer (ascii "A")) (+ 1 c)))
          ((> c (char->integer (ascii "Z"))))
          (vector-set! read-dispatch-vec c read-dispatch-symbol-starter))
 
      (for-each (lambda (c)
              (vector-set! read-dispatch-vec
                           (char->integer c)
                           read-dispatch-symbol-starter))
            (list (ascii "!")
                  (ascii "$")
                  (ascii "%")
                  (ascii "&")
                  (ascii "*")
                  (ascii "/")
                  (ascii ":")
                  (ascii "<")
                  (ascii "=")
                  (ascii ">")
                  (ascii "?")
                  (ascii "@")
                  (ascii "\\")
                  (ascii "^")
                  (ascii "_")
                  (ascii "|")
                  (ascii "~")))
 
      ; Possible symbol starters that require further parsing.
 
      (for-each (lambda (c)
              (vector-set! read-dispatch-vec (char->integer c) read-dispatch-parse))
            (list (ascii "+")
                  (ascii "-")
                  (ascii ".")
                  (ascii "0")
                  (ascii "1")
                  (ascii "2")
                  (ascii "3")
                  (ascii "4")
                  (ascii "5")
                  (ascii "6")
                  (ascii "7")
                  (ascii "8")
                  (ascii "9")))
 
      ; Special characters.
 
      (vector-set! read-dispatch-vec                  ;double quote
                   34
                   (lambda (c p)
;                     (optimize speed)
                     (read-string (tyi p) p '())))
 
      (vector-set! read-dispatch-vec                  ;sharp sign
                   (char->integer (ascii "#"))
                   read-sharp)
 
      (vector-set! read-dispatch-vec                  ;quote
                   (char->integer (ascii "'"))
                   (lambda (c p)
;                     (optimize speed)
                     (list 'quote (read-dispatch (tyi p) p))))

      (vector-set! read-dispatch-vec                  ;left parenthesis
                   (char->integer (ascii "("))
                   (lambda (c p)
;                     (optimize speed)
                     (read-list (tyi p) p)))
 
      (vector-set! read-dispatch-vec                  ;right parenthesis
                   (char->integer (ascii ")"))
                   read-dispatch-extra-paren)
 
      (vector-set! read-dispatch-vec                  ;comma
                   (char->integer (ascii ","))
                   (lambda (c p)
;                     (optimize speed)
                     (let ((c (tyi p)))
                       (if (and (char? c) (char=? c (ascii "@")))
                           (list 'unquote-splicing (read-dispatch (tyi p) p))
                           (list 'unquote (read-dispatch c p))))))
 
      (vector-set! read-dispatch-vec                  ;semicolon
                   (char->integer (ascii ";"))
                   (lambda (c p)
;                     (optimize speed)
                     (flush-comment-and-read p)))

      (vector-set! read-dispatch-vec                  ;backquote
                   (char->integer (ascii "`"))
                   (lambda (c p)
;                     (optimize speed)
                     (list 'quasiquote (read-dispatch (tyi p) p))))
 
      ; Reserved delimiters.
 
      (for-each (lambda (c)
              (vector-set! read-dispatch-vec (char->integer c) read-dispatch-reserved))
            (list (ascii "[")
                  (ascii "]")
                  (ascii "{")
                  (ascii "}")))
 
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
 
      (do ((c (char->integer (ascii "a")) (+ 1 c)))
          ((> c (char->integer (ascii "z"))))
          (vector-set! read-list-vec c read-list-element))
 
      (do ((c (char->integer (ascii "A")) (+ 1 c)))
          ((> c (char->integer (ascii "Z"))))
          (vector-set! read-list-vec c read-list-element))
 
      (for-each (lambda (c)
              (vector-set! read-list-vec (char->integer c) read-list-element))
            (list (ascii "!")
                  (ascii "$")
                  (ascii "%")
                  (ascii "&")
                  (ascii "*")
                  (ascii "/")
                  (ascii ":")
                  (ascii "<")
                  (ascii "=")
                  (ascii ">")
                  (ascii "?")
                  (ascii "@")
                  (ascii "\\")
                  (ascii "^")
                  (ascii "_")
                  (ascii "|")
                  (ascii "~")))
 
      ; Possible symbol starters that require further parsing.
 
      (for-each (lambda (c)
              (vector-set! read-list-vec (char->integer c) read-list-element))
            (list (ascii "+")
                  (ascii "-")
                  (ascii "0")
                  (ascii "1")
                  (ascii "2")
                  (ascii "3")
                  (ascii "4")
                  (ascii "5")
                  (ascii "6")
                  (ascii "7")
                  (ascii "8")
                  (ascii "9")))
 
      ; Special characters.
 
      (vector-set! read-list-vec                      ;double quote
                   34
                   read-list-element)
 
      (vector-set! read-list-vec                      ;sharp sign
                   (char->integer (ascii "#"))
                   read-list-element)
 
      (vector-set! read-list-vec                      ;quote
                   (char->integer (ascii "'"))
                   read-list-element)
 
      (vector-set! read-list-vec                      ;left parenthesis
                   (char->integer (ascii "("))
                   read-list-element)
 
      (vector-set! read-list-vec                      ;right parenthesis
                   (char->integer (ascii ")"))
                   (lambda (c p) '()))
 
      (vector-set! read-list-vec                      ;comma
                   (char->integer (ascii ","))
                   read-list-element)
 
      (vector-set! read-list-vec                      ;dot
                   (char->integer (ascii "."))
                   read-list-dot)
 
      (vector-set! read-list-vec                      ;semicolon
                   (char->integer (ascii ";"))
                   (lambda (c p)
;                     (optimize speed)
                     (flush-comment-and-read-list p)))
 
      (vector-set! read-list-vec                      ;backquote
                   (char->integer (ascii "`"))
                   read-list-element)
 
      ; Reserved delimiters.
 
      (for-each (lambda (c)
              (vector-set! read-list-vec (char->integer c) read-list-reserved))
            (list (ascii "[")
                  (ascii "]")
                  (ascii "{")
                  (ascii "}")))
 
      ;*****************************************************
 
 
      ; Assign variables.

      (set! read
	    (let ((%conin (current-input-port)))
	      (lambda p
;                (optimize speed)
		(let ((p (if (not (null? p)) (car p) %conin)))
		  (read-dispatch (tyi p) p)))))
 
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

; These are defined in reader0.sch

;(define whitespace?)
 
;(define separator?)
 
;(define digit?)
 
;(define double-quote?)
 
;(define backslash?)
