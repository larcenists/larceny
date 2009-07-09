;;; Test suite for SRFI 48.
;;;
;;; $Id$

(cond-expand (srfi-48))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(define help-message
"(format [<port>] <format-string> [<arg>...]) -- <port> is #t, #f or an output-port
OPTION  [MNEMONIC]      DESCRIPTION     -- Implementation Assumes ASCII Text Encoding
~H      [Help]          output this text
~A      [Any]           (display arg) for humans
~S      [Slashified]    (write arg) for parsers
~W      [WriteCircular] like ~s but outputs circular and recursive data structures
~~      [tilde]         output a tilde
~T      [Tab]           output a tab character
~%      [Newline]       output a newline character
~&      [Freshline]     output a newline character if the previous output was not a newline
~D      [Decimal]       the arg is a number which is output in decimal radix
~X      [heXadecimal]   the arg is a number which is output in hexdecimal radix
~O      [Octal]         the arg is a number which is output in octal radix
~B      [Binary]        the arg is a number which is output in binary radix
~w,dF   [Fixed]         the arg is a string or number which has width w and d digits after the decimal
~C      [Character]     charater arg is output by write-char
~_      [Space]         a single space character is output
~Y      [Yuppify]       the list arg is pretty-printed to the output
~?      [Indirection]   recursive format: next 2 args are format-string and list of arguments
~K      [Indirection]   same as ~?
")

(or (equal? (format "~h")
; => 
            help-message)
    (fail 'h))

(or (equal? (format "Hello, ~a" "World!")
            "Hello, World!")
    (fail 'a))

(or (equal? (format "Error, list is too short: ~s" '(one "two" 3))
            "Error, list is too short: (one \"two\" 3)")
    (fail 's))

(or (equal? (format "test me")
            "test me")
    (fail 'testme))

(or (equal? (format "~a ~s ~a ~s" 'this 'is "a" "test")
            "this is a \"test\"")
    (fail 'asas))

(or (parameterize ((current-output-port (open-output-string)))
      (format #t "#d~d #x~x #o~o #b~b~%" 32 32 32 32)
      (equal? (get-output-string (current-output-port))
              "#d32 #x20 #o40 #b100000\n"))
    (fail 'dxob))

(or (equal? (format "~a ~? ~a" 'a "~s" '(new) 'test)
            "a new test")
    (fail 'a?a))

(or (equal? (format #f "~&1~&~&2~&~&~&3~%")
"
1
2
3
")
    (fail '&1&&2&&&3))

(or (equal? (format #f "~a ~? ~a ~%" 3 " ~s ~s " '(2 2) 3)
"3  2 2  3 
")
    (fail 'a?a))

(or (equal? (format "~w" (let ( (c '(a b c)) ) (set-cdr! (cddr c) c) c))
            "#1=(a b c . #1#)")
    (fail 'w))

(or (equal? (format "~8,2F" 32)
            "   32.00")
    (fail 'F1))

(or (equal? (format "~8,3F" (sqrt -3.8))
            "0.000+1.949i")
    (fail 'F2))

(or (equal? (format "~8,2F" 3.4567e11)
            " 3.46e11")
    (fail 'F3))

(or (equal? (format "~6,3F" 1/3)
            " 0.333")
    (fail 'F4))

(or (equal? (format "~4F" 12)
            "  12")
    (fail 'F5))

(or (equal? (format "~8,3F" 123.3456)
            " 123.346")
    (fail 'F6))

(or (equal? (format "~6,3F" 123.3456)
            "123.346")
    (fail 'F7))

(or (equal? (format "~2,3F" 123.3456)
            "123.346")
    (fail 'F8))

(or (equal? (format "~8,3F" "foo")
            "     foo")
    (fail 'F9))

(or (equal? (format "~a~a~&" (list->string (list #\newline)) "")
"
")
    (fail 'aa))

(writeln "Done.")


