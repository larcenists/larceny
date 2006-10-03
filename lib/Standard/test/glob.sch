; Test code for glob.sch
; 2004-01-11 / lth

(require 'glob)

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

(or (glob "*.c" "abracadabra.c")
    (fail 'glob:1))
(or (glob "???.c" "abc.c")
    (fail 'glob:2))
(or (glob "[a-z]foo.c" "x.c")
    (fail 'glob:3))
(or (glob "a/b*/c" "a/bottolf/c")
    (fail 'glob:4))
(or (glob "a/\b*/c" "a/bottolf/c")
    (fail 'glob:5))
(or (glob "a[a-z-]b.c" "a-b.c")
    (fail 'glob:6))
(or (glob "a[-a-z]b.c" "a-b.c")
    (fail 'glob:7))
(or (glob "a[!a-z]b.c" "a-b.c")
    (fail 'glob:8))
(or (glob "a[!]a-]b.c" "a]b.c")
    (fail 'glob:9))
(or (glob "a[!-a-z]b.c" "aAb.c")
    (fail 'glob:10))

(or (not (glob "*.c" "abracadabra.txt"))
    (fail 'not-glob:1))
(or (not (glob "a/b*/c" "a/bottolf/x/c"))
    (fail 'not-glob:2))
(or (not (glob "a/b*/c" "a/bottolf//c"))
    (fail 'not-glob:3))
(or (not (glob "a[!-a-z]b.c" "axb.c"))
    (fail 'not-glob:4))
(or (not (glob "a[!-a-z]b.c" "a-b.c"))
    (fail 'not-glob:5))

(writeln "Done.")

; eof
