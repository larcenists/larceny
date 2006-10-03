; Test cases for HTML parser

(define (test s expected)
  (equal? (html-parser (open-input-string s))
	  expected))

(define (fail . xs)
  (display "FAILURE: ")
  (display xs)
  (newline))

(or (test "foo bar baz" '("foo" "bar" "baz"))
    (fail 'textelements))

(or (test "<pre>foo  bar</pre> baz" '((pre () "foo  bar") "baz"))
    (fail 'textelements:pre))

(or (test "<p>foo bar baz" '((p () "foo" "bar" "baz")))
    (fail 'simple-para:1))

(or (test "<p>foo bar baz</p>" '((p () "foo" "bar" "baz")))
    (fail 'simple-para:2))

(or (test "<p>foo bar baz<p>bum" '((p () "foo" "bar" "baz") (p () "bum")))
    (fail 'simple-para:3))

(or (test "<a>foo bar baz<a>bum" '((a () "foo" "bar" "baz") (a () "bum")))
    (fail 'anchor:1))

; HEAD has optional end tag ...
(or (test "<html><head><title><body>fnord"
	  '((html () (head () (title ())) (body () "fnord"))))
    (fail 'optional-end-tags:head:1))

; ... which coupled with the optional start tag for BODY makes
; it necessary to be careful ...
(or (test "<html><head><title>x</title>fnord"
	  '((html () (head () (title () "x")) "fnord")))
    (fail 'optional-end-tags:head:2))

; ... for several kinds of content.
(or (test "<html><head><title>x</title><p>fnord"
	  '((html () (head () (title () "x")) (p () "fnord"))))
    (fail 'optional-end-tags:head:3))

(or (test "<ul><li>foo<li>bar</ul>"
	  '((ul () (li () "foo") (li () "bar"))))
    (fail 'optional-end-tags:li))

(or (test "<dl><dt>costa<dd>gavras<dt>robbe<dd>millet</dl>"
	  '((dl () 
		(dt () "costa") (dd () "gavras")
		(dt () "robbe") (dd () "millet"))))
    (fail 'optional-end-tags:dt+dl))
