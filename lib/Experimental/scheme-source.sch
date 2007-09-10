;; Ideas for auto-indent support (that might generalize to extensible
;; pretty-printing)

;; Table mapping keywords to indentation-suggestors.

;; An IndentationSuggestor is a fcn (Nat -> [Oneof Nat #f])
;; An IndentationTable is a fcn (Symbol -> IndentationSuggestor)

;; interpretation: 
;; Let table be an IndentationTable
;;
;; Let the j'th s-exp S_j of the combination (sym S_1 ... S_l) be the
;; first form on a new line; let the preceding line with content be
;; denoted P.
;;
;; If ((table sym) j) => (prev k) and P starts with some S_i, then
;; S_j should be indented to line up with S_i.
;; 
;; If ((table sym) j) => (prev k) and P starts with the combination
;; itself (rather than some S_i) then S_j should be indented k units
;; past the open parenthesis in the combination.
;; 
;; If ((table sym) j) => k then the S_j should be indented k units
;; past the open parenthesis in the combination.
;; 
;; If ((table sym) i) => prev-subform and if the i'th s-exp of the
;; combination (sym form ...) is the first form on a new line, then
;; the suggested indentation of the i'th s-exp is to line up with the
;; first form of the combination on the preceding line (*excluding*
;; sym).  It is an error for an IndentationSuggestor to return
;; 'prev-subform for i < 2.
;; 
;; Note that (prev 1) or prev-subform is probably the right thing in
;; most every case...
;; 
;; EXAMPLES (based on Emacs' behavior):
;;
;; ((table 'define) i) => (prev 1)                [ forall i > 0 ]
;; ((table 'lambda) 1) => 3
;; ((table 'lambda) 2) => 1
;; ((table 'lambda) i) => prev-subform            [ forall i > 2 ]
;; ((table 'cond) 1)   => 0
;; ((table 'cond) i)   => prev-subform            [ forall i > 0 ]
;; ((table 'case) 1)   => 3
;; ((table 'case) 2)   => 1
;; ((table 'case) i)   => prev-subform
;; ((table 'call-with-values) 1) => 3
;; ((table 'call-with-values) 2) => 1
;; ((table 'call-with-values) i) => prev-subform  [ forall i > 2 ]
;; ((table 'do) 1) => 3
;; ((table 'do) 2) => 3
;; ((table 'do) i) => prev-subform                [ forall i > 2 ]

;; suggest-indentation : Port -> Nat
;; Assumes that p feeds characters from the text starting from the
;; cursor and working backwards.
;; Returns the suggested amount that that the cursor should be 
;; indented by if we were to start a new line now.
(define (suggest-indentation p)
  ...)

;; Used to define test cases below
(define (reversed-string->input-port s)
  (open-input-string (list->string (reverse (string->list s)))))
(define (test input output)
  (equal? (suggest-indentation (reversed-string->input-port input)) 
          output))
;; Examples:
(test "some-symbol"                 0)
(test "  some-symbol"               0)
(test "(some-symbol"                1)
(test "(  some-symbol"              3)
(test "(define"                     2)
(test "(define f y"                 2)
(test "(define    f y"              2)
(test "(define (f y)"               2)
(test "(define    (f y)"            2)
(test "   (define f  "              5)
(test "(   define"                  2)
(test "  (   define"                4)
(test "(lambda "                    4)
(test "(lambda x "                  2)
(test "  (lambda "                  6)
(test "  (  lambda "                6)
(test "(lambda (x) y "              8)
(test "(lambda   (x) y "           10)
(test "(lambda (x)\n  y "           2)
(test "(lambda (x)\n    y "         4)
(test "((foo)"                      1)
(test "(((foo (baz (bing))"         7)
(test "(((foo (baz (bing)))"        2)
