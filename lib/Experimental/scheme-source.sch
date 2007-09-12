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
;;
;; KNOWN LIMITATIONS (by design)
;; * Multiline string literals are not handled properly in all cases
(define (suggest-indentation p)
  (define (found-end-of-sexp suggest-indent keyword-sym)
    ;; At this point, the port is at the paren associated with
    ;; keyword-sym.  
    ;; Count chars between the paren and the start of the line (or eof)
    ;;
    ;; (suggest-indent is a fallback if keyword-sym is not associated
    ;; with any particular indentation level).

    (let ((remaining-indent
           (do ((i 0 (+ i 1))
                (c (read-char p) (read-char p)))
               ((or (eof-object? c)
                    (char=? c #\newline))
                i))))
      ;; XXX
      (list remaining-indent 
            suggest-indent 
            keyword-sym)))
  
  (let loop (;; A StateInfo is a (list Symbol Depth FormCount [Listof Char])
             (curr-state  (list 'start 0 0 '()))
             (indent-on-line-so-far 0)
             (first-line-indent #f)
             (line-state  (list 'start 0 0 '())))

    (define (peek-depth)     (cadr curr-state))
    (define (peek-id-chars)  (cadddr curr-state))
    (define (peek-symbol)
      (if (null? (peek-id-chars))
          #f
          (string->symbol (list->string (reverse (peek-id-chars))))))

    (define (change-state sym state-info)
      (list sym (cadr state-info) (caddr state-info) (cadddr state-info)))
    (define (deepen-state sym state-info)
      (list sym (+ (cadr state-info) 1) (caddr state-info) (cadddr state-info)))
    (define (shallow-state sym state-info)
      (list sym (- (cadr state-info) 1) (caddr state-info) (cadddr state-info)))
    (define (addchar-state sym state-info char)
      (list sym 
            (cadr state-info) 
            (caddr state-info) 
            (cons char (cadddr state-info))))
    (define (freshid-state sym state-info char)
      (list sym (cadr state-info) (caddr state-info) (list char)))

    (define (next-state state new-indent-for-line)
      (loop state new-indent-for-line first-line-indent line-state))

    (define (next-white next-sym)
      (next-state (change-state next-sym curr-state) 
                  (+ indent-on-line-so-far 1)))
    (define (next-with-char next-sym char)
      (next-state (addchar-state next-sym curr-state char) 
                  0))
    (define (next-new-id next-sym char)
      (next-state (freshid-state next-sym curr-state char)
                  0))

    (define (push-sexp next-sym)
      (next-state (deepen-state next-sym curr-state) 
                  0))
    (define (pop-sexp  next-sym)
      (next-state (shallow-state next-sym curr-state)
                  0))

    (define (reset-line)
      (loop line-state 0 first-line-indent line-state))
    (define (newer-line next-sym)
      (loop (change-state next-sym curr-state) 
            0
            (suggest-indent)
            curr-state))

    (define (suggest-indent)
      (if first-line-indent 
          first-line-indent 
          indent-on-line-so-far))
    
    (letrec-syntax 
        ((dispatch
          (syntax-rules (whitespace else)
            ((_        "BUILD"       CS #f ES c (whitespace ws-exp) elems ...)
             (dispatch "BUILD"       CS (whitespace ws-exp) ES c elems ...))
            ((_        "BUILD"       CS WS #f c (else else-exp))
             (dispatch "GENER"       CS WS (else else-exp) c))
            ((_        "BUILD" (CZ ...) WS ES c clause elems ...)
             (dispatch "BUILD" (CZ ... clause) WS ES c elems ...))
            ((_ "GENER" 
                ((char char-exp) ...)
                (whitespace ws-exp)
                (else else-exp) 
                c)
             (cond
              ((char=? c char) char-exp)
              ...
              ((char-whitespace? c) ws-exp)
              (else else-exp)))
            ((_ "BUILD" args ...) (error 'dispatch-form '(args ...)))
            ((_ "GENER" args ...) (error 'dispatch-form '(args ...)))
            ((_ c elems ...) ;; standard entry point
             (dispatch "BUILD" () #f #f c elems ...))
            )))
      (let ((c (read-char p)))
        (cond 
         ((eof-object? c)      (suggest-indent))
         (else
          (case (car curr-state)
            ((start)   
             (dispatch c 
                       (#\(    (if (zero? (peek-depth))
                                   (found-end-of-sexp (suggest-indent)
                                                      (peek-symbol))))
                       (#\)        (pop-sexp    'start))
                       (#\)        (push-sexp   'start))
                       (#\"        (next-new-id 'mbstr c))
                       (#\\        (next-new-id 'id-bs c))
                       (#\;        (reset-line))
                       (#\newline  (newer-line  'start))
                       (whitespace (next-white  'start))
                       (else       (next-new-id 'id c))))
            ((id)      
             (dispatch c 
                       (#\(    (if (zero? (peek-depth))
                                   (found-end-of-sexp (suggest-indent)
                                                      (peek-symbol))
                                   (pop-sexp    'start)))
                       (#\)        (push-sexp   'start))
                       (#\"        (next-new-id 'mbstr c))
                       (#\\        (next-new-id 'id-bs c))
                       (#\;        (reset-line))
                       (#\newline  (newer-line  'start))
                       (whitespace (next-white  'start))
                       (else       (next-with-char 'id c))))
            ((mbstr)
             (dispatch c
                       (#\\        (next-with-char 'mbid c))
                       (#\"        (next-with-char 'mbstrend c))
                       (else       (next-with-char 'str c))))
            ((mbid)      
             (dispatch c 
                       (#\(    (if (zero? (peek-depth))
                                   (found-end-of-sexp (suggest-indent)
                                                      (peek-symbol))
                                   (pop-sexp    'start)))
                       (#\)        (push-sexp   'start))
                       (#\"        (next-new-id 'mbstr c))
                       (#\\        (next-with-char 'mbstr c))
                       (#\;        (reset-line))
                       (#\newline  (newer-line  'start))
                       (whitespace (next-white  'start))
                       (else       (next-with-char 'id c))))
            ((str)     
             (dispatch c 
                       (#\"       (next-with-char 'mbstrend c))
                       (else      (next-with-char 'str c))))
            ((mbstrend)
             (dispatch c 
                       (#\(    (if (zero? (peek-depth))
                                   (found-end-of-sexp (suggest-indent)
                                                      (peek-symbol))
                                   (pop-sexp    'start)))
                       (#\)        (push-sexp   'start))
                       (#\"        (next-new-id 'mbstr c))
                       (#\\        (next-with-char 'mbstr c))
                       (#\;        (reset-line))
                       (#\newline  (newer-line  'start))
                       (whitespace (next-white  'start))
                       (else       (next-with-char 'id c))))
            ((id-bs)   
             (dispatch c
                       (#\(    (if (zero? (peek-depth))
                                   (found-end-of-sexp (suggest-indent)
                                                      (peek-symbol))
                                   (pop-sexp    'start)))
                       (#\)        (push-sexp   'start))
                       (#\"        (next-new-id 'mbstr c))
                       (#\\        (next-with-char 'id c))
                       (#\;        (reset-line))
                       (#\newline  (newer-line  'start))
                       (whitespace (next-white  'start))
                       (else       (next-with-char 'id c)))))))))))

;; Used to define test cases below
(define (reversed-string->input-port s)
  (open-input-string (list->string (reverse (string->list s)))))
(define (test input output)
  (let ((result (suggest-indentation (reversed-string->input-port input))))
    (cond
     ((equal? result output) 'test-passed)
     (else (display "test failure: ") 
           (write input)
           (display " should be ")
           (write output)
           (display " not ")
           (write result)
           (newline)))))

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

;;; TEST TODO: 
;;; ----------
;;; [ ] cond special form
;;; [ ] if special form
;;; [ ] case special form
;;; [ ] do special form
;;; [ ] semicolon comments
;;; [ ] hashpipe comments
;;; [ ] hashsemicolon comments

