;; Ideas for auto-indent support (that might generalize to extensible
;; pretty-printing)

;; Table mapping keywords to indentation-suggestors.

;; An IndentSuggest is a fcn (Nat -> [Oneof `(prev ,Nat) Nat 'prev-subform #f])
;; An IndentationTable is a fcn (Symbol -> IndentSuggest)

;; interpretation: 
;; Let table be an IndentationTable
;;
;; Let the j'th s-exp S_j of the partial combination (K S_1 ... S_j 
;; be the first form on a new line; let the preceding line with content 
;; be denoted P.
;; e.g., for the partial combination:
;;        (case n            ;; K is case, n is S_1
;;          ((0) a) ((1) b)  ;; ((0) a) is S_2
;;          ((2) c) ((3) d)  ;; ((3) d) is S_5 = S_j
;;                           ;; P is the previous line (including indentation)
;;
;; If ((table sym) j) => (prev k) and P starts with some S_i, then
;; S_j should be indented to line up with S_i.
;; 
;; If ((table sym) j) => (prev k) and P starts with the combination
;; itself (rather than some S_i) then S_j should be indented k units
;; past the open parenthesis in the combination.
;; 
;; If ((table sym) j) => (comb k) then the S_j should be indented k units
;; past the open parenthesis in the combination.
;; 
;; If ((table sym) i) => prev-subform and if the i'th s-exp of the
;; combination (sym form ...) is the first form on a new line, then
;; the suggested indentation of the i'th s-exp is to line up with the
;; first form of the combination on the preceding line (*excluding*
;; sym).  It is an error for an IndentSuggest to return
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

(define *indentation-table-data* (lambda (x) (lambda (form) #f)))

(define (install-indentation-table-entry! keyword suggest)
  (let* ((table *indentation-table-data*)
         (table* (lambda (x) (if (eq? x keyword) suggest (table x)))))
    (set! *indentation-table-data* table*)))

(install-indentation-table-entry! 'define
                                  (lambda (i) '(prev 1)))
(install-indentation-table-entry! 'lambda 
                                  (lambda (i) (case i
                                                ((1) 3)
                                                ((2) 1)
                                                (else 'prev-subform))))

;; lookup-indentation : Symbol Nat Nat [Maybe Nat] Nat -> Nat
(define (lookup-indentation keyword
                            form-indent
                            first-subform-indent
                            prev-line-indent 
                            subform-num)
  (let* ((suggestor (*indentation-table-data* keyword))
         (suggestion (suggestor subform-num)))
    (cond
     ((number? suggestion) (+ form-indent suggestion))
     ((pair? suggestion) (if prev-line-indent
                             prev-line-indent
                             (+ form-indent (cadr suggestion))))
     ((eq? suggestion 'prev-subform) (if prev-line-indent
                                         prev-line-indent
                                         first-subform-indent))
     ((not suggestion) (or prev-line-indent form-indent))
     (else 
      (error 'lookup-indentation ": unexpected suggestion" suggestion)))))

;; suggest-indentation : Port -> Nat
;; Assumes that p feeds characters from the text starting from the
;; cursor and working backwards.
;; Returns the suggested amount that that the cursor should be 
;; indented by if we were to start a new line now.
;;
;; KNOWN LIMITATIONS (by design)
;; * Multiline string literals are not handled properly in all cases
(define (suggest-indentation p)
  (call-with-values (lambda () (gather-indentation-data-from-port p))
    (lambda vals
      (if (null? (cdr vals))
          (car vals)
          (apply lookup-indentation vals)))))

(define (gather-indentation-data-from-port p)
  ;; found-end-of-sexp : [Maybe Nat] Symbol Nat -> Nat
  (define (found-end-of-sexp suggest-indent keyword-sym subform-num 
                             next-form-indent)
    ;; At this point, the port is at the paren associated with
    ;; keyword-sym.  
    ;; Count chars between the paren and the start of the line (or eof)
    ;;
    ;; (suggest-indent is a fallback if keyword-sym is not associated
    ;; with any particular indentation level).

    (let ((remaining-indent
           (do ((i 1 (+ i 1))
                (c (read-char p) (read-char p)))
               ((or (eof-object? c)
                    (char=? c #\newline))
                i))))
      (values keyword-sym
              remaining-indent
              next-form-indent
              suggest-indent
              subform-num)))

  ;; A LineStateInfo is a 
  ;;   (list Symbol Depth FormCount [Listof Char] NextFormIndent)
  (define initial-state (list 'start 0 0 '() 0))

  (define (state-sym state-info)
    (list-ref state-info 0))
  (define (state-depth state-info)
    (list-ref state-info 1))
  (define (state-form-count state-info)
    (list-ref state-info 2))
  (define (state-chars state-info)
    (list-ref state-info 3))
  (define (state-next-form-indent state-info)
    (list-ref state-info 4))

  (define (list-update lst idx xform)
    (if (= idx 0)
        (cons (xform (car lst)) (cdr lst))
        (cons (car lst) (list-update (cdr lst) (- idx 1) xform))))
  (define (add1 n) (+ n 1))
  (define (sub1 n) (- n 1))
  (define (zer0 n) 0)
  (define (clone-state-sym sym state-info) 
    (list-update state-info 0 (lambda (ignor) sym)))
  (define (clone-state-incr-depth state-info)
    (list-update state-info 1 add1))
  (define (clone-state-decr-depth state-info)
    (list-update state-info 1 sub1))
  (define (clone-state-incr-form-count state-info)
    (list-update state-info 2 add1))
  (define (clone-state-add-char char state-info)
    (list-update state-info 3 (lambda (lst) (cons char lst))))
  (define (clone-state-fresh-char char state-info)
    (list-update state-info 3 (lambda (ignore) (list char))))
  (define (clone-state-adjust-next-form-indent adj state-info)
    (list-update state-info 4 adj))

  (let loop ((curr-state initial-state)
             (indent-on-line-so-far 0)
             (last-line-indent #f)
             (line-state initial-state))
    (define (next-state state adj-indent)
      (loop state (adj-indent indent-on-line-so-far) last-line-indent line-state))
    (define (reset-state adj-indent)
      (loop line-state (adj-indent indent-on-line-so-far) last-line-indent line-state))
    (define (lineend-state state adj-indent)
      (loop state 
            (adj-indent indent-on-line-so-far)
            (or last-line-indent indent-on-line-so-far)
            state))
    
    (define (pop-sexp sym)
      (next-state 
       (clone-state-sym sym (clone-state-decr-depth curr-state))
       zer0))
    (define (push-sexp sym)
      (next-state 
       (clone-state-sym sym (clone-state-incr-depth curr-state))
       zer0))
    (define (next-new-id sym char)
      (next-state (clone-state-sym 
                   sym
                   (clone-state-fresh-char
                    char 
                    (clone-state-adjust-next-form-indent
                     (lambda (ign)
                       indent-on-line-so-far)
                     curr-state)))
                  zer0))
    (define (reset-line)
      (reset-state zer0))
    (define (newer-line sym)
      (lineend-state (clone-state-sym sym curr-state) zer0))
    (define (next-white sym)
      (next-state (clone-state-sym 
                   sym 
                   (clone-state-incr-form-count
                    curr-state))
                  add1))
    (define (next-white-again sym)
      (next-state (clone-state-sym sym curr-state) add1))
    (define (next-with-char sym char)
      (next-state (clone-state-sym sym (clone-state-add-char char curr-state))
                  zer0))

    (define (peek-depth)
      (state-depth curr-state))
    (define (peek-symbol)
      (string->symbol (list->string (state-chars curr-state))))
    (define (peek-form-count)
      (state-form-count curr-state))
    (define (peek-next-form-indent)
      (state-next-form-indent curr-state))

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
        
        ;; Code to trace the behavior of the scanner.
        '(begin
          (write c)
          (let ((sp (open-output-string)))
            (write c sp)
            (display (make-string (- 10 (string-length (get-output-string sp)))
                                  #\space)))
          (display " ")
          (write `(loop ,curr-state 
                        ,indent-on-line-so-far 
                        ,last-line-indent
                        ,line-state))
          (newline))


        (cond 
         ((eof-object? c)      0)
         (else
          (case (car curr-state)
            ((start)   
             (dispatch c 
                       (#\(    (if (zero? (peek-depth))
                                   (found-end-of-sexp last-line-indent
                                                      (peek-symbol)
                                                      (peek-form-count)
                                                      (peek-next-form-indent))
                                   (pop-sexp 'start)))
                       (#\)        (push-sexp   'start))
                       (#\"        (next-new-id 'mbstr c))
                       (#\\        (next-new-id 'id-bs c))
                       (#\;        (reset-line))
                       (#\newline  (newer-line  'start))
                       (whitespace (next-white-again  'start))
                       (else       (next-new-id 'id c))))
            ((id)      
             (dispatch c 
                       (#\(    (if (zero? (peek-depth))
                                   (found-end-of-sexp last-line-indent
                                                      (peek-symbol)
                                                      (peek-form-count)
                                                      (peek-next-form-indent))
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
                                   (found-end-of-sexp last-line-indent
                                                      (peek-symbol)
                                                      (peek-form-count)
                                                      (peek-next-form-indent))
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
                                   (found-end-of-sexp last-line-indent
                                                      (peek-symbol)
                                                      (peek-form-count)
                                                      (peek-next-form-indent))
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
                                   (found-end-of-sexp last-line-indent
                                                      (peek-symbol)
                                                      (peek-form-count)
                                                      (peek-next-form-indent))
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

(define case-example-1
"
        (case n            ;; K is case, n is S_1
          ((0) a) ((1) b)  ;; ((0) a) is S_2
          ((2) c) ((3) d)  ;; ((3) d) is S_5 = S_j
                           ;; P is the previous line (including indentation)
")

(define case-example-2
"
     (case    n         ;; K is case, n is S_1
       ((0) a) ((1) b)  ;; ((0) a) is S_2
       ((2) c) 
         ((3) d)        ;; ((3) d) is S_5 = S_j
                        ;; P is the previous line (including indentation)
")

(define case-example-3
"
        (case              ;; K is case
n                          ;; n is S_1
          ((0) a) ((1) b)  ;; ((0) a) is S_2
          ((2) c) 
            ((3) d)        ;; ((3) d) is S_5 = S_j
                           ;; P is the previous line (including indentation)
")


;;; TEST TODO: 
;;; ----------
;;; [ ] cond special form
;;; [ ] if special form
;;; [ ] case special form
;;; [ ] do special form
;;; [ ] semicolon comments
;;; [ ] hashpipe comments
;;; [ ] hashsemicolon comments

