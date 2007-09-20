;; Ideas for auto-indent support (that might generalize to extensible
;; pretty-printing)

;; Table mapping keywords to indentation-suggestors.

;; An Posn is a (list Nat Nat Char)
;; 
;; An IndentSuggest is a fcn: 
;;    (Nat Posn Posn [Maybe Posn] [Maybe Posn] -> Nat)
;;
;; An IndentationTable is a fcn (Symbol -> IndentSuggest)


;; interpretation: 
;; 
;; A Posn (list i j c) is a point indented by i spaces and j lines up;
;; character c is at that point.
;;
;; An IndentSuggest is fed up to four arguments which summarize the
;; character positions of some points of interest on the S-exp that we
;; are in the middle of typing.
;; 
;; The IndentSuggest is in charge of returning how many spaces the
;; cursor should be moved to the right to get "proper" indentation.
;; 
;; (Note the IndentationTable, defined below, maps keywords to
;; distinct IndentSuggests)
;;
;; (indent-suggest form-count to-paren to-keyword to-first to-final)
;; form-count is number of sexp forms between the cursor and the open paren.
;; to-paren is the point immediately past the open paren.
;; to-keyword is the point at the start of the keyword.
;; to-first, if present, is the point at the start of the first subform.
;; to-final, if present, is the point at the start of the left-most
;;    subform on the ``final'' line.

;; (to-keyword is often the same as to-paren)
;;
;; Let table be an IndentationTable
;; 

(define (make-prev-k-suggestor k)
  (lambda (form-count to-paren to-keyword to-first to-final)
    (cond (to-final (car to-final))
          (else (+ (car to-paren) k)))))

(define (make-constant-suggestor k)
  (lambda (form-count to-paren to-keyword to-first to-final)
    (+ (car to-paren) k)))

(define (make-prev-subform-suggestor k)
  (lambda (form-count to-paren to-keyword to-first to-final)
    (cond (to-final (car to-final))
          (to-first (car to-first))
          (else (+ (car to-paren) default)))))


;; The below are old notes when I was trying to design a "simpler"
;; IndentSuggest by constraining its return values. 
;; 
;; Let the j'th s-exp S_j of the partial combination (K S_1 ... S_j 
;; be the first form on a new line; let the last line with content 
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

(define *indentation-table-data* 
  (lambda (x) (lambda (n p f0 f1 fn) 
                (cond (f1 => car)
                      (else (car f0))))))

(define (install-indentation-table-entry! keyword suggest)
  (let* ((table *indentation-table-data*)
         (table* (lambda (x) (if (eq? x keyword) suggest (table x)))))
    (set! *indentation-table-data* table*)))


(let () 
  (define define-suggest 
    (make-prev-k-suggestor 1))
  ;; (trace define-suggest)
  (install-indentation-table-entry! 'define define-suggest))



(let ()
  (define lambda-suggest
    (lambda (form-count to-paren to-keyword to-first to-final)
      (cond 
       ((= form-count 1) (+ (car to-paren) 3))
       ((= form-count 2) (+ (car to-paren) 1))
       (to-final => car)
       (to-first => car)
       (else (car to-paren)))))
  ;; (trace lambda-suggest)
  (install-indentation-table-entry! 'lambda lambda-suggest))

(let ()
  (define case-suggest
    (lambda (form-count to-paren to-keyword to-first to-final)
      (cond
       ((= form-count 1) (+ (car to-paren) 3))
       ((= form-count 2) (+ (car to-paren) 1))
       (to-final => car)
       (to-first => car)
       (else (car to-paren)))))
  (install-indentation-table-entry! 'case case-suggest))

(let () 
  (define do-suggest
    (lambda (form-count to-paren to-keyword to-first to-final)
      (cond
       ((= form-count 1) (+ (car to-paren) 3))
       ((= form-count 2) (+ (car to-paren) 3))
       ((= form-count 3) (+ (car to-paren) 1))
       (to-final => car)
       (to-first => car)
       (else (car to-paren)))))
  (install-indentation-table-entry! 'do do-suggest))

;; A FormInfo is a (list String Nat Nat)
(define (make-forminfo form-start indent line-count)
  (list form-start indent line-count))
(define forminfo-form car)
(define forminfo-indent cadr)
(define forminfo-line-count caddr)
(define (forminfo->posn fi)
  (list (cadr fi) (caddr fi) (string-ref (car fi) 0)))
;; lift/maybe : (X -> Y) -> [Maybe X] -> [Maybe Y]
(define (lift/maybe f)
  (lambda (x)
    (and x (f x))))

;; lookup-indentation : 
;;   Posn [Maybe FormInfo] [Maybe FormInfo] [Maybe FormInfo] Nat -> Nat
(define (lookup-indentation form-indent
                            keyword-forminfo
                            first-subform-forminfo
                            final-subform-forminfo 
                            subform-num)
  (define (maybe-cdr x) (cond ((pair? x) (cdr x)) 
                              ((not x) #f)
                              (else (error 'lookup-indentation))))
  (cond 
   (keyword-forminfo
    (let* ((keyword (string->symbol (forminfo-form keyword-forminfo)))
           (suggestor (*indentation-table-data* keyword))
           (maybe-forminfo->posn (lift/maybe forminfo->posn))
           (suggestion (suggestor subform-num
                                  form-indent
                                  (maybe-forminfo->posn keyword-forminfo)
                                  (maybe-forminfo->posn first-subform-forminfo)
                                  (maybe-forminfo->posn final-subform-forminfo)
                                  )))
      (cond
       ((number? suggestion) suggestion)
       (else 
        (error 'lookup-indentation ": suggestion" 
               suggestion " should be a (natural) number.")))))
   (final-subform-forminfo
    (forminfo-indent final-subform-forminfo))
   (else
    (car form-indent))))

;; suggest-indentation : Port -> Nat
;; Assumes that p feeds characters from the text starting from the
;; cursor and working backwards.
;; Returns the suggested amount that that the cursor should be 
;; indented by if we were to start a new line now.
;;
;; KNOWN LIMITATIONS (by design)
;; * newlines between the open paren and keyword are never handled properly.
;; * Multiline string literals are not handled properly in all cases
(define (suggest-indentation p)
  (call-with-values (lambda () (gather-indentation-data-from-port p))
    lookup-indentation))

;; gather-indentation-data-from-port 
;;  : Port -> (values Posn [Maybe FormInfo] [Maybe FormInfo] [Maybe FormInfo] Nat)
(define (gather-indentation-data-from-port p)
  ;; found-end-of-sexp : [Maybe FormInfo] FormInfo Nat [Maybe FormInfo] -> Nat
  (define (found-end-of-sexp last-line-forminfo 
                             keyword-forminfo 
                             form-count
                             next-forminfo
                             next-line-forminfo
                             line-count)
    ;; At this point, the port is at the paren associated with
    ;; keyword.  
    ;; Count chars between the paren and the start of the line (or eof)
    ;;
    ;; (suggest-indent is a fallback if keyword is not associated
    ;; with any particular indentation level).

    (let ((remaining-indent
           (do ((i 1 (+ i 1))
                (c (read-char p) (read-char p)))
               ((or (eof-object? c)
                    (char=? c #\newline))
                i))))
      (values (list remaining-indent line-count)
              (and keyword-forminfo
                   (if (= line-count (forminfo-line-count keyword-forminfo))
                       (make-forminfo 
                        (forminfo-form keyword-forminfo)
                        (+ remaining-indent (forminfo-indent keyword-forminfo))
                        (forminfo-line-count keyword-forminfo))
                       keyword-forminfo))
              (if next-forminfo
                  (make-forminfo
                   (forminfo-form next-forminfo)
                   (+ remaining-indent 
                      (string-length (forminfo-form keyword-forminfo))
                      (forminfo-indent next-forminfo))
                   (forminfo-line-count next-forminfo))
                  next-line-forminfo)
              last-line-forminfo
              form-count)))

  ;; A LineStateInfo is a 
  ;;   (list Symbol Depth FormCount [Listof Char] [Maybe FormInfo])
  (define initial-state (list 'start 0 0 '() #f))

  (define (state-sym state-info)
    (list-ref state-info 0))
  (define (state-depth state-info)
    (list-ref state-info 1))
  (define (state-form-count state-info)
    (list-ref state-info 2))
  (define (state-chars state-info)
    (list-ref state-info 3))
  (define (state-next-forminfo state-info)
    (list-ref state-info 4))

  (define (list-update lst idx xform)
    (if (= idx 0)
        (cons (xform (car lst)) (cdr lst))
        (cons (car lst) (list-update (cdr lst) (- idx 1) xform))))
  (define (add1 n) (+ n 1))
  (define (sub1 n) (- n 1))
  (define (id n)   n)
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
  (define (clone-state-clear-char state-info)
    (list-update state-info 3 (lambda (ignore) (list))))
  (define (clone-state-next-forminfo forminfo state-info)
    (list-update state-info 4 (lambda (ingore) forminfo)))

  (let loop ((curr-state initial-state)
             (indent-on-line-so-far 0)
             (next-line-forminfo   #f)   ;; [Maybe FormInfo]
             (last-line-forminfo   #f)   ;; [Maybe FormInfo]
             (line-state initial-state)
             (line-count 0))
    (define (next-state state adj-indent adj-line)
      (loop state
            (adj-indent indent-on-line-so-far) 
            next-line-forminfo
            last-line-forminfo
            line-state
            (adj-line line-count)))
    (define (reset-state adj-indent adj-line)
      (loop line-state 
            (adj-indent indent-on-line-so-far) 
            next-line-forminfo
            last-line-forminfo
            line-state
            (adj-line line-count)))
    (define (lineend-state state adj-indent adj-line)
      (let ((newstate (clone-state-clear-char 
                       (clone-state-next-forminfo 
                        #f 
                        state))))
        (loop newstate
              (adj-indent indent-on-line-so-far)
              (and (not (null? (peek-chars)))
                   (make-forminfo (peek-string) 
                                  indent-on-line-so-far 
                                  line-count))
              (or last-line-forminfo
                  (and (zero? (peek-depth))
                       (not (null? (peek-chars)))
                       (make-forminfo (peek-string)
                                      indent-on-line-so-far
                                      line-count)))
              newstate
              (adj-line line-count))))
    
    (define (pop-sexp sym char)
      (let* ((new-state
              (clone-state-sym sym (clone-state-decr-depth 
                                    (clone-state-add-char char curr-state))))
             (new-state
              (cond ((= 1 (peek-depth))
                     (clone-state-incr-form-count new-state))
                    (else
                     new-state))))
        (next-state new-state zer0 id)))
    (define (push-sexp sym char)
      (next-state 
       (clone-state-sym sym (clone-state-incr-depth curr-state))
       zer0 id))
    (define (next-new-id sym char)
      (next-state (clone-state-sym 
                   sym
                   (clone-state-fresh-char
                    char 
                    (if (or (> (peek-depth) 0)
                            (null? (peek-chars)))
                        curr-state
                        (clone-state-next-forminfo
                         (make-forminfo (peek-string) 
                                        indent-on-line-so-far
                                        line-count)
                         curr-state))))
                  zer0 id))
    (define (reset-line)
      (reset-state zer0 id))
    (define (newer-line-from-form sym)
      (lineend-state (clone-state-sym 
                      sym
                      (clone-state-incr-form-count curr-state))
                     zer0 add1))
    (define (newer-line sym)
      (lineend-state (clone-state-sym 
                      sym
                      curr-state)
                     zer0 add1))
    (define (next-white sym)
      (let* ((new-state (clone-state-sym sym curr-state))
             (new-state (cond ((= 0 (peek-depth))
                               (clone-state-incr-form-count new-state))
                              (else
                               new-state))))
        (next-state new-state add1 id)))

    (define (next-white-again sym)
      (next-state (clone-state-sym sym curr-state) add1 id))
    (define (next-with-char sym char)
      (next-state (clone-state-sym sym (clone-state-add-char char curr-state))
                  zer0 
                  (if (char=? char #\newline) add1 id)))


    (define (peek-depth)
      (state-depth curr-state))
    (define (peek-chars)
      (state-chars curr-state))
    (define (peek-string)
      (list->string (state-chars curr-state)))
    (define (peek-symbol)
      (string->symbol (peek-string)))
    (define (peek-form-count)
      (state-form-count curr-state))
    (define (peek-next-forminfo)
      (state-next-forminfo curr-state))

    (define (found-it-white)
      (found-end-of-sexp last-line-forminfo
                         (if (null? (peek-chars))
                             #f
                             (make-forminfo (peek-string) 
                                            indent-on-line-so-far
                                            line-count))
                         (peek-form-count)
                         (peek-next-forminfo)
                         next-line-forminfo
                         line-count))
    (define (found-it)
      (found-end-of-sexp last-line-forminfo
                         (if (null? (peek-chars))
                             #f
                             (make-forminfo (peek-string) 
                                            indent-on-line-so-far
                                            line-count))
                         (+ 1 (peek-form-count))
                         (peek-next-forminfo)
                         next-line-forminfo
                         line-count))
    (define (didnt-find-it)
      (values (list 0 0 #f)
              #f
              (peek-form-count)
              (peek-next-forminfo)
              next-line-forminfo))

    (letrec-syntax 
        ((dispatch
          (syntax-rules (whitespace else)
            ((_        "BUILD"       CS #f ES c (whitespace ws-exp) elems ...)
             (dispatch "BUILD"       CS (whitespace ws-exp) ES c elems ...))
            ((_        "BUILD"       CS #f #f c (else else-exp))
             (dispatch "GENER"       CS (whitespace else-exp) (else else-exp) c))
            ((_        "BUILD"       CS WS #f c (else else-exp))
             (dispatch "GENER"       CS WS (else else-exp) c))
            ((_        "BUILD" (CZ ...) WS ES c ((chars ...) cs-exp) elems ...)
             (dispatch "BUILD" (CZ ... ((chars ...) cs-exp)) WS ES c elems ...))
            ((_        "BUILD" (CZ ...) WS ES c (char c-exp) elems ...)
             (dispatch "BUILD" (CZ ... ((char) c-exp)) WS ES c elems ...))
            ((_ "GENER" 
                (((chars ...) char-exp) ...)
                (whitespace ws-exp)
                (else else-exp) 
                c)
             (cond
              ((memq c '(chars ...)) char-exp)
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
                        ,next-line-forminfo
                        ,last-line-forminfo
                        ,line-state))
          (newline))


        (cond 
         ((eof-object? c)      
          (didnt-find-it))
         (else
          (case (car curr-state)
            ((start)   
             (dispatch c 
                       (#\(    (if (zero? (peek-depth))
                                   (found-it-white)
                                   (pop-sexp 'start c)))
                       (#\)        (push-sexp   'start c))
                       (#\"        (next-new-id 'mbstr c))
                       (#\\        (next-new-id 'id-bs c))
                       (#\;        (reset-line))
                       (#\newline  (newer-line  'start))
                       (whitespace (next-white-again  'start))
                       (else       (next-new-id 'id c))))
            ((id)      
             (dispatch c 
                       (#\(    (if (zero? (peek-depth))
                                   (found-it)
                                   (pop-sexp    'start c)))
                       (#\)        (push-sexp   'start c))
                       (#\"        (next-new-id 'mbstr c))
                       (#\\        (next-new-id 'id-bs c))
                       (#\;        (reset-line))
                       (#\newline  (newer-line-from-form  'start))
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
                                   (found-it)
                                   (pop-sexp    'start c)))
                       (#\)        (push-sexp   'start c))
                       (#\"        (next-new-id 'mbstr c))
                       (#\\        (next-with-char 'mbstr c))
                       (#\;        (reset-line))
                       (#\newline  (newer-line-from-form  'start))
                       (whitespace (next-white  'start))
                       (else       (next-with-char 'id c))))
            ((str)     
             (dispatch c 
                       (#\"       (next-with-char 'mbstrend c))
                       (else      (next-with-char 'str c))))
            ((mbstrend)
             (dispatch c 
                       (#\(    (if (zero? (peek-depth))
                                   (found-it)
                                   (pop-sexp    'start c)))
                       (#\)        (push-sexp   'start c))
                       (#\"        (next-new-id 'mbstr c))
                       (#\\        (next-with-char 'mbstr c))
                       (#\;        (reset-line))
                       (#\newline  (newer-line-from-form  'start))
                       (whitespace (next-white  'start))
                       (else       (next-with-char 'id c))))
            ((id-bs)   
             (dispatch c
                       (#\(    (if (zero? (peek-depth))
                                   (found-it)
                                   (pop-sexp    'start c)))
                       (#\)        (push-sexp   'start c))
                       (#\"        (next-new-id 'mbstr c))
                       (#\\        (next-with-char 'id c))
                       (#\;        (reset-line))
                       (#\newline  (newer-line-from-form  'start))
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

(define (examine string) 
  (display string) (newline)
  (gather-indentation-data-from-port (reversed-string->input-port string)))

(define define-example-1
"
   (define 
")
;;  4 units to define keyword.  No further subforms.

(define define-example-2
"
   (define x 
")
;;  4 units to define keyword.
;; 11 units to x initial subform (4+|define|+1 = 11).  No further subforms.

(define define-example-3
"
   (define 
     x  
")
;;  4 units to define keyword
;;  5 units to x initial subform.  No further subforms.

(define define-example-4
"
   (define (foo)
")
;;  4 units to define keyword.
;; 11 units to (foo initial subform (4+|define|+1 = 11).  No further subforms.

(define define-example-5
"
   (define (foo x)
     (+ a b
        c
")
;;  6 units to + keyword (note that is the closest form to consider).
;;  8 units to a initial subform.
;;  8 units to c final subform.

(define define-example-6
"
   (define (foo x)
     (+ a b
        c)
")
;;  4 units to define keyword.
;; 11 units to (foo initial subform.
;;  5 units to (+ final subform.

(define define-example-7
"
   (define (foo x)
     (+ a b
        c)
     (- a
       b
")
;;  6 units to - keyword (note that it is the closest form to consider).
;;  8 units to a initial subform.
;;  7 units to b final subform.

(define define-example-8
"
   (define (foo x)
     (+ a b
        c)
          (- a
            b)
")
;;  4 units to define keyword.
;; 11 units to (foo initial subform.
;; 10 units to (- final subform.

(define case-example-1
"
        (case n            ;; K is case, n is S_1
          ((0) a) ((1) b)  ;; ((0) a) is S_2
          ((2) c) ((3) d)  ;; ((3) d) is S_5 = S_j
                           ;; P is the previous line (including indentation)
")
;;  9 units to case keyword
;; 14 units to n subform (9+|case|+1 = 14)
;; 10 units to ((2) c) form (first form on last non-trival line)

(define case-example-2
"
     (case     n        ;; K is case, n is S_1
       ((0) a) ((1) b)  ;; ((0) a) is S_2
       ((2) c) 
         ((3) d)        ;; ((3) d) is S_5 = S_j
                        ;; P is the previous line (including indentation)
")
;;  6 units to case keyword
;; 15 units to n subform (6+|case|+5 = 15)
;;  9 units to ((3) d) form (first form on last non-trivial line)

(define case-example-3
"
        (case              ;; K is case
 n                         ;; n is S_1
          ((0) a) ((1) b)  ;; ((0) a) is S_2
          ((2) c) 
            ((3) d)        ;; ((3) d) is S_5 = S_j
                           ;; P is the previous line (including indentation)
") 
;;  9 units to case keyword
;;  1 units to n subform (note that we only count from start of its own line)
;; 12 units to ((3) d) form (first form on last non-trivial line)

(define case-example-4
"
        (case              ;; K is case
 n                         ;; n is S_1
          ((0) a) ((1) b)  ;; ((0) a) is S_2
          ((2) c) 
            ((3) d) ((4    
                   e))
                           ;; P is the previous line (including indentation)
") 
;;  9 units to case keyword
;;  1 units to n subform (note that we only count from start of its own line)
;; 12 units to ((3) d) form (left-most form on last non-trivial line)


;; Examples:
(test "("                           1)
(test " ("                          2)
(test "( "                          1)
(test " ( "                         2)

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

(test "(cond "                      1)
(test "(cond ("                     7)
(test "(case"                       4)
(test "(case n"                     2)
(test "  (case n"                   4)
(test "(do"                         4)
(test "(do    ()"                   4)
(test "(do\n    ()"                 4)
(test "(do\n    ()\n    ()"         2)
(test "(do\n    ()\n    ()\n  a"    2)
(test "(do\n    ()\n    ()\n a"     1)
(test "(do\n    ()\n    ()\na"      0)

(test "(display \"\""               9)

;;; TEST TODO: 
;;; ----------
;;; [-] cond special form
;;; [ ] if special form
;;; [-] case special form
;;; [-] do special form
;;; [-] semicolon comments
;;; [ ] hashpipe comments
;;; [ ] hashsemicolon comments

