; Tries
; 2002-03-18 / lth

; A trie is an assoc list.  The cdr of the cell is always a pair -- either
; another trie or a leaf.  Every leaf has a car that is a special flag
; value (selected when the trie is constructed), the cdr is then the
; value stored in the trie.
   
(require 'list)

; lists -> ( ( ( key ... ) . value ) ... )

(define (make-trie same? leaf-flag lists)
  (let-values (((leaves nonleaves) (partition (lambda (x) (null? (car x))) 
					      lists)))
    (append (if (null? leaves)
		'()
		(list (cons leaf-flag (cdar leaves))))
	    (map (lambda (xs)
		   (cons (caaar xs)
			 (make-trie same? 
				    leaf-flag 
				    (map (lambda (x) (cons (cdar x) (cdr x)))
					 xs))))
		 (accumulate-unordered (lambda (x y) 
					 (or (and (null? (car x)) 
						  (null? (car y)))
					     (same? (caar x) (caar y))))
				       nonleaves)))))

; Other interesting operations to contemplate:
;   make-empty-trie
;   trie-insert
;   trie-remove
;   trie-search
;   trie-search-incremental

(define trie-example
  `(((#\+) . +) 
    ((#\+ #\+) . ,(string->symbol "++"))
    ((#\+ #\=) . ,(string->symbol "+="))
    ((#\+ #\@) . ,(string->symbol "+@"))
    ((#\-) . -)
    ((#\- #\-) . ,(string->symbol "--"))
    ((#\+ #\+ #\+) . ,(string->symbol "+++"))))

