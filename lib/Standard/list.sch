; Useful list processing procedures.
; 2003-01-01 / lth
;
; $Id$

; (cons* a b ... k l) => (a b ... k . l)

(define (cons* x . args)
  (define (loop args)
    (if (null? (cdr args))
	(car args)
	(cons (car args) (loop (cdr args)))))
  (loop (cons x args)))


; (let ((l (list 'a 'b 'c 'd)))
;   (list-set! l 2 'x)
;   l)                           => (a b x d)

(define (list-set! l n v)
  (set-car! (list-tail l n) v))


; (list-head (a1 ... an) m) => (a1 ... am)   for m <= n

(define (list-head l n)
  (if (zero? n)
      '()
      (cons (car l) (list-head (cdr l) (- n 1)))))


; (safe-list-head (a1 ... an) m) = (list-head (a1 ... an) m) if n >= m
; (safe-list-head (a1 ... an) m) = (a1 ... an)               if n < m

(define (safe-list-head l n)
  (cond ((zero? n)       '())
        ((null? l)       '())
        (else            (cons (car l) (safe-list-head (cdr l) (- n 1))))))


; (safe-list-tail l m)             = (safe-list-tail l m '())
; (safe-list-tail (a1 ... an) m x) = (list-tail (a1 ... an) m) if n >= m
; (safe-list-tail (a1 ... an) m x) = x                         if n < m

(define (safe-list-tail l n . rest)
  (let ((x (if (null? rest)
               '()
               (car rest))))
    (let loop ((l l) (n n))
      (cond ((zero? n)       l)
            ((not (pair? l)) x)
            (else            (loop l (- n 1)))))))


; (list-insert x (a b ... k)) => (a x b x ... x k)

(define (list-insert x lst)

  (define (loop lst)
    (if (null? lst)
        lst
        (cons x (cons (car lst) (loop (cdr lst))))))

  (if (null? lst) 
      lst
      (cons (car lst) (loop (cdr lst)))))


; Assq, assv, assoc for association lists where the cdr of
; an entry is the key.

(define (reverse-assq key alist)
  (cond ((null? alist) #f)
        ((eq? (cdar alist) key) (car alist))
        (else (reverse-assq key (cdr alist)))))

(define (reverse-assv key alist)
  (cond ((null? alist) #f)
        ((eqv? (cdar alist) key) (car alist))
        (else (reverse-assv key (cdr alist)))))

(define (reverse-assoc key alist)
  (cond ((null? alist) #f)
        ((equal? (cdar alist) key) (car alist))
        (else (reverse-assoc key (cdr alist)))))


; Destructively remove from "alist" all associations with the matcing "key"

(define (assq-remove! key alist)
  (cond ((null? alist) alist)
	((eq? key (caar alist))
	 (aremq! key (cdr alist)))
	(else
	 (set-cdr! alist (aremq! key (cdr alist)))
	 alist)))

(define (assv-remove! key alist)
  (cond ((null? alist) alist)
	((eqv? key (caar alist))
	 (aremv! key (cdr alist)))
	(else
	 (set-cdr! alist (aremv! key (cdr alist)))
	 alist)))

(define (assoc-remove! key alist)
  (cond ((null? alist) alist)
	((equal? key (caar alist))
	 (aremove! key (cdr alist)))
	(else
	 (set-cdr! alist (aremove! key (cdr alist)))
	 alist)))


; (insert-ordered x (a b c ... k) p) => (a b x c ... k)
;   as long as (p a x) & (p b x) & (p x c)

(define (insert-ordered x l less?)
  (cond ((null? l) 
         (list x))
        ((less? (car l) x)
         (cons (car l) (insert-ordered x (cdr l) less?)))
        (else
         (cons x l))))


; (accumulate = '(1 1 2 4 4 2)) => ((1 1) (2) (4 4) (2))

(define (accumulate same? elts)

  (define (acc elts)
    (if (null? (cdr elts))
        (list (list (car elts)))
        (let ((x (acc (cdr elts))))
          (if (same? (car elts) (car (car x)))
              (cons (cons (car elts) (car x)) (cdr x))
              (cons (list (car elts)) x)))))

  (if (null? elts) 
    '()
    (acc elts)))


; (accumulate-unordered = '(1 1 2 4 4 2)) => ((1 1) (2 2) (4 4))

(define (accumulate-unordered same? elts)

  (define (acc elts buckets)
    (if (null? elts)
        (reverse buckets)
        (let* ((x (car elts))
               (probe 
                (let loop ((buckets buckets))
                  (cond ((null? buckets) #f)
                        ((same? x (caar buckets))
                         buckets)
                        (else
                         (loop (cdr buckets)))))))
          (if probe
              (begin (set-car! probe (cons x (car probe)))
                     (acc (cdr elts) buckets))
              (acc (cdr elts) (cons (list x) buckets))))))

  (acc elts '()))


; (partition odd? '(1 2 3 4 5 6))  =>  (1 3 5) (2 4 6)
; (partition even? '(1 2 3 4 5 6)) =>  (2 4 6) (1 3 5)

(define (partition in? list)
  (let loop ((in '()) (out '()) (list list))
    (cond ((null? list)
	   (values (reverse in) (reverse out)))
	  ((in? (car list))
	   (loop (cons (car list) in) out (cdr list)))
	  (else
	   (loop in (cons (car list) out) (cdr list))))))

; Remove subsequent duplicated elements, leaving the list in the original
; order.  (Note, argument order differs from same procedure in Larceny's
; Auxlib/list.sch but is consistent with rest of this file.)

(define (remove-duplicates same? list)
  (map car (accumulate-unordered same? list)))


; Return a list of elements of `list' selected by the predicate.

(define (filter select? list)
  (cond ((null? list) list)
	((select? (car list))
	 (cons (car list) (filter select? (cdr list))))
	(else
	 (filter select? (cdr list)))))


; Return the first element of `list' selected by the predicate.

(define (find selected? list)
  (cond ((null? list) #f)
	((selected? (car list)) (car list))
	(else (find selected? (cdr list)))))


; Map proc over lists and return all non-#f values.

(define (filter-map proc . lists)
  (filter (lambda (x) x)
          (apply map proc lists)))


; (mappend p l1 l2 ...) == (apply append (map p l1 ...))

(define (mappend p . ls)
  (apply append (apply map p ls)))


; (flatten '(a b (c (d e) f) (g h) i)) => (a b c d e f g h i)

(define (flatten l)

  (define (flatten l accumulator)
    (cond ((null? l) accumulator)
          ((pair? (car l))
           (flatten (cdr l) (flatten (car l) accumulator)))
          (else
           (flatten (cdr l) (cons (car l) accumulator)))))

  (reverse (flatten l '())))


; (least < '(2 1 3)) => 1

(define (least less? list)
  (reduce (lambda (a b) (if (less? a b) a b)) #f list))


; (greatest < '(2 1 3)) => 3

(define (greatest less? list)
  (reduce (lambda (a b) (if (less? a b) b a)) #f list))

  
; (iota n)  => (0 1 2 ... n-1)
; (iota1 n) => (1 2 3 ... n)

(define (iota n)
  (if (<= n 0)
      '()
      (let loop ((n (- n 1)) (r '()))
        (let ((r (cons n r)))
          (if (= n 0)
              r
              (loop (- n 1) r))))))

(define (iota1 n)
  (if (<= n 0)
      '()
      (let loop ((n n) (r '()))
        (let ((r (cons n r)))
          (if (= n 1)
              r
              (loop (- n 1) r))))))

(define (randomize-list ls)
  (let* ((v (list->vector ls))
	 (l (vector-length v)))
    (do ((i 0 (+ i 1)))
	((= i l) (vector->list v))
      (let ((k1 (random l))
	    (k2 (random l)))
	(let ((tmp (vector-ref v k1)))
	  (vector-set! v k1 (vector-ref v k2))
	  (vector-set! v k2 tmp))))))

; (map-leaves p '()) ->  ()
; (map-leaves p '(a (b (c)))) 
;   -> `(,(p a) (,(p b) (,(p c))))
;   for any non-pair a, b, c
; (map-leaves p '(a . ((b . ((c . d))) . e)))  
;   -> `(,(p a) . ((,(p b) . ((,(p c) . ,(p d)))) . ,(p e)))
;   for any non-pair a, b, c, and any non-pair, non-null d and e

(define (map-leaves p tree)

  (define (traverse tree)
    (cond ((pair? tree)
           (cons (traverse (car tree))
                 (if (null? (cdr tree))
                     '()
                     (traverse (cdr tree)))))
          (else
           (p tree))))

  (if (null? tree)
      tree
      (traverse tree)))


; Copies all pairs, stops at non-pairs, does not pay attention to
; shared structure (hence name).

(define (tree-copy tree)
  (if (not (pair? tree))
      tree
      (cons (tree-copy (car tree))
            (tree-copy (cdr tree)))))


; A common abbreviation.

(define (atom? x)
  (not (pair? x)))


; (make-circular '(a b c ... n)) => '(a b c ... n a b c ... n ...)
; Trivial, but often useful.

(define (make-circular l)
  (make-circular! (list-copy l)))


(define (make-circular! l)
  (if (null? l)
      (error "Can't make empty list circular.")
      (append! l l)))                   ; Take that, Shivers!


; General list merge procedure.  less? must implement a total order.
; merge-elts is applied to two elements that are deemed to be the
; same (neither is less than the other) and must return an element
; that will be inserted in the resulting list.
;
; FIXME: This implementation is O(n^2) and conses a lot.  We can do
; better on both counts.

(define (merge less? merge-elts lists)

  (define (merge a b)
    (cond ((null? b) a)
          ((null? a) b)
          (else
           (let loop ((a a) (b b) (r '()))
             (cond ((null? a) 
                    (append (reverse r) b))
                   ((null? b)
                    (append (reverse r) a))
                   ((less? (car a) (car b))
                    (loop (cdr a) b (cons (car a) r)))
                   ((less? (car b) (car a))
                    (loop a (cdr b) (cons (car b) r)))
                   (else
                    (loop (cdr a) 
                          (cdr b)
                          (cons (merge-elts (car a) (car b)) r))))))))

  (if (null? lists)
      '()
      (let loop ((ls lists))
        (if (null? (cdr ls))
            (car ls)
            (loop (cons (merge (car ls) (cadr ls))
                        (cddr ls)))))))


; Sublist

(define (sublist l n1 n2)
  (let loop ((l l) (n n1))
    (if (> n 0)
        (loop (cdr l) (- n 1))
        (let loop ((l l) (r '()) (n (- n2 n1)))
          (if (zero? n)
              (reverse r)
              (loop (cdr l) (cons (car l) r) (- n 1)))))))


; (sublist-matchq l1 l2)
; (sublist-matchv l1 l2)
; (sublist-match l1 l2 [=])
;
; Return a pair in l2 s.t. (list-head l2 (length l1)) == l1,
; where the comparison operator on the elements is eq?, eqv?, or equal?,
; or the user-specified one.

(define sublist-matchq)
(define sublist-matchv)
(define sublist-match)

(let ()

  (define (sublis l1 l2 same?)
    (if (null? l1) 
        l2
        (let outer ((l2 l2))
          (cond ((null? l2)
                 #f)
                ((same? (car l1) (car l2))
                 (let inner ((ll1 (cdr l1)) (ll2 (cdr l2)))
                   (cond ((null? ll1) l2)
                         ((null? ll2) #f)
                         ((same? (car ll1) (car ll2))
                          (inner (cdr ll1) (cdr ll2)))
                         (else
                          (outer (cdr l2))))))
                (else 
                 (outer (cdr l2)))))))

  (set! sublist-matchq (lambda (l1 l2) (sublis l1 l2 eq?)))
  (set! sublist-matchv (lambda (l1 l2) (sublis l1 l2 eqv?)))
  (set! sublist-match  (lambda (l1 l2 . rest)
                         (if (null? rest)
                             (sublis l1 l2 equal?)
                             (sublis l1 l2 (car rest)))))
  'sublist)


; (reduce p x ()) => x
; (reduce p x (a)) => a
; (reduce p x (a b ...)) => (p (p a b) ...))

(define (reduce proc initial l)

  (define (loop val l)
    (if (null? l)
        val
        (loop (proc val (car l)) (cdr l))))

  (cond ((null? l) initial)
	((null? (cdr l)) (car l))
	(else (loop (car l) (cdr l)))))


; (reduce-right p x ()) => x
; (reduce-right p x (a)) => a
; (reduce-right p x (a b ...)) => (p a (p b ...))

(define (reduce-right proc initial l)

  (define (loop l)
    (if (null? (cdr l))
	(car l)
	(proc (car l) (loop (cdr l)))))

  (cond ((null? l) initial)
	((null? (cdr l)) (car l))
	(else (loop l))))


; (fold-left p x (a b ...)) => (p (p (p x a) b) ...)

(define (fold-left proc initial l)
  (if (null? l)
      initial
      (fold-left proc (proc initial (car l)) (cdr l))))


; (fold-right p x (a b ...)) => (p a (p b (p ... x)))

(define (fold-right proc initial l)
  (if (null? l)
      initial
      (proc (car l) (fold-right proc initial (cdr l)))))

; eof
