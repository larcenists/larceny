; An n-to-n map
;
; (nmap p l1 ... ln) -> l1' ... ln'
;
; (nmap (lambda xs (apply values (map - xs))) '(1 2 3) '(10 20 30)) 
;    => (-1 -2 -3) 
;       (-10 -20 -30)
;
(define (nmap p l1 . lists)

  (define (loop lists results)
    (if (null? (car lists))
        (apply values (map reverse results))
        (call-with-values
         (lambda () (apply p (map car lists)))
         (lambda vs
           (loop (map cdr lists)
                 (map cons vs results))))))

  (loop (cons l1 lists) (make-list (+ (length lists) 1) '())))


; An n-to-m map, returns #f on empty input.
; Ugly name, though.
;
; (nm-map (lambda xs (values (map + xs) (map - xs) (map * xs)))
;         '(1 2 3)
;         '(10 20 30)) 
;    => (11 22 33) 
;       (-9 -18 -27)
;       (10 40 90)

(define (nm-map p l1 . lists)

  (define (loop lists results)
    (if (null? (car lists))
        (apply values (map reverse results))
        (call-with-values
         (lambda () (apply p (map car lists)))
         (lambda vs
           (loop (map cdr lists)
                 (map cons vs results))))))

  (if (null? l1)
      #f
      (let ((lists (cons l1 lists)))
        (call-with-values
         (lambda () (apply p (map car lists)))
         (lambda vs
           (loop (map cdr lists)
                 (map list vs)))))))

; Return a list with proc applied to every pair of elements from
; l1 and l2.

(define (cross-map proc l1 l2)
  (let loop ((l1 l1) (r '()))
    (cond ((null? l1)
           (apply append (reverse r)))
          (else
           (let loop2 ((l2 l2) (s '()))
             (cond ((null? l2)
                    (loop (cdr l1) (cons (reverse s) r)))
                   (else
                    (loop2 (cdr l2) (cons (proc (car l1) (car l2)) s)))))))))


; Same as above.

(define (cross-map2 proc l1 l2)
  (mappend (lambda (x)
	     (map (lambda (y) (proc x y)) l2))
	   l1))

; eof
