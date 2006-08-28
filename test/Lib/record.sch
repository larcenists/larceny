; Record tests

(define pt2 (make-record-type "pt2" '(x y)))

(define (access-with-pt2 a b c d e)
  (display (list (list ((record-accessor pt2 'x) a)
                       ((record-accessor pt2 'y) a))
                 (list ((record-accessor pt2 'x) b)
                       ((record-accessor pt2 'y) b))
                 (list ((record-accessor pt2 'x) c)
                       ((record-accessor pt2 'y) c))
                 (list ((record-accessor pt2 'x) d)
                       ((record-accessor pt2 'y) d))
                 (list ((record-accessor pt2 'x) e)
                       ((record-accessor pt2 'y) e))))
  (newline))


(define a ((record-constructor pt2) 2 3))
(define b ((record-constructor pt2 '(x)) 2))
(define c ((record-constructor pt2 '(y)) 3))
(define d ((record-constructor pt2 '(x y)) 2 3))
(define e ((record-constructor pt2 '(y x)) 3 2))

(access-with-pt2 a b c d e)

((record-updater pt2 'x) a 4)
((record-updater pt2 'y) a 5)

(access-with-pt2 a b c d e)

; Extended type

(define pt3 (make-record-type "pt3" '(z) pt2))

(define (access-with-pt3 a b c d e f)
  (display (list (list ((record-accessor pt3 'x) a)
                       ((record-accessor pt3 'y) a)
                       ((record-accessor pt3 'z) a))
                 (list ((record-accessor pt3 'x) b)
                       ((record-accessor pt3 'y) b)
                       ((record-accessor pt3 'z) b))
                 (list ((record-accessor pt3 'x) c)
                       ((record-accessor pt3 'y) c)
                       ((record-accessor pt3 'z) c))
                 (list ((record-accessor pt3 'x) d)
                       ((record-accessor pt3 'y) d)
                       ((record-accessor pt3 'z) d))
                 (list ((record-accessor pt3 'x) e)
                       ((record-accessor pt3 'y) e)
                       ((record-accessor pt3 'z) e))
                 (list ((record-accessor pt3 'x) f)
                       ((record-accessor pt3 'y) f)
                       ((record-accessor pt3 'z) f))))
  (newline))

(define a ((record-constructor pt3) 2 3 4))
(define b ((record-constructor pt3 '(x)) 2))
(define c ((record-constructor pt3 '(y)) 3))
(define d ((record-constructor pt3 '(z)) 4))
(define e ((record-constructor pt3 '(x y)) 2 3))
(define f ((record-constructor pt3 '(z x)) 4 2))

(access-with-pt3 a b c d e f)
(access-with-pt2 a b c d e)

((record-updater pt3 'x) a 5)
((record-updater pt3 'y) a 6)
((record-updater pt3 'z) a 7)

((record-updater pt2 'x) b 5)
((record-updater pt2 'y) b 6)

(access-with-pt3 a b c d e f)


(display
 `((,(record-type-descriptor? pt2) #t)
   (,(record-type-descriptor? pt3) #t)
   (,(record-type-descriptor? "supercallifragilistic") #f)
   (,(record-type-descriptor? '#(1 2 3)) #f)
   (,(record-type-field-names pt2) '(x y))
   (,(record-type-field-names pt3) '(x y z))
   (,(record-type-name pt2) "pt2")
   (,(record-type-name pt3) "pt3")
   (,(record-type-extends? pt2 pt3) #f)
   (,(record-type-extends? pt3 pt2) #t)
   (,(record-type-extends? pt3 (make-record-type "newpt" '(z) pt2)) #f)
   (,(record? a) #t)
   (,(eq? (record-type-descriptor a) pt3) #t)
   (,(eq? (record-type-descriptor a) pt2) #f)
   (,((record-predicate pt2) a) #t)
   (,((record-predicate pt3) a) #t)
   (,((record-predicate pt3) ((record-constructor pt2) 1 2)) #f)))
(newline)

