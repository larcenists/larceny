; Enumeration set tests.

(define (run-enumset-tests)
  (display "Enumeration sets") (newline)
  (basic-enumset-tests))

(define (basic-enumset-tests)

  (let* ((colors
          (make-enumeration '(black white purple maroon)))
         (color-index (enum-set-indexer colors))
         (make-color-set (enum-set-constructor colors))
         (test (lambda (n x => y)
                 (test (string-append "enumset" (number->string n)) x y)))
         (=> 'ignored))

    (allof

     (test 1 (enum-set=? colors (enum-set-universe colors)) => #t)
     (test 2 (color-index 'purple) => 2)
     (test 3 (enum-set->list (make-color-set '(black purple)))
             => '(black purple))
     (test 4 (enum-set-member? 'white (make-color-set '(white maroon)))
             => #t)
     (test 5 (enum-set-subset? (enum-set-complement colors) colors)
             => #t)
     (test 6 (enum-set=? (make-color-set '(black maroon))
                         (enum-set-complement
                          (make-color-set '(white purple))))
             => #t)
     (test 7 (enum-set-subset? (make-color-set '(white))
                               (make-enumeration
                                '(black white red green)))
             => #f)
     (test 8 (enum-set=? (make-color-set '(black white))
                         ((enum-set-constructor
                           (make-enumeration '(black white red green)))
                          '(black white)))
             => #f)
     (test 9 (enum-set-subset? (make-color-set '(white))
                               (make-enumeration
                                '(maroon black purple red green white)))
             => #t)
     (test 10 (enum-set=? (make-color-set '(black white))
                          ((enum-set-constructor
                            (make-enumeration
                             '(maroon black purple red green white)))
                           '(black white)))
              => #f)
     (test 11 (enum-set=? (make-color-set '(black white))
                          ((enum-set-constructor
                            (make-enumeration
                             '(maroon black purple white)))
                           '(black white)))
              => #t)
     (test 12 (enum-set->list (enum-set-projection
                               (make-enumeration '(black white red green))
                               colors))
              => '(black white))
    )

   #t))
