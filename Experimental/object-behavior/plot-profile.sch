; Have to be a little more careful here if there are members of a
; generation that fall out completely -- colors should not be
; reassigned.

(define colors '(#\@ #\# #\% #\$))

(define (plot-profile data)
  (let* ((colors (let ((colors (append colors '())))
                   (set-cdr! (last-pair colors) colors)
                   colors))
         (age-data
          (let loop ((age 0) (data data) (age-data '()))
            (if (null? data)
                (reverse age-data)
                (let ((this-age-data (filter (lambda (x) (= (car x) age)) data)))
                  (loop (+ age 1)
                        (filter (lambda (x) (not (= (car x) age))) data)
                        (cons this-age-data age-data))))))
         (highest-pop (apply max (map (lambda (a) (apply + (map caddr a))) age-data)))
         (cols 120)
         (divisor (quotient highest-pop cols)))
    (do ((age-data age-data (cdr age-data)))
        ((null? age-data))
      (plot-one-age (car age-data) colors divisor))))

(define (plot-one-age data colors divisor)
  (let ((data (sort data (lambda (a b) (> (cadr a) (cadr b))))))
    (do ((data data (cdr data))
         (colors colors (cdr colors)))
        ((null? data))
      (let ((n (max 1 (quotient (caddr (car data)) divisor))))
        (display (make-string n (car colors)))))
    (newline)))
