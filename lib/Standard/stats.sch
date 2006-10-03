; Useful functions to operate on sets of data (often numbers)

(require 'list)

; (sum numbers) == (apply + numbers)
; (sum fn l1 l2 ...) == (apply + (map fn l1 l2 ...))

(define (sum a1 . args)
  (if (null? args) 
      (apply + a1)
      (apply + (apply map a1 args))))


(define (square x) (* x x))


(define (mean numbers)
  (/ (sum numbers) (length numbers)))


(define (weighted-mean data get-val get-weight)
  (/ (sum (lambda (x) (* (get-val x) (get-weight x))) data) 
     (sum get-weight data)))


(define (median numbers)
  (let ((n (length numbers)))
    (if (odd? n)
        (list-ref numbers (quotient n 2))
        (/ (+ (list-ref numbers (quotient n 2))
              (list-ref numbers (- (quotient n 2) 1)))
           2))))


(define (standard-deviation numbers)
  (let ((avg (mean numbers)))
    (sqrt (/ (sum (lambda (x) (square (- x avg))) numbers)
             (length numbers)))))


; Given a list of data, and functions to extract the X and Y values 
; from each datum, return two values a and b such that Y=a+bX is the
; least-squares regression line through the data points.  (That is,
; it returns y-intercept and slope _in that order_.)

(define (least-squares data get-x get-y)
  (let ((n  (length data))
        (xs (map get-x data))
        (ys (map get-y data)))
    (let* ((b (/ (- (* n (sum * xs ys)) (* (sum xs) (sum ys)))
                 (- (* n (sum square xs)) (square (sum xs)))))
           (a (/ (- (sum ys) (* b (sum xs)))
                 n)))
      (values a b))))


; Return the data in the interquartile range from a data set 
; sorted in some order (think nondecreasing).

(define (interquartile datum-value data)
  (let ((total (sum datum-value data)))
    (let loop ((sum 0) (data data))
      (if (< (+ sum (datum-value (car data))) (* 0.25 total))
          (loop (+ sum (datum-value (car data))) (cdr data))
          (let ((start data))
            (let loop ((sum sum) (data data) (n 0))
              (if (> sum (* 0.75 total))
                  (list-head start n)
                  (loop (+ sum (datum-value (car data)))
                        (cdr data) 
                        (+ n 1)))))))))


(define (sort-by-euclidean-distance-from-origin data get-x get-y)
  (sort data
        (lambda (a b)
          (< (sqrt (+ (square (get-x a)) (square (get-y a))))
             (sqrt (+ (square (get-x b)) (square (get-y b))))))))


(define (sort-by-x-and-y data get-x get-y)
  (sort data
        (lambda (a b)
          (or (< (get-x a) (get-x b))
              (and (= (get-x a) (get-x b))
                   (< (get-y a) (get-y b)))))))

; eof
