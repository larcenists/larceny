(load "parse-stats-output.sch")
(load "gnuplot.sch")
(require 'list)

;; An L[i,X] is a Listof[X] of length i.

;; plot-stacked-bars
;;     : L[k,String] Listof[(cons String L[k,Number])] ... -> unspecified
;; renders each arg of form ((a1 .. a_k) (b1 .. b_k) ...)
(define (plot-stacked-bars bar-names . data-args)
  (define (massage-data-arg data bar-x-coord)
    (let* ((entries (append '(("")) ;; shifts bars over within group
                            data))
           (count (+ 1 (length entries)))
           (factor (/ 1 count))
           (numbers
            (map (lambda (e c)
                   (let ((x-coord (+ bar-x-coord (* c factor)))
                         (data-w/o-xtic (cdr e)))
                     (cons x-coord data-w/o-xtic)))
                 entries (iota count)))
           (x-tics (map (lambda (e n)
                          (let ((xtic (car e)) (x-coord (car n)))
                            (list xtic x-coord)))
                        entries numbers)))
      (list (cons '() ;; breaks between data-args separate bar grps in plot
                  numbers)
            x-tics)))
  (let* ((data-and-xtics 
          (map massage-data-arg data-args (iota (length data-args))))
         (data-vals (apply append (map car data-and-xtics)))
         (xtics (apply append (map cadr data-and-xtics))))
    (gnuplot/keep-files
     (lambda (data-file) 
       `((set style fill pattern 1 border)
         (set xrange \[ 0 : * \] )
         (set yrange \[ 0 : * \] )
         (set xtics rotate (,(list->vector xtics)))
         (plot ,(list->vector 
                 (map (lambda (bar-name i) 
                        `(,data-file
                          using 1 : ,(+ i 2) with boxes
                          title ,bar-name))
                      (reverse bar-names)
                      (reverse (iota (length bar-names))))))))
     data-vals)
    xtics))

(plot-stacked-bars 
 '("barf" "foo" "woof")
 '(("a1" 10 12 13) ("a2" 15 16 17) ("a3" 17 17 18) ("a4" 19 20 21)
   ("a5" 30 31 32) ("a6" 19 20 21))
 '(("b1" 20 22 23) ("b2" 25 26 27) ("b3" 27 27 28))
 '(("c1" 15 18 22) ("c2" 24 25 28) ("c3" 37 37 38))
 )

(define (plot-mmu . args)
  (gnuplot (lambda files
             `((plot ,(list->vector (map (lambda (file) `(,file with lines))
                                         files)))))
           (apply render-mmu args)))
