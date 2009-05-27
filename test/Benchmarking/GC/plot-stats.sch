(load "parse-stats-output.sch")
(load "gnuplot.sch")
(require 'list)

;; An L[i,X] is a Listof[X] of length i.

;; plot-stacked-bars : L[k,String] Listof[L[k,Number]] ... -> unspecified
;; renders each arg of form ((a1 .. a_k) (b1 .. b_k) ...)
(define (plot-stacked-bars bar-names . data-args)
  (define (massage-data-arg data x-coord)
    (let* ((entries (cons '() ;; shifts bars over within group
                          data))
           (count (+ 1 (length entries)))
           (factor (/ 1 count)))
      (cons '() ;; breaks between data-args separate bar grps in plot
            (map (lambda (e c)
                   (cons (+ x-coord (* c factor)) e))
                 entries (iota count)))))
  (gnuplot;/keep-files
   (lambda (files) 
     `((set style fill pattern 1 border)
       (set xrange \[ 0 : * \] )
       (set yrange \[ 0 : * \] )
       (plot ,(list->vector 
               (map (lambda (bar-name i) 
                      `(,(vector-ref files 0)
                        using 1 : ,(+ i 2) with boxes
                        title ,bar-name))
                    (reverse bar-names)
                    (reverse (iota (length bar-names))))))))
   (apply append (map massage-data-arg 
                      data-args
                      (iota (length data-args))))))

(plot-stacked-bars 
 '("barf" "foo" "woof")
 '((10 12 13) (15 16 17) (17 17 18) (19 20 21) (30 31 32) (19 20 21))
 '((20 22 23) (25 26 27) (27 27 28))
 '((15 18 22) (24 25 28) (37 37 38))
 )

