(load "parse-stats-output.sch")
(load "gnuplot.sch")
(require 'list)

;; An L[i,X] is a Listof[X] of length i.

;; plot-stacked-bars
;;     : L[k,String] Listof[(cons String L[k,Number])] ... -> unspecified
;; renders each arg of form ((a1 .. a_k) (b1 .. b_k) ...)
(define (plot-stacked-bars bar-names . data-args)
  (define (massage-data-arg data bar-x-coord)
    (let* ((entries (append ; '(("")) ;; shifts bars over within group
                            data))
           (count (+ 1 (length entries)))
           (factor (/ 1 count))
           (numbers
            (map (lambda (e c)
                   (let ((x-coord (+ bar-x-coord (* c factor)))
                         (data-w/o-xtic (cdr e)))
                     (cons x-coord data-w/o-xtic)))
                 entries (map (lambda (i) (+ i 1)) (iota count))))
           (x-tics (map (lambda (e n)
                          (let ((xtic (car e)) (x-coord (car n)))
                            (list xtic x-coord)))
                        entries numbers)))
      (list (cons '() ;; breaks between data-args separate bar grps in plot
                  numbers)
            x-tics)))
  (let* ((count (length data-args))
         (max-group-width (+ 1 (apply max (map length data-args))))
         (data-and-xtics 
          (map massage-data-arg data-args (iota count)))
         (data-vals (apply append (map car data-and-xtics)))
         (xtics (apply append (map cadr data-and-xtics))))
    (gnuplot/keep-files
     (lambda (data-file) 
       `((set style fill pattern 1 border)
         (set xrange \[ 0 : ,count \] )
         (set yrange \[ 0 : * \] )
         (set boxwidth ,(inexact (min 0.5 (/ 1 max-group-width))))
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

;; render-mmu2 :              -> Listof[(list Nat Nat Nat Nat Nat Nat Nat)]
;; render-mmu2 : MmuLogVector -> Listof[(list Nat Nat Nat Nat Nat Nat Nat)]
;; Each entry is (Window MinMut MaxMgr MaxMinor MaxMajor MaxSumz MaxRefine)
(define (render-mmu2 . args)
  (define (extract w real/cpu min/max category)
    (let* ((cat   (assq category (cdr w)))
           (times (assq min/max  (cdr cat)))
           (time  (cadr (memq real/cpu (cadr times)))))
      time))
  (define (window->stats w)
    (assert (eq? 'window (car w)))
    (let* ((window-size (cadr (assq 'size (cdr w))))
           (norm (lambda (t) (inexact (/ t window-size)))))
      (list window-size 
            (norm (extract w 'real 'min 'mutator))
            (norm (extract w 'real 'max 'memmgr))
            (norm (extract w 'real 'max 'minorgc))
            (norm (extract w 'real 'max 'majorgc))
            (norm (extract w 'real 'max 'summarize))
            (norm (extract w 'real 'max 'smircy)))))
  (define (mmu-log->window-data mmu-log)
    (map window->stats (cdr (vector->list mmu-log))))

  (let ((log (if (null? args) (extract-mmu) (car args))))
    (mmu-log->window-data log)))

;; render-mmu2 :              -> unspecified
;; render-mmu2 : MmuLogVector -> unspecified
(define (plot-mmu2 . args)
  (gnuplot (lambda (file)
             (define (min-line col title)
               `(,file using 1 : ,col 
                       axis x1y1
                       with lines 
                       title ,title))
             (define (max-line col title)
               `(,file using 1 : ,col 
                       axis x1y1
                       with linespoints
                       title ,title))
             `((set logscale x)
               (set yrange  \[ 0 : 1 \] )
               (set y2range \[ 1 : 0 \] )
               (plot #(,(min-line 2 "min mutator")
                       ,(max-line 3 "max misc")
                       ,(max-line 4 "max minor gc")
                       ,(max-line 5 "max major gc")
                       ,(max-line 6 "max summarize")
                       ,(max-line 7 "max rs refine")))))
           (apply render-mmu2 args)))


;; A StackedBarSexp is a 
;;   (cons L[k,String] Listof[(cons String L[k,Number])])
;; where
;; An L[i,X] is a Listof[X] of length i.
;; 
;; see contract of plot-stacked-bars for a use of this class of data

;; However, StackedBarSexp is not what I ended up using below for
;; render-memory-usage and plot-memory-usage, b/c plot-stacked-bars is
;; intended for plotting the combination of many results.
;; 
;; One could use render-memory-usage after each benchmark run, writing
;; the s-exp result to a file, and then combine all of the results
;; (and pass that to plot-stacked-bars) after all desired benchmarks
;; have been run.

;; render-memory-usage : GclibStatVector -> (list L[k,String] L[k,Number])
;; render-memory-usage :                 -> (list L[k,String] L[k,Number])
(define (render-memory-usage . args)
  (let* ((vec (if (null? args) (extract-gclib-memstats) (car args)))
         (lst (vector->list vec))
         (get (lambda (key) (cadr (memq key lst)))))
    (let ((heap-max   (get 'heap_allocated_peak))
          (remset-max (get 'remset_allocated_peak))
          (summ-max   (get 'summ_allocated_peak))
          (smircy-max (get 'smircy_allocated_peak))
          (rts-max    (get 'rts_allocated_peak))
          (frag-max   (get 'heap_fragmentation_peak))
          (mem-max    (get 'mem_allocated_max))
          (accum*vals (let ((tot 0) (vals '()))
                        (list (lambda (n) 
                                (set! tot (+ tot n)) 
                                (set! vals (cons tot vals)))
                              (lambda () (reverse vals))))))
      (let* ((accum! (car accum*vals))
             (vallst (cadr accum*vals)))
        (for-each accum! 
                  (list heap-max rts-max remset-max
                        summ-max smircy-max frag-max))
        (list '("heap" "runtime" "remset"
                "summaries" "marker" "waste" "total")
              (append (vallst) (list mem-max)))))))

;; plot-memory-usage : GclibStatVector -> unspecified
;; plot-memory-usage :                 -> unspecified
(define (plot-memory-usage . args)
  (let ((rendered (apply render-memory-usage args)))
    (plot-stacked-bars (car rendered)
                       (list (cons "" (cadr rendered))))))
