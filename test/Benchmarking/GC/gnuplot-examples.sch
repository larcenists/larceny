(load "gnuplot.sch")

;; Example 34.1.4 from gnuplot-4.2.3 manual
;; (illustrates data entry format)
(gnuplot
 (lambda (file) `((pop (x) = 103 * exp((1965 - x) / 10))
                   ;; one way:
                   ;;   (plot \[ 1960 : 1990 \] ,file \, pop(x))
                   ;; but I like below better.
                   (plot \[ 1960 : 1990 \] ,file)
                   (replot pop(x))))
 '((\# Gnu population in Antarctica since 1965)
   (1965 103)
   (1970 55)
   (1975 34)
   (1980 24)
   (1985 10)))

;; Illustration of index keyword (34.1.5 from gnuplot-4.2.3 manual)
(gnuplot
 (lambda (file) `((plot ,file index 2 : 4 
                     \, ,file index 3 : 5 )))
 (let* ((zero..four (iota 5)) ;; generate fake data to index and graph
        (row1 (map (lambda (n) (list (+ (* 1.0 n) 0.0) (+  3 n))) zero..four))
        (row2 (map (lambda (n) (list (+ (* 1.0 n) 0.1) (+  6 n))) zero..four))
        (row3 (map (lambda (n) (list (+ (* 1.0 n) 0.2) (+  9 n))) zero..four))
        (row4 (map (lambda (n) (list (+ (* 1.0 n) 0.3) (+ 12 n))) zero..four))
        (row5 (map (lambda (n) (list (+ (* 1.0 n) 0.4) (+ 15 n))) zero..four))
        (row6 (map (lambda (n) (list (+ (* 1.0 n) 0.5) (+ 18 n))) zero..four))
        (row7 (map (lambda (n) (list (+ (* 1.0 n) 0.6) (+ 21 n))) zero..four))
        (row8 (map (lambda (n) (list (+ (* 1.0 n) 0.7) (+ 24 n))) zero..four))
        (row9 (map (lambda (n) (list (+ (* 1.0 n) 0.8) (+ 27 n))) zero..four))
        )
   `(,@row1 () () ,@row2 () () ,@row3 () () ,@row4 () () ,@row5 () () 
     ,@row6 () () ,@row7 () () ,@row8 () () ,@row9)))

;; Illustration of *ticlabels (34.1.9.2 from gnuplot-4.2.3 manual)
(gnuplot
 (lambda (files) `((splot ,files 
                          using 2 : 4 : 6 : xtic(1) : ytic(3) : ztic(6) )))
 '(("a" 102 "A" 15 16 18 19)
   (20   22 "B"  25 26 28 29)
   (30   32 34 35 36 38 39)
   ("c"  42 "C" 45 46 48 49)
   (50   52 "D" 55 56 58 59)
   ("e"  62 64 65 66 68 69)))

;; Illustration of errorbars (34.2 from gnuplot-4.2.3 manual)
;; (vectors are rendered as comma-separated sequences)
(gnuplot
 (lambda (file0 file1 file2) `((plot 
                                \[ 1960 : 1990 \] 
                                #((,file0 with errorbars)
                                  (,file1 with xerrorbars)
                                  (,file2 with xyerrorbars)))))
 '((1965 103 5)
   (1970 55  48 57)
   (1975 34  5)
   (1980 24  3))
 '((1966 103 5)
   (1971 55  48 57)
   (1976 34  3)
   (1981 24 10)
   (1986 10  1985 1988 9 12))
 '((1967 103 5 8) ;; xyerrorbars only accepts 4 column input
   (1972 55  48 57)
   (1977 34  3 4)
   (1982 24 10 12)
   (1987 10  3 4)))

;; Illustration of errorlines (34.3 from gnuplot-4.2.3 manual)
(gnuplot
 (lambda (file0 file1 file2) `((plot 
                                \[ 1960 : 1990 \] 
                                #((,file0 with errorlines)
                                  (,file1 with xerrorlines)
                                  (,file2 with xyerrorlines)))))
 '((1965 103 5)
   (1970 55  48 57)
   (1975 34  5)
   (1980 24  3))
 '((1966 103 5)
   (1971 55  48 57)
   (1976 34  3)
   (1981 24 10)
   (1986 10  1985 1988 9 12))
 '((1967 103 5 8) ;; xyerrorlines only accepts 4 column input
   (1972 55  48 57)
   (1977 34  3 4)
   (1982 24 10 12)
   (1987 10  3 4)))

;; Illustration of parametric mode
;; (vectors are rendered as comma-separated sequences)
(gnuplot
 (lambda () `((set parametric) 
              (plot #( (sin(t))
                       (t**2)    )))))
(gnuplot
 (lambda () `((set parametric) 
              (splot #( (cos(u)*cos(v))
                        (cos(u)*sin(v))
                        (sin(u))         )))))

;; Illustrations of range specifications (34.5 from gnuplot-4.2.3 manual)
(gnuplot
 (lambda () `((plot cos(x)))))
(gnuplot
 (lambda () `((plot \[ -10 : 30 \] sin(pi*x) / (pi*x)))))
(gnuplot
 (lambda () `((plot \[ t = -10 : 30 \] sin(pi*t) / (pi*t)))))
(gnuplot
 (lambda () `((plot \[ - pi : pi \] \[ -3 : 3 \] 
                    #( (tan(x)) 
                       (1 / x)                   )))))
(gnuplot
 (lambda () `((plot \[ \] \[ - 2 : sin(5)*-8 \] 
                    sin(x)**besj0(x)             ))))
(gnuplot
 (lambda () `((plot \[ :200\] \[- pi : \] 
                    exp(sin(x))))))
;; (there's an example involving timefmt and time series data,
;;  but Felix can't get something plausible going so he's 
;;  skipping it.)

;; Illustrations of with keyword (34.7 from gnuplot-4.2.3 manual)
(gnuplot
 (lambda () `((plot sin(x) with impulses))))
(gnuplot
 (lambda () `((plot #((x with points) (x**2))))))
(gnuplot
 (lambda (data) `((plot \[ \] \[ -2 : 5 \] 
                        #((tan(x))
                          (,data with lines)      ))))
 '(1 1 2 4 1))
(gnuplot 
 (lambda (data) `((plot ,data with impulses)))
 '(1 1 2 4 1 0.5))
(gnuplot
 (lambda (data) `((plot ,data with boxes)))
 '(1 1 2 4 1 0.5))
(gnuplot (lambda (data) `((plot #((,data with lines)
                                  (,data notitle with errorbars)))))
         '((0 10  1)
           (1 10  1)
           (2 20  1)
           (3 40  1) 
           (4 10  1) 
           (5 5.5 1)))
(gnuplot (lambda (data) `((plot ,data with errorlines)))
         '((0 10  1)
           (1 10  1)
           (2 20  1)
           (3 40  1) 
           (4 10  1) 
           (5 5.5 1)))
(gnuplot 
 (lambda () `((plot #((sin(x) 
                          with linespoints linetype 1 pointtype 3)
                      (cos(x) 
                          with linespoints linetype 1 pointtype 4))
                    ))))
(gnuplot
 (lambda (data) `((plot ,data with points pointtype 3 pointsize 2)))
 '(1 1 2 4 1 0.5))
(gnuplot (lambda (data) `((plot ,data using \1:2:4 
                                with points pointtype 5 pointsize variable)))
         '((0 10  1 1)
           (1 10  1 2)
           (2 20  1 3)
           (3 40  1 2) 
           (4 10  1 1) 
           (5 5.5 1 2)))
(gnuplot (lambda (d1 d2) `((plot 
                            #((,d1 title "good" 
                                   with lines linetype 2 linewidth 3)
                              (,d2 title "bad"
                                   with lines linetype 2 linewidth 1)   ))))
         '(12  8 22 42  8 5.5)
         '(10 10 20 40 10 8.5))
(gnuplot (lambda () `((plot #((x*x with filledcurve closed)
                              ( 40 with filledcurve y1=10)   )))))
(gnuplot (lambda () `((plot #((x*x)
                              ((x >= -5 && x <= 5 ? 40 : 1 / 0)
                               with filledcurve y1=10 linetype 8))))))
(gnuplot (lambda () `((splot x*x - y*y with line palette))))
(gnuplot (lambda () `((splot #((x*x - y*y with pm3d)
                               (x*x + y*y with pm3d at t)    )))))

;; Illustrations of set arrow (43.2 from gnuplot-4.2.3 manual)
(gnuplot
 (lambda (file) `((set arrow to #(1 2) linestyle 5)
                  (plot \[ -10 : 10 \] \[ 0 : \] ,file)))
 '(103 55 34 24 10))
(gnuplot
 (lambda (file) `((set arrow 3 from graph #(0 0) to #(-5 5 3))
                  (plot \[ -10 : 10 \] \[ 0 : \] ,file)))
 '(103 55 34 24 10))
(gnuplot
 (lambda (file) `((set arrow 3 from graph #(0 0) to #(-5 5 3))
                  (set arrow 3 to #(1 1 1) nohead linewidth 2)
                  (plot \[ -10 : 10 \] \[ 0 : \] ,file)))
 '(103 55 34 24 10))
(gnuplot
 (lambda (file) `((set arrow from #(3 (graph 0)) to #(3 (graph 1)) nohead)
                  (plot \[ -10 : 10 \] \[ 0 : \] ,file)))
 '(103 55 34 24 10))
(gnuplot
 (lambda (file) `((set arrow 3 from #(0 -5) to #(0 5) 
                       heads size screen #(0.1 90))
                  (plot \[ -10 : 10 \] \[ 0 : \] ,file)))
 '(103 55 34 24 10))
(gnuplot
 (lambda (file) `((set arrow from #(0 -5) rto graph #(0.1 0.1))
                  (plot \[ 0 : 10 \] \[ 0 : \] ,file)))
 '(103 55 34 24 10))

;; below is meant to illustrate log-scaling of just x axis, but 
;; it does not seem to work as I intended...
(gnuplot
 (lambda (file) `((set logscale x)
                  (set arrow from #(100 -5) rto graph #(10 10))
                  (plot \[ 1 : 10000 \] \[ 0 : \] ,file)
                  (replot log(x))
                  ))
 '(103 55 34 24 10))
