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
 '((1967 103 5)
   (1972 55  48 57)
   (1977 34  3)
   (1982 24 10)
   (1987 10  1986 1989 9 12)))

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
 '((1967 103 5)
   (1972 55  48 57)
   (1977 34  3)
   (1982 24 10)
   (1987 10  1986 1989 9 12)))

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
