(cond ((file-exists? "gnuplot.sch") 
       (load "gnuplot.sch"))
      ((file-exists? "../gnuplot.sch")
       (load "../gnuplot.sch"))
      ;; o/w onus is on client to find and load gnuplot-glue helper code
      )

'(define earley10-b4-infamy-data 
   (extract-scan-lines "before-infamy/remset-earley10.txt"))
'(plot-times earley10-b4-infamy-data)
'(plot-slots earley10-b4-infamy-data)

'(plot-times+slots-comparing-infamy
  "earley10" 
  (extract-scan-lines "before-infamy/remset-earley10.txt")
  (extract-scan-lines "with-infamy/remset-earley10.txt"))

'(plot-times+slots-comparing-infamy
  "earley13" 
  (extract-scan-lines "before-infamy/remset-earley13.txt")
  (extract-scan-lines "with-infamy/remset-earley13.txt"))

'(for-each plot-times+slots-comparing-infamy/infer
           '("earley10" "earley13" "gcbench" "nboyer" "sboyer" 
             "perm9" "twobit" "gcold0" "gcold1k" "queue" "pueue"))

'(for-each (lambda (path) 
             (let ((title-string (path->benchmark-name path))
                   (d (extract-scan-lines path)))
               (plot-times+slots (string-append "Scan Times : " title-string) 
                                 (string-append "Scan Slots : " title-string) 
                                 d)))
           datafile-paths)

(define (path->benchmark-name p)
  (let* ((plst (string->list p))
         (plen (string-length p))
         ;; (start & finis strongly tied to patterns seen below)
         (suffix-start 0)
         (suffix-finis (- plen (length (memv #\/ plst))))
         (bmname-start (- plen (- (length (memv #\/ plst)) 8))) 
         (bmname-finis (- plen 4)))
    (string-append (substring p bmname-start bmname-finis)
                   " "
                   (substring p suffix-start suffix-finis))))

(define datafile-paths
  '("before-infamy/remset-earley10.txt"
    "before-infamy/remset-earley13.txt"
    "before-infamy/remset-gcbench.txt"
    "before-infamy/remset-gcold0.txt"
    "before-infamy/remset-gcold1k.txt"
    "before-infamy/remset-nboyer.txt"
    "before-infamy/remset-perm9.txt"
    "before-infamy/remset-pueue.txt"
    "before-infamy/remset-queue.txt"
    "before-infamy/remset-sboyer.txt"
    "before-infamy/remset-twobit.txt"

    "with-infamy/remset-earley10.txt"
    "with-infamy/remset-earley13.txt"
    "with-infamy/remset-gcbench.txt"
    "with-infamy/remset-gcold0.txt"
    "with-infamy/remset-gcold1k.txt"
    "with-infamy/remset-nboyer.txt"
    "with-infamy/remset-perm9.txt"
    "with-infamy/remset-pueue.txt"
    "with-infamy/remset-queue.txt"
    "with-infamy/remset-sboyer.txt"
    "with-infamy/remset-twobit.txt"))

(define (iota n)
  (let loop ((c 0)) (if (= c n) '() (cons c (loop (+ c 1))))))

'(define earley10-b4-infamy-data 
   (extract-scan-lines "before-infamy/remset-earley10.txt"))
'(define small-data 
   (let ((lines (extract-scan-lines "before-infamy/remset-earley10.txt")))
     (map (lambda (i) (list-ref lines i)) (iota 10))))
(define (extract-scan-lines filename)
  (call-with-input-file filename
    (lambda (in)
      (do ((x (read-line in) (read-line in))
           (l '() (if (and (> (string-length x) 0)
                           (char=? #\( (string-ref x 0)))
                      (cons (let* ((in2 (open-input-string x))
                                   (remem (read in2))
                                   (lheap (read in2))
                                   (space (read in2)))
                              (list remem lheap space)) l)
                      l)))
          ((eof-object? x) (reverse l))))))

(define (scan-lines->xxx-entries xxx lines)
  (map (lambda (entry) (assoc xxx entry)) lines))
(define (scan-lines->remem-entries lines)
  (scan-lines->xxx-entries 'remem: lines))
(define (scan-lines->lheap-entries lines)
  (scan-lines->xxx-entries 'lheap: lines))
(define (scan-lines->space-entries lines)
  (scan-lines->xxx-entries 'space: lines))

(define (entries->scan-times entries)
  (map (lambda (entry) (cadr (memq 'real: entry))) entries))
(define (entries->word-count entries)
  (map (lambda (entry) (cadr (memq 'slots: entry))) entries))

(define (scan-lines->remem-tm lines)
  (entries->scan-times (scan-lines->remem-entries lines)))
(define (scan-lines->lheap-tm lines)
  (entries->scan-times (scan-lines->lheap-entries lines)))
(define (scan-lines->space-tm lines)
  (entries->scan-times (scan-lines->space-entries lines)))

(define (scan-lines->remem-wc lines)
  (entries->word-count (scan-lines->remem-entries lines)))
(define (scan-lines->lheap-wc lines)
  (entries->word-count (scan-lines->lheap-entries lines)))
(define (scan-lines->space-wc lines)
  (entries->word-count (scan-lines->space-entries lines)))

(define (plot-times title-string lines)
  (gnuplot/keep-files (lambda (dr dh ds)
             `((set title ,title-string)
               (plot #((,dr title "rem. set" with lines)
                       (,dh title "live heap" with lines)
                       (,ds title "all space" with lines)))))
           (scan-lines->remem-tm lines)
           (scan-lines->lheap-tm lines)
           (scan-lines->space-tm lines)))

(define (plot-slots title-string lines)
  (gnuplot/keep-files (lambda (dr dh ds)
             `((set title ,title-string)
               (plot #((,dr title "words rem. set" with lines)
                       (,dh title "words live heap" with lines)
                       (,ds title "words all space" with lines)))))
           (scan-lines->remem-wc lines)
           (scan-lines->lheap-wc lines)
           (scan-lines->space-wc lines)))

(define (plot-times+slots title-string1 title-string2 lines)
  (gnuplot/keep-files 
   (lambda (dr-tm dr-wc dh-tm dh-wc ds-tm ds-wc)
     `((set multiplot layout #(2 1))
       (set title ,title-string1)
       (plot #((,dr-tm title "rem. set" with lines)
               (,dh-tm title "live heap" with lines)
               (,ds-tm title "all space" with lines)))
       (set title ,title-string2)
       (plot #((,dr-wc title "words rem. set" with lines)
               (,dh-wc title "words live heap" with lines)
               (,ds-wc title "words all space" with lines)))
       (unset multiplot)))
   (scan-lines->remem-tm lines)
   (scan-lines->remem-wc lines)
   (scan-lines->lheap-tm lines)
   (scan-lines->lheap-wc lines)
   (scan-lines->space-tm lines)
   (scan-lines->space-wc lines)
   ))


(define (read-all file)
  (call-with-input-file file
    (lambda (in) (do ((x (read in) (read in)) (l '() (cons x l)))
                     ((eof-object? x) (reverse l))))))

(define (roundup-sig-figs n)
  (define (appropriate-mod n)
    (expt 10 (exact (floor (log n 10)))))
  (let ((m (appropriate-mod n)))
    (+ (- n (modulo n m)) m)))

(define (plot-times+slots-comparing-infamy benchname lines-before-infamy lines-after-infamy)
  (define entry-count (max (length lines-before-infamy) (length lines-after-infamy)))
  (define x-vals (iota entry-count))
  (define (add-x-vals y-vals) (map list x-vals y-vals))

  (define lines-list (list lines-before-infamy lines-after-infamy))
  (define remem-tms (map scan-lines->remem-tm lines-list))
  (define remem-wcs (map scan-lines->remem-wc lines-list))
  (define lheap-tms (map scan-lines->lheap-tm lines-list))
  (define lheap-wcs (map scan-lines->lheap-wc lines-list))
  (define space-tms (map scan-lines->space-tm lines-list))
  (define space-wcs (map scan-lines->space-wc lines-list))

  (gnuplot/keep-files
   (lambda (r-tm-b4 r-wc-b4 h-tm-b4 h-wc-b4 s-tm-b4 s-wc-b4
            r-tm-wi r-wc-wi h-tm-wi h-wc-wi s-tm-wi s-wc-wi)
     (let* ((list-max  (lambda (l) (foldr max 0 l)))
            (lol-max   (lambda (lol) (list-max (map list-max lol))))
            (lll-max (lambda (a b c) (max (lol-max a) (lol-max b) (lol-max c))))
            (max-time  (lll-max remem-tms lheap-tms space-tms))
            (max-slots (lll-max remem-wcs lheap-wcs space-wcs))
            (plot-arg (lambda (source freq title color pointtype)
                        `((,source notitle with dots linecolor ,color)
                          (,source every ,freq
                                   title ,title 
                                   with points linecolor ,color pointtype ,pointtype pointsize 1.0))))
            (rs-code  1) (lh-code  4) (wh-code  6) ; pointtype codes.
            ;; use primes to avoid overlapping point annotations
            ;; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 
            ;;  101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199)
            (rs-freq 131) (lh-freq 137) (wh-freq 139)
            (plot-time (lambda (rs-source lh-source sp-source)
                         `(plot \[ * : * \] \[ 0 : ,(roundup-sig-figs (ceiling (* 9/8 max-time))) \]
                                #(,@(plot-arg lh-source lh-freq "live heap"  1 lh-code)
                                  ,@(plot-arg sp-source wh-freq "whole heap" 1 wh-code)
                                  ,@(plot-arg rs-source rs-freq "rem. set"   1 rs-code)
                                  ))))
            (plot-word (lambda (rs-source lh-source sp-source)
                         `(plot \[ * : * \] \[ 0 : ,(roundup-sig-figs (ceiling (* 9/8 max-slots))) \]
                                #(,@(plot-arg sp-source wh-freq "whole heap" 1 wh-code)
                                  ,@(plot-arg lh-source lh-freq "live heap"  1 lh-code)
                                  ,@(plot-arg rs-source rs-freq "rem. set"   1 rs-code)
                                  )))))
       `((set multiplot layout #(2 2) columnsfirst title ,(string-append "Benchmark: " benchname))
         (set title ,"Scan Times (no infamy support)")
         (set lmargin 8)
         ,(plot-time r-tm-b4 h-tm-b4 s-tm-b4)
         (set title ,"Scan Slots (no infamy support)")
         ,(plot-word r-wc-b4 h-wc-b4 s-wc-b4)
         (set title ,"Scan Times : (with infamy support)")
         ,(plot-time r-tm-wi h-tm-wi s-tm-wi)
         (set title ,"Scan Slots : (with infamy support)")
         ,(plot-word r-wc-wi h-wc-wi s-wc-wi)
         (unset multiplot))))


   (add-x-vals (car remem-tms))
   (add-x-vals (car remem-wcs))
   (add-x-vals (car lheap-tms))
   (add-x-vals (car lheap-wcs))
   (add-x-vals (car space-tms))
   (add-x-vals (car space-wcs))

   (add-x-vals (cadr remem-tms))
   (add-x-vals (cadr remem-wcs))
   (add-x-vals (cadr lheap-tms))
   (add-x-vals (cadr lheap-wcs))
   (add-x-vals (cadr space-tms))
   (add-x-vals (cadr space-wcs))

   ))

(define (plot-times+slots-comparing-infamy/infer benchname)
  (plot-times+slots-comparing-infamy
   benchname
   (extract-scan-lines (string-append "before-infamy/remset-" benchname ".txt"))
   (extract-scan-lines (string-append "with-infamy/remset-" benchname ".txt"))))
