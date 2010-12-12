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

'(for-each 
  (lambda (bname)
    (plot-times+slots-comparing-infamy
     bname
     (extract-scan-lines (string-append "before-infamy/remset-" bname ".txt"))
     (extract-scan-lines (string-append "with-infamy/remset-" bname ".txt"))))
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

'(define earley10-b4-infamy-data 
   (extract-scan-lines "before-infamy/remset-earley10.txt"))
'(define small-data 
   (let ((lines (extract-scan-lines "before-infamy/remset-earley10.txt"))
         (iota (lambda (n)
                 (let loop ((c 0)) (if (= c n) '() (cons c (loop (+ c 1))))))))
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
    (expt 10 (exact (floor (log (/ n 10) 10)))))
  (let ((m (appropriate-mod n)))
    (+ (- n (modulo n m)) m)))

(define (plot-times+slots-comparing-infamy benchname lines-before-infamy lines-after-infamy)
  (gnuplot/keep-files
   (lambda (r-tm-b4 r-wc-b4 h-tm-b4 h-wc-b4 s-tm-b4 s-wc-b4
            r-tm-wi r-wc-wi h-tm-wi h-wc-wi s-tm-wi s-wc-wi)
     (let* ((file-max  (lambda (f) (foldr max 0 (read-all f))))
            (files-max (lambda (fs) (apply max (map file-max fs))))
            (max-time  (files-max (list r-tm-b4 h-tm-b4 s-tm-b4
                                        r-tm-wi h-tm-wi s-tm-wi)))
            (max-slots (files-max (list r-wc-b4 h-wc-b4 s-wc-b4
                                        r-wc-wi h-wc-wi s-wc-wi))))
       `((set multiplot layout #(2 2) columnsfirst title ,(string-append "Benchmark: " benchname))
         (set title ,(string-append "Scan Times (no infamy support)"))
         (plot \[ * : * \] \[ 0 : ,(roundup-sig-figs max-time) \]
               #((,r-tm-b4 title "rem. set" with lines)
                 (,h-tm-b4 title "live heap" with lines)
                 (,s-tm-b4 title "whole heap" with lines)))
         (set title ,(string-append "Scan Slots (no infamy support)"))
         (plot  \[ * : * \] \[ 0 : ,(roundup-sig-figs max-slots) \]
                #((,r-wc-b4 title "rem. set" with lines)
                  (,h-wc-b4 title "live heap" with lines)
                  (,s-wc-b4 title "whole heap" with lines)))

         (set title ,(string-append "Scan Times : (with infamy support)"))
         (plot \[ * : * \] \[ 0 : ,(roundup-sig-figs max-time) \]
               #((,r-tm-wi title "rem. set" with lines)
                 (,h-tm-wi title "live heap" with lines)
                 (,s-tm-wi title "whole heap" with lines)))
         (set title ,(string-append "Scan Slots : (with infamy support)"))
         (plot \[ * : * \] \[ 0 : ,(roundup-sig-figs max-slots) \]
               #((,r-wc-wi title "rem. set" with lines)
                 (,h-wc-wi title "live heap" with lines)
               (,s-wc-wi title "whole heap" with lines)))

       (unset multiplot))))

   (scan-lines->remem-tm lines-before-infamy)
   (scan-lines->remem-wc lines-before-infamy)
   (scan-lines->lheap-tm lines-before-infamy)
   (scan-lines->lheap-wc lines-before-infamy)
   (scan-lines->space-tm lines-before-infamy)
   (scan-lines->space-wc lines-before-infamy)

   (scan-lines->remem-tm lines-after-infamy)
   (scan-lines->remem-wc lines-after-infamy)
   (scan-lines->lheap-tm lines-after-infamy)
   (scan-lines->lheap-wc lines-after-infamy)
   (scan-lines->space-tm lines-after-infamy)
   (scan-lines->space-wc lines-after-infamy)
   ))
