;;; A nursery larger than the initial page table (which covers 32MB).
;;;
;;; The low-level allocator's first allocation, the nursery, did not
;;; grow the page table when the table was too small for it, so marking
;;; the nursery's pages wrote past the end of the table, and later
;;; growth copied garbage into it: some pages of the nursery beyond its
;;; first 32MB had nonsense generations.  A store of a young object into
;;; another on such a page looked to the write barrier like a store into
;;; an older generation, and processing that record crashed (when what
;;; followed the table in memory happened to make it so).  Now an
;;; assertion in gclib_alloc (alloc.c) checks that the table covers each
;;; allocation, so the bug would fail at startup.  This program stores
;;; into objects all over the first 200MB of the nursery.  Run it with
;;;
;;;     larceny -size0 256M -- large-nursery.sch
;;;
;;; It prints "large-nursery: ok" and exits.

(define (stores n)
  (let loop ((i 0) (sum 0))
    (if (= i n)
        sum
        (let ((v (make-vector 4 #f)))
          (vector-set! v 0 (cons i i))
          (loop (+ i 1) (+ sum (car (vector-ref v 0))))))))

(begin (display "large-nursery: ")
       (display (if (= (stores 3000000) (quotient (* 3000000 2999999) 2))
                    "ok"
                    "wrong"))
       (newline))

(exit)
