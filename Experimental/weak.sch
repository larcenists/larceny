; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Weak cells for Larceny.
;
; (make-weak-cell object)  => cell
;
;    Create a cell that holds its object weakly: when the garbage collector
;    discovers that the only reference to the object is from the cell,
;    then the object may be collected and the cell's contents will be
;    replaced by #f (the cell has been "broken").
;
; (weak-cell? object)      => boolean
;
;    Returns TRUE if the object is a weak cell.
;
; (weak-cell-ref cell)     => object
;
;    Returns the object held by the weak cell, which will be #f if the 
;    cell has been broken.
;
; (weak-cell-broken? cell) => boolean
;
;    Returns TRUE if the cell has been broken.  Since #f can be stored in
;    a cell, this test is more accurate than testing whether a cell's content 
;    is #f.

; The following implementation is adequate for casual use but will probably
; be too slow in systems with a large heap or a large number of weak cells.

(define make-weak-cell)
(define weak-cell?)
(define weak-cell-ref)
(define weak-cell-broken?)

(let* ((weak-cell-type
        (make-record-type "weak-cell" '(object broken?)))
       (cell-ref
        (record-accessor weak-cell-type 'object))
       (cell-object-set! 
        (record-updater weak-cell-type 'object))
       (cell-broken-set! 
        (record-updater weak-cell-type 'broken?)))

  (define additions-since-last-clean 0)
  (define table-size-at-last-clean 0)
  (define weak-table '())
  (define weak-table-size 0)

  (define make-cell 
    (let ((make-cell (record-constructor weak-cell-type)))
      (lambda (object)
        (conditionally-clean-weak-table)
        (let ((cell (make-cell object #f)))
          (if (or (pair? object)
                  (vector-like? object)
                  (bytevector-like? object)
                  (procedure? object))
              (begin
                (set! weak-table (cons cell weak-table))
                (set! weak-table-size (+ weak-table-size 1))
                (set! additions-since-last-clean 
                      (+ additions-since-last-clean 1))))
          cell))))

  ; Default policy: Clean when at least as many entries has been added as 
  ; there are elements in the table.

  (define (conditionally-clean-weak-table)
    (if (and (> weak-table-size 0)
             (>= additions-since-last-clean table-size-at-last-clean))
        (clean-weak-table)))

  (define (clean-weak-table)

    (define (lookup-and-remove obj)
    
      (define cell #f)

      (define (loop table)
        (cond ((null? table) '())
              ((eq? obj (cell-ref (car table)))
               (set! cell (car table))
               (cdr table))
              (else
               (set-cdr! table (loop (cdr table))))))

      (set! weak-table (loop weak-table))
      cell)

    (set! additions-since-last-clean 0)
    (let ((referenced-once (sro -1 -1 1)))
      (let ((limit (vector-length referenced-once))
            (removed 0))
        (do ((i 0 (+ i 1)))
            ((= i limit)
             (set! weak-table-size (- weak-table-size removed))
             (set! table-size-at-last-clean weak-table-size)
             removed)
          (let ((cell (lookup-and-remove (vector-ref referenced-once i))))
            (if cell
                (begin
                  (set! removed (+ removed 1))
                  (cell-object-set! cell #f)
                  (cell-broken-set! cell #t))))))))

  (set! make-weak-cell make-cell)
  (set! weak-cell? (record-predicate weak-cell-type))
  (set! weak-cell-ref (record-accessor weak-cell-type 'object))
  (set! weak-cell-broken? (record-accessor weak-cell-type 'broken?))
  'weak-cells)

; eof
