;;; -*- Mode: Scheme; coding: iso-8859-1 -*-

;;; A demo of using dotnet to talk to Excel.
;;;
;;; To use this code, you must have Microsoft Excel installed.
;;;
;;; Follow these steps:
;;;
;;; 1.  Load  dotnet-ffi.sch
;;; 2.  Load  dotnet.sch
;;; 3.  (enable-dotnet!)
;;; 4.  Load  excel-demo.sch  (this file)
;;; 5.  (whizbang-excel-demo)

;;; Note that the first time you call whizbang-excel-demo it will take
;;; quite some time (about a minute on a fast machine) to bootstrap
;;; the reflected methods.

(define *the-excel-assembly* #f)

(define (excel-assembly)
  (or *the-excel-assembly*
      (begin
        (set! *the-excel-assembly*
              (System.Reflection.Assembly.LoadWithPartialName "Microsoft.Office.Interop.Excel"))
        ;; bootstrap excel types
        (for-each (lambda (sym)
                    (.GetType *the-excel-assembly* sym))
                  (list "Microsoft.Office.Interop.Excel.ApplicationClass"
                        "Microsoft.Office.Interop.Excel.WorkbookClass"
                        "Microsoft.Office.Interop.Excel.Sheets"
                        "Microsoft.Office.Interop.Excel._Worksheet"))
        *the-excel-assembly*)))

(define *the-excel-application* #f)

(define (excel-application)
  (or *the-excel-application*
      (begin
        (set! *the-excel-application* (Microsoft.Office.Interop.Excel.ApplicationClass.))
        (set-.visible$! *the-excel-application* #t)
        *the-excel-application*)))

(define *excel-workbooks* #f)
(define *excel-workbook* #f)
(define *excel-worksheets* #f)
(define *excel-active-sheet* #f)

(define (excel-workbooks)
  (or *excel-workbooks*
      (begin
        (set! *excel-workbooks*
               (.Workbooks$ (excel-application)))
        *excel-workbooks*)))

(define (excel-workbook)
  (or *excel-workbook*
      (begin
        (set! *excel-workbook*
              (clr-dynamic-cast
               Microsoft.Office.Interop.Excel.WorkbookClass.class
               (.Add (excel-workbooks))))
        *excel-workbook*)))

(define (excel-worksheets)
  (or *excel-worksheets*
      (begin
        (set! *excel-worksheets*
              (.Worksheets$ (excel-workbook)))
        *excel-worksheets*)))

(define (excel-active-sheet)
  (or *excel-active-sheet*
      (begin
        (excel-worksheets)
        (set! *excel-active-sheet*
              (clr-dynamic-cast
               Microsoft.Office.Interop.Excel._Worksheet.class
               (.ActiveSheet$ (excel-workbook))))
        *excel-active-sheet*)))

;; Not sure how this is defined, so these are simple stubs.
(define (make-cellref row column)
  `(CELLREF ,row ,column))

(define (numbers->cell-ref coords)
  (make-cellref (cadr coords) (car coords)))

(define (cellref->numbers cellref)
  (if (or (not (pair? cellref))
          (not (eq? (car cellref) 'cellref)))
      (error "Not a cellref.")
      (list (caddr cellref) (cadr cellref))))

;; Basic .NET interface
(define (number->cell-alpha n)
  (substring "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (sub1 n) n))

(define (coords->range row col)
  (string-append (number->cell-alpha col)
                 (number->string row)))

(define (get-cell row col)
  (clr-dynamic-cast
   Microsoft.Office.Interop.Excel.Range.class
   (.Item$ (.Cells$ (excel-active-sheet)) row col)))

(define (cellref->rng cellref)
  (let ((xy (cellref->numbers cellref)))
    (get-cell (cadr xy) (car xy))))

(define (get-cell-comment cellref)
  (.Text (.Comment$ (cellref->rng cellref))))

(define (delete-cell-comment! cellref)
  (let* ((rng (cellref->rng cellref))
         (comment-obj (.Comment$ rng)))
    (.Delete comment-obj)))

(define (set-first-comment! cell comment-text)
  (let* ((rng (cellref->rng cell))
         (comment-obj (.Comment$ rng)))
    (.AddComment rng comment-text)))

(define (set-cell-comment! cell comment-text)
  (let* ((rng (cellref->rng cell))
         (comment-obj (.Comment$ rng)))
    (.Delete comment-obj)
    (.AddComment rng comment-text)))

(define (set-cell-border! cell r g b)
  (set-.color$! (.Borders$ cell)  (+ (* b #x660000)
                                     (* g #x006600)
                                     (* r #x000066))))

(define (set-cell-color! cell r g b)
  (set-.color$! (.Interior$ cell)  (+ (* b #x660000)
                                     (* g #x006600)
                                     (* r #x000066))))

(define (set-color! row col r g b)
  (let* ((cell (get-cell row col)))
    (set-cell-color! cell r g b)
    (set-cell-border! cell 4 4 4)))

(define (whizbang-excel-demo)
  (excel-assembly)
  (excel-active-sheet)
  (let* ((cella (get-cell 1 1))
         (comment (.comment$ cella)))
    (if (not (null? comment))
        (.delete comment))
    (.AddComment cella "This is the comment."))
  (let rloop ((red 0))
    (if (< red 6)
        (let gloop ((green 0))
          (if (< green 6)
              (let bloop ((blue 0))
                (if (< blue 6)
                    (begin
                      (set-color! (+ (* green 6) red 2) (+ blue 1) red green blue)
                      (bloop (+ blue 1)))
                    (gloop (+ green 1))))
              (rloop (+ red 1))))))
  (let rloop ((red 0))
    (if (< red 6)
        (let gloop ((green 0))
          (if (< green 6)
              (let bloop ((blue 0))
                (if (< blue 6)
                    (begin
                      (set-color! (+ (* green 6) red 2) (+ blue 1) 5 5 5)
                      (bloop (+ blue 1)))
                    (gloop (+ green 1))))
              (rloop (+ red 1)))))))
