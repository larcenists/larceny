;;; A demo of using dotnet to talk to Excel.
;;; Requires that dotnet be loaded and initialized.

;;; Some stuff to debug.

(define *scheme-dynamic-assembly-name*
  (let ((name (System.Reflection.AssemblyName.)))
    (set-.name$! name '|SchemeDynamicAssembly|)
    name))

(define *scheme-application-domain* #f)

(define (scheme-application-domain)
  (or *scheme-application-domain*
      (begin (set! *scheme-application-domain* (System.AppDomain.CurrentDomain$))
             *scheme-application-domain*)))

(define *scheme-dynamic-assembly* #f)

(define (scheme-dynamic-assembly)
  (or *scheme-dynamic-assembly*
      (begin
        (set! *scheme-dynamic-assembly*
              (.DefineDynamicAssembly (scheme-application-domain)
                                      *scheme-dynamic-assembly-name*
                                      (System.Reflection.Emit.AssemblyBuilderAccess.RunAndSave$)))
        *scheme-dynamic-assembly*)))

(define *scheme-dynamic-module* #f)

(define (scheme-dynamic-module)
  (or *scheme-dynamic-module*
      (begin
        (set! *scheme-dynamic-module*
              (.DefineDynamicModule (scheme-dynamic-assembly) '|SchemeModule.dll| '|SchemeModule.dll|))
        *scheme-dynamic-module*)))

(define *system-drawing-assembly* #f)
(define *system-windows-forms-assembly* #f)

(define (system-drawing-assembly)
  (or *system-drawing-assembly*
      (begin (set! *system-drawing-assembly*
                   (System.Reflection.Assembly.LoadWithPartialName 'System.Drawing))
             *system-drawing-assembly*)))

(define (system-windows-forms-assembly)
  (or *system-windows-forms-assembly*
      (begin (system-drawing-assembly)
             (set! *system-windows-forms-assembly*
                   (System.Reflection.Assembly.LoadWithPartialName 'System.Windows.Forms))
             *system-windows-forms-assembly*)))

(define (window-demo)
  (system-windows-forms-assembly)
  (System.Windows.Forms.Application.Run
   (System.Windows.Forms.ApplicationContext.
    (System.Windows.Forms.form.))))



;;; End of temp code

(define *the-excel-assembly* #f)

(define (excel-assembly)
  (or *the-excel-assembly*
      (begin
        (set! *the-excel-assembly*
              (System.Reflection.Assembly.LoadWithPartialName 'Microsoft.Office.Interop.Excel))
        ;; bootstrap excel types
        (for-each (lambda (sym)
                    (.GetType *the-excel-assembly* sym))
                  (list '|Microsoft.Office.Interop.Excel.ApplicationClass|
                          '|Microsoft.Office.Interop.Excel.WorkbookClass|
                            '|Microsoft.Office.Interop.Excel.Sheets|
                              '|Microsoft.Office.Interop.Excel._Worksheet|))
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

(define (set-cell-comment! cell comment-text)
  (let* ((rng (cellref->rng cell))
         (comment-obj (.Comment$ rng)))
    (.Delete comment-obj)
    (.AddComment rng comment-text)))

