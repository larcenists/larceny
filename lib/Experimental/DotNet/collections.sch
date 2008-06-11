;;; Useful procedures defined below:

;;; make-enumerator : T:ForeignType get-enum-meth-name:String -> x:T -> [Listof Foreign]
;;; enumerate : x : { GetEnumerator() } -> [Listof Foreign]
;;; ilist-add! : IList 

(define make-enumerator
  (let* ((enum-type (find-clr-type "System.Collections.IEnumerator"))
         (move-next-method (clr/%get-method enum-type "MoveNext" '#()))
         (current-prop (clr/%get-property enum-type "Current" '#())))
    (lambda (type get-enumerator-methodname)
      (let ((method (clr/%get-method type get-enumerator-methodname '#())))
        (lambda (obj)
          (let ((enumerator (clr/%invoke method obj '#())))
            (let loop ((lst '()))
              (cond ((clr/foreign->bool (clr/%invoke move-next-method enumerator '#()))
                     (loop (cons (clr/%property-ref current-prop enumerator '#())
                                 lst)))
                    (else
                     lst)))))))))

(define enumerate
  (lambda (obj)
    (let* ((obj-type (clr/%object-type obj)))
      (let ((rator (make-enumerator obj-type "GetEnumerator")))
        (rator obj)))))

;; IList <: ICollection
(define ilist-type (find-clr-type "System.Collections.IList"))
(define icollection-type (find-clr-type "System.Collections.ICollection"))

(define (check-foreign obj)
  (cond ((not (%foreign? obj))
         (begin (display "Warning: you probably did not want to add " 
                         obj " without marshalling to foreign first...")
                (newline)))))

;; IList -> Bool
(define ilist-is-fixed-size? 
  (let ((prop (clr/%get-property ilist-type "IsFixedSize" '#())))
    (lambda (ilist)
      (clr/foreign->bool (clr/%property-ref prop ilist '#())))))

;; IList -> Bool
(define ilist-is-read-only? 
  (let ((prop (clr/%get-property ilist-type "IsReadOnly" '#())))
    (lambda (ilist)
      (clr/foreign->bool (clr/%property-ref prop ilist '#())))))

;; IList Nat -> Foreign
(define ilist-item
  (let ((prop (clr/%get-property ilist-type "Item" 
                                 (vector clr-type-handle/system-int32))))
    (lambda (ilist idx)
      (clr/%property-ref prop ilist (vector (clr/int->foreign idx))))))

;; IList Nat Foreign -> unspecified
(define set-ilist-item!
  (let ((prop (clr/%get-property ilist-type "Item" 
                                 (vector clr-type-handle/system-int32))))
    (lambda (ilist idx obj)
      (check-foreign obj)
      (clr/%property-set! prop ilist obj (vector (clr/int->foreign idx))))))

;; IList Foreign -> unspecified
(define ilist-add!
  (let ((method (clr/%get-method ilist-type "Add" 
                                 (vector clr-type-handle/system-object))))
    (lambda (ilist obj)
      (check-foreign obj)
      (clr/%invoke method ilist (vector obj)))))

;; IList -> unspecified
(define ilist-clear!
  (let ((method (clr/%get-method ilist-type "Clear" '#())))
    (lambda (ilist)
      (clr/%invoke ilist-add-method ilist '#()))))

;; IList Foreign -> Bool
(define ilist-contains?
  (let ((method (clr/%get-method ilist-type "Contains"
                                 (vector clr-type-handle/system-object))))
    (lambda (ilist obj)
      (check-foreign obj)
      (clr/foreign->bool (clr/%invoke method ilist (vector obj))))))

;; IList Foreign -> Nat
(define ilist-index-of 
  (let ((method (clr/%get-method ilist-type "IndexOf"
                                 (vector clr-type-handle/system-object))))
    (lambda (ilist obj)
      (check-foreign obj)
      (clr/foreign->int (clr/%invoke method ilist (vector obj))))))

;; IList Nat Foreign -> unspecified
(define ilist-insert! 
  (let ((method (clr/%get-method ilist-type "Insert" 
                                 (vector clr-type-handle/system-int32
                                         clr-type-handle/system-object))))
    (lambda (ilist idx obj)
      (check-foreign obj)
      (clr/%invoke method ilist (vector (clr/int->foreign idx) obj)))))

;; IList Foreign -> unspecified  
(define ilist-remove!
  (let ((method (clr/%get-method ilist-type "Remove"
                                 (vector clr-type-handle/system-object))))
    (lambda (ilist obj)
      (check-foreign obj)
      (clr/%invoke method ilist (vector obj)))))

;; IList Nat -> unspecified  
(define ilist-remove-at!
  (let ((method (clr/%get-method ilist-type "RemoveAt"
                                 (vector clr-type-handle/system-int32))))
    (lambda (ilist idx)
      (clr/%invoke method ilist (vector (clr/int->foreign idx))))))

;; ICollection -> Nat
(define icollection-count
  (let ((prop (clr/%get-property icollection-type "Count" '#())))
    (lambda (ic)
      (clr/foreign->int (clr/%property-ref prop ic '#())))))

;; StringCollection <: IList
(define stringcollection-type 
  (find-clr-type "System.Collections.Specialized.StringCollection"))

(define make-stringcollection
  (let ((ctor (clr/%get-constructor stringcollection-type '#())))
    (lambda ()
      (clr/%invoke-constructor ctor '#()))))

(define stringcollection->list
  (lambda (sc)
    (let ((count (icollection-count sc)))
      (let loop ((idx (- count 1))
                 (lst '()))
        (cond ((>= idx 0)
               (loop (- idx 1)
                     (cons (clr/foreign->string (ilist-item sc idx))
                           lst)))
              (else lst))))))

(define list->stringcollection
  (lambda (lst)
    (let loop ((sc (make-stringcollection))
               (lst lst))
      (cond
       ((null? lst) sc)
       (else 
        (ilist-add! sc (clr/string->foreign (car lst)))
        (loop sc (cdr lst)))))))
