;For mzscheme-299.400

(require (lib "1.ss" "srfi"))
(require (lib "9.ss" "srfi"))
(require (lib "23.ss" "srfi"))

;=================================;
; 				  ;
; SRFI 60 integers as bits	  ;
; 				  ;
;=================================;
; Sassy uses the following subset:
(define logior bitwise-ior)
(define logand bitwise-and)
(define lognot bitwise-not)
(define ash arithmetic-shift)
(load "other/srfi-60-pieces.scm")

;==================================;
; 				   ;
; SRFI 69 basic hash-tables	   ;
; 				   ;
;==================================;
; Sassy uses the following subset:
(define hash-table-set!    hash-table-put!)
(define hash-table-ref     hash-table-get)
(define alist->hash-table  make-immutable-hash-table)
(define (hash-table-values table) (hash-table-map table (lambda (k v) v)))
(define (hash-table-keys   table) (hash-table-map table (lambda (k v) k)))
;; pnkfelix's ugly hack to make mzscheme hashtables srfi-69 compatible
;; (he thinks)
(define make-hash-table 
  (let ((old-make-hash-table make-hash-table))
    (lambda l
      (apply old-make-hash-table 
	     (map (lambda (x) (if (eq? x equal?)
				  'equal
				  x))
		  l)))))
; (define hash-table?)
