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
; (define make-hash-table ...) mzscheme's default is fine, though it
;                              does default to eq? instead of equal?  
; (define hash-table?)
