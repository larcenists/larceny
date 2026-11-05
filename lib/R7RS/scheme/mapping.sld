;;; (scheme generator)
;;;
;;; R7RS Tangerine Edition

(define-library (scheme mapping)
  (export mapping mapping-unfold
	  mapping? mapping-contains? mapping-empty? mapping-disjoint?
	  mapping-ref mapping-ref/default mapping-key-comparator
	  mapping-set mapping-set! 
	  mapping-replace mapping-replace!
	  mapping-delete mapping-delete! mapping-delete-all mapping-delete-all!
	  mapping-intern mapping-intern!
	  mapping-update mapping-update! mapping-update/default mapping-update!/default
	  mapping-search mapping-search!
	  mapping-size mapping-find mapping-count mapping-any? mapping-every?
	  mapping-keys mapping-values mapping-entries
	  mapping-map mapping-map->list mapping-for-each mapping-fold
	  mapping-filter mapping-filter!
	  mapping-remove mapping-remove!
	  mapping-partition mapping-partition!
	  mapping-copy mapping->alist alist->mapping alist->mapping!
	  mapping=? mapping<? mapping>? mapping<=? mapping>=?
	  mapping-union mapping-intersection mapping-difference mapping-xor
	  mapping-union! mapping-intersection! mapping-difference! mapping-xor!
	  make-mapping-comparator
	  mapping-comparator
	  mapping-min-key mapping-max-key
	  mapping-min-value mapping-max-value
	  mapping-key-predecessor mapping-key-successor
	  mapping-range= mapping-range< mapping-range> mapping-range<= mapping-range>=
	  mapping-range=! mapping-range<! mapping-range>! mapping-range<=! mapping-range>=!
	  mapping-split
	  mapping-catenate mapping-catenate!
	  mapping-map/monotone mapping-map/monotone!
	  mapping-fold/reverse)
  (import (srfi 146)))
