; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Accessors for the vector returned by (memstats).

(define (memstats:words-allocated v)
  (vector-ref v 0))

(define (memstats:words-reclaimed v)
  (vector-ref v 1))

(define (memstats:words-copied v)
  (vector-ref v 2))

(define (memstats:gctime v)
  (vector-ref v 3))

(define (memstats:words-live v)
  (vector-ref v 4))

(define (memstats:generation-of-last-collection v)
  (vector-ref v 5))

(define (memstats:kind-of-last-collection v)
  (if (zero? (vector-ref v 6))
      'collection
      'promotion))

(define (memstats:generation-info v)
  (vector-ref v 7))

(define (memstats:remset-info v)
  (vector-ref v 8))

(define (memstats:stack-frames-flushed v)
  (vector-ref v 9))

(define (memstats:stack-words-flushed v)
  (vector-ref v 10))

(define (memstats:stacks-created v)
  (vector-ref v 11))

(define (memstats:stack-frames-restored v)
  (vector-ref v 12))

(define (memstats:words-in-heap v)
  (vector-ref v 13))

(define (memstats:words-in-remsets v)
  (vector-ref v 14))

(define (memstats:words-in-other v)
  (vector-ref v 15))

(define (memstats:array-assignments v)
  (vector-ref v 16))

(define (memstats:array-assignments-filtered-by-lhs v)
  (vector-ref v 17))

(define (memstats:array-assignments-filtered-by-rhs v)
  (vector-ref v 18))

(define (memstats:array-assignments-filtered-by-gencheck v)
  (vector-ref v 19))

(define (memstats:array-assignments-added-to-remset v)
  (vector-ref v 20))

(define (memstats:elapsed-time v)
  (vector-ref v 21))

(define (memstats:system-cpu-time v)
  (vector-ref v 22))

(define (memstats:user-cpu-time v)
  (vector-ref v 23))

(define (memstats:minor-page-faults v)
  (vector-ref v 24))

(define (memstats:major-page-faults v)
  (vector-ref v 25))

(define (memstats:remset-info-np v)
  (vector-ref v 26))

(define (memstats:maximum-words-in-heap v)
  (vector-ref v 27))

(define (memstats:gctime-for-promotion v)
  (vector-ref v 28))

(define (memstats:gen:collections v i)
  (vector-ref (vector-ref (memstats:generation-info v) i) 0))

(define (memstats:gen:promotions v i)
  (vector-ref (vector-ref (memstats:generation-info v) i) 1))

(define (memstats:gen:gctime v i)
  (vector-ref (vector-ref (memstats:generation-info v) i) 2))

(define (memstats:gen:words-live v i)
  (vector-ref (vector-ref (memstats:generation-info v) i) 3))

(define (memstats:gen:np-young? v i)
  (not (zero? (vector-ref (vector-ref (memstats:generation-info v) i) 4))))

(define (memstats:gen:np-old? v i)
  (not (zero? (vector-ref (vector-ref (memstats:generation-info v) i) 5))))

(define (memstats:gen:j v i)
  (vector-ref (vector-ref (memstats:generation-info v) i) 6))

(define (memstats:gen:k v i)
  (vector-ref (vector-ref (memstats:generation-info v) i) 7))

(define (memstats:gen:words-in-generation v i)
  (vector-ref (vector-ref (memstats:generation-info v) i) 8))

(define (memstats:gen:words-desired-per-semispace v i)
  (vector-ref (vector-ref (memstats:generation-info v) i) 9))

(define (memstats:gen:gctime-for-promotion v i)
  (vector-ref (vector-ref (memstats:generation-info v) i) 10))

(define (memstats:remset:words-allocated-in-nodepool v i)
  (vector-ref (vector-ref (memstats:remset-info v) i) 0))

(define (memstats:remset:words-used-in-nodepool v i)
  (vector-ref (vector-ref (memstats:remset-info v) i) 1))

(define (memstats:remset:words-live-in-nodepool v i)
  (vector-ref (vector-ref (memstats:remset-info v) i) 3))

(define (memstats:remset:words-allocated-to-hashtable v i)
  (vector-ref (vector-ref (memstats:remset-info v) i) 3))

(define (memstats:remset:objects-recorded v i)
  (vector-ref (vector-ref (memstats:remset-info v) i) 4))

(define (memstats:remset:objects-removed v i)
  (vector-ref (vector-ref (memstats:remset-info v) i) 5))

(define (memstats:remset:objects-scanned v i)
  (vector-ref (vector-ref (memstats:remset-info v) i) 6))

(define (memstats:remset:words-scanned v i)
  (vector-ref (vector-ref (memstats:remset-info v) i) 7))

(define (memstats:remset:transactions-recorded v i)
  (vector-ref (vector-ref (memstats:remset-info v) i) 8))

(define (memstats:remset:times-cleared v i)
  (vector-ref (vector-ref (memstats:remset-info v) i) 9))

(define (memstats:remset:times-scanned v i)
  (vector-ref (vector-ref (memstats:remset-info v) i) 10))

(define (memstats:remset:times-compacted v i)
  (vector-ref (vector-ref (memstats:remset-info v) i) 11))

(define (memstats:remset:max-words v i)
  (vector-ref (vector-ref (memstats:remset-info v) i) 12))

; eof
