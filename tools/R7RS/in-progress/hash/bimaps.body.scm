(define (make-bimap bimap)
  'FIXME)

(define (bimap? obj)
  'FIXME)

(define (bimap-forward-hash-table bimap)
  'FIXME)

(define (bimap-reverse-hash-table bimap)
  'FIXME)

(define (bimap-contains? bimap key)
  'FIXME)

(define (bimap-contains-value? bimap value)
  'FIXME)

(define (bimap=? bimap1 bimap2)
  'FIXME)

(define (bimap-ref bimap key . rest)
  'FIXME)

(define (bimap-value-ref bimap value . rest)
  'FIXME)

(define (bimap-ref/default bimap key default)
  'FIXME)

(define (bimap-value-ref/default bimap value default)
  'FIXME)

(define (bimap-copy bimap . rest)
  'FIXME)

(define (bimap-set! bimap . rest)
  'FIXME)

(define (bimap-set-entries! bimap keys-list values-list)
  'FIXME)

(define (bimap-delete! bimap . rest)
  'FIXME)

(define (bimap-delete-keys! bimap keys-list)
  'FIXME)

(define (bimap-extend! bimap key . rest)
  'FIXME)

(define (bimap-extend/default! bimap key default)
  'FIXME)

(define (bimap-replace! bimap key . rest)
  'FIXME)

(define (bimap-replace/default! bimap key . rest)
  'FIXME)

(define (bimap-update! bimap key updater . rest)
  'FIXME)

(define (bimap-update/default! bimap key updater default)
  'FIXME)

(define (bimap-clear! bimap)
  'FIXME)

(define (bimap-filter! comparator proc bimap)
  'FIXME)

(define (bimap-remove! comparator proc bimap)
  'FIXME)

;;; FIXME: This procedure has no hash-table analogue, so its
;;; specification by reference to that analogue is vacuous.

(define (bimap-partition! bimap)
  'FIXME)

