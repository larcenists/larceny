; Given a set of root nodes and a directed graph,
; returns the set of nodes that are accessible from the roots.
;
; Assumes the following operations:
;    same-node? : node x node -> boolean
;    neighbors  : node x graph -> set of node

(define (accessibility-parameters same-node? neighbors)
  (define (accessible roots graph)
    (define (loop untraced traced)
      (cond ((null? untraced) traced)
            ((select (lambda (node)
                       (same-node? node (car untraced)))
                     traced)
             (loop (cdr untraced) traced))
            (else (loop (append (neighbors (car untraced) graph)
                                (cdr untraced))
                        (cons (car untraced) traced)))))
    (loop roots '()))
  (lambda (roots graph)
    (accessible roots graph)))
