; Generic topological sorting.
;
; These could be phrased in terms of sets, but the specialised operations
; are presumably faster.
;
; A graph is created by calling make-graph (specifying an equality predicate)
; and is a collection of nodes. A node is created in the context of the graph
; using make-node and has a value, a set of children, a parent graph, and 
; a sort value. Each child is an actual node and must be added with 
; node.add-child!.

(define (make-graph same?) (list same? '()))

(define graph.same car)
(define graph.nodes cadr)
(define (graph.nodes! graph nodes) (set-car! (cdr graph) nodes))
(define (graph.size g) (length (graph.nodes g)))

(define (graph.create-node! graph val)
  (let ((new (list val graph '() 0)))
    (graph.nodes! graph (cons new (graph.nodes graph)))
    new))

(define node.val car)
(define node.graph cadr)
(define node.children caddr)
(define (node.children! node children) (set-car! (cddr node) children))
(define node.orderval cadddr)
(define (node.orderval! node n) (set-car! (cdddr node) n))

; Add a child to a node if the child is not already there.

(define (node.add-child! node child)
  (let ((same? (graph.same (node.graph node))))
    (let loop ((n (node.children node)))
      (cond ((null? n)
             (node.children! node (cons child (node.children node))))
            ((same? (node.val (car n)) (node.val child))
             #f)
            (else
              (loop (cdr n)))))))

(define (make-node graph val)
  (let ((same? (graph.same graph)))
    (let loop ((n (graph.nodes graph)))
      (cond ((null? n)
             (graph.create-node! graph val))
            ((same? (node.val (car n)) val)
             (car n))
            (else
              (loop (cdr n)))))))

; Sort the graph, if possible. Return the list in order from source to
; sink if an ordering exists, and 'circularity if not.
;
; Do a depth-first postorder traversal of the graph. Nodes are consed
; onto the result list after all children have been visited; the result
; list is the topological sort.
;
; To detect cycles, we mark each node with a preorder -1. If we reach
; a node with a -1, we have a cycle.

(define (toposort-1 graph)
  (call-with-current-continuation
    (lambda (k)
      
      (define (visit node l)
        (cond ((= (node.orderval node) -1)
               (k 'circularity))
              ((not (zero? (node.orderval node)))
               l)
              (else
                (node.orderval! node -1)
                (let loop ((l l) (nodes (node.children node)))
                  (if (null? nodes)
                      (begin (node.orderval! node 1)
                             (cons node l))
                      (loop (visit (car nodes) l) (cdr nodes)))))))
    
      (let loop ((nodes (graph.nodes graph)) (l '()))
        (if (not (null? nodes))
            (loop (cdr nodes) (visit (car nodes) l))
            l)))))


; This algorithm returns the list of nodes which form a cycle (with no
; duplicates), or 'ok.

(define (toposort-2 graph)
  (call-with-current-continuation
    (lambda (k)

      (define push cons)
      (define pop cdr)
      (define empty? null?)

      (define (find-cycle stk node)
        (let loop ((l (list node)) (stk stk))
          (cond ((empty? stk)
                 (error "Bug in toposort-2"))
                ((eq? node (pop stk))
                 l)
                (else
                  (loop (cons (pop stk) l) (pop stk))))))

      (define (visit node l)
        (cond ((= (node.orderval node) -1)
               (k 'circularity))
              ((not (zero? (node.orderval node)))
               l)
              (else
                (node.orderval! node -1)
                (let loop ((l l) (nodes (node.children node)))
                  (if (null? nodes)
                      (begin (node.orderval! node 1)
                             (cons node l))
                      (loop (visit (car nodes) l) (cdr nodes)))))))
    
      (let loop ((nodes (graph.nodes graph)) (l '()))
        (if (not (null? nodes))
            (loop (cdr nodes) (visit (car nodes) l))
            l)))))

; test code follows

(define (print-graph g)
  (for-each print-node (graph.nodes g)))

(define (print-node n)
  (display (node.val n)) 
  (display " ")
  (display (node.orderval n))
  (newlime))

(define (ts-1 graph)
  (for-each print-node (toposort-1 graph)))

(define g1 (make-graph =))
(define n0 (make-node g1 0))
(define n1 (make-node g1 1))
(define n2 (make-node g1 2))
(define n6 (make-node g1 6))
(define n3 (make-node g1 3))
(define n4 (make-node g1 4))
(define n5 (make-node g1 5))
(define n7 (make-node g1 7))

(node.add-child! n0 n2)
(node.add-child! n1 n2)
(node.add-child! n1 n5)
(node.add-child! n2 n3)
(node.add-child! n2 n4)
(node.add-child! n4 n5)
(node.add-child! n6 n7)

