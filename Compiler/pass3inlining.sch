; Copyright 1999 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 14 April 1999.
;
; Inlining of known local procedures.
;
; First find the known and escaping procedures and compute the call graph.
;
; If a known local procedure is not called at all, then delete its code.
;
; If a known local procedure is called exactly once,
; then inline its code at the call site and delete the
; known local procedure.  Change the size of the code
; at the call site by adding the size of the inlined code.
;
; Divide the remaining known and escaping procedures into categories:
;     1.  makes no calls to known local procedures
;     2.  known procedures that call known procedures;
;         within this category, try to sort so that procedures do not
;         call procedures that come later in the sequence; or sort by
;         number of calls and/or size
;     3.  escaping procedures that call known procedures
;
; Approve each procedure in category 1 for inlining if its code size
; is less than some threshold.
;
; For each procedure in categories 2 and 3, traverse its code, inlining
; where it seems like a good idea.  The compiler should be more aggressive
; about inlining non-tail calls than tail calls because:
;
;     Inlining a non-tail call can eliminate a stack frame
;     or expose the inlined code to loop optimizations.
;
;     The main reason for inlining a tail call is to enable
;     intraprocedural optimizations or to unroll a loop.
;
; After inlining has been performed on a known local procedure,
; then approve it for inlining if its size is less than some threshold.
;
; FIXME:
; This strategy avoids infinite unrolling, but it also avoids finite
; unrolling of loops.

; Parameters to control inlining.
; These can be tuned later.

(define *tail-threshold* 10)
(define *nontail-threshold* 20)
(define *multiplier* 300)

; Given a callgraph, performs inlining of known local procedures
; by side effect.  The original expression must then be copied to
; reinstate Twobit's invariants.

; FIXME:  This code doesn't yet do the right thing with known local
; procedures that aren't called or are called in exactly one place.

(define (inline-using-callgraph! g)
  (let ((known (make-hashtable))
        (category2 '())
        (category3 '()))
    (for-each (lambda (node)
                (let ((name (callgraphnode.name node))
                      (tcalls (callgraphnode.tailcalls node))
                      (ncalls (callgraphnode.nontailcalls node)))
                  (if (symbol? name)
                      (hashtable-put! known name node))
                  (if (and (null? tcalls)
                           (null? ncalls))
                      (if (< (callgraphnode.size node)
                             *nontail-threshold*)
                          (callgraphnode.info! node #t))
                      (if (symbol? name)
                          (set! category2 (cons node category2))
                          (set! category3 (cons node category3))))))
              g)
    (set! category2 (twobit-sort (lambda (x y)
                                   (< (callgraphnode.size x)
                                      (callgraphnode.size y)))
                                 category2))
    (for-each (lambda (node)
                (inline-node! node known))
              category2)
    (for-each (lambda (node)
                (inline-node! node known))
              category3)
    ; FIXME:
    ; Inlining destroys the callgraph, so maybe this cleanup is useless.
    (hashtable-for-each (lambda (name node) (callgraphnode.info! node #f))
                        known)))

; Given a node of the callgraph and a hash table of nodes for
; known local procedures, performs inlining by side effect.

(define (inline-node! node known)
  (let* ((debugging? #f)
         (name (callgraphnode.name node))
         (exp (callgraphnode.code node))
         (size0 (callgraphnode.size node))
         (budget (quotient (* (- *multiplier* 100) size0) 100))
         (tail-threshold *tail-threshold*)
         (nontail-threshold *nontail-threshold*))
    
    ; Given an expression,
    ; a boolean indicating whether the expression is in a tail context,
    ; a list of procedures that should not be inlined,
    ; and a size budget,
    ; performs inlining by side effect and returns the unused budget.
    
    (define (inline exp tail? budget)
        (if (positive? budget)
            
            (case (car exp)
              
              ((quote lambda)
               budget)
              
              ((set!)
               (inline (assignment.rhs exp) #f budget))
              
              ((if)
               (let* ((budget (inline (if.test exp) #f budget))
                      (budget (inline (if.then exp) tail? budget))
                      (budget (inline (if.else exp) tail? budget)))
                 budget))
              
              ((begin)
               (if (variable? exp)
                   budget
                   (do ((exprs (begin.exprs exp) (cdr exprs))
                        (budget budget
                                (inline (car exprs) #f budget)))
                       ((null? (cdr exprs))
                        (inline (car exprs) tail? budget)))))
              
              (else
               (let ((budget (do ((exprs (call.args exp) (cdr exprs))
                                  (budget budget
                                          (inline (car exprs) #f budget)))
                                 ((null? exprs)
                                  budget))))
                 (let ((proc (call.proc exp)))
                   (cond ((variable? proc)
                          (let* ((procname (variable.name proc))
                                 (procnode (hashtable-get known procname)))
                            (if procnode
                                (let ((size (callgraphnode.size procnode))
                                      (info (callgraphnode.info procnode)))
                                  (if (and info
                                           (<= size budget)
                                           (<= size
                                               (if tail?
                                                   tail-threshold
                                                   nontail-threshold)))
                                      (begin
                                       (if debugging?
                                           (begin
                                            (display "    Inlining ")
                                            (write (variable.name proc))
                                            (newline)))
                                       (call.proc-set!
                                        exp
                                        (copy-exp
                                         (callgraphnode.code procnode)))
                                       (callgraphnode.size!
                                        node
                                        (+ (callgraphnode.size node) size))
                                       (- budget size))
                                      (begin
                                       (if (and #f debugging?)
                                           (begin
                                            (display "    Declining to inline ")
                                            (write (variable.name proc))
                                            (newline)))
                                       budget)))
                                budget)))
                         ((lambda? proc)
                          (inline (lambda.body proc) tail? budget))
                         (else
                          (inline proc #f budget)))))))
            -1))
    
    (if (and #f debugging?)
        (begin
         (display "Processing ")
         (write name)
         (newline)))
    
    (let ((budget (inline (if (lambda? exp)
                              (lambda.body exp)
                              exp)
                          #t
                          budget)))
      (if (and (negative? budget)
               debugging?)
          ; This shouldn't happen very often.
          (begin (display "Ran out of inlining budget for ")
                 (write (callgraphnode.name node))
                 (newline)))
      (if (<= (callgraphnode.size node) nontail-threshold)
          (callgraphnode.info! node #t))
      #f)))

; For testing.

(define (test-inlining test0)
  (begin (define exp0 (begin (display "Compiling...")
                             (newline)
                             (pass2 (pass1 test0))))
         (define g0 (begin (display "Computing call graph...")
                           (newline)
                           (callgraph exp0)))
         (display "Inlining...")
         (newline)
         (inline-using-callgraph! g0)
         (pretty-print (make-readable (copy-exp exp0)))))
