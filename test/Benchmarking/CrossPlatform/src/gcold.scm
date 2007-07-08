;
;  GCOld.sch x.x 00/08/03
;  translated from GCOld.java 2.0a 00/08/23
; 
; Copyright 2000 Sun Microsystems, Inc. All rights reserved.
; 
;

; Should be good enough for this benchmark.

(define (newRandom)
  (letrec ((random14
            (lambda (n)
              (set! x (remainder (+ (* a x) c) m))
              (remainder (quotient x 8) n)))
           (a 701)
           (x 1)
           (c 743483)
           (m 524288)
           (loop
            (lambda (q r n)
              (if (zero? q)
                  (remainder r n)
                  (loop (quotient q 16384)
                        (+ (* 16384 r) (random14 16384))
                        n)))))
    (lambda (n)
      (if (and (exact? n) (integer? n) (< n 16384))
          (random14 n)
          (loop n (random14 16384) n)))))

; A TreeNode is a record with three fields: left, right, val.
; The left and right fields contain a TreeNode or 0, and the
; val field will contain the integer height of the tree.

(define-syntax newTreeNode
  (syntax-rules ()
    ((newTreeNode left right val)
     (vector left right val))
    ((newTreeNode)
     (vector 0 0 0))))

(define-syntax TreeNode.left
  (syntax-rules ()
    ((TreeNode.left node)
     (vector-ref node 0))))

(define-syntax TreeNode.right
  (syntax-rules ()
    ((TreeNode.right node)
     (vector-ref node 1))))

(define-syntax TreeNode.val
  (syntax-rules ()
    ((TreeNode.val node)
     (vector-ref node 2))))

(define-syntax setf
  (syntax-rules (TreeNode.left TreeNode.right TreeNode.val)
    ((setf (TreeNode.left node) x)
     (vector-set! node 0 x))
    ((setf (TreeNode.right node) x)
     (vector-set! node 1 x))
    ((setf (TreeNode.val node) x)
     (vector-set! node 2 x))))

;  Args:
;  live-data-size: in megabytes.
;  work: units of mutator non-allocation work per byte allocated,
;    (in unspecified units.  This will affect the promotion rate
;     printed at the end of the run: more mutator work per step implies
;     fewer steps per second implies fewer bytes promoted per second.)
;  short/long ratio: ratio of short-lived bytes allocated to long-lived
;     bytes allocated.
;  pointer mutation rate: number of pointer mutations per step.
;  steps: number of steps to do.
;

(define (GCOld size workUnits promoteRate ptrMutRate steps)

  (define (println . args)
    (for-each display args)
    (newline))

  ; Rounds an inexact real to two decimal places.

  (define (round2 x)
    (/ (round (* 100.0 x)) 100.0))

  ; Returns the height of the given tree.

  (define (height t)
    (if (eqv? t 0)
        0
        (+ 1 (max (height (TreeNode.left t))
                  (height (TreeNode.right t))))))

  ; Returns the length of the shortest path in the given tree.

  (define (shortestPath t)
    (if (eqv? t 0)
        0
        (+ 1 (min (shortestPath (TreeNode.left t))
                  (shortestPath (TreeNode.right t))))))

  ; Returns the number of nodes in a balanced tree of the given height.

  (define (heightToNodes h)
    (- (expt 2 h) 1))

  ; Returns the height of the largest balanced tree
  ; that has no more than the given number of nodes.

  (define (nodesToHeight nodes)
    (do ((h 1 (+ h 1))
         (n 1 (+ n n)))
        ((> (+ n n -1) nodes)
         (- h 1))))

  (let* (

         ; Constants.

         (null 0)            ; Java's null
         (pathBits 65536)    ; to generate 16 random bits

         (MEG 1000000)
         (INSIGNIFICANT 999) ; this many bytes don't matter
         (bytes/word 4)
         (bytes/node 20)     ; bytes per tree node in typical JVM
         (words/dead 100)    ; size of young garbage objects

         ; Returns the number of bytes in a balanced tree of the given height.

         (heightToBytes
           (lambda (h)
             (* bytes/node (heightToNodes h))))

         ; Returns the height of the largest balanced tree
         ; that occupies no more than the given number of bytes.

         (bytesToHeight
           (lambda (bytes)
             (nodesToHeight (/ bytes bytes/node))))

         (treeHeight 14)
         (treeSize (heightToBytes treeHeight))

         (msg1 "Usage: java GCOld <size> <work> <ratio> <mutation> <steps>")
         (msg2 "  where <size> is the live storage in megabytes")
         (msg3 "        <work> is the mutator work per step (arbitrary units)")
         (msg4 "        <ratio> is the ratio of short-lived to long-lived allocation")
         (msg5 "        <mutation> is the mutations per step")
         (msg6 "        <steps> is the number of steps")

         ; Counters (and global variables that discourage optimization).

         (youngBytes 0)
         (nodes 0)
         (actuallyMut 0)
         (mutatorSum 0)
         (aexport '#())

         ; Global variables.

         (trees '#())
         (where 0)
         (rnd (newRandom))

         )

    ; Returns a newly allocated balanced binary tree of height h.

    (define (makeTree h)
      (if (zero? h)
          null
          (let ((res (newTreeNode)))
            (set! nodes (+ nodes 1))
            (setf (TreeNode.left res) (makeTree (- h 1)))
            (setf (TreeNode.right res) (makeTree (- h 1)))
            (setf (TreeNode.val res) h)
            res)))

    ; Allocates approximately size megabytes of trees and stores
    ; them into a global array.

    (define (init)
      ; Each tree will be about a megabyte.
      (let ((ntrees (quotient (* size MEG) treeSize)))
	(set! trees (make-vector ntrees null))
	(println "Allocating " ntrees " trees.")
        (println "  (" (* ntrees treeSize) " bytes)")
	(do ((i 0 (+ i 1)))
	    ((>= i ntrees))
	  (vector-set! trees i (makeTree treeHeight))
	  (doYoungGenAlloc (* promoteRate ntrees treeSize) words/dead))
	(println "  (" nodes " nodes)")))

    ; Confirms that all trees are balanced and have the correct height.

    (define (checkTrees)
      (let ((ntrees (vector-length trees)))
        (do ((i 0 (+ i 1)))
            ((>= i ntrees))
          (let* ((t (vector-ref trees i))
                 (h1 (height t))
                 (h2 (shortestPath t)))
            (if (or (not (= h1 treeHeight))
                    (not (= h2 treeHeight)))
                (println "*****BUG: " h1 " " h2))))))

    ; Called only by replaceTree (below) and by itself.

    (define (replaceTreeWork full partial dir)
      (let ((canGoLeft (and (not (eq? (TreeNode.left full) null))
			    (> (TreeNode.val (TreeNode.left full))
			       (TreeNode.val partial))))
	    (canGoRight (and (not (eq? (TreeNode.right full) null))
			     (> (TreeNode.val (TreeNode.right full))
			        (TreeNode.val partial)))))
	(cond ((and canGoLeft canGoRight)
	       (if dir
		   (replaceTreeWork (TreeNode.left full)
				    partial
				    (not dir))
		   (replaceTreeWork (TreeNode.right full)
				    partial
				    (not dir))))
	      ((and (not canGoLeft) (not canGoRight))
	       (if dir
		   (setf (TreeNode.left full) partial)
		   (setf (TreeNode.right full) partial)))
	      ((not canGoLeft)
	       (setf (TreeNode.left full) partial))
	      (else
	       (setf (TreeNode.right full) partial)))))

    ; Given a balanced tree full and a smaller balanced tree partial,
    ; replaces an appropriate subtree of full by partial, taking care
    ; to preserve the shape of the full tree.

    (define (replaceTree full partial)
      (let ((dir (zero? (modulo (TreeNode.val partial) 2))))
        (set! actuallyMut (+ actuallyMut 1))
	(replaceTreeWork full partial dir)))

    ; Allocates approximately n bytes of long-lived storage,
    ; replacing oldest existing long-lived storage.

    (define (oldGenAlloc n)
      (let ((full (quotient n treeSize))
	    (partial (modulo n treeSize)))
	;(println "In oldGenAlloc, doing "
	;         full
	;         " full trees and one partial tree of size "
	;         partial)
	(do ((i 0 (+ i 1)))
	    ((>= i full))
	  (vector-set! trees where (makeTree treeHeight))
	  (set! where
                (modulo (+ where 1) (vector-length trees))))
	(let loop ((partial partial))
          (if (> partial INSIGNIFICANT)
              (let* ((h (bytesToHeight partial))
                     (newTree (makeTree h)))
                (replaceTree (vector-ref trees where) newTree)
                (set! where
                      (modulo (+ where 1) (vector-length trees)))
                (loop (- partial (heightToBytes h))))))))

    ; Interchanges two randomly selected subtrees (of same size and depth).

    (define (oldGenSwapSubtrees)
      ; Randomly pick:
      ;   * two tree indices 
      ;   * A depth
      ;   * A path to that depth.
      (let* ((index1 (rnd (vector-length trees)))
	     (index2 (rnd (vector-length trees)))
	     (depth (rnd treeHeight))
	     (path (rnd pathBits))
	     (tn1 (vector-ref trees index1))
	     (tn2 (vector-ref trees index2)))
	(do ((i 0 (+ i 1)))
	    ((>= i depth))
	  (if (even? path)
	      (begin (set! tn1 (TreeNode.left tn1))
		     (set! tn2 (TreeNode.left tn2)))
	      (begin (set! tn1 (TreeNode.right tn1))
		     (set! tn2 (TreeNode.right tn2))))
	  (set! path (quotient path 2)))
	(if (even? path)
	    (let ((tmp (TreeNode.left tn1)))
	      (setf (TreeNode.left tn1) (TreeNode.left tn2))
	      (setf (TreeNode.left tn2) tmp))
	    (let ((tmp (TreeNode.right tn1)))
	      (setf (TreeNode.right tn1) (TreeNode.right tn2))
	      (setf (TreeNode.right tn2) tmp)))
	(set! actuallyMut (+ actuallyMut 2))))

    ; Update "n" old-generation pointers.

    (define (oldGenMut n)
      (do ((i 0 (+ i 1)))
	  ((>= i (quotient n 2)))
	(oldGenSwapSubtrees)))

    ; Does the amount of mutator work appropriate for n bytes of young-gen
    ; garbage allocation.

    (define (doMutWork n)
      (let ((limit (quotient (* workUnits n) 10)))
	(do ((k 0 (+ k 1))
	     (sum 0 (+ sum 1)))
	    ((>= k limit)
             ; We don't want dead code elimination to eliminate this loop.
	     (set! mutatorSum (+ mutatorSum sum))))))

    ; Allocate n bytes of young-gen garbage, in units of "nwords"
    ; words.

    (define (doYoungGenAlloc n nwords)
      (let ((nbytes (* nwords bytes/word)))
	(do ((allocated 0 (+ allocated nbytes)))
	    ((>= allocated n)
             (set! youngBytes (+ youngBytes allocated)))
	  (set! aexport (make-vector nwords 0)))))

    ; Allocate "n" bytes of young-gen data; and do the
    ; corresponding amount of old-gen allocation and pointer
    ; mutation.

    ; oldGenAlloc may perform some mutations, so this code
    ; takes those mutations into account.

    (define (doStep n)
      (let ((mutations actuallyMut))
	(doYoungGenAlloc n words/dead)
	(doMutWork n)
	; Now do old-gen allocation
	(oldGenAlloc (quotient n promoteRate))
	(oldGenMut (max 0 (- (+ mutations ptrMutRate) actuallyMut)))))

    (println size " megabytes")
    (println workUnits " work units per step.")
    (println "promotion ratio is 1:" promoteRate)
    (println "pointer mutation rate is " ptrMutRate)
    (println steps " steps")

    (init)
    (checkTrees)
    (set! youngBytes 0)
    (set! nodes 0)

    (println "Initialization complete...")

    (run-benchmark "GCOld"
		   1
		   (lambda (result) #t)
		   (lambda ()
                     (lambda ()
                       (do ((step 0 (+ step 1)))
                           ((>= step steps))
                         (doStep MEG)))))

    (checkTrees)

    (println "Allocated " steps " Mb of young gen garbage")
    (println "    (actually allocated "
             (round2 (/ youngBytes MEG))
             " megabytes)")
    (println "Promoted " (round2 (/ steps promoteRate)) " Mb")
    (println "    (actually promoted "
             (round2 (/ (* nodes bytes/node) MEG))
             " megabytes)")
    (if (not (zero? ptrMutRate))
        (println "Mutated " actuallyMut " pointers"))

    ; This output serves mainly to discourage optimization.

    (+ mutatorSum (vector-length aexport))))

(define (main . args)
  (GCOld 25 0 10 10 gcold-iters))
