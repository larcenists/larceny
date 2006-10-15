; This benchmark was obtained from Andrew Wright.
; 970215 / wdc Changed {box, unbox, set-box!} to {list, car, set-car!},
;              flushed #% prefixes, defined void,
;              and added nbody-benchmark.
; 981116 / wdc Replaced nbody-benchmark by main, added apply:+.

(define void
  (let ((invisible (string->symbol "")))
    (lambda () invisible)))

(define (apply:+ xs)
  (do ((result 0.0 (FLOAT+ result (car xs)))
       (xs xs (cdr xs)))
      ((null? xs) result)))

;; code is slightly broken...

(define vect (vector))
; minimal standard random number generator
; 32 bit integer version
; cacm 31 10, oct 88
;

(define *seed* (list 1))

(define (srand seed)
  (set-car! *seed* seed))

(define (rand)
  (let* ((hi (quotient (car *seed*) 127773))
         (lo (modulo (car *seed*) 127773))
         (test (- (* 16807 lo) (* 2836 hi))))
    (if (> test 0)
        (set-car! *seed* test)
        (set-car! *seed* (+ test 2147483647)))
    (car *seed*)))

;; return a random number in the interval [0,n)
(define random
  (lambda (n)
    (modulo (abs (rand)) n)))
    

(define (array-ref a . indices)
  (let loop ((a a) (indices indices))
    (if (null? indices)
        a
	(loop (vector-ref a (car indices)) (cdr indices)))))

(define (atan0 y x)
  (if (and (= x y) (= x 0)) 0 (atan y x)))


;change this to desired precision
;measured in order of expansions calculated
(define precision 10)

;;; =========================================================
;;;                        Algorithm
;;; =========================================================

(define (cartesian-algorithm tree)
  (up! tree
       cartesian-make-multipole-expansion
       cartesian-multipole-shift
       cartesian-expansion-sum)
  (down! tree
         cartesian-multipole-to-local-convert
         cartesian-local-shift
         cartesian-eval-local-expansion
         cartesian-expansion-sum
         cartesian-zero-expansion))

(define (spherical-algorithm tree)
  (up! tree
       spherical-make-multipole-expansion
       spherical-multipole-shift
       spherical-expansion-sum)
  (down! tree
         spherical-multipole-to-local-convert
         spherical-local-shift
         spherical-eval-local-expansion
         spherical-expansion-sum
         spherical-zero-expansion))

;;; The upward path in the algorithm calculates
;;; multipole expansions at every node.

(define (up! tree
             make-multipole-expansion
             multipole-shift
             expansion-sum)
  (let loop ((node (tree-body tree)))
    (define center (node-center node))

    (if (leaf-node? node)

	(let ((multipole-expansion
	       (expansion-sum
		(map (lambda (particle)
		       (make-multipole-expansion
			(pt- (particle-position particle) center)
			(particle-strength particle)))
		     (node-particles node)))))
	  (set-node-multipole-expansion! node multipole-expansion)
	  (cons center multipole-expansion))

	(let ((multipole-expansion
	       (expansion-sum
		(map (lambda (child)
		       (define center-and-expansion (loop child))
		       (multipole-shift (pt- (car center-and-expansion)
					     center)
					(cdr center-and-expansion)))
		     (node-children node)))))
	  (set-node-multipole-expansion! node multipole-expansion)
	  (cons center multipole-expansion)))))

;;; Downward path of the algorithm which calculates local expansionss
;;; at every node and accelerations and potentials at each particle.

(define (down! tree
               multipole-to-local-convert
               local-shift
               eval-local-expansion
               expansion-sum
               zero-expansion)
  (let loop ((node (tree-body tree))
	     (parent-local-expansion (zero-expansion))
	     (parent-center (node-center (tree-body tree))))
    (let* ((center (node-center node))
	   (interactive-sum
            (expansion-sum
	     (map (lambda (interactive)
		    (multipole-to-local-convert
		     (pt- (node-center interactive) center)
		     (node-multipole-expansion interactive)))
		  (node-interactive-field node))))
	   (local-expansion
	    (expansion-sum (list (local-shift (pt- center parent-center)
					      parent-local-expansion)
				 interactive-sum))))
      (if (leaf-node? node)
	  (eval-potentials-and-accelerations
	   node
	   local-expansion
	   eval-local-expansion)
	  (for-each (lambda (child) (loop child local-expansion center))
		    (node-children node))))))

(define (eval-potentials-and-accelerations
         node
         local-expansion
         eval-local-expansion)
  (let ((center (node-center node))
        (near-field (apply append (map node-particles
					(node-near-field node)))))
    (for-each (lambda (particle)
		(let* ((pos (particle-position particle))
		       (far-field-accel-and-poten
			(eval-local-expansion (pt- pos center)
					      local-expansion)))
		  (set-particle-acceleration!
		   particle
		   (pt+ (car far-field-accel-and-poten)
			(sum-vectors
			 (map (lambda (near)
				(direct-accel (pt- (particle-position near)
						   pos)
					      (particle-strength near)))
			      (nfilter near-field
				      (lambda (near)
					(not (eq? near particle))))))))
		  (set-particle-potential!
		   particle
		   (+ (cdr far-field-accel-and-poten)
		      (apply:+ (map (lambda (near)
				      (direct-poten
				       (pt- (particle-position near) pos)
				       (particle-strength near)))
				    (nfilter near-field
					    (lambda (near)
					      (not
					       (eq? near particle))))))))))
	      (node-particles node))))

;;; ================================================================
;;;                    Expansion Theorems
;;; ================================================================

(define (cartesian-make-multipole-expansion pt strength)
  (let ((-x (- (pt-x pt)))
        (-y (- (pt-y pt)))
        (-z (- (pt-z pt))))
    (make-cartesian-expansion
     (lambda (i j k) (* strength
                        (expt -x i)
                        (expt -y j)
                        (expt -z k)
                        (1/prod-fac i j k))))))

(define (spherical-make-multipole-expansion pt strength)
  (let ((r (pt-r pt))
        (theta (pt-theta pt))
        (phi (pt-phi pt)))
    (make-spherical-expansion
     (lambda (l m)
       (* strength
          (expt r l)
          (eval-spherical-harmonic l (- m) theta phi))))))

;;; ==================================================================
;;;                    Shifting lemmas
;;; ==================================================================

;;; Shift multipole expansion

(define (cartesian-multipole-shift pt multipole-expansion)
  (let ((pt-expansion (cartesian-make-multipole-expansion pt 1)))
    (make-cartesian-expansion
     (lambda (i j k)
       (sum-3d i j k
	       (lambda (l m n)
		 (* (array-ref multipole-expansion l m n)
		    (array-ref pt-expansion (- i l) (- j m) (- k n)))))))))

(define (spherical-multipole-shift pt multipole-expansion)
  (let ((r (pt-r pt))
        (theta (pt-theta pt))
        (phi (pt-phi pt)))
    (letrec ((foo (lambda (a b) 
		    (if (< (* a b) 0)
			(expt -1 (min (abs a) (abs b)))
			1)))
	     (bar (lambda (a b) 
		    (/ (expt -1 a)
		       (sqrt (* (fac (- a b)) (fac (+ a b))))))))
      (let ((pt-expansion (make-spherical-expansion
			   (lambda (l m)
			     (* (eval-spherical-harmonic l m theta phi)
				(bar l m)
				(expt r l))))))
	(make-spherical-expansion
	 (lambda (j k)
	   (sum-2d j
		   (lambda (l m)
		     (if (> (abs (- k m)) (- j l))
			 0
			 (* (spherical-ref multipole-expansion (- j l) (- k m))
			    (foo m (- k m))
			    (bar (- j l) (- k m))
			    (spherical-ref pt-expansion l (- m))
			    (/ (bar j k))))))))))))

;;; Convert multipole to local

(define (cartesian-multipole-to-local-convert pt multipole-expansion)
  (define pt-expansion
    (let* ((1/radius (/ (pt-r vect)))
	   (2cosines (pt-scalar* (* 2 1/radius) vect))
	   (x (pt-x 2cosines))
	   (y (pt-y 2cosines))
	   (z (pt-z 2cosines)))
      (make-cartesian-expansion
       (lambda (i j k)
	 (define ijk (+ i j k))
	 (* (expt -1 ijk)
	    (expt 1/radius (+ 1 ijk))
	    (prod-fac i j k)
	    (sum-3d (/ i 2) (/ j 2) (/ k 2)
		    (lambda (l m n)
		      (* (fac-1 (- ijk l m n))
			 (1/prod-fac l m n)
			 (1/prod-fac (- i (* 2 l))
				     (- j (* 2 m))
				     (- k (* 2 n)))
			 (expt x (- i (* 2 l)))
			 (expt y (- j (* 2 m)))
			 (expt z (- k (* 2 n)))))))))))
  (make-cartesian-expansion
   (lambda (i j k)
     (sum2-3d (- precision i j k)
	      (lambda (l m n)
		(* (array-ref multipole-expansion l m n)
		   (array-ref pt-expansion (+ i l) (+ j m) (+ k n))))))))

(define (spherical-multipole-to-local-convert pt multipole-expansion)
  (let* ((r (pt-r pt))
	 (theta (pt-theta pt))
	 (phi (pt-phi pt))
	 (foo (lambda (a b c) 
		(* (expt -1 b)
		   (if (> (* a c) 0)
		       (expt -1 (min (abs a) (abs c)))
		       1))))
	 (bar (lambda (a b) 
		(/ (expt -1 a)
		   (sqrt (* (fac (- a b)) (fac (+ a b)))))))
	 (pt-expansion (make-spherical-expansion
			(lambda (l m)
			  (/ (eval-spherical-harmonic l m theta phi)
			     (bar l m)
			     (expt r (+ 1 l)))))))
    (make-spherical-expansion
     (lambda (j k)
       (* (bar j k)
	  (sum-2d (- precision j 1)
		  (lambda (l m)
		    (* (spherical-ref multipole-expansion l m)
		       (foo k l m)
		       (bar l m)
		       (spherical-ref pt-expansion (+ j l) (- m k))))))))))

;;; Shift local expansion

(define (cartesian-local-shift pt local-expansion)
  (let* ((x (pt-x pt))
	 (y (pt-y pt))
	 (z (pt-z pt))
	 (expts (make-cartesian-expansion
		 (lambda (l m n) (* (expt x l)
				    (expt y m)
				    (expt z n))))))
    (make-cartesian-expansion
     (lambda (i j k)
       (sum2-3d (- precision i j k)
		(lambda (l m n)
		  (* (array-ref local-expansion (+ i l) (+ j m) (+ k n))
		     (array-ref expts l m n)
		     (1/prod-fac l m n))))))))

(define (spherical-local-shift pt local-expansion)
  (let* ((pt (pt- (make-pt 0 0 0) pt))
	 (r (pt-r pt))
	 (theta (pt-theta pt))
	 (phi (pt-phi pt))
	 (foo (lambda (a b c) 
		(* (expt -1 a)
		   (expt -1 (/ (+ (abs (- b c))
				  (abs b)
				  (- (abs c)))
			       2)))))
	 (bar (lambda (a b) 
		(/ (expt -1 a)
		   (sqrt (* (fac (- a b))
			    (fac (+ a b)))))))
	 (stuff (make-spherical-expansion
		   (lambda (l m)
		     (* (eval-spherical-harmonic l m theta phi)
			(expt r l))))))
    (make-spherical-expansion
     (lambda (j k)
       (sum2-2d j (lambda (l m)
		    (if (> (abs (- m k)) (- l j))
			0
			(* (spherical-ref local-expansion l m)
			   (bar (- l j) (- m k))
			   (bar j k)
			   (spherical-ref stuff (- l j) (- m k))
			   (/ (foo (- l j) (- m k) m) (bar l m))))))))))

;;; Evaluate the resulting local expansion at point pt.

(define (cartesian-eval-local-expansion pt local-expansion)
  (let* ((x (pt-x pt))
	 (y (pt-y pt))
	 (z (pt-z pt))
	 (local-expansion
	  (make-cartesian-expansion
	   (lambda (i j k)
	     (* (array-ref local-expansion i j k)
		(1/prod-fac i j k)))))
	 (expts (make-cartesian-expansion
		 (lambda (i j k)
		   (* (expt x i) (expt y j) (expt z k))))))
    (cons (make-pt
           (sum2-3d (- precision 1)
                    (lambda (l m n)
                      (* (array-ref expts l m n)
                         (+ 1 l)
                         (array-ref local-expansion (+ 1 l) m n))))
           (sum2-3d (- precision 1)
                    (lambda (l m n)
                      (* (array-ref expts l m n)
                         (+ 1 m)
                         (array-ref local-expansion l (+ 1 m) n))))
           (sum2-3d (- precision 1)
                    (lambda (l m n)
                      (* (array-ref expts l m n)
                         (+ 1 n)
                         (array-ref local-expansion l m (+ 1 n))))))
          (sum2-3d precision
                   (lambda (l m n)
                     (* (array-ref expts l m n)
                        (array-ref local-expansion l m n)))))))

(define (spherical-eval-local-expansion pt local-expansion)
  (let* ((r (pt-r pt))
	 (x (pt-x pt))
	 (y (pt-y pt))
	 (z (pt-z pt))
	 (rho-sq (+ (* x x) (* y y)))
	 (theta (pt-theta pt))
	 (phi (pt-phi pt))
	 (r-deriv
	  (real-part
	   (sum-2d (- precision 1)
		   (lambda (l m)
		     (* (spherical-ref local-expansion l m)
			(expt r (- l 1))
			l
			(eval-spherical-harmonic l m theta phi))))))
	 (theta-deriv
	  (real-part
	   (sum-2d (- precision 1)
		   (lambda (l m)
		     (* (spherical-ref local-expansion l m)
			(expt r l)
			(eval-spher-harm-theta-deriv l m theta phi))))))
	 (phi-deriv
	  (real-part
	   (sum-2d (- precision 1)
		   (lambda (l m)
		     (* (spherical-ref local-expansion l m)
			(expt r l)
			(eval-spher-harm-phi-deriv l m theta phi)))))))
    (cons (make-pt
	   (+ (* r-deriv (/ x r))
	      (* theta-deriv (/ x (sqrt rho-sq) (+ z (/ rho-sq z))))
	      (* phi-deriv -1 (/ y rho-sq)))
	   (+ (* r-deriv (/ y r))
	      (* theta-deriv (/ y (sqrt rho-sq) (+ z (/ rho-sq z))))
	      (/ phi-deriv x (+ 1 (/ (* y y) x x))))
	   (+ (* r-deriv (/ z r))
	      (* theta-deriv (/ -1 r r) (sqrt rho-sq))))
	  (real-part (sum-2d (- precision 1)
			     (lambda (l m)
			       (* (spherical-ref local-expansion l m)
				  (eval-spherical-harmonic l m theta phi)
				  (expt r l))))))))

;;; Direct calculation of acceleration and potential

(define (direct-accel pt strength)
  (pt-scalar* (/ strength (expt (pt-r pt) 3))
              pt))

(define (direct-poten pt strength)
  (/ strength (pt-r pt)))

;;; =================================================================
;;;                 TREES NODES PARTICLES and POINTS
;;; =================================================================

(begin (begin (begin (define make-raw-tree
                       (lambda (tree-1 tree-2 tree-3)
                         (vector '<tree> tree-1 tree-2 tree-3)))
                     (define tree?
                       (lambda (obj)
                         (if (vector? obj)
                             (if (= (vector-length obj) 4)
                                 (eq? (vector-ref obj 0) '<tree>)
                                 #f)
                             #f)))
                     (define tree-1
                       (lambda (obj)
                         (if (tree? obj)
                             (void)
                             (error
                               'tree-1
                               "~s is not a ~s"
                               obj
                               '<tree>))
                         (vector-ref obj 1)))
                     (define tree-2
                       (lambda (obj)
                         (if (tree? obj)
                             (void)
                             (error
                               'tree-2
                               "~s is not a ~s"
                               obj
                               '<tree>))
                         (vector-ref obj 2)))
                     (define tree-3
                       (lambda (obj)
                         (if (tree? obj)
                             (void)
                             (error
                               'tree-3
                               "~s is not a ~s"
                               obj
                               '<tree>))
                         (vector-ref obj 3)))
                     (define set-tree-1!
                       (lambda (obj newval)
                         (if (tree? obj)
                             (void)
                             (error
                               'set-tree-1!
                               "~s is not a ~s"
                               obj
                               '<tree>))
                         (vector-set! obj 1 newval)))
                     (define set-tree-2!
                       (lambda (obj newval)
                         (if (tree? obj)
                             (void)
                             (error
                               'set-tree-2!
                               "~s is not a ~s"
                               obj
                               '<tree>))
                         (vector-set! obj 2 newval)))
                     (define set-tree-3!
                       (lambda (obj newval)
                         (if (tree? obj)
                             (void)
                             (error
                               'set-tree-3!
                               "~s is not a ~s"
                               obj
                               '<tree>))
                         (vector-set! obj 3 newval))))
              (define make-tree
                (lambda (body low-left-front-vertex up-right-back-vertex)
                  ((lambda ()
                     (make-raw-tree
                       body
                       low-left-front-vertex
                       up-right-back-vertex)))))
              (define tree-body tree-1)
              (define tree-low-left-front-vertex tree-2)
              (define tree-up-right-back-vertex tree-3)
              (define set-tree-body! set-tree-1!)
              (define set-tree-low-left-front-vertex! set-tree-2!)
              (define set-tree-up-right-back-vertex! set-tree-3!))
       (begin (begin (define make-raw-node
                       (lambda (node-1
                                node-2
                                node-3
                                node-4
                                node-5
                                node-6
                                node-7
                                node-8)
                         (vector
                           '<node>
                           node-1
                           node-2
                           node-3
                           node-4
                           node-5
                           node-6
                           node-7
                           node-8)))
                     (define node?
                       (lambda (obj)
                         (if (vector? obj)
                             (if (= (vector-length obj) 9)
                                 (eq? (vector-ref obj 0) '<node>)
                                 #f)
                             #f)))
                     (define node-1
                       (lambda (obj)
                         (if (node? obj)
                             (void)
                             (error
                               'node-1
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-ref obj 1)))
                     (define node-2
                       (lambda (obj)
                         (if (node? obj)
                             (void)
                             (error
                               'node-2
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-ref obj 2)))
                     (define node-3
                       (lambda (obj)
                         (if (node? obj)
                             (void)
                             (error
                               'node-3
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-ref obj 3)))
                     (define node-4
                       (lambda (obj)
                         (if (node? obj)
                             (void)
                             (error
                               'node-4
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-ref obj 4)))
                     (define node-5
                       (lambda (obj)
                         (if (node? obj)
                             (void)
                             (error
                               'node-5
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-ref obj 5)))
                     (define node-6
                       (lambda (obj)
                         (if (node? obj)
                             (void)
                             (error
                               'node-6
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-ref obj 6)))
                     (define node-7
                       (lambda (obj)
                         (if (node? obj)
                             (void)
                             (error
                               'node-7
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-ref obj 7)))
                     (define node-8
                       (lambda (obj)
                         (if (node? obj)
                             (void)
                             (error
                               'node-8
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-ref obj 8)))
                     (define set-node-1!
                       (lambda (obj newval)
                         (if (node? obj)
                             (void)
                             (error
                               'set-node-1!
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-set! obj 1 newval)))
                     (define set-node-2!
                       (lambda (obj newval)
                         (if (node? obj)
                             (void)
                             (error
                               'set-node-2!
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-set! obj 2 newval)))
                     (define set-node-3!
                       (lambda (obj newval)
                         (if (node? obj)
                             (void)
                             (error
                               'set-node-3!
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-set! obj 3 newval)))
                     (define set-node-4!
                       (lambda (obj newval)
                         (if (node? obj)
                             (void)
                             (error
                               'set-node-4!
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-set! obj 4 newval)))
                     (define set-node-5!
                       (lambda (obj newval)
                         (if (node? obj)
                             (void)
                             (error
                               'set-node-5!
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-set! obj 5 newval)))
                     (define set-node-6!
                       (lambda (obj newval)
                         (if (node? obj)
                             (void)
                             (error
                               'set-node-6!
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-set! obj 6 newval)))
                     (define set-node-7!
                       (lambda (obj newval)
                         (if (node? obj)
                             (void)
                             (error
                               'set-node-7!
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-set! obj 7 newval)))
                     (define set-node-8!
                       (lambda (obj newval)
                         (if (node? obj)
                             (void)
                             (error
                               'set-node-8!
                               "~s is not a ~s"
                               obj
                               '<node>))
                         (vector-set! obj 8 newval))))
              (define make-node
                (lambda (center
                         low-left-front-vertex
                         up-right-back-vertex
                         children
                         particles
                         multipole-expansion
                         near-field
                         interactive-field)
                  ((lambda ()
                     (make-raw-node
                       center
                       low-left-front-vertex
                       up-right-back-vertex
                       children
                       particles
                       multipole-expansion
                       near-field
                       interactive-field)))))
              (define node-center node-1)
              (define node-low-left-front-vertex node-2)
              (define node-up-right-back-vertex node-3)
              (define node-children node-4)
              (define node-particles node-5)
              (define node-multipole-expansion node-6)
              (define node-near-field node-7)
              (define node-interactive-field node-8)
              (define set-node-center! set-node-1!)
              (define set-node-low-left-front-vertex! set-node-2!)
              (define set-node-up-right-back-vertex! set-node-3!)
              (define set-node-children! set-node-4!)
              (define set-node-particles! set-node-5!)
              (define set-node-multipole-expansion! set-node-6!)
              (define set-node-near-field! set-node-7!)
              (define set-node-interactive-field! set-node-8!))
       (define leaf-node?
         (lambda (node) (null? (node-children node))))
       (begin (begin (define make-raw-particle
                       (lambda (particle-1
                                particle-2
                                particle-3
                                particle-4
                                particle-5
                                particle-6)
                         (vector
                           '<particle>
                           particle-1
                           particle-2
                           particle-3
                           particle-4
                           particle-5
                           particle-6)))
                     (define particle?
                       (lambda (obj)
                         (if (vector? obj)
                             (if (= (vector-length obj) 7)
                                 (eq? (vector-ref obj 0) '<particle>)
                                 #f)
                             #f)))
                     (define particle-1
                       (lambda (obj)
                         (if (particle? obj)
                             (void)
                             (error
                               'particle-1
                               "~s is not a ~s"
                               obj
                               '<particle>))
                         (vector-ref obj 1)))
                     (define particle-2
                       (lambda (obj)
                         (if (particle? obj)
                             (void)
                             (error
                               'particle-2
                               "~s is not a ~s"
                               obj
                               '<particle>))
                         (vector-ref obj 2)))
                     (define particle-3
                       (lambda (obj)
                         (if (particle? obj)
                             (void)
                             (error
                               'particle-3
                               "~s is not a ~s"
                               obj
                               '<particle>))
                         (vector-ref obj 3)))
                     (define particle-4
                       (lambda (obj)
                         (if (particle? obj)
                             (void)
                             (error
                               'particle-4
                               "~s is not a ~s"
                               obj
                               '<particle>))
                         (vector-ref obj 4)))
                     (define particle-5
                       (lambda (obj)
                         (if (particle? obj)
                             (void)
                             (error
                               'particle-5
                               "~s is not a ~s"
                               obj
                               '<particle>))
                         (vector-ref obj 5)))
                     (define particle-6
                       (lambda (obj)
                         (if (particle? obj)
                             (void)
                             (error
                               'particle-6
                               "~s is not a ~s"
                               obj
                               '<particle>))
                         (vector-ref obj 6)))
                     (define set-particle-1!
                       (lambda (obj newval)
                         (if (particle? obj)
                             (void)
                             (error
                               'set-particle-1!
                               "~s is not a ~s"
                               obj
                               '<particle>))
                         (vector-set! obj 1 newval)))
                     (define set-particle-2!
                       (lambda (obj newval)
                         (if (particle? obj)
                             (void)
                             (error
                               'set-particle-2!
                               "~s is not a ~s"
                               obj
                               '<particle>))
                         (vector-set! obj 2 newval)))
                     (define set-particle-3!
                       (lambda (obj newval)
                         (if (particle? obj)
                             (void)
                             (error
                               'set-particle-3!
                               "~s is not a ~s"
                               obj
                               '<particle>))
                         (vector-set! obj 3 newval)))
                     (define set-particle-4!
                       (lambda (obj newval)
                         (if (particle? obj)
                             (void)
                             (error
                               'set-particle-4!
                               "~s is not a ~s"
                               obj
                               '<particle>))
                         (vector-set! obj 4 newval)))
                     (define set-particle-5!
                       (lambda (obj newval)
                         (if (particle? obj)
                             (void)
                             (error
                               'set-particle-5!
                               "~s is not a ~s"
                               obj
                               '<particle>))
                         (vector-set! obj 5 newval)))
                     (define set-particle-6!
                       (lambda (obj newval)
                         (if (particle? obj)
                             (void)
                             (error
                               'set-particle-6!
                               "~s is not a ~s"
                               obj
                               '<particle>))
                         (vector-set! obj 6 newval))))
              (define make-particle
                (lambda (position
                         acceleration
                         d-acceleration
                         potential
                         d-potential
                         strength)
                  ((lambda ()
                     (make-raw-particle
                       position
                       acceleration
                       d-acceleration
                       potential
                       d-potential
                       strength)))))
              (define particle-position particle-1)
              (define particle-acceleration particle-2)
              (define particle-d-acceleration particle-3)
              (define particle-potential particle-4)
              (define particle-d-potential particle-5)
              (define particle-strength particle-6)
              (define set-particle-position! set-particle-1!)
              (define set-particle-acceleration! set-particle-2!)
              (define set-particle-d-acceleration! set-particle-3!)
              (define set-particle-potential! set-particle-4!)
              (define set-particle-d-potential! set-particle-5!)
              (define set-particle-strength! set-particle-6!))
       (begin (begin (define make-raw-pt
                       (lambda (pt-1 pt-2 pt-3)
                         (vector '<pt> pt-1 pt-2 pt-3)))
                     (define pt?
                       (lambda (obj)
                         (if (vector? obj)
                             (if (= (vector-length obj) 4)
                                 (eq? (vector-ref obj 0) '<pt>)
                                 #f)
                             #f)))
                     (define pt-1
                       (lambda (obj)
                         (if (pt? obj)
                             (void)
                             (error 'pt-1 "~s is not a ~s" obj '<pt>))
                         (vector-ref obj 1)))
                     (define pt-2
                       (lambda (obj)
                         (if (pt? obj)
                             (void)
                             (error 'pt-2 "~s is not a ~s" obj '<pt>))
                         (vector-ref obj 2)))
                     (define pt-3
                       (lambda (obj)
                         (if (pt? obj)
                             (void)
                             (error 'pt-3 "~s is not a ~s" obj '<pt>))
                         (vector-ref obj 3)))
                     (define set-pt-1!
                       (lambda (obj newval)
                         (if (pt? obj)
                             (void)
                             (error
                               'set-pt-1!
                               "~s is not a ~s"
                               obj
                               '<pt>))
                         (vector-set! obj 1 newval)))
                     (define set-pt-2!
                       (lambda (obj newval)
                         (if (pt? obj)
                             (void)
                             (error
                               'set-pt-2!
                               "~s is not a ~s"
                               obj
                               '<pt>))
                         (vector-set! obj 2 newval)))
                     (define set-pt-3!
                       (lambda (obj newval)
                         (if (pt? obj)
                             (void)
                             (error
                               'set-pt-3!
                               "~s is not a ~s"
                               obj
                               '<pt>))
                         (vector-set! obj 3 newval))))
              (define make-pt
                (lambda (x y z) ((lambda () (make-raw-pt x y z)))))
              (define pt-x pt-1)
              (define pt-y pt-2)
              (define pt-z pt-3)
              (define set-pt-x! set-pt-1!)
              (define set-pt-y! set-pt-2!)
              (define set-pt-z! set-pt-3!)))

;(define-structure (tree
;		   body
;		   low-left-front-vertex
;		   up-right-back-vertex))
;
;(define-structure (node
;		   center
;		   low-left-front-vertex
;		   up-right-back-vertex
;		   children
;		   particles
;		   multipole-expansion
;		   near-field
;		   interactive-field))
;
;(define (leaf-node? node)
;  (null? (node-children node)))
;
;(define-structure (particle
;		   position
;		   acceleration
;		   d-acceleration
;		   potential
;		   d-potential
;		   strength))
;
;(define-structure (pt x y z))
;
(define (pt-r pt)
  (sqrt (+ (* (pt-x pt) (pt-x pt))
           (* (pt-y pt) (pt-y pt))
           (* (pt-z pt) (pt-z pt)))))

(define (pt-theta pt)
  (let ((x (pt-x pt))
        (y (pt-y pt))
        (z (pt-z pt)))
    (atan0 (sqrt (+ (* x x) (* y y))) z)))

(define (pt-phi pt)
  (let ((x (pt-x pt)) (y (pt-y pt)))
    (atan0 y x)))

(define (pt+ pt1 pt2)
  (make-pt (+ (pt-x pt1) (pt-x pt2))
           (+ (pt-y pt1) (pt-y pt2))
           (+ (pt-z pt1) (pt-z pt2))))

(define (sum-vectors vectors)
  (make-pt (apply:+ (map pt-x vectors))
           (apply:+ (map pt-y vectors))
           (apply:+ (map pt-z vectors))))

(define (pt- pt1 pt2)
  (make-pt (- (pt-x pt1) (pt-x pt2))
           (- (pt-y pt1) (pt-y pt2))
           (- (pt-z pt1) (pt-z pt2))))

(define (pt-average pt1 pt2)
  (pt-scalar* .5 (pt+ pt1 pt2)))

(define (pt-scalar* scalar pt)
  (make-pt (* scalar (pt-x pt))
           (* scalar (pt-y pt))
           (* scalar (pt-z pt))))

(define (within-box? pt pt1 pt2)
  (and (<= (pt-x pt) (pt-x pt2)) (> (pt-x pt) (pt-x pt1))
       (<= (pt-y pt) (pt-y pt2)) (> (pt-y pt) (pt-y pt1))
       (<= (pt-z pt) (pt-z pt2)) (> (pt-z pt) (pt-z pt1))))

;;; ==========================================================
;;;                     Useful Things
;;; ==========================================================

(define (nfilter list predicate)
  (let loop ((list list))
    (cond ((null? list) '())
          ((predicate (car list)) (cons (car list) (loop (cdr list))))
          (else (loop (cdr list))))))

;;; array in the shape of a pyramid with each
;;; element a function of the indices

(define (make-cartesian-expansion func)
  (let ((expansion (make-vector precision 0)))
    (let loop1 ((i 0))
      (if (= i precision)
          expansion
          (let ((foo (make-vector (- precision i) 0)))
            (vector-set! expansion i foo)
            (let loop2 ((j 0))
              (if (= j (- precision i))
                  (loop1 (+ 1 i))
                  (let ((bar (make-vector (- precision i j) 0)))
                    (vector-set! foo j bar)
                    (let loop3 ((k 0))
                      (if (= k (- precision i j))
                          (loop2 (+ 1 j))
                          (begin (vector-set! bar k (func i j k))
                                 (loop3 (+ 1 k)))))))))))))

;;; array in the shape of a triangle with each
;;; element a function of the indices

(define (make-spherical-expansion func)
  (let ((expansion (make-vector precision 0)))
    (let loop1 ((l 0))
      (if (= l precision)
          expansion
          (let ((foo (make-vector (+ 1 l) 0)))
            (vector-set! expansion l foo)
            (let loop2 ((m 0))
              (if (= m (+ 1 l))
                  (loop1 (+ 1 l))
                  (begin (vector-set! foo m (func l m))
                         (loop2 (+ 1 m))))))))))

(define (spherical-ref expansion l m)
  (let ((conj (lambda (z) (make-rectangular (real-part z) (- (imag-part z))))))
    (if (negative? m)
	(conj (array-ref expansion l (- m)))
	(array-ref expansion l m))))

(define (cartesian-expansion-sum expansions)
  (make-cartesian-expansion
   (lambda (i j k)
     (apply:+ (map (lambda (expansion)
                     (array-ref expansion i j k))
                   expansions)))))

(define (spherical-expansion-sum expansions)
  (make-spherical-expansion
   (lambda (l m)
     (apply:+ (map (lambda (expansion)
                     (spherical-ref expansion l m))
                   expansions)))))

(define (cartesian-zero-expansion)
  (make-cartesian-expansion (lambda (i j k) 0)))

(define (spherical-zero-expansion)
  (make-spherical-expansion (lambda (l m) 0)))

(define (sum-3d end1 end2 end3 func)
  (let loop1 ((l 0) (sum 0))
    (if (> l end1)
        sum
        (loop1
         (+ 1 l)
         (+ sum
            (let loop2 ((m 0) (sum 0))
              (if (> m end2)
                  sum
                  (loop2
                   (+ 1 m)
                   (+ sum
                      (let loop3 ((n 0) (sum 0))
                        (if (> n end3)
                            sum
                            (loop3 (+ 1 n)
                                   (+ sum (func l m n))))))))))))))

(define (sum2-3d end func)
  (let loop1 ((l 0) (sum 0))
    (if (= l end)
        sum
        (loop1
         (+ 1 l)
         (+ sum
            (let loop2 ((m 0) (sum 0))
              (if (= (+ l m) end)
                  sum
                  (loop2
                   (+ 1 m)
                   (+ sum
                      (let loop3 ((n 0) (sum 0))
                        (if (= (+ l m n) end)
                            sum
                            (loop3 (+ 1 n)
                                   (+ sum (func l m n))))))))))))))

(define (sum-2d end func)
  (let loop1 ((l 0) (sum 0))
    (if (> l end)
        sum
        (loop1 (+ 1 l)
               (+ sum (let loop2 ((m (- l)) (sum 0))
                        (if (> m l)
                            sum
                            (loop2 (+ 1 m)
                                   (+ sum (func l m))))))))))

(define (sum2-2d init func)
  (let loop1 ((l init) (sum 0))
    (if (= l precision)
        sum
        (loop1 (+ 1 l)
               (+ sum (let loop2 ((m (- l)) (sum 0))
                        (if (> m l)
                            sum
                            (loop2 (+ 1 m)
                                   (+ sum (func l m))))))))))

;;; Storing factorials in a table

(define fac
  (let ((table (make-vector (* 4 precision) 0)))
    (vector-set! table 0 1)
    (let loop ((n 1))
      (if (= n (* 4 precision))
          (lambda (x) (vector-ref table x))
          (begin (vector-set! table
                              n
                              (* n (vector-ref table (- n 1))))
                 (loop (+ 1 n)))))))

;;; The table for (* (-0.5) (-1.5) (-2.5) ... (+ -0.5 -i 1))

(define fac-1
  (let ((table (make-vector precision 0)))
    (vector-set! table 0 1)
    (let loop ((n 1))
      (if (= n precision)
          (lambda (x) (vector-ref table x))
          (begin (vector-set! table
                              n
                              (* (- .5 n)
                                 (vector-ref table (- n 1))))
                 (loop (+ 1 n)))))))

(define fac-2
  (let ((table (make-vector (* 4 precision) 0)))
    (vector-set! table 0 1)
    (let loop ((n 1))
      (if (= n (* 4 precision))
          (lambda (n) (if (< n 0)
                          1
                          (vector-ref table n)))
          (begin (vector-set! table n (* (if (even? n) 1 n)
                                         (vector-ref table (- n 1))))
                 (loop (+ 1 n)))))))

;;; Storing the products of factorials in a table.

(define prod-fac
  (let ((table (make-cartesian-expansion
                (lambda (i j k) (* (fac i) (fac j) (fac k))))))
    (lambda (i j k) (array-ref table i j k))))

(define 1/prod-fac
  (let ((table (make-cartesian-expansion
                (lambda (i j k) (/ (prod-fac i j k))))))
    (lambda (i j k) (array-ref table i j k))))

(define (assoc-legendre l m x)
  (cond ((= l m) (* (expt -1 m)
                    (fac-2 (- (* 2 m) 1))
                    (expt (- 1 (* x x)) (/ m 2))))
        ((= l (+ 1 m)) (* x (+ 1 (* 2 m)) (assoc-legendre m m x)))
        (else (/ (- (* x (- (* 2 l) 1) (assoc-legendre (- l 1) m x))
                    (* (+ l m -1) (assoc-legendre (- l 2) m x)))
                 (- l m)))))

(define (eval-spherical-harmonic l m theta phi)
  (let ((mm (abs m)))
    (* (sqrt (/ (fac (- l mm)) (fac (+ l mm))))
       (assoc-legendre l mm (cos theta))
       (make-polar 1 (* m phi)))))

(define (eval-spher-harm-phi-deriv l m theta phi)
  (* (eval-spherical-harmonic l m theta phi)
     m
     (make-rectangular 0 1)))

(define (eval-spher-harm-theta-deriv l m theta phi)
  (let ((mm (abs m)))
    (* (sqrt (/ (fac (- l mm)) (fac (+ l mm))))
       (make-polar 1 (* m phi))
       (- (sin theta))
       (assoc-legendre-deriv l mm (cos theta)))))

(define (assoc-legendre-deriv l m x)
  (cond ((= l m) (* (expt -1 (+ 1 m))
                    (fac-2 (- (* 2 m) 1))
                    m
                    (expt (- 1 (* x x)) (- (/ m 2) 1))
                    x))
        ((= l (+ 1 m)) (* (+ 1 (* 2 m))
                          (+ (assoc-legendre m m x)
                             (* x (assoc-legendre-deriv m m x)))))
        (else (/ (- (* (- (* 2 l) 1)
                       (+ (assoc-legendre (- l 1) m x)
                          (* x (assoc-legendre-deriv (- l 1) m x))))
                    (* (+ l m -1) (assoc-legendre-deriv (- l 2) m x)))
                 (- l m)))))

;;; ================================================================
;;;                          TREE CODE
;;; ================================================================

(define (build-tree height near-size)
  (let* ((vertex1 (make-pt -10 -10 -10))
         (vertex2 (make-pt 10 10 10))
	 (tree (make-tree '() vertex1 vertex2)))
    (let loop ((level 0) (pt1 vertex1) (pt2 vertex2))
      (let* ((half-diagonal (pt-scalar* .5 (pt- pt2 pt1)))
	     (diag-length/2 (pt-x half-diagonal)))
	(insert-node tree level pt1 pt2)
	(if (< level height)
	    (let ((child-pt1s
		   (map (lambda (offset)
			  (pt+ pt1 (pt-scalar* diag-length/2 offset)))
			(list (make-pt 0 0 0)
			      (make-pt 0 0 1)
			      (make-pt 0 1 0)
			      (make-pt 1 0 0)
			      (make-pt 0 1 1)
			      (make-pt 1 0 1)
			      (make-pt 1 1 0)
			      (make-pt 1 1 1)))))
	      (for-each (lambda (child-pt1)
			  (loop (+ 1 level)
				child-pt1
				(pt+ child-pt1 half-diagonal)))
			child-pt1s)))))
    (calc-near-and-interaction tree near-size)
    tree))

(define (insert-node tree level pt1 pt2)
  (let* ((center (pt-average pt1 pt2))
         (new-node (make-node center pt1 pt2 '() '() '() '() '())))
    (letrec ((insert-internal (lambda (node depth)
				(if (= level depth)
				    (set-node-children! node 
							(cons new-node 
							      (node-children node)))
				    (insert-internal (find-child node center) 
						     (+ 1 depth))))))
      (if (= level 0)
	  (set-tree-body! tree new-node)
	  (insert-internal (tree-body tree) 1)))))

(define (find-child node pos)
  (let loop ((children (node-children node)))
    (let ((child (car children)))
      (if (within-box? pos
		       (node-low-left-front-vertex child)
		       (node-up-right-back-vertex child))
	  child
	  (loop (cdr children))))))

(define (insert-particle tree particle)
  (let* ((pos (particle-position particle)))
    (letrec ((insert-internal (lambda (node)
				(if (leaf-node? node)
				    (set-node-particles!
				     node
				     (cons particle (node-particles node)))
				    (insert-internal (find-child node pos))))))
      (if (within-box? pos
		       (tree-low-left-front-vertex tree)
		       (tree-up-right-back-vertex tree))
	  (insert-internal (tree-body tree))
	  (error 'insert-particle "particle not within boundaries of tree" particle)))))

;;; This function finds the near and
;;; interaction fields for every node in the tree.

(define (calc-near-and-interaction tree near-size)
  (set-node-near-field! (tree-body tree) (list (tree-body tree)))
  (let loop ((node (tree-body tree)) (parent #f))
    (if parent
	(let* ((center (node-center node))
	       (dist (* near-size
			(abs (- (pt-x center)
				(pt-x (node-center parent)))))))
	  (for-each
	   (lambda (parent-near)
	     (let ((interactives (list '())))
	       (for-each
		(lambda (child)
		  (if (> (pt-r (pt- center (node-center child))) dist)
		      (set-car! interactives (cons child (car interactives)))
		      (set-node-near-field!
		       node
		       (cons child (node-near-field node)))))
		(node-children parent-near))
	       (set-node-interactive-field!
		node
		(append (car interactives) (node-interactive-field node)))))
	   (node-near-field parent))))
    (for-each (lambda (child) (loop child node)) (node-children node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GO

(define (initial-particle x y z m)
    (make-particle (make-pt x y z)
		   (make-pt 0 0 0)
		   (make-pt 0 0 0)
		   0 0 m))

(define (random-float bot top)
  (+ (* (- top bot)
        (/ (* (random 1000000) 1.0) 1000000.0))
     bot))

(define (random-particle)
  (make-particle (make-pt (random-float -10.0 10.0)
			  (random-float -10.0 10.0)
			  (random-float -10.0 10.0))
		 (make-pt 0 0 0)
		 (make-pt 0 0 0)
		 0 0 1.0))

(define *particles* (list '()))

(define (go depth precision n-particles)
  (let ((tree (build-tree depth 27))
	(particles (let next ((i 0) (ps '()))
		     (if (<= i n-particles)
		       (next (+ i 1) (cons (random-particle) ps))
		       ps))))
    (for-each (lambda (p) (insert-particle tree p)) particles)
    (cartesian-algorithm tree)
    (set-car! *particles* particles)))

;;; virtual time for cartesian-algorithm step
;;; (go 1 3 10)      0.31 seconds
;;; (go 3 5 128)  1397.31
;;; (go 3 5 256)  1625.29
;;; (go 3 5 512)  2380.35
;;; (go 2 5 128)    27.44 seconds

(define (main . args)
  (run-benchmark
    "nbody"
    nbody-iters
    (lambda () #t)
    (lambda (i j k) (go i j k))
    2 5 128))
