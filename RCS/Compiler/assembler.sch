; -*- Scheme -*-
;
; The assembler takes virtual machine code and creates loadable scheme objects.

(define (assemble expr)

  ; simple queue package

  (define (new-queue)
    (cons '() '()))

  (define (enqueue! q v)
    (let ((vcell (cons v '())))
      (if (null? (cdr q))
	  (begin (set-car! q vcell)
		 (set-cdr! q vcell))
	  (begin (set-cdr! (cdr q) vcell)
		 (set-cdr! q vcell)))))

  (define (dequeue! q)
    (let ((vcell (car q)))
      (set-car! q (cdr vcell))
      (if (null? (car q))
	  (set-cdr! q '()))
      (car vcell)))

  (define (queue-empty? q)
    (null? (cdr q)))

  ; proc has form (lambda <expr-list>)
  ; returns new codevector. Side-effects queue.

  (define (expand-proc q proc vector)
    (let loop ((instr (cadr proc)) (v vector))
      (cond ((null? instr)
	     v)
	    ((eq? (caar instr) 'lambda)
	     (enqueue! q (car instr))
	     (loop (cdr instr)))
	    (else
	     (loop (cdr instr) (expand-instr (car instr) v))))))

  ; rudimentary...

  (define (objgen codevector)
    (reverse codevector))

  ; main

  (let ((q (new-queue)))
    (enqueue! q expr)
    (let loop ((v (empty-codevector)))
      (if (not (queue-empty? q))
	  (loop (expand-proc q (dequeue! q) v))
	  (objgen v))))


; 

(define (gen-const i)
  (let ((opd (instruction.arg1 i)))
    (cond ((integer? opd)
	   (if (fixnum-range? opd)
	       (m-fixnum->reg opd $reg-result)
	       (gen-bignum->reg opd $reg-result)))
	  ((rational? opd)
	   '(() ()))
	  ((rectangular? opd)
	   '(() ()))
	  ((real? opd)
	   '(()()))
	  ((complex? opd)
	   '())
	  ((string? opd)
	   '(() ()))
	  ((character? opd)
	   '(() ()))
	  ((pair? opd)
	   '(() ()))
	  ((boolean? opd)
	   '(() ()))
	  ((null? opd)
	   '(() ()))
	  ((vector? opd)
	   '(() ()))
	  (else
	   (error 'assemble "Unknown datatype as arg to `const'")))))

(define instruction-table
  (let ((v (make-vector instuction-count
			(lambda (x)
			  (display "Illegal instruction: ")
			  (display x)
			  (newline)))))
    (vector-set! v $const   gen-const)
    (vector-set! v $global  gen-global)
    (vector-set! v $setglbl gen-setglbl)
    (vector-set! v $lexical gen-lexical)
    (vector-set! v $setlex  gen-setlex)
    (vector-set! v $stack   gen-stack)
    (vector-set! v $setstk  gen-setstk)
    (vector-set! v $reg     gen-reg)
    (vector-set! v $setreg  gen-setreg)
    (vector-set! v $movereg gen-movereg)
    (vector-set! v $lambda  gen-lambda)
    (vector-set! v $chain   gen-chain)
    (vector-set! v $args=   gen-args=)
    (vector-set! v $args>=  gen-args>=)
    (vector-set! v $invoke  gen-invoke)
    (vector-set! v $save    gen-save)
    (vector-set! v $setrtn  gen-setrtn)
    (vector-set! v $restore gen-restore)
    (vector-set! v $pop     gen-pop)
    (vector-set! v $return  gen-return)
    (vector-set! v $mvrtn   gen-mvrtn)
    (vector-set! v $apply   gen-apply)
    (vector-set! v $branch  gen-branch)
    (vector-set! v $branchf gen-branchf)))

    
