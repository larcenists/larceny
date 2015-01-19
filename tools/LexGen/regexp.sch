; Lexical analyzer generator.
; Copyright 1995 William Clinger.
;
; Cambridge Polish notation for regular expressions.
;
; R  -->  <simple datum> ; as in R4RS
;      |  (R ...)        ; catenation
;      |  (! R ...)      ; alternation
;      |  (* R)          ; Kleene star
;      |  (+ R)          ; same as (R (* R))
;
; To get the effect of !, *, or + as a <simple datum>,
; use (() !), (() *), or (() +).

; MacScheme-specific code.

(define (mysort x y)
  (if (procedure? x)
      (sort y x)
      (sort x y)))

;
; Regular expressions.
; NFA.
; DFA.
; Minimal DFA.

; Given a labelled list of regular expressions, returns a
; representation of a nondeterministic finite automaton (NFA)
; that recognizes the set denoted by the regular expressions.
;
; The input is represented as a list of entries of the form
; (<label> R), where <label> is any value except #f.
; The output is represented by a list of the form
;
;    ((n <accepts> <transition> ...) ...)
; where
;    n is the number of the state;
;    <accepts> is the <label> for the R accepted by state n,
;      or #f if state n is not an accepting state;
;    each <transition> is of the form (<input> n'),
;      where <input> is a <simple datum> or ().

(define (make-nfa-entry state accepts transitions)
  `(,state ,accepts ,@transitions))

(define (make-transition token target) (list token target))

(define (nfa-entry-state x) (car x))
(define (nfa-entry-accepts x) (cadr x))
(define (nfa-entry-transitions x) (cddr x))
(define (transition-token x) (car x))
(define (transition-target x) (cadr x))
;
; The first element of the output represents the start state.
; The second element will always be an accepting state.

(define (regular->nfa labelled-regular-expressions)
  (state-counter 'init)
  (let* ((start (state-counter))
         (nfas (map reg->nfa
                    (map car labelled-regular-expressions)
                    (map cadr labelled-regular-expressions))))
    (cons (make-nfa-entry start
                          #f
                          (map (lambda (state)
                                 (make-transition '() state))
                               (map (lambda (nfa)
                                      (nfa-entry-state (car nfa)))
                                    nfas)))
          (apply append nfas))))

(define state-counter
  (let ((n 0))
    (lambda args
      (if (null? args)
          (let ((state n))
            (set! n (+ n 1))
            state)
          (set! n 0)))))

(define (reg->nfa label r)
  (let* ((n0 (state-counter))
         (n1 (state-counter)))
    (cond ((null? r)
           (list (make-nfa-entry n0 #f (list (make-transition '() n1)))
                 (make-nfa-entry n1 label '())))
          ((not (pair? r))
           (list (make-nfa-entry n0 #f (list (make-transition r n1)))
                 (make-nfa-entry n1 label '())))
          ((eq? (car r) '!)
           (let* ((nfas (map (lambda (r)
                               (reg->nfa #f r))
                             (cdr r)))
                  (nfas (map (lambda (nfa)
                               `(,(car nfa)
                                 ,(let ((entry (cadr nfa)))
                                       (make-nfa-entry
                                        (nfa-entry-state entry)
                                        (nfa-entry-accepts entry)
                                        (cons (make-transition '() n1)
                                              (nfa-entry-transitions entry))))
                                 ,@(cddr nfa)))
                             nfas)))
             `(,(make-nfa-entry
                 n0 #f (map (lambda (nfa)
                              (make-transition '() (nfa-entry-state (car nfa))))
                            nfas))
               ,(make-nfa-entry n1 label '())
               ,@(apply append nfas))))
          ((eq? (car r) '*)
           (let ((nfa (reg->nfa label (cons '+ (cdr r)))))
             `(,(let ((entry (car nfa)))
                     (make-nfa-entry
                      (nfa-entry-state entry)
                      #f
                      (cons (make-transition '() (nfa-entry-state (cadr nfa)))
                            (nfa-entry-transitions entry))))
               ,@(cdr nfa))))
          ((eq? (car r) '+)
           (let* ((nfa (reg->nfa label (cadr r)))
                  (nfa `(,(car nfa)
                         ,(let ((start (nfa-entry-state (car nfa)))
                                (entry (cadr nfa)))
                               (make-nfa-entry
                                (nfa-entry-state entry)
                                (nfa-entry-accepts entry)
                                (cons (make-transition '() start)
                                      (nfa-entry-transitions entry))))
                         ,@(cddr nfa))))
             nfa))
          (else
           (do ((exprs (cdr r) (cdr exprs))
                (nfa (reg->nfa label (car r))
                     (let ((nfa2 (reg->nfa label (car exprs))))
                       `(,(car nfa)
                         ,(cadr nfa2)
                         ,(make-nfa-entry
                           (nfa-entry-state (cadr nfa))
                           #f
                           (cons (make-transition '() (nfa-entry-state (car nfa2)))
                                 (nfa-entry-transitions (cadr nfa))))
                         ,@(cddr nfa)
                         ,(car nfa2)
                         ,@(cddr nfa2)))))
               ((null? exprs) nfa))))))

; Given an nfa represented as above, returns an equivalent dfa.

(define (nfa->dfa nfa)

  ; For large automata such as the one in Larceny's get-datum
  ; procedure, this procedure was spending more than 80% of its
  ; time in the assv procedure.  The following code redefines
  ; assv locally to exploit the fact that the states of the nfa
  ; are usually small exact integers.

  (let* ((states (map nfa-entry-state nfa))
         (bummed? (and (every? number? states)
                       (every? exact? states)
                       (every? integer? states)))
         (minstate (apply min states))
         (maxstate (apply max states))
         (bummed? (<= 0 minstate maxstate 100000))
         (statevec (if bummed?
                       (make-vector (+ 1 maxstate) #f)
                       '#()))
         (ignored (if bummed?
                      (for-each (lambda (entry)
                                  (vector-set! statevec
                                               (nfa-entry-state entry)
                                               entry))
                                nfa)
                      #f))
         (assv (if bummed?
                   (lambda (state nfa) (vector-ref statevec state))
                   assv)))

    (define (closure states)
      (define (loop done todo)
        (if (null? todo)
            (mysort <= done)
            (loop2 (cons (car todo) done)
                   (cdr todo)
                   (nfa-entry-transitions (assv (car todo) nfa)))))

      (define (loop2 done todo transitions)
        (cond ((null? transitions)
               (loop done todo))
              ((null? (transition-token (car transitions)))
               (let ((target (transition-target (car transitions))))
                 (loop2 done
                        (if (or (memv target done) (memv target todo))
                            todo
                            (cons target todo))
                        (cdr transitions))))
              (else
               (loop2 done todo (cdr transitions)))))
      (loop '() states))

    (define (dfa-transitions transitions)
      (if (null? transitions)
          '()
          (let* ((token (transition-token (car transitions)))
                 (transitions0 (filter (lambda (t)
                                         (eqv? token (transition-token t)))
                                       transitions))
                 (transitions1 (filter (lambda (t)
                                         (not (eqv? token
                                                    (transition-token t))))
                                       transitions)))
            (cons (make-transition token
                                   (closure
                                    (make-set
                                     (map transition-target transitions0))))
                  (dfa-transitions transitions1)))))

    (define (loop dfa todo)
      (if (null? todo)
          (reverse dfa)
          (let* ((state (car todo))
                 (entries (map (lambda (n) (assv n nfa)) state))
                 (entry (make-nfa-entry
                         state
                         (filter (lambda (x) x)
                                 (map nfa-entry-accepts entries))
                         (dfa-transitions
                          (filter (lambda (t)
                                    (not (null? (transition-token t))))
                                  (apply append
                                         (map nfa-entry-transitions
                                              entries)))))))
            (loop (cons entry dfa)
                  (append (filter (lambda (state)
                                    (and (not (assoc state dfa))
                                         (not (member state todo))))
                                  (map transition-target
                                       (nfa-entry-transitions entry)))
                          (cdr todo))))))
    (loop '()
          (list (closure (list (nfa-entry-state (car nfa))))))))

; Given a dfa represented as above, return the minimal equivalent dfa.

(define (dfa->minimal dfa)
  
  (define (initial-partition dfa alist)
    (if (null? dfa)
        (map cdr alist)
        (let ((probe (select (lambda (alist-entry)
                               (set-equal? (car alist-entry)
                                           (nfa-entry-accepts (car dfa))))
                             alist)))
          (if probe
              (begin (set-cdr! probe
                               (cons (car dfa) (cdr probe)))
                     (initial-partition (cdr dfa) alist))
              (initial-partition (cdr dfa)
                                 (cons (list (nfa-entry-accepts (car dfa))
                                             (car dfa))
                                       alist))))))
  
  (define (refine-partition dfa partition)
    (define (loop parts new-parts)
      (if (null? parts)
          new-parts
          (loop (cdr parts)
                (append (refine-part (car parts) '())
                        new-parts))))
    (define (refine-part part new-parts)
      (if (null? part)
          new-parts
          (let ((p (select (lambda (new-part)
                             (equivalent-so-far? (car part) (car new-part)))
                           new-parts)))
            (refine-part
             (cdr part)
             (if p
                 (begin (set-cdr! p (cons (car part) (cdr p)))
                        new-parts)
                 (cons (list (car part)) new-parts))))))
    (define (equivalent-so-far? entry1 entry2)
      (and (set-equal? (nfa-entry-accepts entry1)
                       (nfa-entry-accepts entry2))
           (let ((transitions1 (nfa-entry-transitions entry1))
                 (transitions2 (nfa-entry-transitions entry2)))
             (and (every? (lambda (t1)
                            (let ((t2 (assoc (transition-token t1)
                                             transitions2)))
                              (and t2
                                   (in-same-partition? (transition-target t1)
                                                       (transition-target t2)))))
                          transitions1)
                  (every? (lambda (t2)
                            (let ((t1 (assoc (transition-token t2)
                                             transitions1)))
                              (and t1
                                   (in-same-partition? (transition-target t1)
                                                       (transition-target t2)))))
                          transitions2)))))
    (define (in-same-partition? s1 s2)
      (let ((part (select (lambda (part) (assv s1 part))
                          partition)))
        (assv s2 part)))
    (let ((partition2 (loop partition '())))
      (if (= (length partition) (length partition2))
          partition
          (refine-partition dfa partition2))))
  
  (define (partition->dfa partition states)
    (map (lambda (part state)
           (make-nfa-entry state
                           (nfa-entry-accepts (car part))
                           (map (lambda (t)
                                  (make-transition
                                   (transition-token t)
                                   (map nfa-entry-state
                                        (select (lambda (part)
                                                  (assv (transition-target t)
                                                        part))
                                                partition))))
                                (nfa-entry-transitions (car part)))))
         partition
         states))                                                       
  
  (state-counter 'init)
  (let* ((dfa (relabel-states dfa))
         (partition (mysort (lambda (part1 part2)
                              (assv 0 part1))
                            (refine-partition
                             dfa
                             (initial-partition dfa '()))))
         (dfa (partition->dfa partition
                              (map (lambda (part)
                                     (map nfa-entry-state part))
                                   partition))))
    (state-counter 'init)
    (mysort (lambda (entry1 entry2)
              (<= (nfa-entry-state entry1)
                  (nfa-entry-state entry2)))
            (relabel-states dfa))))

(define (regular->minimal labelled-regular-expressions)
  (dfa->minimal
   (nfa->dfa
    (regular->nfa labelled-regular-expressions))))

; FIXME: for timing purposes; commented out.

'
(define (regular->minimal labelled-regular-expressions)
  (let* ((nfa (time (regular->nfa labelled-regular-expressions)))
         (ignored (begin (write (length nfa)) (newline)))
         (dfa (time (nfa->dfa nfa)))
         (ignored (begin (write (length dfa)) (newline)))
         (dfa (time (dfa->minimal dfa)))
         (ignored (begin (write (length dfa)) (newline))))
    dfa))

    


  
; Generate a more readable set of labels for the states of a dfa.
; Bug: doesn't eliminate states that can't accept anything.
  
(define (relabel-states dfa)
  
  ; Given a set of entries that have been relabelled,
  ; a set of entries to do, and an alist that renames
  ; states, returns a relabelled dfa.
  
  (define (loop done todo alist)
    (if (null? todo)
        (reverse done)
        (let ((entry (car todo)))
          (if (assoc (nfa-entry-state entry) alist)
              (relabel-transitions
               (nfa-entry-transitions entry)
               alist
               (lambda (alist transitions)
                 (loop (cons (make-nfa-entry
                              (cdr (assoc (nfa-entry-state entry) alist))
                              (nfa-entry-accepts entry)
                              transitions)
                             done)
                       (append (map (lambda (state)
                                      (assoc state dfa))
                                    (filter (lambda (t)
                                              (not (assoc t alist)))
                                            (map transition-target
                                                 (nfa-entry-transitions entry))))
                               (cdr todo))
                       alist)))
              (loop done
                    todo
                    (cons (cons (nfa-entry-state entry) (state-counter))
                          alist))))))
  
  ; CPS because Scheme ain't got no way to return multiple values.
  ; Given a list of transitions and an alist that renames states,
  ; calls the continuation k on a possibly larger alist and the
  ; relabelled list of transitions.
  
  (define (relabel-transitions transitions alist k)
    (if (null? transitions)
        (k alist '())
        (let* ((t (car transitions))
               (probe (assoc (transition-target t) alist)))
          (if probe
              (relabel-transitions
               (cdr transitions)
               alist
               (lambda (alist transitions)
                 (k alist
                    (cons (make-transition (transition-token t)
                                           (cdr probe))
                          transitions))))
              (relabel-transitions
               transitions
               (cons (cons (transition-target t) (state-counter))
                     alist)
               k)))))
  
  (loop '()
        dfa
        (list (cons (nfa-entry-state (car dfa))
                    (state-counter)))))

; Extracts the accessible states,
; sorts them so the start state is first and blocking states are last,
; and then relabels.

(define (relabel-states dfa)
  
  (define (lookup-entry-for-state state nfa)
    (select (lambda (entry)
              (equal? state (nfa-entry-state entry)))
            nfa))
  
  (define accessible
    (accessibility-parameters
     equal?
     (lambda (state nfa)
       (map transition-target
            (nfa-entry-transitions
             (lookup-entry-for-state state nfa))))))
  
  (define (iota n)
    (do ((n (- n 1) (- n 1))
         (x '() (cons n x)))
        ((negative? n) x)))
  
  (let* ((states (accessible (list (nfa-entry-state (car dfa)))
                             dfa))
         (entries (map (lambda (state)
                         (lookup-entry-for-state state dfa))
                       states))
         (dfa (mysort (lambda (entry1 entry2)
                        (or (eq? entry1 (car dfa))
                            (null? (nfa-entry-transitions entry2))))
                      entries))
         (alist (map cons
                     (map nfa-entry-state dfa)
                     (iota (length dfa))))
         (dfa (map (lambda (entry)
                     (make-nfa-entry
                      (cdr (assoc (nfa-entry-state entry) alist))
                      (nfa-entry-accepts entry)
                      (map (lambda (t)
                             (make-transition
                              (transition-token t)
                              (cdr (assoc (transition-target t) alist))))
                           (nfa-entry-transitions entry))))
                   dfa)))
    dfa))
