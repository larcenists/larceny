; Copyright (C) 1999 Lars T Hansen
;
; $Id$
;
; Andrew Wright's MATCH syntax as almost-portable R5RS macro.
; Last modified 990910 / lth

; Doesn't optimize the search yet.
; Doesn't do `qp or #&pat or $ or set! or get! or (=> ident) yet.
; Doesn't do ___, __k yet.
; Doesn't do MATCH-LET, MATCH-LET*, MATCH-LETREC, named MATCH-LET.

(define-syntax match
  (syntax-rules (_ quote and or not ?)

    ;; Note structure on 2nd arg to distinguish from following rules.

    ((match exp (p0 e0) ...)
     (let ((v exp))
       (match "main" v (p0 e0) ...)))

    ;; "main" -- basic setup

    ((match "main" v)
     (error "No patterns matched " v))

    ((match "main" v clause0 clause1 ...)
     (letrec ((otherwise 
               (lambda () (match "main" v clause1 ...))))
       (match "clause" v clause0 (otherwise))))

    ;; "clause" -- process the clauses

    ((match "clause" v (_ body) k)
     body)

    ((match "clause" v ((and) body) k)
     body)

    ((match "clause" v ((and p0 p1 ...) body) k)
     (match "clause" v (p0 (match "clause" v ((and p1 ...) body) k)) k))

    ((match "clause" v ((or) body) k)
     k)

    ((match "clause" v ((or p0 p1 ...) body) k)
     (letrec ((orbody (lambda () body))
              (otherwise
               (lambda () (match "clause" v ((or p1 ...) (orbody)) k))))
       (match "clause" v (p0 (orbody)) (otherwise))))

    ((match "clause" v ((not p) body) k)
     (match "clause" v (p k) body))

    ((match "clause" v ((not p0 ...) body) k)
     (match "clause" v ((and (not p0) ...) body) k))

    ((match "clause" v ((? pred p0 ...) body) k)
     (if (pred v)
         (match "clause" v ((and p0 ...) body) k)
         k))

    ((match "clause" v ((quote x) body) k)
     (if (equal? v (quote x))
         body
         k))

    ((match "clause" v (#t body) k)
     (if (eq? v #t)
         body
         k))

    ((match "clause" v (#f body) k)
     (if (eq? v #f)
         body
         k))

    ((match "clause" v ((p0 ...) body) k)
     (match "list" v (p0 ...) body k))
    
    ((match "clause" v ((p0 . p1) body) k) ; _any_ improper list
     (match "improper" v (p0 . p1) body k))

    ((match "clause" v (#(p0 ...) body) k)
     (if (not (vector? v))
         k
         (let ((l (- (vector-length v) 1))
               (j 0))
           (match "vector" v l j (p0 ...) body k))))

    ;; We've four cases left: string, number, character, and identifier. 
    ;; The R5RS macro system doesn't let us match on these classes of 
    ;; objects so we have to escape.

    ((match "clause" v (pat body) k)
     (match-rewrite-atomic-pattern v pat body k))


    ;; Lists

    ((match "list" v () body k)
     (if (null? v)
         body
         k))

    ((match "list" v (p0 p1 ...) body k)
     (if (not (pair? v))
         k
         (let ((x (car v))
               (y (cdr v)))
           (match "clause" x (p0 (match "list" y (p1 ...) body k)) k))))

    ;; Improper lists

    ((match "improper" v (p0 . p1) body k)
     (if (pair? v)
         (let ((a (car v))
               (b (cdr v)))
           (match "clause" a (p0 (match "improper" b p1 body k)) k))
         k))

    ((match "improper" v p0 body k)
     (match "clause" v (p0 body) k))

    ;; Vectors

    ((match "vector" v i j () body k)
     (if (>= i 0)
         k
         body))

    ((match "vector" v i j (p0 p1 ...) body k)
     (if (>= i 0)
         (let ((x (vector-ref v j))
               (i (- i 1))
               (j (+ j 1)))
           (match "clause" x (p0 (match "vector" v i j (p1 ...) body k)) k))
         k))

    ))

(define-syntax match-lambda
  (syntax-rules ()
    ((match-lambda clause ...)
     (lambda (x) (match x clause ...)))))

(define-syntax match-lambda*
  (syntax-rules ()
    ((match-lambda* clause ...)
     (lambda x (match x clause ...)))))


; Dept. of non-portable hacks.

; Use the low-level macro system to distinguish four cases:
;   string constant
;   character constant
;   number constant
;   identifier
; and expand into appropriate code.

(define-syntax match-rewrite-atomic-pattern
  (transformer
   (lambda (exp rename compare)
     (let ((v    (cadr exp))
           (pat  (caddr exp))
           (body (cadddr exp))
           (k    (car (cddddr exp))))
       (if (or (string? pat) (number? pat) (char? pat))
           `(,(rename 'match) "clause" ,v ((,(rename 'quote) ,pat) ,body) ,k)
           `(,(rename 'let) ((,pat ,v)) ,body))))))

; Testing

(let ()

  (define (map p l)
    (match l 
           (() '())
           ((a . b) (cons (p a) (map p b)))))

  (or (equal? (match 37 (_ #f)) #f)
      (error "#2"))
  (or (equal? (match 37 (a a)) 37)
      (error "#3"))
  (or (equal? (match 37 (37 #t) (_ #f)) #t)
      (error "#4"))
  (or (equal? (match 37 ((or) #t) (_ #f)) #f)
      (error "#5"))
  (or (equal? (match 37 ((or 37 44) #t)) #t)
      (error "#6"))
  (or (equal? (match 37 ((or 44 48) #t) ((or 37) #f)) #f)
      (error "#7"))
  (or (equal? (match 37 ((and) #t) (_ #f)) #t)
      (error "#8"))
  (or (equal? (match 37 ((and 37) #t) (_ #f)) #t)
      (error "#9"))
  (or (equal? (match 37 ((and 44) #t) (_ #f)) #f)
      (error "#10"))
  (or (equal? (match 37 ((and 37 37 37) #t) (_ #f)) #t)
      (error "#11"))
  (or (equal? (match 37 ((and 44 44 48) #t) (_ #f)) #f)
      (error "#12"))
  (or (equal? (match 37 ((not 44) #t) (_ #f)) #t)
      (error "#13"))
  (or (equal? (match 37 ((not 37) #t) (_ #f)) #f)
      (error "#14"))
  (or (equal? (match 37 ((not 44 48 52) #t) (_ #f)) #t)
      (error "#15"))
  (or (equal? (match 37 ((not 37 37 37) #t) (_ #f)) #f)
      (error "#16"))
  (or (equal? (match 37 ((? (lambda (x) (= x 37))) #t) (_ #f)) #t)
      (error "#17"))
  (or (equal? (match 37 ((? (lambda (x) #t) 37 37) #t) (_ #f)) #t)
      (error "#18"))
  (or (equal? (match 37 ('supercallifragilistic #t) (a a)) 37)
      (error "#19"))
  (or (equal? (match 37 (#t #t) (_ #f)) #f)
      (error "#20"))
  (or (equal? (match #t (#t #f) (_ #t)) #f)
      (error "#21"))
  (or (equal? (match 37 (#f #t) (_ #f)) #f)
      (error "#22"))
  (or (equal? (match #f (#f #t) (_ #f)) #t)
      (error "#23"))
  (or (equal? (match '(1 2 3) ((a b c) (list a b c)) (a (cdr a))) '(1 2 3))
      (error "#24"))
  (or (equal? (match '(1 2 3) ((a b) (list a b)) ((a b c d) (list a b c d)) (a a)) '(1 2 3))
      (error "#25"))
  (or (equal? (match '(1 2 3 . 4) ((a b c . d) (list c d))) '(3 4))
      (error "#26"))
  (or (equal? (match '(1 2 3 . 4) ((a b . c) (list a b c))) '(1 2 (3 . 4)))
      (error "#27"))
  (or (equal? (match 37 ("supercallifragilistic" #t) (a a)) 37)
      (error "#28"))
  (or (equal? (match "supercallifragilistic" ("foo" 1) (a a)) "supercallifragilistic")
      (error "#29"))
  (or (equal? (match #\a (#\a #t)) #t)
      (error "#30"))
  (or (equal? (match #\a (#\b #t) (#\a #f)) #f)
      (error "#31"))
  (or (equal? (match '#(1 2 3) (#(1 2 3) #t)) #t)
      (error "#32"))
  (or (equal? (match '#(1 2 3) (#(a b c) (list a b c))) '(1 2 3))
      (error "#33"))
  (or (equal? (match 37 (#(a b c) (list a b c)) (_ #f)) #f)
      (error "#34"))
  (or (equal? (match '#(1 2) (#(a b c) 1) (#(a) 2) (#() 3) (#(a b) (list a b))) '(1 2))
      (error "#35"))

  ; Funky stuff
  (or (equal? (match '(1 2 3) ((and a (x y z)) (list a x y z))) '((1 2 3) 1 2 3))
      (error "#100"))
  (or (equal? (map (lambda (x) (* x x)) '(1 2 3)) '(1 4 9))
      (error "#101"))
  #t)

; eof
