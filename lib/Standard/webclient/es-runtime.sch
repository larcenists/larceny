(require 'word)

(define (es+ a b)
  (let ((a (es-toprimitive a))
	(b (es-toprimitive b)))
    (if (or (string? a) (string? b))
	(string-append (es-tostring a) (es-tostring b))
	(+ (es-tonumber a) (es-tonumber b)))))

(define (es- a b)
  (- (es-tonumber a) (es-tonumber b)))

(define (es* a b)
  (* (es-tonumber a) (es-tonumber b)))

(define (es/ a b)
  (lambda (a b)
    (/ (es-tonumber a) (es-tonumber b))))

(define (es% a b)
  ...)

(define (es-bitand a b)
  (word->integer (word-and (es-toint32 a) (es-toint32 b))))

(define (es-bitior a b)
  (word->integer (word-ior (es-toint32 a) (es-toint32 b))))

(define (es-bitxor a b)
  (word->integer (word-xor (es-toint32 a) (es-toint32 b))))

(define (es-bitshl a b)
  (word->integer 
   (word-shift (es-toint32 a) 
	       (remainder (word->integer (es-touint32 b)) 32))))

(define (es-bitshr a b)
  (word->integer 
   (word-shift (es-toint32 a) 
	       (- (remainder (word->integer (es-touint32 b)) 32)))))

(define (es-bitshru a b)
  (let ((shift (remainder (word->integer (es-touint32 b)) 32)))
    (word->integer 
     (word-and (word-shift (es-touint32 a) (- shift))
	       (word- (word-shift 1 (- 32 shift)) (integer->word 1))))))

(define (es< a b) ...)
(define (es<= a b) ...)
(define (es> a b) ...)
(define (es>= a b) ...)
(define (es== a b) ...)
(define (es=== a b) ...)
(define (es!= a b) ...)
(define (es!== a b) ...)

(define (es-instanceof x y) ...)
(define (es-in x y) ...)

(define (es-negate a) ...)
(define (es-identity a) ...)
(define (es-not a) ...)
(define (es-bitnot a) ...)

(define (es-typeof v)
  (cond ((boolean? v)       "boolean")
	((eq? 'undefined v) "undefined")
	((null? v)          "null")
	((string? v)        "string")
	((number? v)        "number")
	((null? (cddr v))   "object")
	(else               "function")))

(define (es-toprimitive x . rest)
  (let ((preferred (cond ((not (null? rest)) (car rest))
			 ((es-dateobj? x)    'string)
			 (else               'number))))
    (cond ((not (pair? x))
	   x)
	  ((eq? preferred 'string)
	   (let ()
	     (define (toString)
	       (let ((toString (es-get x name:toString)))
		 (if (pair? toString)
		     (let ((v (es-methodcall x toString '())))
		       (if (not (pair? v))
			   v
			   (valueOf)))
		     (valueOf))))
	     (define (valueOf)
	       (let ((valueOf (es-get x name:valueOf)))
		 (if (pair? valueOf)
		     (let ((v (es-methodcall x valueOf '())))
		       (if (not (pair? v))
			   v
			   (es-throw-typeerror "toPrimitive")))
		     (es-throw-typeerror "toPrimitive"))))
	     (toString)))
	  (else
	   (let ()
	     (define (toString)
	       (let ((toString (es-get x name:toString)))
		 (if (pair? toString)
		     (let ((v (es-methodcall x toString '())))
		       (if (not (pair? v))
			   v
			   (es-throw-typeerror "toPrimitive")))
		     (es-throw-typeerror "toPrimitive"))))
	     (define (valueOf)
	       (let ((valueOf (es-get x name:valueOf)))
		 (if (pair? valueOf)
		     (let ((v (es-methodcall x valueOf '())))
		       (if (not (pair? v))
			   v
			   (toString)))
		     (toString))))
	     (valueOf))))))

(define (es-toboolean x) 
  (cond ((boolean? x) x)
	((null? x) #f)
	((eq? x 'undefined) #f)
	((number? x)
	 (and (not (zero? x)) (not (= x x))))
	((string? x)
	 (not (zero? (string-length x))))
	(else
	 #t)))

(define (es-tonumber x)
  (cond ((eq? x 'undefined) +nan.0)
	((null? x) 0.0)
	((boolean? x) (if x 1.0 0.0))
	((number? x) x)
	((string? x) (es-string->number x))
	(else (es-tonumber (es-toprimitive x 'number)))))

(define (es-tointeger x) 
  (let ((x (es-tonumber x)))
    (cond ((not (= x x))
	   0.0)
	  ((or (zero? x) (= x +inf.0) (= x -inf.0))
	   x)
	  ((< x 0)
	   (- (floor (abs x))))
	  (else
	   (floor x)))))

(define (es-toint32 x)			; OPTIMIZEME
  (let ((x (es-tonumber x)))
    (integer->word 
     (if (or (not (= x x)) (zero? x) (= x +inf.0) (= x -inf.0))
	 0
	 (let ((y (modulo (inexact->exact (if (< x 0)
					      (- (floor (abs x)))
					      (floor x)))
			  (expt 2 32))))
	   (if (>= y (expt 2 31))
	       (- (expt 2 32) y)
	       y))))))

(define (es-touint32 x)			; OPTIMIZEME
  (let ((x (es-tonumber x)))
    (integer->word 
     (if (or (not (= x x)) (zero? x) (= x +inf.0) (= x -inf.0))
	 0
	 (modulo (inexact->exact (if (< x 0)
				     (- (floor (abs x)))
				     (floor x)))
		 (expt 2 32))))))

(define (es-tostring x) 
  (cond ((eq? x 'undefined) "undefined")
	((null? x)          "null")
	((eq? x #t)         "true")
	((eq? x #f)         "false")
	((number? x)        (es-number->string x))
	((string? x)        x)
	(else (es-tostring (es-toprimitive x 'string)))))

(define (es-toobject x) 
  (cond ((eq? x 'undefined) (es-throw-typeerror "ToObject" x))
	((null? x)          (es-throw-typeerror "ToObject" x))
	((boolean? x)       (es-make-object (es-boolean-proto) 'value x))
	((number? x)        (es-make-object (es-number-proto) 'value x))
	((string? x)        (es-make-object (es-string-proto) 'value x))
	(else x)))

(define (es-string->number x)
  (let loop ((i 0))
    (cond ((= i (string-length x)) 
	   +nan.0)
	  ((char-whitespace? (string-ref x i))
	   (loop + i 1))
	  (else 
	   (let ((n (let loop ((j (string-length x)))
		      (cond ((= j i) 
			     +nan.0)
			    ((char-whitespace? (string-ref x (- j 1)))
			     (loop (- j 1)))
			    (else
			     (let ((s (substring x i j)))
			       (cond ((and (> (string-length s) 2)
					   (char=? (string-ref s 0) 0)
					   (char-ci=? (string-ref s 1) #\x))
				      (string->number 
				       (substring s 2 (string-length s)) 
				       16))
				     ((string=? s "+Infinity") +inf.0)
				     ((string=? s "-Infinity") -inf.0)
				     ((string=? s "Infinity") +inf.0)
				     (else (number->string s)))))))))
	     (if n
		 n
		 +nan.0))))))

(define (es-number->string x)
  (cond ((not (= x x)) "NaN")
	((zero? x)     "0")
	((= x -inf.0)  "-Infinity")
	((= x +inf.0)  "Infinity")
	(else          (number->string x))))

(define (es-throw-typeerror msg) ...)
(define (es-throw obj) ...)

(define (es-true? x) (eq? #t (es-toboolean x)))
(define (es-false? x) (eq? #f (es-toboolean x)))
(define (es-nan? x) ...)
(define (es-finite? x) ...)

; Returns a global-object an an assoc list of prototype objects and
; constructors, for system use.

(define (es-make-global-environment)

  (define object-proto
    `(((|constructor| . ,es-builtin-object-constructor)
       (|toString| . ,es-object-toString)
       (|toLocaleString| . ,es-object-toString)
       (|valueOf| . ,es-object-valueOf)
       (|hasOwnProperty| . ,es-object-hasOwnProperty)
       (|isPrototypeOf| . ,es-object-isPrototypeOf)
       (|propertyIsEnumerable| . ,es-propertyIsEnumerable))
      #f))

  ...
  
  (define es-global-isNan
    `(((length 1 (DontEnum DontDelete ReadOnly)))
      ,function-proto
      (lambda args
	(let ((args (es-argument-parser 1)))
	  (es-nan? (es-tonumber (car args)))))))

  (define es-global-isFinite
    `(((length 1 (DontEnum DontDelete ReadOnly)))
      ,function-proto
      (lambda args
	(let ((args (es-argument-parser 1)))
	  (es-finite? (es-tonumber (car args)))))))

  (values
   `(((|NaN| +nan.0 (DontEnum DontDelete))
      (|Infinity| +inf.0 (DontEnum DontDelete))
      (|undefined| undefined (DontEnum DontDelete))
      (|eval| ,es-global-eval (DontEnum))
      (|parseInt| ,es-global-parseInt (DontEnum))
      (|parseFloat| ,es-global-parseFloat (DontEnum))
      (|isNan| ,es-global-isNan (DontEnum))
      (|isFinite| ,es-global-isFinite (DontEnum))
      (|decodeURI| ,es-global-decodeURI (DontEnum))
      (|decodeURIComponent| ,es-global-decodeURIComponent (DontEnum))
      (|encodeURI| ,es-global-encodeURI (DontEnum))
      (|encodeURIComponent| ,es-global-encodeURIComponent (DontEnum))
      (|Object| ,es-global-Object (DontEnum))
      (|Function| ,es-global-Function (DontEnum))
      (|Array| ,es-global-Array (DontEnum))
      (|String| ,es-global-String (DontEnum))
      (|Boolean| ,es-global-Boolean (DontEnum))
      (|Number| ,es-global-Number (DontEnum))
      (|Date| ,es-global-Date (DontEnum))
      (|RegExp| ,es-global-RegExp (DontEnum))
      (|Error| ,es-global-Error (DontEnum))
      (|EvalError| ,es-global-EvalError (DontEnum))
      (|RangeError| ,es-global-RangeError (DontEnum))
      (|ReferenceError| ,es-global-ReferenceError (DontEnum))
      (|SyntaxError| ,es-global-SyntaxError (DontEnum))
      (|TypeError| ,es-global-TypeError (DontEnum))
      (|URIError| ,es-global-URIError (DontEnum))
      (|Math| ,es-global-Math (DontEnum)))
     #f)
   `((object . ,object-proto)
     (boolean . ,boolean-proto)
     (number . ,number-proto)
     (string . ,string-proto)
     (function . ,function-proto)
     ...)))

