; HUFF and PUFF -- Huffman encoding and decoding.
;
; (huff-file filename) creates the compressed file filename.huff.
; (puff-file filename.huff) creates the uncompressed file filename.puff.

(define (huff-file filename)

  (define (display-stats input-bytes output-bytes output-bits)
    (display input-bytes)
    (display " input bytes; ")
    (display output-bytes)
    (display " output bytes (")
    (display (- 100 (round (* 100 (/ output-bytes input-bytes)))))
    (display "% compression); ")
    (display output-bits)
    (display " output bits.")
    (newline))
  
  (let* ((bytes           (read-file filename))
         (frequency-table (compute-frequencies bytes))
         (code            (compute-huffman-code frequency-table))
         (encoding        (huff bytes code))
         (bit-count       (car encoding))
         (bits            (list->vector (cdr encoding))))
    (display-stats (vector-length bytes) (vector-length bits) bit-count)
    (write-file 
      (string-append filename ".huff")
      (vector-append (build-header frequency-table)
                     (number->bytevector4 bit-count)
                     bits))))

(define (puff-file filename)
  (let* ((bytes (read-file filename))
         (bit-count (bytevector4->number (vector (vector-ref bytes 1024)
                                                 (vector-ref bytes 1025)
                                                 (vector-ref bytes 1026)
                                                 (vector-ref bytes 1027)))))
    (display (- (vector-length bytes) 1028))
    (display " bytes.")
    (newline)
    (display bit-count)
    (display " bits.")
    (newline)))

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (do ((c (read-char in) (read-char in))
           (l '() (cons (char->integer c) l)))
          ((eof-object? c)
           (list->vector (reverse l)))))
    'byte))

(define (write-file filename bytes)
  (call-with-output-file filename
    (lambda (out)
      (do ((i 0 (+ i 1)))
          ((= i (vector-length bytes)))
        (write-char (integer->char (vector-ref bytes i)) out)))
    'byte))

(define (huff bytes code-vector)
  
  (define bits 0)
  (define n 0)
  (define results '())
  
  (let loop ((i 0))
    (if (= i (vector-length bytes))
        (if (zero? (remainder n 8))
            (cons n (reverse results))
            (cons n (reverse (cons bits results))))
        (let loop2 ((new-bits (vector-ref code-vector (vector-ref bytes i))))
          (if (null? new-bits) 
              (loop (+ i 1))
              (begin (set! bits (+ (* bits 2) (car new-bits)))
                     (set! n (+ n 1))
                     (if (zero? (remainder n 8))
                         (begin (set! results (cons bits results))
                                (set! bits 0)))
                     (loop2 (cdr new-bits))))))))

(define (puff bit-count bytes code-vector)
  ...)


; Takes a vector of frequencies (probabilities) and returns a code vector:
; a vector of bit lists.

(define (compute-huffman-code frequencies)
  
  (define less? (lambda (a b) (< (cdr a) (cdr b))))
  
  (define (build-tree fvector)
    (let ((flist (sort (filter (map cons (iota 256) (vector->list fvector))
                               (lambda (x) (not (zero? (cdr x)))))
                       less?)))
      (let loop ((flist flist))
        (if (null? (cdr flist))
            (caar flist)
            (let ((x (car flist))
                  (y (cadr flist)))
              (loop (insert-ordered (cons (cons (car x) (car y)) 
                                          (+ (cdr x) (cdr y)))
                                    (cddr flist)
                                    less?)))))))
  
  (define (build-code tree)
    (define (search tree bits)
      (if (integer? tree)
          (list (cons tree (reverse bits)))
          (append (search (car tree) (cons 0 bits))
                  (search (cdr tree) (cons 1 bits)))))
    (let ((bvector (make-vector 256 #f)))
      (do ((l (search tree '()) (cdr l)))
          ((null? l) bvector)
        (vector-set! bvector (caar l) (cdar l)))))
  
  (build-code (build-tree frequencies)))

(define (compute-frequencies bytes)
  (let ((fvector (make-vector 256 0)))
    (do ((i (- (vector-length bytes) 1) (- i 1)))
        ((< i 0) fvector)
      (let ((x (vector-ref bytes i)))
        (vector-set! fvector x (+ 1 (vector-ref fvector x)))))))

; Header construction.
;
; There are many ways to compress the header.  The header construction 
; applies several methods and chooses the one that produces the smallest
; header.  The methods are:
;  - store the 256 counters uncompressed
;  - store the counters for typical ASCII text values but no others,
;    if only those values appear in the frequency table
;  - store a 256-bit mask and then follow it by counters for the `1' bits
;  - store a bit mask for typical ASCII text values, and then the counters
;    for the `1' bits
;  - store a packed representation: a counter of entries, and then a list
;    of entries, each a pair: the byte value and its counter.
;
; For each of the five methods, there are three encoding methods for the
; counter:
;  - 8-bit counts with 16- and 32-bit values escaped by #xFE and #xFF
;  - 16-bit counts with 32-bit values escaped by #xFF
;  - 32-bit counts.

; Header type tags.

(define frequency-table:uncompressed 0)  ; 256 entries
(define frequency-table:ascii-only 1)    ; ASCII entries
(define frequency-table:full-mask 2)     ; 256-bit bitmap 
(define frequency-table:ascii-map 3)     ; ASCII-valued bitmap
(define frequency-table:packed 4)        ; name, value listed for each
(define frequency-table:8bit 16)         ; entries are 8-bit with escapes
(define frequency-table:16bit 32)        ; entries are 16-bit with escapes
(define frequency-table:32bit 33)        ; entries are 32-bit

(define (build-header frequency-table)
  
  (define (encode8 x)
    (cond ((< x #xFE) 
           (list x))
          ((< x 65536) 
           (list #xFE (quotient x 256) (remainder x 256)))
          (else
           (list #xFF (encode32 x)))))
  
  (define (encode16 x)
    (cond ((< x #xFF00) 
           (list (quotient x 256) (remainder x 256)))
          (else
           (cons #xFF (encode32 x)))))
  
  (define (encode32 x)
    (list (quotient x 16777216)
          (remainder (quotient x 65536) 256)
          (remainder (quotient x 256) 256)
          (remainder x 256)))

  (define (in-ascii-range? x)
    (or (<= 32 x 126) (= x 9) (= x 10) (= x 12) (= x 13)))
  
  (define (select-ascii table)
    (append (list (vector-ref table 9)
                  (vector-ref table 10)
                  (vector-ref table 12)
                  (vector-ref table 13))
            (do ((i 32 (+ i 1))
                 (l '() (cons (vector-ref table i) l)))
                ((= i 127) (reverse l)))))

  (define (create-mask bitmap table encoding)
    (do ((i 0 (+ i 1)))
        ((= i (vector-length table))
         (vector-append bitmap 
                        (pack-nonzero-entries table encoding #f)))
      (let ((x (vector-ref table i)))
        (if (not (zero? x))
            (begin ... set the bit ...)))))
  
  ; 256 entries
  
  (define (header-uncompressed encoding)
    (list->vector (apply append (map encoding (vector->list frequency-table)))))
  
  ; 99 entries: SPACE through ~, plus ^I ^J ^L ^M

  (define (header-ascii encoding)
    (list->vector (apply append (map encoding (select-ascii frequency-table)))))

  ; 256-bit bitmap (32 bytes) followed by entries

  (define (header-fullmask encoding)
    (create-mask (make-vector 32 0) frequency-table encoding))
  
  ; 99-bit bitmap (13 bytes) for the ASCII set.

  (define (header-asciimask encoding)
    (create-mask (make-vector 13 0) 
                 (list->vector (select-ascii frequency-table)) 
                 encoding))
  
  ; 16-bit count of entries followed by entries
  
  (define (header-packed encoding)
    (let* ((entries 0)
           (bytes (apply append (map (lambda (i x)
                                       (if (not (zero? x))
                                           (begin (set! entries (+ entries 1))
                                                  (cons i (encoding x)))))
                                     (iota 256)
                                     (vector->list frequency-table)))))
      (list->vector (cons (encode16 entries) bytes))))
  
  '(let ((outside-ascii 0))
    (do ((i 0 (+ i 1)))
        ((= i 256))
      (let ((x (vector-ref frequency-table i)))
        (if (and (not (in-ascii-range? i)) (not (zero? x)))
            (set! outside-ascii (+ outside-ascii 1)))))
    (if (> outside-ascii 0)
        (least (cross-map 
                 (lambda (compressor encoding)
                   (cons (+ (car compressor) (car encoding))
                         ((cdr compressor) (cdr encoding))))
                 `((,frequency-table:uncompressed . ,header-uncompressed)
                   (,frequency-table:packed . ,header-packed)
                   (,frequency-table:fullmask . ,header-fullmask))
                 `((8 . ,encode8) (16 . ,encode16) (32 . ,encode32)))
               (lambda (x y) 
                 (< (vector-length (cdr x)) (vector-length (cdr y)))))
        (least (cross-map 
                 (lambda (compressor encoding)
                   (cons (+ (car compressor) (car encoding))
                         ((cdr compressor) (cdr encoding))))
                 `((,frequency-table:ascii . ,header-ascii)
                   (,frequency-table:packed . ,header-packed)
                   (,frequency-table:ascii-mask . ,header-asciimask))
                 `((8 . ,encode8) (16 . ,encode16) (32 . ,encode32)))
               (lambda (x y) 
                 (< (vector-length (cdr x)) (vector-length (cdr y)))))))
  '#(0))

(define (number->bytevector4 n)
  (vector (quotient n 16777216)
          (remainder (quotient n 65536) 256)
          (remainder (quotient n 256) 256)
          (remainder n 256)))
  
(define (bytevector4->number v)
  (+ (* 256 (+ (* 256 (+ (* 256 (vector-ref v 0))
                         (vector-ref v 1)))
               (vector-ref v 2)))
     (vector-ref v 3)))
  

;; The following belong in a standard library.

(define (iota limit)
  (define (iota0 n)
    (if (= n limit)
      '()
      (cons n (iota0 (+ n 1)))))
  (iota0 0))

(define (insert-ordered x l less?)
  (cond ((null? l) 
         (list x))
        ((less? (car l) x)
         (cons (car l) (insert-ordered x (cdr l) less?)))
        (else
         (cons x l))))

(define (filter l keep?)
  (cond ((null? l) l)
        ((keep? (car l))
         (cons (car l) (filter (cdr l) keep?)))
        (else
         (filter (cdr l) keep?))))

(define (sort l less?)
  (let loop ((l l) (r '()))
    (if (null? l)
        r
        (loop (cdr l) (insert-ordered (car l) r less?)))))

(define (vector-append . rest)
  (list->vector (apply append (map vector->list rest))))

(define (cross-map proc l1 l2)
  (let loop ((l1 l1) (r '()))
    (cond ((null? l1)
           (apply append (reverse r)))
          (else
           (let loop2 ((l2 l2) (s '()))
             (cond ((null? l2)
                    (loop (cdr l1) (cons (reverse s) r)))
                   (else
                    (loop2 (cdr l2) (cons (proc (car l1) (car l2)) s)))))))))

; This is smarter :-)

'(define (cross-map2 proc l1 l2)
  (mappend (lambda (x)
	     (map (lambda (y) (proc x y)) l2))
	   l1))

;; Test code

(define (print-huffman-code code-vector)
  (do ((i 0 (+ i 1)))
      ((= i (vector-length code-vector)))
    (if (vector-ref code-vector i)
      (let ((x (vector-ref code-vector i)))
        (if (<= 33 x 126)
            (display (integer->char i))
            (display i))
        (display " ")
        (display x)
        (newline)))))
 
; Takes a string and a frequency table, prints the encoding.

(define (huff-test msg frequencies)
  (let* ((bytes (list->vector (map char->integer (string->list msg))))
         (code (compute-huffman-code frequencies))
         (encoded (huff bytes code)))
    (display (car encoded))
    (display " bits.")
    (newline)
    (do ((n (car encoded) (- n 8))
         (l (cdr encoded) (cdr l)))
        ((<= n 0))
      (let ((x (car l)))
        (let ((s (number->string x 2)))
          (display (make-string (- (min n 8) (string-length s)) #\0))
          (display s)
          (display " "))))))
