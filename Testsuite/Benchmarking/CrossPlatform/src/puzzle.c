#include <stdio.h>

/*---------------------------------------------------------------------------*/

/* Support for Scheme like stuff */

#define CUSTOM_ALLOC 1

int *alloc;

#define CAR(pair) *(pair)
#define CDR(pair) *((pair)+1)

#if CUSTOM_ALLOC

int *old_alloc;

#define CONS(car,cdr) ( *(--alloc) = cdr, *(--alloc) = car, alloc )

#define HEAP_SIZE 1000000

void init()
{
  /* char *heap = (char *) malloc( HEAP_SIZE ); */
  int heap_size_in_bytes = HEAP_SIZE * sizeof(int);
  int * heap = (int *) malloc( heap_size_in_bytes );

if (heap == NULL)
  {
    printf( "Not enough memory (%d bytes needed)\n", heap_size_in_bytes );
    exit(1);
  }

alloc = heap + HEAP_SIZE; /* alloc from end */
}

#else

#define CONS(car,cdr) ( alloc = (int *) malloc(2*sizeof(int)), \
                        *(alloc) = (car), \
                        *(alloc+1) = (cdr), \
                        alloc)

void init()
{
return;
}

#endif

/* Other globals */

int result;

/*=============================================================================

                                 THE BENCHMARKS

=============================================================================*/

/*-----------------------------------------------------------------------------

PUZZLE

(define (iota n)
  (do ((n n (- n 1))
       (list '() (cons (- n 1) list)))
      ((zero? n) list)))

;;; PUZZLE -- Forest Baskett's Puzzle benchmark, originally written in Pascal.

(define size 511)
(define classmax 3)
(define typemax 12)

(define *iii* 0)
(define *kount* 0)
(define *d* 8)

(define *piececount* (make-vector (+ classmax 1) 0))
(define *class* (make-vector (+ typemax 1) 0))
(define *piecemax* (make-vector (+ typemax 1) 0))
(define *puzzle* (make-vector (+ size 1)))
(define *p* (make-vector (+ typemax 1)))

(define (fit i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((or (> k end)
             (and (vector-ref (vector-ref *p* i) k)
                  (vector-ref *puzzle* (+ j k))))
         (if (> k end) '#t '#f)))))

(define (place i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((> k end))
        (cond ((vector-ref (vector-ref *p* i) k)
               (vector-set! *puzzle* (+ j k) '#t)
               '#t)))
    (vector-set! *piececount*
                 (vector-ref *class* i)
                 (- (vector-ref *piececount* (vector-ref *class* i)) 1))
    (do ((k j (+ k 1)))
        ((or (> k size) (not (vector-ref *puzzle* k)))
         ;        (newline)
         ;        (display "*Puzzle* filled")
         (if (> k size) 0 k)))))

(define (puzzle-remove i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((> k end))
        (cond ((vector-ref (vector-ref *p* i) k)
               (vector-set! *puzzle* (+ j k) '#f)
               '#f)))
    (vector-set! *piececount*
                 (vector-ref *class* i)
                 (+ (vector-ref *piececount* (vector-ref *class* i)) 1))))

(define (trial j)
  (let ((k 0))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 0 (+ i 1)))
           ((> i typemax) (set! *kount* (+ *kount* 1)) '())
           (cond
            ((not
              (zero?
               (vector-ref *piececount* (vector-ref *class* i))))
             (cond
              ((fit i j)
               (set! k (place i j))
               (cond
                ((or (trial k) (zero? k))
                 ;(trial-output (+ i 1) (+ k 1))
                 (set! *kount* (+ *kount* 1))
                 (return '#t))
                (else (puzzle-remove i j))))))))))))

(define (trial-output x y)
  (newline)
  (display (string-append "Piece "
                          (number->string x)
                          " at "
                          (number->string y)
                          ".")))

(define (definepiece iclass ii jj kk)
  (let ((index 0))
    (do ((i 0 (+ i 1)))
        ((> i ii))
        (do ((j 0 (+ j 1)))
            ((> j jj))
            (do ((k 0 (+ k 1)))
                ((> k kk))
                (set! index (+ i (* *d* (+ j (* *d* k)))))
                (vector-set! (vector-ref *p* *iii*) index  '#t))))
    (vector-set! *class* *iii* iclass)
    (vector-set! *piecemax* *iii* index)
    (cond ((not (= *iii* typemax))
           (set! *iii* (+ *iii* 1))))))

(define (start)
  (do ((m 0 (+ m 1)))
      ((> m size))
      (vector-set! *puzzle* m '#t))
  (do ((i 1 (+ i 1)))
      ((> i 5))
      (do ((j 1 (+ j 1)))
          ((> j 5))
          (do ((k 1 (+ k 1)))
              ((> k 5))
              (vector-set! *puzzle* (+ i (* *d* (+ j (* *d* k)))) '#f))))
  (do ((i 0 (+ i 1)))
      ((> i typemax))
      (do ((m 0 (+ m 1)))
          ((> m size))
          (vector-set! (vector-ref *p* i) m '#f)))
  (set! *iii* 0)
  (definePiece 0 3 1 0)
  (definePiece 0 1 0 3)
  (definePiece 0 0 3 1)
  (definePiece 0 1 3 0)
  (definePiece 0 3 0 1)
  (definePiece 0 0 1 3)

  (definePiece 1 2 0 0)
  (definePiece 1 0 2 0)
  (definePiece 1 0 0 2)

  (definePiece 2 1 1 0)
  (definePiece 2 1 0 1)
  (definePiece 2 0 1 1)

  (definePiece 3 1 1 1)

  (vector-set! *piececount* 0 13)
  (vector-set! *piececount* 1 3)
  (vector-set! *piececount* 2 1)
  (vector-set! *piececount* 3 1)
  (let ((m (+ (* *d* (+ *d* 1)) 1))
        (n 0))
    (cond ((fit 0 m) (set! n (place 0 m)))
          (else (begin (newline) (display "Error."))))
    (cond ((trial n)
           (begin (newline)
                  (display "Success in ")
                  (write *kount*)
                  (display " trials.")))
          (else (begin (newline) (display "Failure."))))))

(for-each (lambda (i) (vector-set! *p* i (make-vector (+ size 1))))
          (iota (+ typemax 1)))

;;; call:  (start)

(run-benchmark "Puzzle" (start))

*/

#define SIZE     511
#define CLASSMAX 3
#define TYPEMAX  12

int _iii_ = 0;
int _kount_ = 0;
int _d_ = 8;

int _piececount_[CLASSMAX+1];
int _class_[TYPEMAX+1];
int _piecemax_[TYPEMAX+1];
int _puzzle_[SIZE+1];
/* int *_p_[TYPEMAX+1]; */
int _p_[TYPEMAX+1][SIZE+1];  /*  I changed this.  - Will  */

int fit(i,j)
int i,j;
{
register int end = _piecemax_[i], k;
for (k = 0; (k <= end) && (!_p_[i][k] || !_puzzle_[j+k]); k++) ;
if (k > end) return 1; else return 0;
}

int place(i,j)
int i,j;
{
register int end = _piecemax_[i], k;
for (k = 0; k <= end; k++) if (_p_[i][k]) _puzzle_[j+k] = 1;
_piececount_[_class_[i]]--;
k = j;
while ((k <= SIZE) && _puzzle_[k]) k++;
if (k > SIZE) return 0; else return k;
}

void puzzle_remove(i,j)
int i,j;
{
register int end = _piecemax_[i];
register int k;
for (k = 0; k <= end; k++) if (_p_[i][k]) _puzzle_[j+k] = 0;
_piececount_[_class_[i]]++;
}

int trial(j)
int j;
{
register int k = 0;
register int i;

for (i = 0; i <= TYPEMAX; i++ )
  if (_piececount_[_class_[i]] != 0)
    if (fit(i,j)) {
      k = place(i,j);
      if (trial(k) || (k == 0)) { _kount_++; return 1; }
      puzzle_remove(i,j);
      }
_kount_++;
return 0;
}

void definepiece(iclass,ii,jj,kk)
int iclass,ii,jj,kk;
{
register int index, i, j, k;

for (i = 0; i <= ii; i++)
  for (j = 0; j <= jj; j++)
    for (k = 0; k <= kk; k++) {
      index = i + _d_*(j+_d_*k);
      _p_[_iii_][index] = 1;
      }

_class_[_iii_] = iclass;
_piecemax_[_iii_] = index;

if (_iii_ != TYPEMAX) _iii_++;
}

void start()
{
register int m, i, j, k, n;

for (m = 0; m <= SIZE; m++) _puzzle_[m] = 1;

for (i = 1; i <= 5; i++)
  for (j = 1; j <= 5; j++)
    for (k = 1; k <= 5; k++)
      _puzzle_[i + _d_*(j+_d_*k)] = 0;

for (i = 0; i <= TYPEMAX; i++)
  for (m = 0; m <= SIZE; m++)
    _p_[i][m] = 0;

_iii_ = 0;

definepiece(0,3,1,0);
definepiece(0,1,0,3);
definepiece(0,0,3,1);
definepiece(0,1,3,0);
definepiece(0,3,0,1);
definepiece(0,0,1,3);

definepiece(1,2,0,0);
definepiece(1,0,2,0);
definepiece(1,0,0,2);

definepiece(2,1,1,0);
definepiece(2,1,0,1);
definepiece(2,0,1,1);

definepiece(3,1,1,1);

_piececount_[0] = 13;
_piececount_[1] = 3;
_piececount_[2] = 1;
_piececount_[3] = 1;

m = _d_*(_d_+1) + 1;
n = 0;

if (fit(0,m))
  n = place(0,m);
else
  printf( "\nError." );

if (!trial(n)) printf( "\nFailure." );
}

void test_puzzle()
{
int i;

/*  I changed the declaration of _p_ to eliminate this.  - Will  */
/*  for (i = 0; i <= TYPEMAX; i++) { alloc -= SIZE+1; _p_[i] = alloc; }  */

_kount_ = 0;
start();
result = _kount_;
}

/*===========================================================================*/

int main (argc, argv)
int argc;
char *argv[];
{
  int i;

  init();
  for (i=0; i<100; i++)
    test_puzzle();
  if (result != 2005)
    printf ("*** wrong result ***\n");
  return 0;
}
