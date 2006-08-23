#include <stdio.h>

/*---------------------------------------------------------------------------*/

/* Support for Scheme like stuff */

#ifdef FAST_ALLOCATOR
#define CUSTOM_ALLOC   1           /* Set to 1 for custom allocation */
#else
#define CUSTOM_ALLOC   0
#endif

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

;;; TRIANGL -- Board game benchmark.
 
(define *board*
  (list->vector '(1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1)))

(define *sequence*
  (list->vector '(0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(define *a*
  (list->vector '(1 2 4 3 5 6 1 3 6 2 5 4 11 12
                  13 7 8 4 4 7 11 8 12 13 6 10
                  15 9 14 13 13 14 15 9 10
                  6 6)))

(define *b*
  (list->vector '(2 4 7 5 8 9 3 6 10 5 9 8
                  12 13 14 8 9 5 2 4 7 5 8
                  9 3 6 10 5 9 8 12 13 14
                  8 9 5 5)))

(define *c*
  (list->vector '(4 7 11 8 12 13 6 10 15 9 14 13
                  13 14 15 9 10 6 1 2 4 3 5 6 1
                  3 6 2 5 4 11 12 13 7 8 4 4)))

(define *answer* '())
 
(define (attempt i depth)
  (cond ((= depth 14)
         (set! *answer*
               (cons (cdr (vector->list *sequence*)) *answer*))
         #t)
        ((and (= 1 (vector-ref *board* (vector-ref *a* i)))
              (= 1 (vector-ref *board* (vector-ref *b* i)))
              (= 0 (vector-ref *board* (vector-ref *c* i))))
         (vector-set! *board* (vector-ref *a* i) 0)
         (vector-set! *board* (vector-ref *b* i) 0)
         (vector-set! *board* (vector-ref *c* i) 1)
         (vector-set! *sequence* depth i)
         (do ((j 0 (+ j 1))
              (depth (+ depth 1)))
             ((or (= j 36) (attempt j depth)) #f))
         (vector-set! *board* (vector-ref *a* i) 1)
         (vector-set! *board* (vector-ref *b* i) 1)
         (vector-set! *board* (vector-ref *c* i) 0) #f)
        (else #f)))
 
(define (test)
  (set! *answer* '())
  (attempt 22 1)
  (car *answer*))
 
(define (main . args)
  (run-benchmark
    "triangl"
    triangl-iters
    (lambda () (test))
    (lambda (result) (equal? result '(22 34 31 15 7 1 20 17 25 6 5 13 32)))))
*/

/*  Given a list of integers, prints it.  For debugging.  */

void printlist (x)
int * x;
{
  printf ("( ");
  while (x != NULL) {
    printf ("%d ", CAR(x));
    x = (int *) CDR(x);
  }
  printf (")\n");
}

/*  Given an array v of length n, returns a list with the same elements.  */

int *vector_list(v, n)
int *v;
int n;
{
register int i, *l = NULL;
for (i = n-1; i >= 0; i--) l = CONS( v[i], (int) l );
return l;
}

/*  Given two lists of integers, are they the same?  */

int equalp (x, y)
int *x;
int *y;
{
    if (x == NULL)
        return y == NULL;
    else if (y == NULL)
        return 0;
    else if (CAR(x) == CAR(y))
        return equalp ((int *) CDR(x), (int *) CDR(y));
    else
        return 0;
}

int _board_[] = {1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1};
int _sequence_[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0};
int _a_[] = {1,2,4,3,5,6,1,3,6,2,5,4,11,12,
             13,7,8,4,4,7,11,8,12,13,6,10,
             15,9,14,13,13,14,15,9,10,
             6,6};
int _b_[] = {2,4,7,5,8,9,3,6,10,5,9,8,
             12,13,14,8,9,5,2,4,7,5,8,
             9,3,6,10,5,9,8,12,13,14,
             8,9,5,5};
int _c_[] = {4,7,11,8,12,13,6,10,15,9,14,13,
             13,14,15,9,10,6,1,2,4,3,5,6,1,
             3,6,2,5,4,11,12,13,7,8,4,4};
int *_answer_ = NULL;

int attempt(i,depth)
int i,depth;
{
if (depth == 14)
  {
  int * temp = (int *) CDR( vector_list( _sequence_, 14 ) );
  _answer_ = CONS( (int) temp, (int) _answer_ );
  return 1;
  }
else
  if ((_board_[_a_[i]] == 1) &&
      (_board_[_b_[i]] == 1) &&
      (_board_[_c_[i]] == 0))
    {
    int j;
    _board_[_a_[i]] = 0;
    _board_[_b_[i]] = 0;
    _board_[_c_[i]] = 1;
    _sequence_[depth] = i;
    depth = depth + 1;
    for (j = 0; (j != 36) && !attempt(j,depth); j++) ;
    _board_[_a_[i]] = 1;
    _board_[_b_[i]] = 1;
    _board_[_c_[i]] = 0;
    return 0;
    }
  else
    return 0;
}

int test(i)
int i;
{
_answer_ = NULL;
attempt(i,1);
return CAR(_answer_);
}

void test_triangle()
{
result = test(22);
}

/*===========================================================================*/

int correct[] = {22, 34, 31, 15, 7, 1, 20, 17, 25, 6, 5, 13, 32};

int main (argc, argv)
int argc;
char *argv[];
{
  int i;

  init();
  for (i=0; i<10; i++)
    test_triangle();
  /*  printlist (result); */
  if (!equalp (result, vector_list (correct, 13)))
    printf ("*** wrong result ***\n");
  return 0;
}
