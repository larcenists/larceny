#include <stdio.h>

/*---------------------------------------------------------------------------*/

/* Support for Scheme like stuff */
/* Assumes sizeof(int) == sizeof(int *). */

#ifdef FAST_ALLOCATOR
#define CUSTOM_ALLOC   1           /* Set to 1 for custom allocation */
#else
#define CUSTOM_ALLOC   0
#endif

int *alloc;

#define CAR(pair) *(pair)
#define CDR(pair) ((int *) *((pair)+1))

/*  This custom allocator is moby fragile.  */

#if CUSTOM_ALLOC

#define CONS(car,cdr) ( *(--alloc) = (int) cdr, *(--alloc) = car, alloc )

#define RECLAIM(x) if (x == alloc) alloc = alloc + 2

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
                        *(alloc+1) = (int) (cdr), \
                        alloc)

#define RECLAIM(x) free(x)

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

;;; DIVITER -- Benchmark which divides by 2 using lists of n ()'s.
 
(define (create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))
 
(define *ll* (create-n 200))

(define (recursive-div2 l)
  (cond ((null? l) '())
        (else (cons (car l) (recursive-div2 (cddr l))))))
  
(define (main . args)
  (run-benchmark
    "diviter"
    diviter-iters
    (lambda () (iterative-div2 *ll*))
    (lambda (result)
      (equal? result
            '(() () () () () () () () () () () () () () () () () () () ()
              () () () () () () () () () () () () () () () () () () () ()
              () () () () () () () () () () () () () () () () () () () ()
              () () () () () () () () () () () () () () () () () () () ()
              () () () () () () () () () () () () () () () () () () () ())))))

*/

int * ll;

int *create_n(n)
int n;
{
  int *result = NULL;
  while (n > 0) {
    n = n - 1;
    result = CONS( 0, result );
  }
  return result;
}

int *recursive_div2 (int * l)
{
  if (l == NULL)
    return NULL;
  else {
    int * y = recursive_div2 (CDR(CDR( l )));
    return CONS( CAR(l), y );
  }
}

void reclaim_list ( int * x ) {

  int * y = x;
  while (y != NULL) {
    x = y;
    y = CDR(y);
    RECLAIM(x);
  }
}

#define LENGTH_BODY \
{                   \
  int n = 0;        \
  while (l != 0) {  \
    l = CDR(l);     \
    n++;            \
  }                 \
  return n;         \
}

int list_length( int * l )
  LENGTH_BODY

/*===========================================================================*/

int main (argc, argv)
int argc;
char *argv[];
{
  int i;
  int *result;
  int *ll;

  init();
  ll = create_n (200);

  result = recursive_div2 (ll);
  for (i=1; i<400000; i++) {
    reclaim_list (result);
    result = recursive_div2 (ll);
  }

  if (list_length (result) != 100)
    printf ("*** wrong result ***\n");
  return 0;
}
