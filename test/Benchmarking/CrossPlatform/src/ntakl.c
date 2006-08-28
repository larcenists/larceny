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

TAKL

(define (listn n)
  (if (not (= 0 n))
      (cons n (listn (- n 1)))
      '()))

(define l18 (listn 18))
(define l12 (listn 12))
(define  l6 (listn 6))

(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x)
                 y z)
            (mas (cdr y)
                 z x)
            (mas (cdr z)
                 x y))))

(define (shorterp x y)
  (and y (or (null? x)
             (shorterp (cdr x)
                       (cdr y)))))

;;; call: (mas l18 l12 l6)

*/

int *listn(n)
int n;
{
if (n != 0)
  { int *rest = listn( n-1 ); return CONS( n, (int) rest ); }
else
  return NULL;
}

int shorterp(x,y)
int *x, *y;
{
while ((x != NULL) && (y != NULL)) {
  x = (int *) CDR(x);
  y = (int *) CDR(y);
}
if (y == NULL)
  return 0;
else return 1;

#if 0
/*  This is Marc Feeley's code.  I changed it to the above loop for ntakl.  */
if (y == NULL)
  return 0;
else
  if (x == NULL)
    return 1;
  else
    return shorterp( CDR(x), CDR(y) );
#endif
}

int *mas(x,y,z)
int *x, *y, *z;
{
if (!shorterp(y,x))
  return z;
else
  return mas( mas( CDR(x), y, z ),
              mas( CDR(y), z, x ),
              mas( CDR(z), x, y ) );
}

int *l18 = 0;
int *l12 = 0;
int *l6  = 0;

void test_takl()
{
  result = CAR( mas( l18, l12, l6 ) );
}

/*===========================================================================*/

int main (argc, argv)
int argc;
char *argv[];
{
  int i;

  init();
  l18 = listn(18);
  l12 = listn(12);
  l6  = listn(6);

  for (i=0; i<200; i++)
    test_takl();
  if (result != 7)
    printf ("*** wrong result ***\n");
  return 0;
}
