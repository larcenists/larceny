/******************************************************************************
*
* Usage: permsort n
*
* This benchmark is in four parts.  Each tests a different aspect of
* the memory system.
*
*    perm8            storage allocation
*    10perm8          storage allocation and deallocation
*    sumperms         sequential traversal
*    mergesort        side effects
*
* The perm8 benchmark generates a list of all 40320 permutations of
* the first 8 integers, allocating 149912 pairs (typically 1199296 bytes),
* all of which goes into the generated list.  (That is, the perm8
* benchmark generates absolutely no garbage.)  The generated permutations
* are in order of a grey code that bears no obvious relationship to a
* lexicographic order.
*
* The sumperms benchmark computes the sum of the permuted integers
* over all permutations.
*
* The 10perm8 benchmark repeats the perm8 benchmark 10 times, so it
* allocates and reclaims 1499120 pairs (typically 11,992,960 bytes).
*
* The mergesort! benchmark destructively sorts the generated permutations
* into lexicographic order, allocating no storage whatsoever.
******************************************************************************/

/* Implementation
 *   Translated from Standard ML and from Scheme.  This translation turns
 *   tail recursion into loops wherever possible.
 *
 *   A number of switches (#defined below) controls the program:
 *   - 'car' and 'cdr' are always open-coded, and 'cons' is open-coded by
 *     turning on the switch INLINE_CONS. 
 *   - Normally malloc() and free() are used to manage memory, but turning
 *     on FAST_ALLOC makes the program utilize a custom allocator and
 *     deallocator.
 *   - To print the results of the permutation and of the sort, turn on
 *     the switch 'PRINT'.
 *   - To gather allocation/deallocation statistics, turn on STATS.
 *
 *   The custom allocator allocates linearly from a segment of free
 *   storage until that segment is exhausted.
 *   Then it allocates from a free list of previously allocated storage
 *   that has been reclaimed, until that free list becomes exhausted.
 *   If both the segment of free storage and the free list are exhausted,
 *   then the allocator creates a new segment of free storage.
 */

#define MACINTOSH 0
#define FREE_IS_BROKEN 1         /* Sun's CC thinks free takes a (char *). */

#include <stdlib.h>
#include <stdio.h>

#if MACINTOSH
#include <Time.h>
#else
#include <sys/time.h>
#include <sys/resource.h>
#endif

/* Customization macros */

#define INLINE_CONS  1           /* Set to 1 to inline CONS calls */
#ifdef FAST_ALLOCATOR
#define FAST_ALLOC   1           /* Set to 1 for custom allocation */
#else
#define FAST_ALLOC   0
#endif
#define DONT_FREE    0           /* Set to 1 to avoid all deallocation */
#define STATS        0           /* Set to 1 for allocation statistics */
#define PRINT        0           /* Set to 1 to print perms and sort */

#if MACINTOSH
#define SEGSIZE 1000             /* MPW 3.2 can't malloc > 64K at once ?!? */
#else
#define SEGSIZE 10000            /* Used only for custom allocation. */
#endif

/* The macro version does not allow nested CONS calls. */

#if FAST_ALLOC

#if INLINE_CONS

#define int_CONS( x, y, dest )  \
 do { \
  ONEMORECONS(); \
  if (int_segment_free-- > 0) { \
    int_segment->num = (x); \
    int_segment->cdr = (y); \
    (dest) = int_segment++; \
  } \
  else if (free_int_list != 0) { \
    int_list int_list_result = free_int_list; \
    free_int_list = free_int_list->cdr; \
    int_list_result->num = (x); \
    int_list_result->cdr = (y); \
    (dest) = int_list_result; \
  } \
  else { \
    (dest) = int_cons( (x), (y) ); \
  }} while(0)

#define perm_CONS( x, y, dest )  \
 do { \
  ONEMORECONS(); \
  if (perm_segment_free-- > 0) { \
    perm_segment->car = (x); \
    perm_segment->cdr = (y); \
    (dest) = perm_segment++; \
  } \
  else if (free_perm_list != 0) { \
    perm_list perm_list_result = free_perm_list; \
    free_perm_list = free_perm_list->cdr; \
    perm_list_result->car = (x); \
    perm_list_result->cdr = (y); \
    (dest) = perm_list_result; \
  } \
  else { \
    (dest) = perm_cons( (x), (y) ); \
  }} while(0)

#else /* not INLINE_CONS */

#define int_CONS( A, B, D )   D = int_cons( A, B )
#define perm_CONS( A, B, D )   D = perm_cons( A, B )

#endif /* INLINE_CONS */

#define int_RECLAIM(x)  \
  x->cdr = free_int_list; \
  free_int_list = x
#define perm_RECLAIM(x)  \
  x->cdr = free_perm_list; \
  free_perm_list = x

#else /* not FAST_ALLOC */

#if INLINE_CONS

#define int_CONS( x, y, dest ) \
 do { \
      int_list TMP = (int_list) malloc( sizeof( struct int_cell ) ); \
      TMP->num = (x); \
      TMP->cdr = (y); \
      dest = TMP; \
      ONEMORECONS(); \
    } while(0)
#define perm_CONS( x, y, dest ) \
 do { \
      perm_list TMP = (perm_list) malloc( sizeof( struct perm_cell ) ); \
      TMP->car = (x); \
      TMP->cdr = (y); \
      dest = TMP; \
      ONEMORECONS(); \
    } while(0)

#else /* not INLINE_CONS */

#define int_CONS( A, B, D )   D = int_cons( A, B )
#define perm_CONS( A, B, D )   D = perm_cons( A, B )

#endif /* INLINE_CONS */

#if FREE_IS_BROKEN
#define int_RECLAIM(x) free((char *) x);
#define perm_RECLAIM(x) free((char *) x);
#else
#define int_RECLAIM(x) free(x);
#define perm_RECLAIM(x) free(x);
#endif

#endif /* FAST_ALLOC */

#if STATS
#define ONEMORECONS()  conscount++
#define ONEMORESEG()   segcount++
#define ONEMOREFREE()  freecount++
int conscount = 0;
int freecount = 0;
int segcount = 0;
#else
#define ONEMORECONS()  
#define ONEMOREFREE()  
#define ONEMORESEG()   
#endif

struct int_cell {
  struct int_cell *cdr;
  int num;
};

typedef struct int_cell *int_list;

struct perm_cell {
  struct perm_cell *cdr;
  int_list          car;
};

typedef struct perm_cell *perm_list;

perm_list permutations( int_list );

long sumperms( perm_list );
long factorial ( int );

int_list int_cons( int, int_list );
perm_list perm_cons( int_list, perm_list );
void reclaim_storage( perm_list );

void message( char * );
void printints( int_list );
void printperms( perm_list );
int int_list_length( int_list );
int perm_list_length( perm_list );

/* These variables are used by the custom memory allocator.
   They are declared here because the inline CONS macros need them. */

#if FAST_ALLOC
static int_list free_int_list = 0;
static perm_list free_perm_list = 0;
struct int_cell  *int_segment = 0;
struct perm_cell *perm_segment = 0;
int int_segment_free = 0;
int perm_segment_free = 0;
#endif

int main( int argc, char **argv )
{
  int n, nn;
  int_list one_to_n;
  perm_list m, m2;
  long sum;
  int k;

#if MACINTOSH
  n = 9;
#else
  if (argc != 2 || sscanf( argv[1], "%d%c", &n ) != 1) {
    n = 9;
  }
#endif
  printf( "n = %d\n\n", n );

  /* Create the list [1, 2, ..., n]. */
  one_to_n = 0;
  nn = n;
  while (nn > 0) {
    int_CONS( nn--, one_to_n, one_to_n );
  }

  m = permutations( one_to_n );
#if PRINT
  printperms( m );
#endif

  for ( k = 5; k > 0; --k ) {
    /*
     * For C only, we are recreating the list [1, 2, ..., n] each time.
     * This means C will allocate more storage than the garbage-collected
     * languages, but it avoids sharing of structure between the results
     * of each iteration.  Shared structure would hurt C a lot more than
     * a little extra allocation.
     */
    one_to_n = 0;
    nn = n;
    while (nn > 0) {
      int_CONS( nn--, one_to_n, one_to_n );
    }
    m2 = permutations( one_to_n );
    reclaim_storage( m );
    m = m2;
  }

  sum = sumperms( m );
  if (sum != (n * (n + 1) * factorial (n)) / 2)
    printf ("*** wrong result ***\n");
  reclaim_storage( m );

#if STATS
  printf( "Pairs allocated:   %d\n", conscount );
  printf( "Pairs deallocated: %d\n", freecount );
  printf( "Segments allocated: %d\n\n", segcount );
#endif

  return 0;
}

/******************************************************************************
*
*   permutations
*
******************************************************************************/

perm_list perms;    /* local to these functions */
int_list x;         /* local to these functions */

int_list revloop( int_list x, int n, int_list y )
{
  while (n-- != 0) {
    int_CONS( x->num, y, y );
    x = x->cdr;
  }
  return y;
}

int_list tail( int_list l, int n )
{
  while (n-- > 0) {
    l = l->cdr;
  }
  return l;
}
    
void F( int n )
{
  x = revloop( x, n, tail( x, n ) );
  perm_CONS( x, perms, perms );
}

void P( int n )
{
  int j;

  if (n > 1) {
    for ( j = n-1; j != 0; --j ) {
      P( n-1 );
      F( n );
    }
    P( n-1 );
  }
}

perm_list permutations( int_list the_x )
{
  x = the_x;
  perm_CONS( the_x, 0, perms );
  P( int_list_length( the_x ) );
  return perms;
}

/******************************************************************************
*
*   sumperms
*
******************************************************************************/

long sumperms( perm_list x ) {
    int_list y;
    long sum = 0;

    for (; x != 0; x = x->cdr)
      for (y = x->car; y != 0; y = y->cdr)
        sum = sum + y->num;

    return sum;
}

/******************************************************************************
*
*   factorial
*
******************************************************************************/

long factorial( int n ) {
    int f = 1;
    while (n > 0) {
        f = n * f;
        n = n - 1;
    }
    return f;
}

/******************************************************************************
*
*   Storage allocation and deallocation.
*
******************************************************************************/

int_list int_cons( int n, int_list y )
{
  int_list int_list_result;
  ONEMORECONS();

#if FAST_ALLOC
  if (int_segment_free-- > 0) {
    int_segment->num = n;
    int_segment->cdr = y;
    return int_segment++;
  }
  else if (free_int_list != 0) {
    int_list_result = free_int_list;
    free_int_list = free_int_list->cdr;
    int_list_result->num = n;
    int_list_result->cdr = y;
    return int_list_result;
  }
  else {
    ONEMORESEG();
/*  printf("Allocating a segment for int lists\n");  */
    int_segment = (struct int_cell *)
                  malloc( sizeof( struct int_cell ) * SEGSIZE );
    int_segment_free = SEGSIZE - 1;
    int_segment->num = n;
    int_segment->cdr = y;
    return int_segment++;
  }

#else

  int_list_result = (int_list) malloc( sizeof( struct int_cell ) );
  int_list_result->num = n;
  int_list_result->cdr = y;
  return int_list_result;
#endif
}

perm_list perm_cons( int_list x, perm_list y )
{
  perm_list perm_list_result;
  ONEMORECONS();

#if FAST_ALLOC
  if (perm_segment_free-- > 0) {
    perm_segment->car = x;
    perm_segment->cdr = y;
    return perm_segment++;
  }
  else if (free_perm_list != 0) {
    perm_list_result = free_perm_list;
    free_perm_list = free_perm_list->cdr;
    perm_list_result->car = x;
    perm_list_result->cdr = y;
    return perm_list_result;
  }
  else {
    ONEMORESEG();
/*  printf("Allocating a segment for perm lists\n");  */
    perm_segment = (struct perm_cell *)
                   malloc( sizeof( struct perm_cell ) * SEGSIZE );
    perm_segment_free = SEGSIZE - 1;
    perm_segment->car = x;
    perm_segment->cdr = y;
    return perm_segment++;
  }

#else

  perm_list_result = (perm_list) malloc( sizeof( struct perm_cell ) );
  perm_list_result->car = x;
  perm_list_result->cdr = y;
  return perm_list_result;
#endif
}

/*  Storage is reclaimed by a two-pass process of computing reference
 *  counts, then sweeping.  These two passes rely upon the following:
 *
 *    *  The given perm_list, including all storage accessible from it,
 *       is garbage.  That is, it does not share storage with any live
 *       structure and is not referred to by any live variables.
 *    *  The given perm_list contains no cycles.  The permutations may
 *       share structure with each other, but the structure that links
 *       them together is unshared.
 *    *  Each permutation in the given perm_list is a permutation of
 *       positive integers.
 */

void reclaim_storage( perm_list perms ) {

  perm_list p, q;
  int_list x, y;

#if DONT_FREE

  return;

#else
#if FAST_ALLOC
  for ( p = perms; (p != 0) && (p->car != 0); ) {
    for ( x = p->car; (x != 0) && (x->num != 0); ) {
      y = x;
      x = x->cdr;
      y->num = 0;
      int_RECLAIM(y);
      ONEMOREFREE();
    }
    q = p;
    p = p->cdr;
    q->car = 0;
    perm_RECLAIM(q);
    ONEMOREFREE();
  }
#else
  /*  Compute reference counts.
   *  During this pass, a positive integer in x->num means the node pointed to
   *  by x has not yet been marked.  A zero in x->num means the node pointed to
   *  by x has been marked to account for one reference.
   *  A -1 means two references,  -2 means three references, and so on.
   */

  for ( p = perms; p != 0; p = p->cdr ) {
    for ( x = p->car; x != 0; x = x->cdr ) {
      if ( x->num > 0 )
        x->num = 0;
      else --(x->num);
    }
  }

  /*  Sweep, freeing storage.  */

  for ( p = perms; p != 0; ) {
    for ( x = p->car; x != 0; ) {
      if ( x->num == 0 ) {
        y = x;
        x = x->cdr;
        int_RECLAIM(y);
        ONEMOREFREE();
      }
      else {
        (x->num)++;
        x = x->cdr;
      }
    }
    q = p;
    p = p->cdr;
    perm_RECLAIM(q);
    ONEMOREFREE();
  }
#endif
#endif
}


/******************************************************************************
*
*   Miscellaneous.
*
******************************************************************************/

/*  Prints a string for debugging.  */

void message( char *msg ) {
    printf( msg );
    printf( "\n" );
}

/*  Prints a list for debugging.  */

void printints ( int_list l ) {
    putchar( '(' );
    while (l != 0) {
      printf( "%d", l->num );
      l = l->cdr;
      if (l != 0) putchar( ' ' );
    }
    printf( ")\n" );
}

void printperms( perm_list perms ) {
  while (perms != 0) {
    printints( perms->car );
    perms = perms->cdr;
  }
  printf( "\n" );
}

#define LENGTH_BODY \
{                   \
  int n = 0;        \
  while (l != 0) {  \
    l = l->cdr;     \
    n++;            \
  }                 \
  return n;         \
}

int int_list_length( int_list l )
  LENGTH_BODY

int perm_list_length( perm_list l )
  LENGTH_BODY


#if MACINTOSH
/* Returns time in ms since an arbitrary origin. */
long timenow() {
  unsigned long t = __tickcount();
  return ((t & 0x000fffff) * 1000) / CLOCKS_PER_SEC;
}
#else

#if 0
/* Returns elapsed time in ms since start of process */
long timenow()
{
  struct timeval tv;
  struct timezone tz;

  gettimeofday( &tv, &tz );
  return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}
#endif

#if 0
/* Returns user + sys time in ms since start of process */

long timenow()
{
  struct rusage r;

  getrusage( RUSAGE_SELF, &r );
  return (r.ru_utime.tv_sec * 1000 + r.ru_utime.tv_usec / 1000)
       + (r.ru_stime.tv_sec * 1000 + r.ru_stime.tv_usec / 1000);
}
#endif

/* Returns user time in ms since start of process */

long timenow()
{
  struct rusage r;

  getrusage( RUSAGE_SELF, &r );
  return (r.ru_utime.tv_sec * 1000 + r.ru_utime.tv_usec / 1000);
}

#endif /* MACINTOSH */

/* eof */
