/*
 * Larceny Runtime System.
 * Memory management system support code.
 *
 * $Id: memsupport.c,v 1.13 92/02/23 16:56:38 lth Exp Locker: lth $
 *
 * The procedures in here initialize the memory system, perform tasks 
 * associated with garbage collection, and manipulate the stack cache.
 * They are likely a bit dependent upon calling conventions etc, although
 * I've tried to keep most such nasties confined to the file "layouts.h".
 */

#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include "larceny.h"
#include "machine.h"
#include "gcinterface.h"
#include "layouts.h"
#include "offsets.h"
#include "macros.h"
/* #include "main.h" */
#include "millicode.h"
#include "memstats.h"

#define GLOBALCELLREF( cp )  (*(word *) ((word)(cp) & ~TAG_MASK))
#define VECTORREF( vp, i )   (*((word *)((word)(vp) & ~TAG_MASK)+(i)+1))
#define VECTORSET( vp, i, v) (*((word *)((word)(vp) & ~TAG_MASK)+(i)+1)=(v))

#define LARGEST_FIXNUM (2147483644L)  /* ((2^29)-1)*4 */

/* Calculate free bytes in ephemeral space. Can be negative! */
#define free_e_space() \
            ((long)globals[E_LIMIT_OFFSET] - (long)globals[E_TOP_OFFSET])

/* Ditto used bytes, although never negative. */
#define used_e_space() \
            ((long)globals[ E_TOP_OFFSET ] - (long) globals[ E_BASE_OFFSET ])

static int local_collect(),
           setup_memory_limits(),
           setup_stack();
static unsigned long resourcenow();

/*
 * Initialize memory management system. Allocate spaces, setup limits,
 * and initialize fake continuation in stack cache.
 */
C_init_mem( e_size, t_size, s_size, stk_size, e_lim )
unsigned e_size, t_size, s_size, stk_size, e_lim;
{
  if (init_collector( s_size, t_size, e_size, stk_size ) == 0)
    return 0;

  globals[ CONTINUATION_OFFSET ] = FALSE_CONST;
  globals[ T_TRANS_OFFSET ] = globals[ T_MAX_OFFSET ];
  globals[ E_MARK_OFFSET ] = roundup8( e_lim );
  setup_memory_limits();
  setup_stack();
  return 1;
}


/*
 * Allocate a chunk of memory. `n' is a number of bytes.
 */
word *C_alloc( n )
int n;
{
  word p;

  n = roundup8( n );

  if (globals[ E_TOP_OFFSET ] + n > globals[ E_LIMIT_OFFSET ])
    C_gcstart2( n );

  p = globals[ E_TOP_OFFSET ];
  globals[ E_TOP_OFFSET ] += n;
  return (word *)p;
}


/*
 * This is the procedure that is called by the assembly-language memory
 * management routines when there is a need for garbage collection.
 *
 * [These comments are out of date.]
 *
 * If the argument 'n' is fixnum( -1 ) then we perform a tenuring collection
 * and return. If it is any other number (and it had better be nonnegative,
 * then!), then we perform an ephemeral collection. If, after the collection,
 * there is room to allocate nativeint( n ) words on the heap, we return.
 * Otherwise, we perform a tenuring collection. If, after the collection,
 * we cannot allocate the memory, then the user is requesting a chunk of
 * memory which is bigger than the ephemeral space, and we call gc_trap().
 * in the hope that it will figure out what to do. When gc_trap() returns,
 * we repeat the process. Otherwise, we return.
 */
word C_gcstart2( n )
word n;
{
  long before, after;
  long n_bytes = nativeint( n ) * 4;

  before = resourcenow();

  C_flush_stack_cache();
  if (n == fixnum( -1 ))
    local_collect( TENURING_COLLECTION );
  else if (n == fixnum( -2 ))
    local_collect( FULL_COLLECTION );
  else {
    local_collect( EPHEMERAL_COLLECTION );

    if (n_bytes > free_e_space()) {
      /* 
       * We only get here if the object to be allocated is bigger than the
       * free space in the ephemeral area after a collection.
       */
      local_collect( TENURING_COLLECTION );

      while (n_bytes > free_e_space()) {
	/* 
	 * This loop should only be entered when the program is attempting to
	 * allocate an object larger than the ephemeral area. Don't expect
	 * any solution to this problem soon.
	 */
	gc_trap( EPHEMERAL_TRAP );
	setup_memory_limits();
      }
    }
  }

  flush_icache();

  after = resourcenow();
  set_memstat( MEMSTAT_TIME_SPENT_COLLECTING, 
	       memstat( MEMSTAT_TIME_SPENT_COLLECTING ) 
	     + fixnum( after - before ) );
}


/*
 * Load a heap image into memory. There are two kinds of heaps, single and
 * split. Both have a version number as the first word followed by the
 * roots. The high 16 bits of the version number is the heap type; 0=single,
 * 1=split. The low 16 bits is the version proper.
 *
 * Single heaps are loaded into the tenured or ephemeral area depending on
 * the "which_heap" parameter; 0=ephemeral, 1=tenured. Split heaps are
 * loaded into the static area and the tenured or ephemeral area.
 *
 * Single heap format:
 *  - version number (1 word)
 *  - roots (n words; depends on version)
 *  - word count for the heap data
 *  - heap data
 * All pointers in the roots or in the heap are from base 0. They
 * are adjusted as the heap is read in.
 *
 * Split heap format (WRONG! Cannot have pointers from vheap to sheap!)
 *  - version number (1 word)
 *  - roots (n words; depends on version)
 *  - word count for the static heap data
 *  - word count for the tenured or ephemeral heap data
 *  - static heap data
 *  - tenured or ephemeral data
 *
 * All words are stored in big-endian format.
 *
 * Returns 0 if everything went fine; -1 otherwise.
 */
C_load_heap( fp, which_heap )
FILE *fp;
int which_heap;
{
  word base, magic, limit, scount, hcount, *p, w;
  static word *loadit();
  unsigned i;

  if (which_heap == 0) {
    base = globals[ E_BASE_OFFSET ];
    limit = globals[ E_MAX_OFFSET ];
  }
  else {
    base = globals[ T_BASE_OFFSET ];
    limit = globals[ T_MAX_OFFSET ];
  }
  magic = getword( fp );
  if ((magic & 0xFFFF) != HEAP_VERSION)
    C_panic( "Wrong version heap image." );

  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ ) {
    w = getword( fp );
    globals[ i ] = (isptr( w ) ? w + base : w);
  }

  if ((magic & 0xFFFF0000) == 1) {
    C_panic( "Can't do split heaps!!" );
    scount = getword( fp );
    hcount = getword( fp );
    loadit( fp, globals[ S_BASE_OFFSET ], globals[ S_MAX_OFFSET ], scount );
    p = loadit( fp, base, limit, hcount );
  }
  else {
    hcount = getword( fp );
    p = loadit( fp, base, limit, hcount );
  }
  if (which_heap == 0)
    globals[ E_TOP_OFFSET ] = (word) p;
  else
    globals[ T_TOP_OFFSET ] = (word) p;
  return 0;
}

static word *loadit( fp, base, limit, count )
FILE *fp;
word base, limit;
unsigned count;
{
  word *p, w;
  unsigned i;

  if (base + count*4 > limit)
    C_panic( "Heap image will not fit in heap.");

  if (fread( (word *) base, 4, count, fp ) < count)
    C_panic( "Corrupt heap file!" );
  p = (word *) base;
  while (count > 0) {
    w = *p;
    count--;
    if (isptr( w ))
      *p = w + base;
    p++;
    if (header( w ) == BV_HDR) { /* is well-defined on non-hdrs */
      i = roundup4( sizefield( w ) ) / 4;
      p += i; count -= i;
    }
  }
  return p;
}


static getword( fp )
FILE *fp;
{
  word a = getc( fp );
  word b = getc( fp );
  word c = getc( fp );
  word d = getc( fp );

  return (a << 24) | (b << 16) | (c << 8) | d;
}


/*
 * Dumps the tenured heap. The heap layout is described above.
 * Returns 0 if everything went fine; EOF otherwise.
 *
 * The tenured heap is dumped "as is"; pointers into other areas are
 * dumped literally and should not exist (how do we deal with pointers into
 * the static area? At least pointers into the tenured area can be gc'd away.)
 *
 * UNTESTED!!
 */
C_dump_heap( fp )
FILE *fp;
{
  word base, top, count, w, *p;
  int i, align;

  base = globals[ T_BASE_OFFSET ];
  top  = globals[ T_TOP_OFFSET ];
  if (putword( HEAP_VERSION, fp ) == EOF)
    return EOF;

  for (i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    if (putword( globals[ i ], fp ) == EOF)
      return EOF;

  count = (top - base) / 4;
  if (putword( count, fp ) == EOF)
    return EOF;

  p = (word *) base;
  while (count--) {
    w = *p++;
    if (putword( isptr( w ) ? w-base : w, fp ) == EOF)
      return EOF;
    
    if (header( w ) == BV_HDR) {
      i = roundup4( sizefield( w ) ) / 4;
      align = (i % 2 == 0);
      while (i--) {
	if (putword( *p++, fp ) == EOF)
	  return EOF;
      }
      if (align)
	if (putword( 0, fp ) == EOF)
	  return EOF;
    }
  }

  return 0;
}


static putword( w, fp )
word w;
FILE *fp;
{
  if (putc( (w >> 24) & 0xFF, fp ) == EOF) return EOF;
  if (putc( (w >> 16) & 0xFF, fp ) == EOF) return EOF;
  if (putc( (w >> 8) & 0xFF, fp ) == EOF) return EOF;
  if (putc( w & 0xFF, fp ) == EOF) return EOF;
  return 0;
}


/*
 * Do a collection and gather collection statistics.
 *
 * The words allocated since the last collection is the difference in E_TOP
 * between then and now. The old value is stored in the MEMSTATS_VECTOR global
 * at offset MEMSTAT_ESIZE_AFTER_LAST.
 *
 * Note the overloading of heap sizes here: in the tables they are in words,
 * as fixnums, here they are calculated in bytes. The representations are 
 * trivially convertable: A fixnum count of words is a native count of bytes
 * (if aligned on 4 or 8 byte boundary, which heap sizes and heaps are).
 *
 * For the sake of sanity it would perhaps be better to explicitly convert.
 */
static local_collect( requested_type )
int requested_type;
{
  int performed_type, must_tenure;
  word before, after, tsize, esize, tsize2, esize2, copied, allocated;
  word collected, allocated_hi, allocated_lo, collected_hi, collected_lo;
  word copied_hi, copied_lo;
  word old_e_size;

  if (globals[ DEBUGLEVEL_OFFSET ]) {
    printf( "garbage collection commencing, requested type %d\n", 
	    requested_type );
    before = resourcenow();
  }

  must_tenure = memstat( MEMSTAT_MUST_TENURE ) == TRUE_CONST;
  old_e_size = memstat( MEMSTAT_ESIZE_AFTER_LAST );
  tsize = globals[ T_TOP_OFFSET ] - globals[ T_BASE_OFFSET ];
  esize = globals[ E_TOP_OFFSET ] - globals[ E_BASE_OFFSET ];

  if (requested_type == EPHEMERAL_COLLECTION && must_tenure)
    performed_type = collect( TENURING_COLLECTION );
  else 
    performed_type = collect( requested_type );
  setup_memory_limits();

  tsize2 = globals[ T_TOP_OFFSET ] - globals[ T_BASE_OFFSET ];
  esize2 = globals[ E_TOP_OFFSET ] - globals[ E_BASE_OFFSET ];

  if (performed_type == EPHEMERAL_COLLECTION)
    copied = esize2;
  else if (performed_type == TENURING_COLLECTION)
    copied = tsize2 - tsize;
  else
    copied = tsize2;

  if (globals[ DEBUGLEVEL_OFFSET ]) {
    printf( "garbage collection done, performed type %d\n", performed_type );
    after = resourcenow();
    printf( "bytes copied: %ld in %u milliseconds\n", copied, after-before );
  }

  collected = (tsize + esize) - (tsize2 + esize2);
  allocated = esize - old_e_size;
  
  allocated_hi = memstat( MEMSTAT_WORDS_ALLOCATED_HI );
  allocated_lo = memstat( MEMSTAT_WORDS_ALLOCATED_LO );
  collected_hi = memstat( MEMSTAT_WORDS_COLLECTED_HI );
  collected_lo = memstat( MEMSTAT_WORDS_COLLECTED_LO );
  copied_hi = memstat( MEMSTAT_WORDS_COPIED_HI );
  copied_lo = memstat( MEMSTAT_WORDS_COPIED_LO );

  add( &allocated_hi, &allocated_lo, allocated );
  add( &collected_hi, &collected_lo, collected );
  add( &copied_hi, &copied_lo, copied );

  if (performed_type == EPHEMERAL_COLLECTION) {
    set_memstat( MEMSTAT_EPHEMERAL_COLLECTIONS,
		 memstat( MEMSTAT_EPHEMERAL_COLLECTIONS ) + fixnum( 1 ) );
  }
  else if (performed_type == TENURING_COLLECTION ) {
    set_memstat( MEMSTAT_TENURING_COLLECTIONS,
		 memstat( MEMSTAT_TENURING_COLLECTIONS ) + fixnum( 1 ) );
  }
  else {
    set_memstat( MEMSTAT_FULL_COLLECTIONS,
		memstat( MEMSTAT_FULL_COLLECTIONS ) + fixnum( 1 ) );
  }
  set_memstat( MEMSTAT_WORDS_ALLOCATED_HI, allocated_hi );
  set_memstat( MEMSTAT_WORDS_ALLOCATED_LO, allocated_lo );
  set_memstat( MEMSTAT_WORDS_COLLECTED_HI, collected_hi );
  set_memstat( MEMSTAT_WORDS_COLLECTED_LO, collected_lo );
  set_memstat( MEMSTAT_WORDS_COPIED_HI, copied_hi );
  set_memstat( MEMSTAT_WORDS_COPIED_LO, copied_lo );
  set_memstat( MEMSTAT_ESIZE_AFTER_LAST, esize2 );

  if (used_e_space() > globals[ E_MARK_OFFSET ])
    set_memstat( MEMSTAT_MUST_TENURE, TRUE_CONST );
  else
    set_memstat( MEMSTAT_MUST_TENURE, FALSE_CONST );

}


/*
 * Adds a word to a doubleword with carry propagation and stuff, both
 * parts of the doubleword are independently represented as fixnums, as is
 * 'x'. [ i.e. all operands better be congruent to 0 mod 4. ]
 */
static add( hi, lo, x )
unsigned *hi, *lo, x;
{
  *lo += x;
  if (*lo > LARGEST_FIXNUM) {
    *lo -= LARGEST_FIXNUM;
    *hi += 4;
  }
}

static unsigned long resourcenow()
{
  struct rusage r1;

  getrusage( RUSAGE_SELF, &r1 );
  return r1.ru_utime.tv_sec * 1000 + r1.ru_utime.tv_usec / 1000;
}

static memstat( i )
int i;
{
  word cell;

  cell = globals[ MEMSTATS_VECTOR_OFFSET ];
  if (GLOBALCELLREF( cell ) == UNSPECIFIED_CONST)
    return 0;
  else
    return VECTORREF( GLOBALCELLREF( cell ), i );
}


static set_memstat( i, v )
{
  word cell;

  cell = globals[ MEMSTATS_VECTOR_OFFSET ];
  if (GLOBALCELLREF( cell ) == UNSPECIFIED_CONST)
    return 0;
  else
    return VECTORSET( GLOBALCELLREF( cell ), i, v );
}


/*
 * Given that the spaces have been set up and that pointers are in the
 * "globals" array, we calculate the ephemeral heap limit.
 */
static setup_memory_limits()
{
  /*
   * Currently we reserve twice the stack space for the overflow area.
   * This is almost certainly too much (but that is not harmful).
   */

  globals[ E_LIMIT_OFFSET ] = globals[ E_MAX_OFFSET ]+4 - 
    roundup8( (globals[ STK_MAX_OFFSET ] - globals[ STK_BASE_OFFSET ] + 4)*2 );
}


/*
 * Calculate stack limit in some implementation-dependent fashion, and set
 * up a fake continuation at the bottom (highest addresses) of the stack.
 */
static setup_stack()
{
  extern void stkuflow();
  word *stktop;

#if SPARC
  stktop = (word *) globals[ STK_MAX_OFFSET ];
  *stktop = 0x81726354;                                /* dummy magic # */
  *(stktop-1) = 0;                                     /* saved proc */
  *(stktop-2) = 16;                                    /* continuation size */
  *(stktop-3) = (word) millicode[ M_STKUFLOW+1 ];      /* underflow handler */
  globals[ STK_START_OFFSET ] = (word) (stktop-4);
  globals[ SP_OFFSET ] = (word) (stktop-3);

  /* 
   * Need to calculate max continuation size in order to find the
   * lower stack limit. We'll just gratuitiously assume that 100 words is
   * plenty, and deal with elegance later.
   */

  globals[ STK_LIMIT_OFFSET ] = (word) ((word *)globals[STK_BASE_OFFSET]+100);
#endif
}


/*
 * Flush the instruction cache, if there is one.
 * On some systems this probably cannot be done in a high-level language.
 */
static flush_icache()
{
#if SPARC1 || SPARC2
  /* 
   * Cache flush is a no-op on the Sparcstation 1 and the Sparcstation 2 
   * because these machines have shared I/D caches, ensuring consistency 
   * even if code is moved around.
   */
#else
#endif
}


/*
 * Restore one stack frame from the heap. We also have to maintain the
 * fake continuation at the bottom of the heap; however, that continuation
 * is still intact, and so we simply put the new one on top of it on
 * the stack.
 *
 * We do not check for stack overflow, as this would be a silly thing to do.
 * 
 * Refer to "conventions.txt" for an explanation of the continuation layouts.
 */
void C_restore_frame()
{
  word *sframe, *hframe;
  unsigned sframesize, hframesize;

#ifdef DEBUG
  if (globals[ DEBUGLEVEL_OFFSET ] > 1) {
    printf( "Restoring stack frame; stkptr is %08x.\n", globals[ SP_OFFSET ] );
    if (globals[ CONTINUATION_OFFSET ] == FALSE_CONST)
      C_panic( "restore_frame: no more continuation frames!" );

  }
  if (globals[ SP_OFFSET ] % 8 != 0) {
    printf( "restore_frame: botched stack pointer %08x!\n", 
	   globals[ SP_OFFSET ] );
    C_localdebugger();
    C_panic( "Not possible to continue. Goodbye." );
  }
#endif

  /* Get heap continuation */

  hframe = ptrof( globals[ CONTINUATION_OFFSET ] );
  hframesize = sizefield( *hframe ) + 4;

#ifdef DEBUG
  if (globals[ DEBUGLEVEL_OFFSET ] > 1) {
    printf( "Frame @ %08x, header %08x, size %d\n", hframe, *hframe, 
	   hframesize );
  }
#endif

  /* Allocate stack frame and bump saved stack pointer */

  sframesize = hframesize - HC_OVERHEAD*4 + STK_OVERHEAD*4;
  if (sframesize % 8 != 0) sframesize += 4;
  sframe = (word *) globals[ SP_OFFSET ] - sframesize / 4;
  globals[ SP_OFFSET ] = (word) sframe;

  /* Follow continuation chain. */

  globals[ CONTINUATION_OFFSET ] = *(hframe + HC_DYNLINK);

  /* Setup stack frame header. Special case for procedures with value 0. 
   * Also note that since the offset in the heap frame is a bytevector
   * index (rather than an offset from the bytevector header), then
   * 4 have to be added.
   * Observe the inverse code in flush_stack_cache(), below.
   */

  { word *procptr, codeptr;

    procptr = ptrof( *(hframe + HC_PROC) );
    if ((word) procptr != 0) {
      codeptr = (word) ptrof( *(procptr + PROC_CODEPTR) );
      *(sframe + STK_RETADDR) = (codeptr + *(hframe+HC_RETOFFSET) + 4);
    }
    else
      *(sframe + STK_RETADDR) = *(hframe + HC_RETOFFSET);
    *(sframe + STK_CONTSIZE) = sframesize;
  }
#ifdef DEBUG
  if (globals[ DEBUGLEVEL_OFFSET ] > 2) {
    printf( "return address %08x\n", *(sframe + STK_RETADDR ) );
  }
#endif

  /* Copy the generic "saved slots" */

  { word *src, *dest;
    unsigned count;

    count = hframesize / 4 - HC_OVERHEAD;
    src = hframe + HC_SAVED;
    dest = sframe + STK_SAVED;
    while (count-- > 0)
      *dest++ = *src++;
  }

  if (globals[ SP_OFFSET ] % 8 != 0)
    C_panic( "restore_frame(2): botched stack pointer!" );
}

  
/* 
 * Flush the stack cache to the heap.
 *
 * Due to the setup of and rules for allocating space in the ephemeral
 * area, we are guaranteed that there will be room for the flush.
 */
void C_flush_stack_cache()
{
  word *sp, *first_cont, *e_top, *prev_cont, *stk_start;
#ifdef DEBUG
  int framecount = 0;
#endif

#ifdef DEBUG
  if (globals[ DEBUGLEVEL_OFFSET ] > 1) {
    printf( "Flushing stack cache; base = %x, limit = %x, ptr = %x.\n",
	   globals[ STK_START_OFFSET ], globals[ STK_LIMIT_OFFSET ],
	   globals[ SP_OFFSET ]);
  }
#endif

  sp = (word *) globals[ SP_OFFSET ];
  stk_start = (word *) globals[ STK_START_OFFSET ];
  e_top = (word *) globals[ E_TOP_OFFSET ];
  first_cont = prev_cont = NULL;

  while (sp < stk_start) {
    word retaddr, *procptr, *sframe, *hframe, codeptr;
    unsigned sframesize, hframesize;

    /* Get stack frame stuff */

    sframe = sp;
    sframesize = *(sframe + STK_CONTSIZE);   /* unadjusted size of frame */
    retaddr = *(sframe + STK_RETADDR);       /* unadjusted return address */
    procptr = ptrof( *(sframe + STK_PROC) ); /* pointer to the procedure */

#ifdef DEBUG
    if (globals[ DEBUGLEVEL_OFFSET ] > 2) {
      printf( "return address %08x\n", retaddr );
    }
#endif

    /* Move stack pointer */

    sp += sframesize / 4;
    if (sframesize % 8 != 0) sp++;

    /* Calculate heap frame size */

    hframesize = sframesize - STK_OVERHEAD*4 + HC_OVERHEAD*4;

    /* Allocate memory on heap */

    hframe = e_top;
    e_top += hframesize / 4;
    if (hframesize % 8 != 0) e_top++;

    /* Setup heap continuation header */

    *hframe = mkheader( hframesize-4, VEC_HDR | CONT_SUBTAG );
    *(hframe + HC_DYNLINK) = FALSE_CONST;
    if ((word) procptr != 0) {
      codeptr = (word) ptrof( *(procptr + PROC_CODEPTR));
      *(hframe + HC_RETOFFSET) = retaddr - codeptr - 4;  /* offset is bv idx */
    }
    else
      *(hframe + HC_RETOFFSET) = retaddr;

    /* Copy saved values */

    { unsigned count;
      word *src, *dest;

      count = sframesize / 4 - STK_OVERHEAD;
      src = sframe + STK_SAVED;
      dest = hframe + HC_SAVED;
      while (count-- > 0)
	*dest++ = *src++;
    }

    /* Link continuation frame into chain */

    if (prev_cont != NULL)
      *(prev_cont + HC_DYNLINK) = (word) tagptr( hframe, VEC_TAG );
    else
      first_cont = hframe;
    prev_cont = hframe;
#ifdef DEBUG
    framecount++;
    if (globals[ E_TOP_OFFSET ] > globals[ E_MAX_OFFSET ]) 
      printf( "Major overflow in flush!\n" );
#endif
  }

  /* Get the last link, too */

  if (prev_cont != NULL)
    *(prev_cont + HC_DYNLINK) = globals[ CONTINUATION_OFFSET ];

  /* Now restore the virtual machine state */

  globals[ SP_OFFSET ] = (word) sp;
  globals[ E_TOP_OFFSET ] = (word) e_top;
  if (first_cont != NULL)
    globals[ CONTINUATION_OFFSET ] = (word) tagptr( first_cont, VEC_TAG );

#ifdef DEBUG
  if (globals[ DEBUGLEVEL_OFFSET ] > 1) {
    printf( "%d frames flushed.\n", framecount );
  }
#endif
}


#ifdef DEBUG
dumpchain()
{
  word w, *p;
  int i;

  p = ptrof( globals[ CONTINUATION_OFFSET ] );
  for (i = 0 ; i < 5 ; i++ ) 
    printf( "%08x\n", *p++ );
}
#endif


/* eof */
