/* Copyright 1998 Lars T Hansen.              -*- indent-tabs-mode: nil -*-
 * 
 * $Id: cheney.c 5158 2007-11-26 20:55:58Z pnkfelix $
 *
 * Larceny -- copying garbage collector library, for all the GCs except
 * the DOF and conservative GCs.
 *
 * This header file gathers macros and function prototypes shared 
 * among cheney*.c
 */

/* Forwarding header (should be defined elsewhere?).

   This bit pattern is an unused immediate and can be generated in a single
   cycle on most machines (it's -2).
   */
#define FORWARD_HDR      0xFFFFFFFE

/* Copy loop implementation.

   FORW_BY_LOOP is biased in favor of smaller structures, which is probably
   the right thing.  The other two have more startup overhead but will
   probably run faster for larger structures.
   */
#define FORW_BY_LOOP     1      /* Double-word copy loop + memcpy */
#define FORW_BY_DUFF     0      /* 1 iteration of Duff's device + memcpy */
#define FORW_BY_MEMCPY   0      /* Memcpy only */

/* 
 * Normally, resist the temptation to shadow gclib_desc_g and 
 * gclib_pagebase in locals in the scanning loops -- the low-level 
 * memory manager may change them at any time!
 * However, when GCLIB_LARGE_TABLE is defined, gclib_desc_gis guaranteed
 * not to change, and can be cached.  Also, gclib_pagebase is always 0
 * and can be ignored.
 */
#define SHADOW_TABLE     1      /* Cache gclib_desc_g in the scanning loops */

/* Checking code */

#define CHECK_EVERY_WORD 0

#if CHECK_EVERY_WORD
# define check_memory( ptr, nwords )            \
    gclib_check_memory_validity( ptr, nwords )
# define check_address( ptr )                                           \
    do { if (((word)(ptr) & 7) != 0)                                    \
           panic_abort( "Odd address for forw. ptr: 0x%08x!", (ptr) );  \
    } while(0)
#else
# define check_memory( ptr, nwords ) (void)0
# define check_address( ptr )  (void)0
#endif

/* Parameter container data structure */

typedef struct cheney_env cheney_env_t;
struct cheney_env {
  /* Controlling parameters */
  int  effective_generation;
    /* A generation number s.t. all (or some, see may_be_partial, below) 
       objects from younger generations are copied into tospace.
       */

  bool enumerate_np_remset;
    /* TRUE if the non-predictive 'extra' remembered set needs to be
       enumerated.
       */

  bool scan_static;
    /* TRUE if the static area needs to be scanned in its entirety.
       */

  bool np_promotion;
    /* TRUE if this is a promotion into both the old and young non-predictive
       areas.
       */

  bool splitting;
    /* TRUE if this is a limited 'splitting' garbage collection, used to
       reorganize the heap image into text and data areas.
       */

  bool barrier_gc;
    /* True if this is collection into fixed-size areas with a barrier
       on the collection, as for the DOF collector.
      */

  bool iflush;
    /* TRUE if the instruction cache must be flushed for the destination
       address of codevectors.
       */

  void (*scan_from_globals)( word *loc, void *data );
    /* Scanner function for forwarding from globals[]
       */

  bool (*scan_from_remsets)( word obj, void *data, unsigned *count );
    /* Scanner function for forwarding from remembered sets.
       */

  void (*scan_from_tospace)( cheney_env_t *e );
    /* Scanner function for forwarded objects.
       */

  gc_t *gc;                     /* The garbage collector. */
  los_t *los;                   /* The collector's Large Object Space */

  gclib_desc_t *gclib_desc_g;   /* Descriptor table */
  
  int tospaces_len;             /* Number of initialized tospaces */
  int tospaces_cap;             /* tospaces capacity (>= tospaces_len) */
  int tospaces_cur_scan;        /* tospaces index of scanning semispace */
  int tospaces_cur_dest;        /* tospaces index of forw'ing semispace */
  semispace_t **tospaces;       /* The first tospace */
  semispace_t *tospace2;        /* The second tospace, or 0 */
  word *dest;                   /* Copy pointer of tospace */
  word *dest2;                  /* Copy pointer of tospace2, or 0 */
  word *lim;                    /* Copy limit of tospace */
  word *lim2;                   /* Copy limit of tospace2, or 0 */
  word *scan_ptr;               /* Initial scan pointer in tospace */
  word *scan_ptr2;              /* Initial scan pointer in tospace2, or 0 */
  word *scan_lim;               /* Initial scan limit in tospace */
  word *scan_lim2;              /* Initial scan limit in tospace2, or 0 */
  int  scan_idx;                /* Initially the index of the chunk in 
                                   tospace into which scan_ptr and scan_lim
                                   point; later, garbage. */
  int  scan_idx2;               /* Ditto for tospace2, or 0 */

  /* Non-predictive promotion */
  struct {
    bool has_switched;          /* 1 if we're now promoting into young */
    int  old_steps_remaining;   /* Number of full steps remaining */
    int  young_steps_remaining; /* Ditto */
    int  old_los_bytes;         /* Space used by newly promoted objects */
    int  young_los_bytes;       /* Ditto young */
    int  old_los_steps;         /* ceiling( old_los_bytes / stepsize ) */
    int  young_los_steps;       /* Ditto young */
    word **ssbtop;              /* For the NP 'extra' set */
    word **ssblim;              /* Ditto */
  } np;
};

/* Attribute bits to be passed to init_env() */
#define NP_PROMOTION        1
#define SCAN_STATIC         2
#define ENUMERATE_NP_REMSET 4
#define SPLITTING_GC        8

/* Ad-hoc instrumentation */
static gc_event_stats_t cheney; /* FIXME: duplicated across each include */

#if GC_HIRES_TIMERS
static struct {
  int      type;
  hrtime_t now;
  hrtime_t *ptr;
} cheney_event;

# define CHENEY_TYPE( t )    cheney_event.type = (t)

static void start( hrtime_t *type0, hrtime_t *type1 ) 
{
  cheney_event.now = gethrtime();
  cheney_event.ptr = (cheney_event.type ? type1 : type0);
}

static void stop( void )
{
  *cheney_event.ptr += gethrtime() - cheney_event.now;
  cheney_event.ptr = 0;
}
#else
# define CHENEY_TYPE(x)  (void)0
# define start( a, b )   (void)0
# define stop()          (void)0
#endif

#if GC_EVENT_COUNTERS
# define COUNT_REMSET_LARGE_OBJ(x)                      \
  do { if (x > GC_LARGE_OBJECT_LIMIT) {                 \
         cheney.remset_large_objs_scanned++;            \
         cheney.remset_large_obj_words_scanned += x;    \
       }                                                \
  } while(0) 
#else
# define COUNT_REMSET_LARGE_OBJ(x) (void)0
#endif

/* ptr is scan-pointer for Cheney algorithm (often named loc in this file).
 * FORW is a command that, when appropriate, will copy **ptr into
 * to-space and update *ptr.
 * When this command completes, ptr should have advanced by one object.
 */
#define scan_core( ptr, iflush, FORW )                                        \
  do {                                                                        \
    word T_w = *ptr;                                                          \
    assert2( T_w != FORWARD_HDR);                                             \
    if (ishdr( T_w )) {                                                       \
      word T_h = header( T_w );                                               \
      if (T_h == BV_HDR) {                                                    \
        /* bytevector: skip it, and flush the icache if code */               \
        word *T_oldptr = ptr;                                                 \
        word T_bytes = roundup4( sizefield( T_w ) );                          \
        ptr = (word *)((word)ptr + (T_bytes + 4)); /* doesn't skip padding */ \
        if (!(T_bytes & 4)) *ptr++ = 0;             /* pad. */                \
        /* Only code vectors typically use a plain bytevector typetag,        \
         * so almost any bytevector will be a code vector that must           \
         * be flushed.                                                        \
         */                                                                   \
        if (iflush && typetag( T_w ) == BVEC_SUBTAG)                          \
          mem_icache_flush( T_oldptr, ptr );                                  \
      }                                                                       \
      else {                                                                  \
        /* vector or procedure: scan in a tight loop */                       \
        word T_words = sizefield( T_w ) >> 2;                                 \
        ptr++;                                                                \
        while (T_words--) {                                                   \
          FORW;                                                               \
          ptr++;                                                              \
        }                                                                     \
        if (!(sizefield( T_w ) & 4)) *ptr++ = 0; /* pad. */                   \
      }                                                                       \
    }                                                                         \
    else {                                                                    \
      FORW; ptr++; FORW; ptr++;                                               \
    }                                                                         \
  } while (0)

/* 'p' is not local to the macro because it is also used by the expansion 
   of FORW.
   */
#define remset_scanner_core( ptr, p, FORW, count )      \
  p = ptrof( ptr );                                     \
  if (tagof( ptr ) == PAIR_TAG) {                       \
    FORW;                                               \
    ++p;                                                \
    FORW;                                               \
    count += 2;                                         \
  }                                                     \
  else {                                                \
    word words = sizefield( *p ) / 4;                   \
    COUNT_REMSET_LARGE_OBJ( words );                    \
    count += words;                                     \
    while (words--) {                                   \
      ++p;                                              \
      FORW;                                             \
    }                                                   \
  }

#define tospace_scan( e ) ((e)->tospaces[(e)->tospaces_cur_scan])
#define tospace_dest( e ) ((e)->tospaces[(e)->tospaces_cur_dest])

/* private procedures shared among cheney*.c */
void scan_oflo_normal( cheney_env_t *e );
word forward( word, word **, cheney_env_t *e );
void oldspace_copy( cheney_env_t *e );
void seal_chunk( semispace_t *ss, word *lim, word *dest );
void sweep_large_objects( gc_t *gc, int sweep_oldest, int g1, int g2 );
void expand_semispace( semispace_t *, word **, word **, unsigned );
void init_env( cheney_env_t *e, gc_t *gc,
	       semispace_t **tospaces, int tospaces_len, int tospaces_cap,
               semispace_t *tospace2,
	       int  effective_generation,
	       int  attributes,
	       void (*scanner)( cheney_env_t * ) );

