/* Copyright 1998 Lars T Hansen.              -*- indent-tabs-mode: nil -*-
 * 
 * $Id$
 *
 * Larceny -- copying garbage collector library, for all the GCs except
 * the DOF and conservative GCs.
 */

#define GC_INTERNAL

#include <sys/time.h>
#include <string.h>
#include "larceny.h"
#include "memmgr.h"
#include "gc_t.h"
#include "semispace_t.h"
#include "los_t.h"
#include "static_heap_t.h"
#include "gclib.h"
#include "barrier.h"
#include "stats.h"

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

/* Forwarding macros for normal copying collection and promotion.

   Forw_oflo() forwards the contents of a location after obtaining
   enough memory for the copy, if needed.

   Forw_oflo_record() is like forw_oflo() but is used during remembered-set
   scanning.  In addition to forwarding a location in a remembered object,
   it remembers whether any locations in the object were changed.
 
   Forw_core() implements the meat of the forwarding operation.

   Check_space() checks whether the semispace has room for 'wanted' bytes,
   and if not, it expands the semispace and updates the 'dest' and 'lim' 
   variables.

   Scan_core() implements the semispace scanning operation.  It is
   parameterized by an expression that calls the appropriate forwarding macro.

   Remset_scanner_core() scans a single remembered set entry.  It is
   parameterized by an expression that calls the appropriate forwarding macro.
   */

/* Assumes that all parameters except forw_limit_gen are lvalues whose
   evaluation has no side effect and whose value will not change in ways
   not controlled by the macro.  Forw_limit_gen may be any expression.
   */
#define forw_oflo( loc, forw_limit_gen, dest, lim, e )                      \
  do { word T_obj = *loc;                                                   \
       if (isptr(T_obj) && gen_of(T_obj) < (forw_limit_gen)){ \
          forw_core( T_obj, loc, dest, lim, e );                            \
       }                                                                    \
  } while( 0 )

/* Old_obj_gen is the generation of the object being scanned.
   Note that if the object being scanned is in the generation being collected,
   which happens during a promotion into that generation, then some objects
   will be believed to have intergenerational pointers even though they
   don't, because T_obj_gen is computed only once.  This is OK, because
   the collectors always clear the required remembered sets after a promotion.
   The alternative, to recompute T_obj_gen after forwarding, is more
   expensive.  Since the remembered set should be cleaned anyway to 
   reset pointers and so on, I forego the expense of fixing this macro.
   Same goes for forw_np_record.
   */
#define forw_oflo_record( loc, forw_limit_gen, dest, lim, has_intergen_ptr, \
                          old_obj_gen, e )                                  \
  do { word T_obj = *loc;                                                   \
       if (isptr( T_obj )) {                                                \
          unsigned T_obj_gen = gen_of(T_obj);                 \
          if (T_obj_gen < (forw_limit_gen)) {                               \
            forw_core( T_obj, loc, dest, lim, e );                          \
          }                                                                 \
          if (T_obj_gen < old_obj_gen) has_intergen_ptr=1;                  \
       }                                                                    \
  } while( 0 )

#define forw_core( T_obj, loc, dest, lim, e )           \
  word *TMP_P = ptrof( T_obj );                         \
  word TMP_W = *TMP_P;                                  \
  if (TMP_W == FORWARD_HDR)                             \
    *loc = *(TMP_P+1);                                  \
  else if (tagof( T_obj ) == PAIR_TAG) {                \
    check_space(dest,lim,8,e);                          \
    *dest = TMP_W;                                      \
    *(dest+1) = *(TMP_P+1);                             \
    check_address( TMP_P );                             \
    *TMP_P = FORWARD_HDR;                               \
    *(TMP_P+1) = *loc = (word)tagptr(dest, PAIR_TAG);   \
    check_memory( dest, 2 );                            \
    dest += 2;                                          \
  }                                                     \
  else {                                                \
    word *TMPD;                                         \
    check_space(dest,lim,sizefield(TMP_W)+4,e);         \
    TMPD = dest;                                        \
    *loc = forward( T_obj, &TMPD, e ); dest = TMPD;     \
  }

/* Large objects must be handled here so we don't allocate space to
   handle them; by letting the check succeed for large objects, the
   subsequent call to forward() will handle the object properly.
   */
#define check_space( dest, lim, wanted, e )                                  \
  if ((char*)lim-(char*)dest < (wanted) && (wanted)<=GC_LARGE_OBJECT_LIMIT){ \
    word *CS_LIM=lim, *CS_DEST=dest;                                         \
    expand_semispace( e->tospace, &CS_LIM, &CS_DEST, (wanted) );             \
    dest = CS_DEST; lim = CS_LIM;                                            \
  }

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


/* Additional forwarding macros for a splitting garbage collection.

   This is a hack and should go away, but it's still used to support
   the Boehm collector by allowing the creation of a clean split heap.

   A splitting gc uses two destination semispaces: one for pointer data
   (ss_data) and one for non-pointer data (ss_text).

   Forw_oflo2() is like forw_oflo(); forw_core2() is like forw_core().
   The same scanning macros are used for this type of collection as
   for a normal collection.

   Check_space2() differs from check_space() in that large objects are
   not handled specially -- during splitting gc, large objects are
   copied the same way as other objects.
   */

#define forw_oflo2( loc, forw_limit_gen, dest, dest2, lim, lim2, e )          \
  do { word T_obj = *loc;                                                     \
       if (isptr( T_obj ) && gen_of(T_obj) < (forw_limit_gen)){ \
          forw_core2( T_obj, loc, dest, dest2, lim, lim2, e );                \
       }                                                                      \
  } while( 0 )

#define forw_core2( T_obj, loc, dest, dest2, lim, lim2, e )             \
  word *TMP_P = ptrof( T_obj );                                         \
  if (*TMP_P == FORWARD_HDR)                                            \
    *loc = *(TMP_P+1);                                                  \
  else if (tagof( T_obj ) == PAIR_TAG) {                                \
    check_space2(dest,lim,8,e->tospace); /*data*/                       \
    *dest = *TMP_P;                                                     \
    *(dest+1) = *(TMP_P+1);                                             \
    check_address( TMP_P );                                             \
    *TMP_P = FORWARD_HDR;                                               \
    *(TMP_P+1) = *loc = (word)tagptr(dest, PAIR_TAG);                   \
    check_memory( dest, 2 );                                            \
    dest += 2;                                                          \
  }                                                                     \
  else if (tagof( T_obj ) == BVEC_TAG) {                                \
    word *TMPD;                                                         \
    check_space2(dest2,lim2,sizefield(*TMP_P)+4,e->tospace2); /*text*/  \
    TMPD = dest2;                                                       \
    *loc = forward( T_obj, &TMPD, e ); dest2 = TMPD;              \
  }                                                                     \
  else {                                                                \
    word *TMPD;                                                         \
    check_space2(dest,lim,sizefield(*TMP_P)+4,e->tospace);/*data*/      \
    TMPD = dest;                                                        \
    *loc = forward( T_obj, &TMPD, e ); dest = TMPD;               \
  }

#define check_space2( dest, lim, wanted, semispace )            \
  if ((char*)lim-(char*)dest < (wanted)) {                      \
    word *CS_LIM=lim, *CS_DEST=dest;                            \
    expand_semispace( semispace, &CS_LIM, &CS_DEST, (wanted) ); \
    dest = CS_DEST; lim = CS_LIM;                               \
  }


/* Additional forwarding macros for non-predictive promotion.

   During non-predictive promotion, objects are moved first into the old
   area until it fills up, then into the young area.  The counter
   e.np.old_steps_remaining keeps track of available space in the old
   area; when a chunk is full and this counter is found to be 0, then
   the old area is full.

   Forw_np() is like forw_oflo() but is only suitable for scanning the
   'old' area after promotion.

   Forw_np_record() is like forw_oflo_record().

   Forw_np_partial() is like forw_np(), but suitable for scanning the
   non-predictive 'young' area after promotion into it.  It records
   whether an object needs to be added to the non-predictive extra
   remembered set.

   Forw_core_np() is like forw_core().

   Check_space_np() is like check_space().

   Scan_core_partial() is like scan_core(), but is useful only when
   scanning the non-predictive 'young' area, and adds an object to the
   non-predictive extra remembered set if forw_np_partial() indicates
   that it must be done.
   */

#define forw_np( loc, forw_limit_gen, dest, lim, e )                          \
  do { word T_obj = *loc;                                                     \
       if (isptr( T_obj ) && gen_of(T_obj) < (forw_limit_gen)){ \
          forw_core_np( T_obj, loc, dest, lim, e );                           \
       }                                                                      \
  } while( 0 )

/* See comments for forw_oflo_record regarding this implementation. 
   */
#define forw_np_record( loc, forw_limit_gen, dest, lim, has_intergen_ptr, \
                        old_obj_gen, e )                                  \
  do { word T_obj = *loc;                                                 \
       if (isptr( T_obj )) {                                              \
          unsigned T_obj_gen = gen_of(T_obj);               \
          if (T_obj_gen < (forw_limit_gen)) {                             \
            forw_core_np( T_obj, loc, dest, lim, e );                     \
          }                                                               \
          if (T_obj_gen < (old_obj_gen)) has_intergen_ptr=1;              \
       }                                                                  \
  } while( 0 )

/* In general, the generation lookup must be done twice because
   it is possible for an object to be forwarded into the non-predictive
   'old' space during a promotion to both 'old' and 'young'.
   In particular, this happens if one object is forwarded into 'old',
   then a second object, pointing to the first, is forwarded into 'young',
   and then scanned.  The pointer in the second object will then
   change from a pointer into the ephemeral area to a pointer into
   'old'.  The second object must now be added to the non-predictive
   remembered set.

   In the old non-predictive collector, this did not happen, and the logic 
   was less expensive.
   */
#define forw_np_partial( loc, forw_limit_gen, dest, lim, np_young_gen,  \
                         must_add_to_extra, e )                         \
  do { word T_obj = *loc;                                               \
       if ( isptr( T_obj ) ) {                                          \
           if (gen_of(T_obj) < (forw_limit_gen)) {      \
             forw_core_np( T_obj, loc, dest, lim, e );                  \
           }                                                            \
           T_obj = *loc;                                                \
           if (gen_of(T_obj) < (np_young_gen))            \
             must_add_to_extra = 1;                                     \
       }                                                                \
  } while( 0 )

#define forw_core_np( T_obj, loc, dest, lim, e )        \
  word *TMP_P = ptrof( T_obj );                         \
  word TMP_W = *TMP_P;                                  \
  if (TMP_W == FORWARD_HDR)                             \
    *loc = *(TMP_P+1);                                  \
  else if (tagof( T_obj ) == PAIR_TAG) {                \
    check_space_np(dest,lim,8,e);                       \
    *dest = TMP_W;                                      \
    *(dest+1) = *(TMP_P+1);                             \
    check_address( TMP_P );                             \
    *TMP_P = FORWARD_HDR;                               \
    *(TMP_P+1) = *loc = (word)tagptr(dest, PAIR_TAG);   \
    check_memory( dest, 2 );                            \
    dest += 2;                                          \
  }                                                     \
  else {                                                \
    word *TMPD;                                         \
    check_space_np(dest,lim,sizefield(TMP_W)+4,e);      \
    TMPD = dest;                                        \
    *loc = forward( T_obj, &TMPD, e ); dest = TMPD; \
  }

#define check_space_np( dest, lim, wanted, e )                               \
  if ((char*)lim-(char*)dest < (wanted) && (wanted)<=GC_LARGE_OBJECT_LIMIT){ \
    word *CS_LIM=lim, *CS_DEST=dest;                                         \
    expand_semispace_np( &CS_LIM, &CS_DEST, (wanted), e );                   \
    dest = CS_DEST; lim = CS_LIM;                                            \
  }

#define scan_core_partial( ptr, iflush, FORW, must_add_to_extra, e )          \
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
        if (!(T_bytes & 4)) *ptr++ = 0;            /* pad. */                 \
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
        word* T_objp = ptr;                                                   \
        int must_add_to_extra = 0;                                            \
        ptr++;                                                                \
        while (T_words--) {                                                   \
          FORW;                                                               \
          ptr++;                                                              \
        }                                                                     \
        if (must_add_to_extra) remember_vec( tagptr( T_objp, VEC_TAG ), e );  \
        if (!(sizefield( T_w ) & 4)) *ptr++ = 0; /* pad. */                   \
      }                                                                       \
    }                                                                         \
    else {                                                                    \
      int must_add_to_extra = 0;                                              \
      FORW;                                                                   \
      ptr++;                                                                  \
      FORW;                                                                   \
      ptr++;                                                                  \
      if (must_add_to_extra) remember_pair( tagptr( ptr-2, PAIR_TAG ), e );   \
    }                                                                         \
  } while (0)

#define remember_vec( w, e )                                    \
 do {  **e->np.ssbtop = w; *e->np.ssbtop = *e->np.ssbtop+1;     \
       if (*e->np.ssbtop == *e->np.ssblim) {                    \
         gc_compact_np_ssb( e->gc );                            \
       }                                                        \
 } while(0)

#define remember_pair( w, e ) remember_vec( w, e )

#define FORW_NP_ENV_BEGIN( e_, dest_, lim_ )                    \
  word *dest_ = (e_->np.has_switched ? e_->dest2 : e_->dest);   \
  word *lim_ = (e_->np.has_switched ? e_->lim2 : e_->lim);

#define FORW_NP_ENV_END( e_, dest_, lim_ )                              \
  if (e_->np.has_switched) { e_->dest2 = dest_; e_->lim2 = lim_; }      \
  else { e_->dest = dest_; e_->lim = lim_; }


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
  
  semispace_t *tospace;         /* The first tospace */
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


/* External */

extern void mem_icache_flush( void *start, void *end );

/* Private procedures */

static void oldspace_copy( cheney_env_t *e );
static void sweep_large_objects( gc_t *gc, int sweep_oldest, int g1, int g2 );
static void init_env( cheney_env_t *e, gc_t *gc,
                      semispace_t *tospace, semispace_t *tospace2,
                      int  effective_generation,
                      int  attributes,
                      void (*scanner)( cheney_env_t * ) );
static void scan_static_area( cheney_env_t *e );
static void root_scanner_oflo( word *addr, void *data );
static void root_scanner_np( word *ptr, void *data );
static bool remset_scanner_oflo( word obj, void *data, unsigned *count );
static bool remset_scanner_np( word obj, void *data, unsigned *count );
static void scan_oflo_normal( cheney_env_t *e );
static void scan_oflo_splitting( cheney_env_t *e );
static void scan_oflo_np_promote( cheney_env_t *e );
static void expand_semispace( semispace_t *, word **, word **, unsigned );
static void expand_semispace_np( word **, word **, unsigned, cheney_env_t* );
static word forward_large_object( cheney_env_t *e, word *ptr, int tag );
static word forward( word, word **, cheney_env_t *e );

/* Attribute bits to be passed to init_env() */
#define NP_PROMOTION        1
#define SCAN_STATIC         2
#define ENUMERATE_NP_REMSET 4
#define SPLITTING_GC        8

/* Ad-hoc instrumentation */
static gc_event_stats_t cheney;

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

void gclib_stopcopy_promote_into( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;

  CHENEY_TYPE( 0 );
  init_env( &e, gc, tospace, 0, tospace->gen_no, 0, scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no-1, tospace->gen_no, -1 );
  stats_add_gc_event_stats( &cheney );
}

void gclib_stopcopy_collect( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;

  CHENEY_TYPE( 1 );
  init_env( &e, gc, tospace, 0, tospace->gen_no+1, 0, scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no, tospace->gen_no, -1 );
  stats_add_gc_event_stats( &cheney );
}

void gclib_stopcopy_collect_and_scan_static( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;

  CHENEY_TYPE( 1 );
  init_env( &e, gc, tospace, 0, tospace->gen_no+1, SCAN_STATIC, 
            scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no, tospace->gen_no, -1 );
  stats_add_gc_event_stats( &cheney );
}

void gclib_stopcopy_promote_into_np( gc_t *gc,
                                     semispace_t *old, semispace_t *young,
                                     int old_remaining, int young_remaining )
{
  cheney_env_t e;

  CHENEY_TYPE( 0 );
  init_env( &e, gc, old, young, old->gen_no, NP_PROMOTION, 
            scan_oflo_np_promote);
  e.np.old_steps_remaining = old_remaining / GC_CHUNK_SIZE;
  e.np.young_steps_remaining = young_remaining / GC_CHUNK_SIZE;
  gc_np_remset_ptrs( gc, &e.np.ssbtop, &e.np.ssblim );

  oldspace_copy( &e );
  gc_compact_np_ssb( gc );
  sweep_large_objects( gc, old->gen_no-1, old->gen_no, young->gen_no );
  stats_add_gc_event_stats( &cheney );
}

void gclib_stopcopy_collect_np( gc_t *gc, semispace_t *tospace )
{
  cheney_env_t e;

  CHENEY_TYPE( 1 );
  init_env( &e, gc, tospace, 0, tospace->gen_no, ENUMERATE_NP_REMSET, 
            scan_oflo_normal );
  oldspace_copy( &e );
  sweep_large_objects( gc, tospace->gen_no-1, tospace->gen_no, -1 );
  stats_add_gc_event_stats( &cheney );
}

void gclib_stopcopy_split_heap( gc_t *gc, semispace_t *data, semispace_t *text)
{
  cheney_env_t e;

  init_env( &e, gc, data, text, data->gen_no+1, SPLITTING_GC,
            scan_oflo_splitting );
  oldspace_copy( &e );
  /* Note: No LOS sweeping */
}

static void sweep_large_objects( gc_t *gc, 
                                 int sweep_oldest, 
                                 int dest, 
                                 int dest2 )
{
  int i;

  for ( i=0 ; i <= sweep_oldest ; i++ )
    los_sweep( gc->los, i );
  los_append_and_clear_list( gc->los, gc->los->mark1, dest );
  if (dest2 >= 0) los_append_and_clear_list( gc->los, gc->los->mark2, dest2 );
}

static void init_env( cheney_env_t *e, 
                      gc_t *gc,
                      semispace_t *tospace, 
                      semispace_t *tospace2,
                      int effective_generation,
                      int attributes,
                      void (*scanner)( cheney_env_t * ) )
{
  memset( e, 0, sizeof( cheney_env_t ) );
  e->gc = gc;
  e->gclib_desc_g = gclib_desc_g;
  e->effective_generation = effective_generation;
  e->scan_static = attributes & SCAN_STATIC;
  e->np_promotion = attributes & NP_PROMOTION;
  e->enumerate_np_remset = attributes & ENUMERATE_NP_REMSET;
  e->splitting = attributes & SPLITTING_GC;
  e->iflush = gc_iflush( gc );
  e->tospace = tospace;
  e->tospace2 = tospace2;
  e->dest = tospace->chunks[tospace->current].top;
  e->dest2 = (tospace2 ? tospace2->chunks[tospace2->current].top : 0);
  e->lim = tospace->chunks[tospace->current].lim;
  e->lim2 = (tospace2 ? tospace2->chunks[tospace2->current].lim : 0);
  e->los = (e->splitting ? 0 : gc->los);

  if (e->np_promotion)    e->scan_from_globals = root_scanner_np;
  else                    e->scan_from_globals = root_scanner_oflo;

  if (e->np_promotion)    e->scan_from_remsets = remset_scanner_np;
  else                    e->scan_from_remsets = remset_scanner_oflo;

  e->scan_from_tospace = scanner;
}

static void oldspace_copy( cheney_env_t *e )
{
  /* Setup */
  e->scan_idx = e->tospace->current;
  e->scan_idx2 = (e->tospace2 ? e->tospace2->current : 0);
  e->scan_ptr = e->tospace->chunks[e->scan_idx].top;
  e->scan_ptr2 = (e->tospace2 ? e->tospace2->chunks[e->scan_idx2].top : 0);
  e->scan_lim = e->tospace->chunks[e->scan_idx].lim;
  e->scan_lim2 = (e->tospace2 ? e->tospace2->chunks[e->scan_idx2].lim : 0);

  /* Collect */
  start( &cheney.root_scan_prom, &cheney.root_scan_gc );
  gc_enumerate_roots( e->gc, e->scan_from_globals, (void*)e );
  gc_enumerate_remsets_older_than( e->gc,
                                   e->effective_generation-1,
                                   e->scan_from_remsets,
                                   (void*)e,
                                   e->enumerate_np_remset );
  if (e->scan_static && e->gc->static_area)
    scan_static_area( e );
  stop();

  start( &cheney.tospace_scan_prom, &cheney.tospace_scan_gc );
  e->scan_from_tospace( e );
  stop();

  /* Shutdown */
  e->tospace->chunks[e->tospace->current].top = e->dest;
  if (e->tospace2)
    e->tospace2->chunks[e->tospace2->current].top = e->dest2;
}

static void scan_static_area( cheney_env_t *e )
{
  int         forw_limit_gen = e->effective_generation;
  semispace_t *s_data = e->gc->static_area->data_area;
  word        *dest = e->dest;
  word        *lim = e->lim;
  word        *loc, *limit;
  int         i;
#if GCLIB_LARGE_TABLE && SHADOW_TABLE
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif
  
  for ( i=0 ; i <= s_data->current ; i++ ) {
    loc = s_data->chunks[i].bot;
    limit = s_data->chunks[i].top;
    while ( loc < limit )
      scan_core( loc, e->iflush, forw_oflo(loc, forw_limit_gen, dest, lim, e));
  }

  e->dest = dest;
  e->lim = lim;
}

static void root_scanner_oflo( word *ptr, void *data )
{
  cheney_env_t *e = (cheney_env_t*)data;
  forw_oflo( ptr, e->effective_generation, e->dest, e->lim, e );
}

static void root_scanner_np( word *ptr, void *data )
{
  cheney_env_t *e = (cheney_env_t*)data;
  FORW_NP_ENV_BEGIN( e, dest, lim )

  forw_np( ptr, e->effective_generation, dest, lim, e );

  FORW_NP_ENV_END( e, dest, lim )
}

static bool remset_scanner_oflo( word object, void *data, unsigned *count )
{
  cheney_env_t *e = (cheney_env_t*)data;
  unsigned     forw_limit_gen = e->effective_generation;
  unsigned     old_obj_gen = gen_of(object);
  bool         has_intergen_ptr = 0;
  word         *dest = e->dest;
  word         *lim = e->lim;
  word         *loc;            /* Used as a temp by scanner and fwd macros */

  remset_scanner_core( object, loc, 
                       forw_oflo_record( loc, forw_limit_gen, dest, lim,
                                         has_intergen_ptr, old_obj_gen, e ),
                       *count );

  e->dest = dest;
  e->lim = lim;
  return has_intergen_ptr;
}

static bool remset_scanner_np( word object, void *data, unsigned *count )
{
  cheney_env_t *e = (cheney_env_t*)data;
  unsigned     forw_limit_gen = e->effective_generation;
  unsigned     old_obj_gen = gen_of(object);
  bool         has_intergen_ptr = 0;
  word         *loc;            /* Used as a temp by scanner and fwd macros */
  FORW_NP_ENV_BEGIN( e, dest, lim )

  remset_scanner_core( object, loc, 
                       forw_np_record( loc, forw_limit_gen, dest, lim,
                                       has_intergen_ptr, old_obj_gen, e ),
                       *count );

  FORW_NP_ENV_END( e, dest, lim )
  return has_intergen_ptr;
}

static void scan_oflo_normal( cheney_env_t *e )
{
  unsigned gno = e->effective_generation;
  word     *scanptr = e->scan_ptr;
  word     *scanlim = e->scan_lim;
  word     *dest = e->dest;
  word     *copylim = e->lim;
  word     *los_p = 0, *p;
  int      morework;
#if GCLIB_LARGE_TABLE && SHADOW_TABLE
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif

  do {
    morework = 0;

    while (scanptr != dest) {
      while (scanptr != dest && scanptr < scanlim) {
        scan_core( scanptr, e->iflush,
                   forw_oflo( scanptr, gno, dest, copylim, e ));
      }

      if (scanptr != dest) {
        e->scan_idx++;
        scanptr = e->tospace->chunks[e->scan_idx].bot;
        scanlim = e->tospace->chunks[e->scan_idx].lim;
      }
    }

    while ((p = los_walk_list( e->los->mark1, los_p )) != 0) {
      los_p = p;
      morework = 1;
      assert2( ishdr( *p ) );
      scan_core( p, e->iflush, forw_oflo( p, gno, dest, copylim, e ));
    }
  } while (morework);

  e->dest = dest;
  e->lim = copylim;
}

static void scan_np_old( cheney_env_t *e );
static void scan_np_young( cheney_env_t *e );
static void scan_np_los_old( cheney_env_t *e, word **los_p_arg );
static void scan_np_los_young( cheney_env_t *e, word **los_p_arg );

static void scan_oflo_np_promote( cheney_env_t *e )
{
  word *los_p_old = 0, *los_p_young = 0;
  bool work;

  do {
    if (e->scan_ptr == e->scan_lim && e->scan_idx < e->tospace->current) {
      e->scan_idx++;
      e->scan_ptr = e->tospace->chunks[ e->scan_idx ].bot;
      e->scan_lim = e->tospace->chunks[ e->scan_idx ].lim;
    }
    if (e->scan_ptr2 == e->scan_lim2 && e->scan_idx2 < e->tospace2->current) {
      e->scan_idx2++;
      e->scan_ptr2 = e->tospace2->chunks[ e->scan_idx2 ].bot;
      e->scan_lim2 = e->tospace2->chunks[ e->scan_idx2 ].lim;
    }

    work = 0;
    if (e->scan_ptr != e->scan_lim && e->scan_ptr != e->dest) {
      scan_np_old( e );
      work=1;
    }
    if (los_walk_list( e->los->mark1, los_p_old ) != 0) {
      scan_np_los_old( e, &los_p_old );
      work=1;
    }
    if (e->scan_ptr2 != e->scan_lim2 && e->scan_ptr2 != e->dest2) {
      scan_np_young( e );
      work=1;
    }
    if (los_walk_list( e->los->mark2, los_p_young ) != 0) {
      scan_np_los_young( e, &los_p_young );
      work=1;
    }
  } while( work );

  assert( e->scan_idx == e->tospace->current );
  assert( e->scan_idx2 == e->tospace2->current );
}

static void scan_np_old( cheney_env_t *e )
{
  unsigned forw_limit_gen = e->tospace->gen_no;
  word     *scanptr = e->scan_ptr;
  word     *scanlim = e->scan_lim;
  FORW_NP_ENV_BEGIN( e, dest, copylim )
#if GCLIB_LARGE_TABLE && SHADOW_TABLE
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif

  while (scanptr != dest && scanptr < scanlim) {
    scan_core( scanptr, e->iflush,
               forw_np( scanptr, forw_limit_gen, dest, copylim, e ) );
  }

  e->scan_ptr = scanptr;
  e->scan_lim = scanlim;
  FORW_NP_ENV_END( e, dest, copylim )
}

static void scan_np_young( cheney_env_t *e )
{
  unsigned forw_limit_gen = e->tospace->gen_no; /* [sic] */
  unsigned np_young_gen = e->tospace2->gen_no;
  word     *scanptr = e->scan_ptr2;
  word     *scanlim = e->scan_lim2;
  FORW_NP_ENV_BEGIN( e, dest, copylim )
#if GCLIB_LARGE_TABLE && SHADOW
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif

  /* must_add_to_extra is a name used by the scanning and fwd macros as a 
     temp */
  while (scanptr != dest && scanptr < scanlim) {
    scan_core_partial( scanptr, e->iflush,
                       forw_np_partial( scanptr, forw_limit_gen, dest, copylim,
                                        np_young_gen, must_add_to_extra, e ),
                       must_add_to_extra, e );
  }

  e->scan_ptr2 = scanptr;
  e->scan_lim2 = scanlim;
  FORW_NP_ENV_END( e, dest, copylim )
}

static void scan_np_los_old( cheney_env_t *e, word **los_p )
{
  unsigned forw_limit_gen = e->tospace->gen_no;
  FORW_NP_ENV_BEGIN( e, dest, copylim )
  word     *p;
#if GCLIB_LARGE_TABLE && SHADOW
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif

  while ((p = los_walk_list( e->los->mark1, *los_p )) != 0) {
    *los_p = p;
    assert2( ishdr( *p ) );
    scan_core( p, e->iflush,
               forw_np( p, forw_limit_gen, dest, copylim, e ));
  }

  FORW_NP_ENV_END( e, dest, copylim )
}

static void scan_np_los_young( cheney_env_t *e, word **los_p )
{
  unsigned forw_limit_gen = e->tospace->gen_no;      /* [sic] */
  unsigned np_young_gen = e->tospace2->gen_no;
  FORW_NP_ENV_BEGIN( e, dest, copylim )
  word     *p;
#if GCLIB_LARGE_TABLE && SHADOW
  gclib_desc_t *gclib_desc_g = e->gclib_desc_g;
#endif

  /* must_add_to_extra is a name used by the scanning and fwd macros as a 
     temp */
  while ((p = los_walk_list( e->los->mark2, *los_p )) != 0) {
    *los_p = p;
    assert2( ishdr( *p ) );
    scan_core_partial( p, e->iflush,
                       forw_np_partial( p, forw_limit_gen, dest, copylim,
                                        np_young_gen, must_add_extra, e ),
                       must_add_extra, e );
  }

  FORW_NP_ENV_END( e, dest, copylim )
}

static void scan_oflo_splitting( cheney_env_t *e )
{
  unsigned forw_limit_gen = e->effective_generation;
  word     *scanptr = e->scan_ptr;
  word     *scanlim = e->scan_lim;
  word     *dest = e->dest;
  word     *dest2 = e->dest2;
  word     *copylim = e->lim;
  word     *copylim2 = e->lim2;

  while (scanptr != dest) {
    while (scanptr != dest && scanptr < scanlim) {
      scan_core( scanptr, e->iflush,
                 forw_oflo2( scanptr, forw_limit_gen, dest, dest2,
                             copylim, copylim2, e ));
    }

    if (scanptr != dest) {
      e->scan_idx++;
      scanptr = e->tospace->chunks[e->scan_idx].bot;
      scanlim = e->tospace->chunks[e->scan_idx].lim;
    }
  }

  e->dest = dest;
  e->dest2 = dest2;
  e->lim = copylim;
  e->lim2 = copylim2;
}

/* "p" is a tagged pointer into oldspace;
 * "*dest" is a pointer into newspace, the destination of the next object.
 *
 * Forward() returns the forwarding value of "ptr"; it does this by
 * copying the object and returning the new address.
 *
 * Most objects are smallish, so this code should be biased in favor
 * of small objects.
 */
static word forward( word p, word **dest, cheney_env_t *e )
{
  word hdr, newptr, *p1, *p2, tag, *ptr;

  tag = tagof( p ); 
  ptr = ptrof( p );

  /* Copy the structure into newspace and pad if necessary. */
  p1 = *dest;
  newptr = (word)p1;    /* really *dest, but the compiler is dumb. */
  p2 = ptr;

  hdr = *ptr;
  assert2( ishdr( hdr ) );

#if FORW_BY_LOOP
  { unsigned words;

    /* gcc gets this right, so no sense in being obscure.
       words = (((hdr >> 8) + 11) >> 3) << 1; */
    words = roundup8( sizefield( hdr ) + 4 ) / 4;

#if CHECK_EVERY_WORD
    switch (tag) {
    case VEC_TAG : case PROC_TAG :
      gclib_check_memory_validity( p2, (sizefield( hdr ) + 4)/4 );
    }
#endif
    /* 32 is pretty arbitrary; chosen to match overhead of memcpy(). */
    /* gcc doesn't schedule real well. */
    if (words < 32) {
      while (words > 0) {
        p1[0] = p2[0];
        p1 += 2;
        p1[-1] = p2[1];
        p2 += 2;
        words -= 2;
      }
    }
    else if (words > GC_LARGE_OBJECT_LIMIT/4 && e->los) 
      return forward_large_object( e, ptr, tag );
    else {
      memcpy( p1, p2, words*4 );
      p1 += words;
    }
    *dest = p1;
  }
#endif

#if FORW_BY_DUFF
  { unsigned bytes;

  bytes = roundup8( sizefield( hdr ) + 4 );
  if (bytes > GC_LARGE_OBJECT_LIMIT && los) 
    return forward_large_object( e, ptr, tag );

  switch (bytes >> 3) {
    case 8  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 7  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 6  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 5  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 4  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 3  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 2  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 1  : *p1++ = *p2++;
              *p1++ = *p2++;
    case 0  : break;
    default : memcpy( p1, p2, bytes );  p1 += (bytes >> 2);
  }
  *dest = p1;
  }
#endif

#if FORW_BY_MEMCPY
  { unsigned bytes;
  /* This appears to be slowest */
  bytes = roundup8( sizefield( hdr ) + 4 );
  if (bytes > GC_LARGE_OBJECT_LIMIT && los)
    return forward_large_object( e, ptr, tag );

  memcpy( p1, p2, bytes );
  *dest = p1 + (bytes >> 2);
  }
#endif

  newptr = (word) tagptr( newptr, tag );

  /* leave forwarding pointer */
  check_address( ptr );
  *ptr = FORWARD_HDR;
  *(ptr+1) = newptr;

  return newptr;
}


/* Expand the semispace by switching to a new chunk.  To make things
   simple for the scanning code, a string header is put into the
   heap at the current location (unless we're at the end of the chunk)
   with a length field that covers the rest of the chunk.  The scanning
   code will simply skip the data.  The string's first four bytes
   are initialized with a recognizable but unlikely pattern, in the event
   any code needs to grovel over the heap and count padding.

   A string header is used rather than a generic bytevector header since
   the latter value would cause the scanner to flush the icache for the
   garbage area.
   */
static void seal_chunk( semispace_t *ss, word *lim, word *dest )
{
  if (dest < lim) {
    word len = (lim - dest)*sizeof(word);
    *dest = mkheader(len-sizeof(word),STR_HDR);
    *(dest+1) = 0xABCDABCD;
  }
  ss->chunks[ ss->current ].top = dest;
}

static void
expand_semispace( semispace_t *ss, word **lim, word **dest, unsigned bytes )
{
  seal_chunk( ss, *lim, *dest );
  ss_expand( ss, max( bytes, GC_CHUNK_SIZE ) );
  *lim = ss->chunks[ss->current].lim;
  *dest = ss->chunks[ss->current].top;
}

static void
expand_semispace_np( word **lim, word **dest, unsigned bytes, cheney_env_t *e )
{
  semispace_t *ss;

 again:
  if (e->np.has_switched) {
    ss = e->tospace2;
    e->np.young_steps_remaining--;
  }
  else if (e->np.old_steps_remaining - e->np.old_los_steps > 0) {
    ss = e->tospace;
    e->np.old_steps_remaining--;
  }
  else {
    e->np.has_switched = 1;
    e->dest = *dest;
    e->lim = *lim;
    seal_chunk( e->tospace, *lim, *dest );
    ss = e->tospace2;
    *dest = ss->chunks[ ss->current ].top; /* [sic] */
    *lim = ss->chunks[ ss->current ].lim;
    if ((char*)*lim - (char*)*dest < bytes) goto again; else return;
  }
  seal_chunk( ss, *lim, *dest );
  ss_expand( ss, max( bytes, GC_CHUNK_SIZE ) );
  *dest = ss->chunks[ ss->current ].bot;
  *lim = ss->chunks[ ss->current ].lim;
}

/* FIXME: Note a problem with the following code.  When a large object is
   forwarded, its generation bits are not changed until after scanning
   is over.  That means that any code that checks the generation bits
   of a large object, eg like a GC write barrier might during scanning,
   will get the wrong generation: the number will be too low.
   That can be fixed here by using los_mark_and_set_generation()
   rather than just los_mark(), and allocating the object with the
   correct generation instead of that of the existing generation.
   I don't want to fix that now.  See similar, but fixed, code in
   the DOF collector.

   Note that the code that is sensitive to it, forw_np_partial,
   uses the test in such a way that the bug does not trigger incorrect
   behavior, though a large object might be added to the remembered
   set when it should not have been.
*/
static word forward_large_object( cheney_env_t *e, word *ptr, int tag )
{
  los_t *los = e->los;
  los_list_t *mark_list;
  word hdr, ret;
  int bytes, was_marked, sub1 = 0, sub2 = 0;

  hdr = *ptr;
  bytes = roundup8( sizefield( hdr ) + 4 );

#if CHECK_EVERY_WORD
    switch (tag) {
    case VEC_TAG : case PROC_TAG :
      gclib_check_memory_validity( ptr, (sizefield(hdr)+4)/4 );
      break;
    }
#endif
  mark_list = los->mark1;
  sub1 = bytes;
  if (e->np_promotion) {
    int free1, free2;

    free1 = e->np.old_steps_remaining*GC_CHUNK_SIZE - 
      los_bytes_used( los, LOS_MARK1 );
    free2 = e->np.young_steps_remaining*GC_CHUNK_SIZE - 
      los_bytes_used(los, LOS_MARK2);

    if (bytes > free1 && bytes <= free2) {
      mark_list = los->mark2;
      sub2 = sub1; sub1 = 0;
    }
  }

  if (attr_of(ptr) & MB_LARGE_OBJECT) {
    was_marked = los_mark( los, mark_list, ptr, gen_of(ptr) );
    ret = tagptr( ptr, tag );
  }
  else {
    /* The large object was not allocated specially, so we must move it. */
    word *new;

    hdr = *ptr;
    bytes = roundup8( sizefield( hdr ) + 4 );
    new = los_allocate( los, bytes, gen_of( ptr ) );
    memcpy( new, ptr, bytes );
    
    /* Must mark it also! */
    was_marked = los_mark( los, mark_list, new, gen_of( ptr ));
    
    /* Leave a forwarding pointer */
    check_address( ptr );
    *ptr = FORWARD_HDR;
    *(ptr+1) = tagptr( new, tag );
    ret = *(ptr+1);
  }

  if (e->np_promotion && !was_marked) {
    e->np.old_los_bytes += sub1;
    e->np.young_los_bytes += sub2;
    e->np.old_los_steps = ceildiv( e->np.old_los_bytes, GC_CHUNK_SIZE );
    e->np.young_los_steps = ceildiv( e->np.young_los_bytes, GC_CHUNK_SIZE );
  }
  return ret;
}

void gclib_check_memory_validity( word *p, int n )
{
  int i;

  for ( i=0 ; i < n ; i++ ) {
    word x = p[i], y;

    if (ishdr( x )) {
      if (i != 0) {
        hardconsolemsg( "Header 0x%08x found at offset %d in object 0x%08x!",
                        x, i, (word)p );
        conditional_abort();
      }
      else if (sizefield( x ) > 4*1024*1024) {
        /* Bigger than the big array in gcbench! */
        hardconsolemsg( "Implausible but valid size %u in header "
                        "0x%08x in  object 0x%08x.", sizefield(x), x, (word)p);
      }
    }
    else {
      switch (tagof( x )) {
      case 0 : case 4 :         /* fixnum */
        break;
      case 6 :                  /* immediate */
        if ((x & 0xFF) == IMM_CHAR
            || x == TRUE_CONST || x == FALSE_CONST || x == NIL_CONST 
            || x == UNSPECIFIED_CONST || x == UNDEFINED_CONST
            || x == EOF_CONST)
          ;
        else {
          hardconsolemsg( "Invalid immediate 0x%08x found at offset %d"
                         " in object 0x%08x!", x, i, (word)p );
          conditional_abort();
        }
        break;
      case 1 :                  /* pair */
        y = *ptrof( x );
        if (y != FORWARD_HDR && ishdr( y )) {
          hardconsolemsg( "Pair pointer 0x%08x at offset %d in object 0x%08x"
                          " points to a header (0x%08x)!", x, i, (word)p, y );
          conditional_abort();
        }
        break;
      case 3 :                  /* vector */
        y = *ptrof( x );
        if (y != FORWARD_HDR && (!ishdr( y ) || header( y ) != VEC_HDR)) {
          hardconsolemsg( "Vector pointer 0x%08x at offset %d in object 0x%08x"
                          " does not point to a vector header (0x%08x)!", 
                          x, i, (word)p, y );
          conditional_abort();
        }
        break;
      case 5 :                  /* bytevector */
        y = *ptrof( x );
        if (y != FORWARD_HDR && (!ishdr( y ) || header( y ) != BV_HDR)) {
          hardconsolemsg( "Bytevector pointer 0x%08x at offset %d in object "
                          "0x%08x does not point to a bytevector header "
                          "(0x%08x)!",
                          x, i, (word)p, y );
          conditional_abort();
        }
        break;
      case 7 :                  /* procedure */
        y = *ptrof( x );
        if (y != FORWARD_HDR && 
            (!ishdr( y ) || header(y) != header(PROC_HDR))) {
          hardconsolemsg( "Procedure pointer 0x%08x at offset %d in object "
                          "0x%08x does not point to a procedure header "
                          "(0x%08x)!", 
                          x, i, (word)p, y );
          conditional_abort();
        }
        break;
      }
    }
  }
}

/* Obj must be an object pointer */
void gclib_check_object( word obj )
{
  word firstword;

  switch (tagof(obj)) {
  case PAIR_TAG :
    gclib_check_memory_validity( ptrof( obj ), 2 );
    break;
  case VEC_TAG :
    firstword = *ptrof( obj );
    if (!ishdr( firstword ) || header( firstword ) != VEC_HDR) {
      hardconsolemsg( "gclib_check_object: Inconsistent header: 0x%08x 0x%08x",
                      obj, firstword );
      conditional_abort();
    }
    gclib_check_memory_validity( ptrof( obj ), (sizefield( firstword ) + 4)/4 );
    break;
  case PROC_TAG :
    firstword = *ptrof( obj );
    /* Procedure headers are weird (known bug, bit me big-time here) */
    if (!ishdr( firstword ) || header( firstword ) != header(PROC_HDR)) {
      hardconsolemsg( "gclib_check_object: Inconsistent header: 0x%08x 0x%08x",
                      obj, firstword );
      conditional_abort();
    }
    gclib_check_memory_validity( ptrof( obj ), (sizefield( firstword ) + 4)/4 );
    break;
  case BVEC_TAG :
    firstword = *ptrof( obj );
    if (!ishdr( firstword ) || header( firstword ) != BV_HDR) {
      hardconsolemsg( "gclib_check_object: Inconsistent header: 0x%08x 0x%08x",
                      obj, firstword );
      conditional_abort();
    }
    break;
  default :
    hardconsolemsg( "gclib_check_object: Not a pointer as expected: 0x%08x",
                    obj );
    conditional_abort();
    }
}

/* eof */
