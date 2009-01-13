#define GC_INTERNAL

#define dbmsg( format, args... ) if (0) consolemsg( format, ## args )

#define bounded_decrement( x, y ) if (x > 0) x = max( 0, x - (y) );

#include "larceny.h"
#include "gc_t.h"
#include "gclib.h"
#include "smircy.h"
#include "smircy_checking.h"
#include "msgc-core.h"
#include "los_t.h"

#include "smircy_internal.h"

bool smircy_assert_conservative_approximation( smircy_context_t *context );
static void check_rgn_stkp_all_present( smircy_context_t *context );
static void check_stack_invs( smircy_context_t *context );
static void check_gnos_match( smircy_context_t *context );
static void *smircy_enumerate_stack_of_rgn2( smircy_context_t *context, 
                                             int rgn, 
                                             void* (*visit)(word *pw, int *pgno,
                                                            void *data),
                                             void *orig_data );
static void *visit_check_gnos_match( word w, int gno, void *data );
static void *visit_check_gno_matches( word *pw, int *pgno, void *data );
static bool is_stkp_already_present( smircy_context_t *context, 
                                     obj_stack_entry_t *the_stkp);
static void check_gnos_match( smircy_context_t *context );
static void *smircy_enumerate_whole_stack2( smircy_context_t *context,
                                            void *visit(word w, int gno, void *data),
                                            void *orig_data );

void smircy_check_internal_invs( smircy_context_t *context ) {
  check_stack_invs( context );
}


static void *visit_check_gnos_match( word w, int gno, void *data )
{
  if (w != 0x0) {
    assert2( isptr(w) );
    if (gen_of(w) != gno) {
      consolemsg("visit_check_gnos_match( 0x%08x (%d), %d, data )", 
                 w, gen_of(w), gno );
    }
    assert2( gen_of(w) == gno );
  }
  return data;
}

static void check_stack_invs( smircy_context_t *context ) {
  check_rgn_stkp_all_present( context );
  check_gnos_match( context );
}

static void check_rgn_stkp_all_present( smircy_context_t *context )
{
  int rgn;
  obj_stack_entry_t *obj_entry;
  for (rgn = 1; rgn < context->num_rgns; rgn++) {
    obj_entry = context->rgn_to_obj_entry[ rgn ];
    while (obj_entry != NULL ) {
      if (! is_stkp_already_present( context, obj_entry )) {
        consolemsg( "check_rgn_stkp_all_present: "
                    "rgn %d obj_entry: 0x%08x not present",
                    rgn, obj_entry );
      }
      assert( is_stkp_already_present( context, obj_entry ));
      obj_entry = obj_entry->next_in_rgn;
    }
  }
}

static void check_stkp_not_already_present_in_rgn_stks( smircy_context_t *context, 
                                            obj_stack_entry_t *stkp) 
{
  int rgn;
  obj_stack_entry_t *obj_entry;
  for (rgn = 1; rgn < context->num_rgns; rgn++) {
    obj_entry = context->rgn_to_obj_entry[ rgn ];
    while (obj_entry != NULL ) {
      if (! ( obj_entry != stkp ) ) {
        consolemsg( "check_stkp_not_already_present_in_rgn_stks mrrh "
                    "stkp: 0x%08x rgn: %d", stkp, rgn );
      }
      assert( obj_entry != stkp );
      obj_entry = obj_entry->next_in_rgn;
    }
  }
}

static bool is_stkp_already_present( smircy_context_t *context, 
                                     obj_stack_entry_t *the_stkp) 
{
  obj_stack_t *obj_stack;
  obj_stackseg_t *seg;
  obj_stack_entry_t *stkp;
  obj_stack_entry_t *stkbot;
  obj_stack = &context->stack.obj;
  seg = obj_stack->seg;
  stkp = obj_stack->stkp;
  stkbot = obj_stack->stkbot;
  while (stkp > stkbot) {
    if (stkbot <= the_stkp && the_stkp < stkp)
      return TRUE;
    stkp = stkbot;
    if (seg->next != NULL) {
      seg = seg->next;
      stkp = seg->data+OBJ_STACK_SIZE;
      stkbot = seg->data;
    }
  }
  return FALSE;
}

static void check_gnos_match( smircy_context_t *context ) {
  int rgn;
  smircy_enumerate_whole_stack2( context, visit_check_gnos_match, NULL );
  for (rgn = 1; rgn < context->num_rgns; rgn++) {
    smircy_enumerate_stack_of_rgn2( context, rgn, visit_check_gno_matches, &rgn );
  }
}

static void *smircy_enumerate_stack_of_rgn2( smircy_context_t *context, 
                                             int rgn, 
                                             void* (*visit)(word *pw, int *pgno,
                                                            void *data),
                                             void *orig_data )
{
  obj_stack_entry_t     *obj_entry;
  obj_stack_entry_t     *obj_entry_next;
  large_object_cursor_t *los_entry;
  word old_word, new_word;
  obj_entry = context->rgn_to_obj_entry[ rgn ];
  los_entry = context->rgn_to_los_entry[ rgn ];
  while (obj_entry != NULL) {
    if (!  ( obj_entry->gno ==  -1 || obj_entry->gno == rgn )) {
      consolemsg("smircy_enumerate_stack_of_rgn "
                 "rgn: %d obj_entry gno: %d val: 0x%08x (%d)",
                 rgn, obj_entry->gno, obj_entry->val, 
                 isptr(obj_entry->val)?gen_of(obj_entry->val):-1);
    }
    assert2( obj_entry->gno ==  -1 || obj_entry->gno == rgn );
    assert2( obj_entry->val == 0x0 || isptr( obj_entry->val ));

    if (!  ( !isptr(obj_entry->val) || gen_of(obj_entry->val) == rgn )) {
      consolemsg("smircy_enumerate_stack_of_rgn "
                 "rgn: %d obj_entry gno: %d val: 0x%08x (%d)",
                 rgn, obj_entry->gno, obj_entry->val, 
                 isptr(obj_entry->val)?gen_of(obj_entry->val):-1);
    }
    /* FSK: not sure I believe this assertion */
    assert2( !isptr(obj_entry->val) || gen_of(obj_entry->val) == rgn );

    if (obj_entry->val != 0x0) {
      visit( &obj_entry->val, &obj_entry->gno, orig_data );
      new_word = obj_entry->val;

#if 1
      assert( gen_of(new_word) == rgn );
#else
      if (gen_of(new_word) != rgn) { /* moved to different region; cleanup. */
        obj_entry->val = 0x0;
        obj_entry->gno = -1;
        forcibly_push( context, new_word, 0x0 );
        /* XXX should actually shift prev ptr downward; but I can
         * avoid that for this first draft, I think ... */
      } else {

        /* XXX in the future, I'll need to deal with this case too; once
         * I start shifting the prev ptr downward, I'll need to update
         * the prev ptr here since I'm keeping this new new_word
         * entry... */

        /* (I could also just try assert(0) here, since I do not expect
         * to see this case to happen in the client code I expect to
         * actually end up using this.  Then again, this is meant to be
         * a generic function for debugging and instrumentation as well,
         * so I am not sure an assert(0) on this path is warranted... */
        /* OOPS the assert(0) I tried failed quite quickly, so don't be
         * so quick to assume that that case does not arise... */
      }
#endif
    }
    obj_entry = obj_entry->next_in_rgn;
  }

  while (los_entry != NULL) {
    assert2( los_entry->object == 0x0 || isptr( los_entry->object ));
    assert2( gen_of(los_entry->object) == rgn );
    visit( &los_entry->object, NULL, orig_data );
    new_word = los_entry->object;

#if 1
    assert( gen_of(new_word) == rgn );
#else
    if (gen_of(new_word) != rgn) { /* moved to different region; cleanup. */
      los_entry->object = 0x0;
      los_push( context, los_entry->index, new_word );
      /* XXX see above notes for standard object stack.
       * Same probably applies here. */
    } else {
      /* XXX see above notes for standard object stack.
       * Same probably applies here. */
    }
#endif
    los_entry = los_entry->next_in_rgn;
  }
}

static void *smircy_enumerate_whole_stack( smircy_context_t *context,
                                           void *visit(word w, void *data),
                                           void *orig_data )
{
  obj_stack_t *obj_stack;
  los_stack_t *los_stack;
  obj_stackseg_t *seg;
  obj_stack_entry_t *stkp;
  obj_stack_entry_t *stkbot;
  word w;
  void *data = orig_data;

  obj_stack = &context->stack.obj;
  los_stack = &context->stack.los;
  seg = obj_stack->seg;
  stkp = obj_stack->stkp;
  stkbot = obj_stack->stkbot;
  while (stkp > stkbot) {
    stkp--;
    w = stkp->val;
    data = visit(w, data);
    if (stkp == stkbot && seg->next != NULL) {
      seg = seg->next;
      stkp = seg->data+OBJ_STACK_SIZE;
      stkbot = seg->data;
    }
  }
  return data;
}

static void *smircy_enumerate_whole_stack2( smircy_context_t *context,
                                            void *visit(word w, int gno, void *data),
                                            void *orig_data )
{
  obj_stack_t *obj_stack;
  los_stack_t *los_stack;
  obj_stackseg_t *seg;
  obj_stack_entry_t *stkp;
  obj_stack_entry_t *stkbot;
  word w;
  void *data = orig_data;

  obj_stack = &context->stack.obj;
  los_stack = &context->stack.los;
  seg = obj_stack->seg;
  stkp = obj_stack->stkp;
  stkbot = obj_stack->stkbot;
  while (stkp > stkbot) {
    stkp--;
    w = stkp->val;
    data = visit(w, stkp->gno, data);
    if (stkp == stkbot && seg->next != NULL) {
      seg = seg->next;
      stkp = seg->data+OBJ_STACK_SIZE;
      stkbot = seg->data;
    }
  }
  return data;
}

static void *visit_check_gno_matches( word *pw, int *pgno, void *data )
{
  int rgn = *(int*)data;
  word w = *pw;
  if (w != 0x0) {
    assert2( isptr( w ));
    assert2( pgno == 0x0 || *pgno == gen_of(w) );
    assert2( gen_of(w) == rgn );
  }
}

static void smircy_clone_bitmap( msgc_context_t *context_new, 
                                 smircy_context_t *context_old ) 
{
  word *low, *hgh, *ptr, tptr;

  low = context_old->lowest_heap_address;
  hgh = context_old->highest_heap_address;
  for ( ptr = low; ptr < hgh; ptr += 2 ) {
    tptr = ((word)ptr) | PAIR_TAG; /* artificial pair tag */
    if (smircy_object_marked_p( context_old, tptr )) {
      msgc_mark_object( context_new, tptr );
      // consolemsg(  "smircy_clone_bitmap mark: 0x%08x (%d)", tptr, gen_of(tptr));
    }
  }
}

static void smircy_unmark_and_push_stack( msgc_context_t *context_new, 
                                          smircy_context_t *context_old,
                                          bool loud )
{
  obj_stack_t *obj_stack;
  los_stack_t *los_stack;
  obj_stackseg_t *seg;
  obj_stack_entry_t *stkp;
  obj_stack_entry_t *stkbot;
  word w;

  obj_stack = &context_old->stack.obj;
  los_stack = &context_old->stack.los;
  seg = obj_stack->seg;
  stkp = obj_stack->stkp;
  stkbot = obj_stack->stkbot;
  while (stkp > stkbot) {
    stkp--;
    w = stkp->val;
    if (w != 0x0) {
      if (loud)
        consolemsg( "smircy_push_stack unmark and push: "
                    "0x%08x (%d)", w, gen_of(w));
      msgc_unmark_object( context_new, w );
      msgc_push_object( context_new, w );
    }
    if (stkp == stkbot && seg->next != NULL) {
      seg = seg->next;
      stkp = seg->data+OBJ_STACK_SIZE;
      stkbot = seg->data;
    }
  }
  
  { 
    los_stackseg_t *seg = los_stack->seg;
    large_object_cursor_t *stkp   = los_stack->stkp;
    large_object_cursor_t *stkbot = los_stack->stkbot;
    large_object_cursor_t *stklim = los_stack->stklim;
    word obj;
    int window_start, objwords, size, lim, i;
    while (seg != NULL) {
      assert( stkp >= stkbot );
      if (stkp == stkbot) {
        seg = seg->next;
        if (seg == NULL) {
          stkbot = 0;
          stklim = 0;
          stkp = 0;
          break; /* done traversing los stack */
        } else {
          stkbot = seg->data;
          stklim = seg->data+LOS_STACK_SIZE;
          stkp = stklim;
        }
      }
      assert( stkp > stkbot );
      assert( seg != NULL );
      stkp--;
      obj = stkp->object;
      window_start = stkp->index;
      objwords = bytes2words( sizefield( *ptrof(obj) ));
      size = objwords - window_start;
      lim = window_start + size;
      for ( i = window_start; i < lim ; i++ ) {
        w = vector_ref( obj, i );
        /* msgc_unmark_object( context_new, w ); */ /* los stk elems not marked on push */
        msgc_push_object( context_new, w );
      }
    }
  }
}

static void smircy_complete( smircy_context_t *context ) 
{
  int marked_ign, traced_ign, words_marked_ign;
  marked_ign = 0;
  traced_ign = 0;
  words_marked_ign = 0;
#if 1
  smircy_progress(      context, -1, -1, -1, 
                        &marked_ign, &traced_ign, &words_marked_ign );
#else
  smircy_progress_core( context, -1, -1, -1, 
                        &marked_ign, &traced_ign, &words_marked_ign,
                        FALSE /* not loud */ );
#endif
}

static void *visit_make_clone_mark( word obj, word src, void *data ) 
{
  if (isptr(obj))
    consolemsg("visit_make_clone_mark( "
               "obj: 0x%08x (%d), src: 0x%08x (%d), data )",
               obj, gen_of(obj), src, isptr(src)?gen_of(src):-1 );
  return data;
}

msgc_context_t *smircy_clone_begin( smircy_context_t *context_old, 
                                    bool loud ) 
{
  int num_rgns = context_old->num_rgns;
  int words_in_bitmap;
  msgc_context_t *const context_new = 
    msgc_begin_range( context_old->gc, 
                      (caddr_t)context_old->lowest_heap_address, 
                      (caddr_t)context_old->highest_heap_address );
  consolemsg("smircy_clone_begin");
  if (loud)
    msgc_set_object_visitor( context_new, visit_make_clone_mark, NULL );
  smircy_clone_bitmap( context_new, context_old );
  smircy_unmark_and_push_stack( context_new, context_old, loud );
  return context_new;
}

void smircy_clone_end( msgc_context_t *c ) {
  consolemsg("smircy_clone_end");
  msgc_end( c );
}

static bool smircy_is_marked_via_completion( smircy_context_t *context, word obj ) 
{
  msgc_context_t *clone;
  bool rtn;
  clone = smircy_clone_begin( context, FALSE );
  msgc_mark_objects_from_nil( clone );
  rtn = msgc_object_marked_p( clone, obj );
  smircy_clone_end( clone );
  return rtn;
}

void *visit_check_clone_mark( word obj, word src, void *data ) 
{
  msgc_context_t *clone = (msgc_context_t*)data;

  bool obj_beyond, obj_marked;

  if ( ! isptr( obj ))
    return data;

  obj_beyond = ! msgc_object_in_domain( clone, obj );
  obj_marked = obj_beyond || msgc_object_marked_p( clone, obj );

  if ( isptr(obj) ) {
    if (! ( gen_of(obj) == 0 || obj_marked )) {
      if (! isptr(src)) {
        consolemsg("visit_check_clone_mark( "
                   "obj: 0x%08x (%d), "
                   "src: 0x%08x (%d), data ) smircy: obj %s", 
                   obj, gen_of(obj), src, -1, 
                   obj_beyond?"B":obj_marked?"Y":"N" );
      } else {
        bool src_beyond = ! msgc_object_in_domain( clone, src );
        bool src_marked = src_beyond || msgc_object_marked_p( clone, src );
        consolemsg("visit_check_clone_mark( "
                   "obj: 0x%08x (%d), "
                   "src: 0x%08x (%d), data ) smircy: obj %s src %s",
                   obj, gen_of(obj), src, gen_of(src), 
                   obj_beyond?"B":obj_marked?"Y":"N", 
                   src_beyond?"B":src_marked?"Y":"N" );
      }
    }
    assert( gen_of(obj) == 0 || obj_marked );
  }
  return data;
}

bool smircy_assert_conservative_approximation( smircy_context_t *context ) 
{
  /* For debugging.  Checks that context's bitmap+stack is a
   * conservative approximation of the true mark bitmap (that is, a
   * superset of objects marked by MSGC should be treated as live by
   * SMIRCY.
   * 
   * XXX "treated as live" usually means marked, but Felix anticipates
   * needing special treatment for blocks of memory that lie outside
   * the bitmap... that can wait until things are running more
   * smoothly though...
   */

  /* saving original so that we can distinquish clone from it. */
  smircy_context_t * const context_orig = context;
  msgc_context_t * clone;

  /* If the mark stack is empty, then we could just compare the SMIRCY
   * context to the MSGC context directly; but if the mark stack is
   * non-empty, then we must first simulate the completion of the
   * marking on the SMIRCY context.  It is simplest to just make a
   * copy (clone) of the SMIRCY context as an MSGC context and
   * completing that, using the mark stack as the roots for the heap
   * traversal that guides the completion.
   */
  consolemsg("smircy_assert_conservative_approximation( context ) "
             "cloning context");
  { /* out with the old, in with the new */
    clone = smircy_clone_begin( context, FALSE );
    msgc_mark_objects_from_nil( clone );
  }

  {
    msgc_context_t *msgc_ctxt; 
    word *low, *hgh, *ptr, tptr;
    int marked, traced, words_marked;

    caddr_t msg_low, msg_hgh;
    gclib_memory_range( &msg_low, &msg_hgh );

    msgc_ctxt = msgc_begin( context->gc );
    msgc_set_object_visitor( msgc_ctxt, visit_check_clone_mark, clone );
    msgc_mark_objects_from_roots( msgc_ctxt, &marked, &traced, &words_marked );
    low = min( (word*)msg_low, context->lowest_heap_address );
    hgh = max( (word*)msg_hgh, context->highest_heap_address );
    for ( ptr = low; ptr < hgh; ptr += 2 ) {
      tptr = ((word)ptr) | PAIR_TAG; /* artificial pair tag */
      if ((caddr_t)ptr < msg_low) {  /* assume unreachable in current heap */
        assert( ! msgc_object_marked_p( clone, tptr ));
      } else if ((caddr_t)ptr >= msg_hgh) { /* assume unreachable in current heap */
        assert( ! msgc_object_marked_p( clone, tptr ));
      } else if (msgc_object_marked_p( msgc_ctxt, tptr )) {
        switch (header(*ptr)) {
        case VEC_HDR: 
          tptr = ((word)ptr) | VEC_TAG; break;
        case BV_HDR:
          tptr = ((word)ptr) | BVEC_TAG; break;
        case PROC_HDR:
          tptr = ((word)ptr) | PROC_TAG; break;
        default:
          tptr = ((word)ptr) | PAIR_TAG; break;
        }
        assert(msgc_object_marked_p( msgc_ctxt, tptr ));
        if (gen_of(tptr) != 0 && ! msgc_object_marked_p( clone, tptr )) {
          dbmsg( "SMIRCY: 0x%08x (%d) msgc says reachable smircy did not mark "
                 "[low: 0x%08x, hgh: 0x%08x]",
                 tptr, gen_of(tptr), low, hgh );
        }
        assert( (gen_of(tptr) == 0) || msgc_object_marked_p( clone, tptr ));
      }
    }
    msgc_end( msgc_ctxt );
  }

  smircy_clone_end( clone );
}
