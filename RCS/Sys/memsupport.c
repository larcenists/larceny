/*
 * Helper procedures for memory.s.
 *
 * $Id$
 */

/*
 * "gc()" is the entry point for the allocation procedures to the
 * garbage collector.
 *
 * Assume, for the sake of argument, that R1 thru R8 are rootable.
 * CREG is the continuation register; flush_stack() flushes the stack
 * cache out to the heap.
 */
void gcstart2( void )
{
  flush_stack_cache();
  collect();
}

  
/* Conceptual (yeah, yeah...) flush_stack_cache() */

/* the topmost continuaton must be created last (conceptually) since it links
   to the previous onw which links to the previous one which ... We can get 
   away with a one-element "cache" though that keeps the previous 
   continuation we made so that this continuation can be inserted into it.

   The last to be created (i.e. the bottommost in the stack cache) gets a
   link which is the link out to the previous one; how does this work in 
   practice (i.e. where is that link stored? as a regular dynamic link?)
 */

flush_stack_cache()
{
  word *p = SP;
  word *prev = NULL;
  word retaddr;
  word size;
  word proc;
  word *top = CREG;

  while (p < stack_cache_limit) {
    retaddr = *p++;
    size = *p++;
    proc = *p++;

    /* OOPS! we may not be able to do this unless we can guarantee room for
       the stack cache structures in e_mem!!!!!! Either way we should not
       really call alloc here. */
       
    q = alloc( size );   /* this is right because the size counts the size
			    field (which stays out) but not the header
			    (which comes in) */
    
    *q = <make vector header saying this is a continuation>;
    *(q+1) = CONST_FALSE;
    *(q+2) = <calculate offset from proc and retaddr>;
    *(q+3) = proc;
    for (i = 0 ; i < size - 3 ; i++ )
      *q(4+i) = *p++;
    if (prev != NULL)
      *(prev+1) = q;
    else
      top = q;
    prev = q;
  }
  *(prev+1) = CREG;
}
