/* Rts/Sys/los_t.h
 * Larceny run-time system -- Large Object Space data type.
 *
 * $Id$
 *
 * How the Large Object Space is used:
 *
 * A large object is allocated with los_allocate() and is added to a
 * data structure for the generation it's allocated in.  During a garbage
 * collection in that or an older generation, the collector moves the large
 * objects from the generation's data structure to the common marked list
 * with a call to los_mark().  The marked list can be walked during the scan
 * with los_next_marked().  After the scan is done, all remaining large
 * objects in the generations being collected are garbage, and can be
 * reclaimed with los_sweep().  Finally, a call to los_append_marked()
 * moves the marked objects to a target generation.
 *
 * The garbage collector should use the page attribute table to determine
 * whether a particular object is managed by the Large Object Space.
 *
 * Invariants:
 *
 * When a large object is seen by any routine in this module (except
 * los_allocate()), its object header must be valid, and if the requested
 * size of the object was n, then the size field s of the header must
 * have a value n-7 <= s <= n.
 *
 * A large object is always on either one of the object_lists or on 
 * the marked list.
 */

#include "larceny-types.h"

typedef struct los_list los_list_t;

struct los {
  int generations;		/* Number of generations */
  los_list_t **object_lists;	/* One list for each generation */
  los_list_t *marked;		/* One list for marking during GC */
};

los_t *create_los( int generations );
  /* Create and initialize a LOS structure.

     generations > 0
     */

int los_bytes_used( los_t *los, int gen_no );
  /* Returns the number of bytes allocated in generation `gen_no' in the 
     LOS.  Generally, the value is inaccurate between a call to los_mark() 
     and the first subsequent call to los_sweep(), i.e., during GC.

     0 <= gen_no < los.generations
     */

word *los_allocate( los_t *los, int nbytes, int gen_no );
  /* Allocate nbytes from the large object space with the given generation
     and return a pointer to the block.  The large object is allocated
     to its own set of pages, and the page attribute on those pages
     has the MB_LARGE_OBJECT bit set.

     nbytes > 0
     0 <= gen_no < los.generations
     */

int los_mark( los_t *los, word *w, int gen_no );
  /* Mark the block by moving it to the end of the marked list, if it
     is not already on the marked list.  Returns 1 if the object was
     already marked, 0 if not.  Gen_no is the generation number of the
     object being moved.

     w must be the address of a live large object.
     */

void los_sweep( los_t *los, int gen_no );
  /* Sweep the indicated generation list and free all the blocks on it.

     0 <= gen_no < los.generations
     */

void los_append_and_clear_list( los_t *los, los_list_t *l, int to_gen );
  /* Append the list to the list of to_gen, and clear the list.
     The generation numbers on the pages of the appended objects
     are changed to to_gen.

     0 <= to_gen < los.generations
     */

word *los_walk_list( los_list_t *list, word *p );
  /* Given a pointer p to a block on the given list (or NULL for the first
     such block), return a pointer to the next block.  Returns NULL when
     the list has been exhausted.

     p == 0 or p is the address of a large live object on the list.
     */

/* eof */

