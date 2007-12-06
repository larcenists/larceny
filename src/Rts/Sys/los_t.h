/* Copyright 1998 Lars T Hansen.
 *
 * $Id$
 *
 * Larceny run-time system -- Large Object Space data type.
 *
 * How the Large Object Space is used:
 *
 * A large object is allocated with los_allocate() and is added to a
 * data structure for the generation it is allocated in.  During a garbage
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
 * the marked list (FSK: that is an *exclusive* or, yes?)
 */

#include "larceny-types.h"

typedef struct los_list los_list_t;

struct los {
  int generations;		/* Number of generations */
  los_list_t **object_lists;	/* One list for each generation */
  los_list_t *mark1;		/* Two lists for */
  los_list_t *mark2;            /*   marking during GC */
};

los_t *create_los( int generations );
  /* Create and initialize a LOS structure.

     generations > 0
     */

void expand_los_gnos( los_t *los, int fresh_gno );
  /* Adds a new generation, with unique fresh_gno, to the LOS structure.
     Objects in los change their gno assignment to accomodate fresh_gno.
     */

los_list_t *create_los_list(void);
  /* Create a free-standing LOS list.
     */

#define LOS_MARK1  -1		/* Secret value */
#define LOS_MARK2  -2		/* Secret value */

int los_bytes_used( los_t *los, int gen_no );
  /* Returns the number of bytes allocated to large objects in generation
     `gen_no' or on one of the mark lists.

     0 <= gen_no < los.generations 
  or gen_no == LOS_MARK1 
  or gen_no == LOS_MARK2
     */

word *los_allocate( los_t *los, int nbytes, int gen_no );
  /* Allocate nbytes from the large object space with the given generation
     and return a pointer to the block.  The large object is allocated
     to its own set of pages, and the page attribute on those pages
     has the MB_LARGE_OBJECT bit set.

     nbytes > 0
     0 <= gen_no < los.generations
     */

bool los_mark( los_t *los, los_list_t *marked, word *w, int gen_no );
  /* Mark the block by moving it to the end of the 'marked' list, which
     should be a mark list, if it is not already on a mark list.  Returns
     true if the object was already marked, false if not.  Gen_no is the 
     generation number of the object being moved.

     w must be the address of a live large object.
     */

bool los_mark_and_set_generation( los_t *los, los_list_t *marked, word *w, 
				  int gen_no, int new_gen_no );
  /* Exactly like los_mark() except that the generation number of the object
     is immediately changed to new_gen_no.

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

void los_list_set_gen_no( los_list_t *list, int gen_no );
  /* Set the generation number on the pages occupied by large objects
     in the list to `gen_no'.
     */

word *los_walk_list( los_list_t *list, word *p );
  /* Given a pointer p to a block on the given list (or NULL for the first
     such block), return a pointer to the next block.  Returns NULL when
     the list has been exhausted.

     p == 0 or p is the address of a large live object on the list.
     */

void los_free_list( los_list_t *list );
  /* Free the list.  Does not affect any objects on the list.
     */

/* eof */

