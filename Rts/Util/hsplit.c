/* hsplit.c -- program that shuffles code into the static area
 * $Id: hsplit.c,v 1.1.1.1 1998/11/19 21:51:38 lth Exp $
 *
 * Lars Thomas Hansen [lth@cs.uoregon.edu]
 * May 28, 1996
 *
 * DESCRIPTION
 *   The input is a single or split heap image; the output is a split
 *   heap image. The output image must be garbage collected and re-dumped
 *   from inside Larceny to get rid of the obsolete code vectors.
 *
 * USAGE
 *   hsplit infile outfile
 *   hsplit [-info] infile
 *
 * BUGS
 *   - the extra gc should be performed here. (minor nuisance)
 *   - this code should really be part of the larceny executable. (feature)
 *   - if you run it on a newly split (i.e. not gc'd) heap, you'll end
 *     up with two sets of the code vectors. And so on. (annoying)
 *
 * Some potential flags:
 *  - promote all strings (valid in initial heap only)
 *  - promote constant vectors
 *  - promote strings pointed to by name field of symbol (always valid).
 *
 * FUTURE THINGS
 *  - the heap image should really have several static areas: one for
 *    code (what is now called "static", should be called "text"), and one
 *    for Scheme structures that are writeable but should not be copied
 *    (procedures, constants); should be called "static".
 */

#include <stdio.h>
#include <stdarg.h>
#include "larceny.h"
#include "cdefs.h"
#include "macros.h"

typedef struct snode snode;

struct snode {
  word *ptr;
  int words;
  snode *next;
  word first, second, newptr;
};

word getword();

snode *slist = 0, *slast = 0;
word roots[ LAST_ROOT-FIRST_ROOT+1 ];

#define FORWARD_PTR 0xFFFFFF7E
#define HIBIT 0x80000000
#define BYTES_PER_CHAR 1

int main( argc, argv )
int argc;
char **argv;
{
  int i;
  FILE *ifp, *ofp;
  word magic, *smem;
  int split;
  int newtcount, newscount;
  word *tmem;
  int tcount, scount, ssize;
  char *fn1, *fn2;
  int do_info, do_split;

  argc == 3 || panic( "Usage: %s infile outfile\n"
		      "       %s -info infile\n", 
		      argv[0], argv[0] );
  if (strcmp( argv[1], "-info") == 0) {
    fn2 = 0;
    fn1 = argv[2];
    do_info = 1;
    do_split = 0;
  }
  else {
    printf( "Warning: hsplit is not sufficient for the BDW collector.\n" );
    fn1 = argv[1];
    fn2 = argv[2];
    do_info = 0;
    do_split = 1;
  }

  ifp = fopen( fn1, "r" );
  ifp != 0 || panic( "Unable to open %s for input.", fn1 );

  if (do_split) {
    ofp = fopen( fn2, "w" );
    ofp != 0 || panic( "Unable to open %s for output.", fn2 );
  }

  magic = getword( ifp );

  /* This is too fascist, but OK for now. */
  ((magic & 0xFFFF) == HEAP_VERSION) ||
    panic( "Unable to deal with heap version %s", magic&0xFFFF);

  for ( i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
    roots[ i-FIRST_ROOT ] = getword( ifp );

  if (!(magic & 0xFFFF0000)) {
    scount = 0;
    tcount = getword( ifp );
    split = 0;
  }
  else {
    scount = getword( ifp );
    tcount = getword( ifp );
    split = 1;
  }

  /* dump some statistics */
  printf( "Input file\n" );
  printf( "Heap version...: %d\n", magic & 0xFFFF );
  printf( "Heap type......: %s\n", (split ? "split" : "single") );
  printf( "Number of roots: %d\n", LAST_ROOT-FIRST_ROOT+1 );
  if (split) 
    printf( "Static size....: %d words\n", scount );
  printf( "Tenured size...: %d words\n", tcount );
  printf( "\n" );

  if (do_info) {
    printf( "Roots:\n" );
    for ( i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
      printf( "  %02d: %08lx\n", i-FIRST_ROOT, roots[ i-FIRST_ROOT ] );
  }

  /* We don't really need to read the static data here, as it will never
     be inspected. We just need to know how much there is, and later we
     must copy it from the old file to the new file. FIXME.
   */
  tmem = (word*)malloc( tcount*sizeof(word) );
  tmem != 0 || panic( "Can't allocate %ld words of memory.", tcount );
  if (scount) {
    smem = (word*)malloc( scount*sizeof(word) );
    smem != 0 || panic( "Can't allocate %ld words of memory.", scount );
  }

  if (split)
    if (fread( (char*)smem, sizeof( word ), scount, ifp ) < scount)
      panic( "Can't read static heap data.");
  if (fread( (char*)tmem, sizeof( word ), tcount, ifp ) < tcount)
    panic( "Can't read tenured heap data." );

  if (do_info) {
    object_stats( tmem, tcount, scount );
  }

  if (do_split) {
    ssize = reorg( tmem, tcount, scount );

    putword( (magic | 0x00010000), ofp );
    for ( i = FIRST_ROOT ; i <= LAST_ROOT ; i++ )
      putword( roots[ i-FIRST_ROOT ], ofp );
    putword( ssize, ofp );
    putword( tcount, ofp ); /* note: unmodified */
    
    /* write static */
    if (fwrite( (char*)smem, sizeof( word ), scount, ofp ) < scount)
      panic( "Can't write static data." );
    newscount = scount;
    newtcount = tcount;
    while (slist != 0) {
      int words = slist->words;
      if (newscount*sizeof( word ) != (slist->newptr & ~(HIBIT | 7)))
	panic( "Internal inconsistency (#1): newscount=%08x newptr=%08x", 
	      newscount, slist->newptr );
      if (fwrite( (char*)(slist->ptr), sizeof( word ), words, ofp ) < words)
	panic( "Can't write static data." );
      slist = slist->next;
      newscount += words;
      newtcount -= words;
    }

    if (newscount != ssize) 
      panic( "Internal inconsistency (#2): newscount != ssize: %x %x",
	    newscount, ssize );

    /* write tenured */
    if (fwrite( (char*)tmem, sizeof( word ), tcount, ofp ) < tcount)
      panic( "Can't write tenured data." );
    
    if (fclose( ofp ) == EOF)
      panic( "Couldn't close output file %s", argv[2] );

    /* Dump some statistics */
    printf( "Output file\n" );
    printf( "Static size....: %d words (0x%08x)\n", newscount, newscount );
    printf( "Tenured size...: %d words (0x%08x)\n", newtcount, newtcount );
  }

  fclose( ifp );

  return 0;
}


/*
 * Scan the heap image counting the various object types.
 * Calculate the sizes in words, then print the stats.
 *
 * FIXME: We'd also like to count the number of occurrences of each type,
 * and for some types, the largest and smallest.  Might want to display
 * each type also in terms of percentage of total heap size.
 */
int object_stats( tmem, tcount, scount )
word *tmem;
unsigned tcount, scount;
{
  unsigned cnt_null = 0;
  unsigned cnt_boolean = 0;
  unsigned cnt_char = 0;
  unsigned cnt_fixnum = 0;
  unsigned cnt_other_imm = 0;
  unsigned cnt_immediate = 0;
  unsigned cnt_ptr = 0;
  unsigned cnt_pair = 0;
  unsigned cnt_procedure = 0;
  unsigned cnt_vector_total = 0;
  unsigned cnt_bytevector_total = 0;
  unsigned cnt_vector_like[8] = { 0,0,0,0,0,0,0,0 };
  unsigned cnt_bytevector_like[8] = { 0,0,0,0,0,0,0,0 };
  unsigned cnt_pad = 0;
  unsigned cnt_string_chars = 0;
  unsigned in_struct = 0;
  unsigned the_typetag = 0, the_header = 0;
  word *p, *limit, n, words;

  /* Scan heap from start to end.  A word is either a header, or it
   * follows a header within that structure, or it is (implicitly)
   * a pair.
   */
  p = tmem; limit = tmem+tcount;
  while (p < limit) {
    if (!in_struct && ishdr( *p )) {
      n = sizefield( *p ); 
      the_typetag=typetag(*p)/4; 
      the_header=header(*p);

      switch( the_header ) {
	case VEC_HDR :
	  words = roundup8( n + 4 ) / 4;
	  cnt_vector_total += words;
	  cnt_vector_like[the_typetag] += words;
	  in_struct=words;
	  if (n % 8 == 0) cnt_pad++;
	  continue;
        case BV_HDR :
	  words = roundup8( n + 4 ) / 4;
	  in_struct=0;
	  cnt_other_imm += 1;
	  cnt_bytevector_total += words;
	  cnt_bytevector_like[the_typetag] += words;
	  p += roundup8( n+4 )/4;
	  if (n % 8 < 4) cnt_pad++;
	  if (the_typetag == 1) {
	    /* string */
	    cnt_string_chars += n/BYTES_PER_CHAR;
	  }
	  continue;
        case header(PROC_HDR) :  /* bug workaround */
	  words = roundup8( n + 4 ) / 4;
	  cnt_procedure += words;
	  in_struct=words;
	  if (n % 8 == 0) cnt_pad++;
	  continue;
	default : 
	  panic( "Impossible case in object_stats at offset %d: 0x%08x\n", 
		 p-tmem, *p );
	  break;
      }
    }
    else if (in_struct == 0) {
      cnt_pair+=2;
      in_struct=2;
      continue;
    }
    else {
      switch( tagof( *p ) ) {
        case 0: case 4: 
	  /* fixnum */
	  cnt_fixnum++;
	  break;
	case 2: case 6:
	  /* immediate */
	  cnt_immediate++;
	  if (*p == TRUE_CONST || *p == FALSE_CONST)
	    cnt_boolean++;
	  else if (*p == NIL_CONST)
	    cnt_null++;
	  else if ((*p & 0xFF) == 0x26) /* character. FIXME: define macro */
	    cnt_char++;
	  else
	    cnt_other_imm++;
	  break;
	case 1: case 3: case 5: case 7:
	  cnt_ptr++;
	  break;
      }
      p++;
    }
    in_struct--;
  }

  printf( "\n" );
  printf( "Heap Object Memory Usage Statistics\n\n" );
  printf( "                 Words of memory\n" );
  printf( "Pairs..........: %u\n", cnt_pair );
  printf( "Vector-like....: %u\n", cnt_vector_total );
  printf( "  vectors......: %u\n", cnt_vector_like[0] );
  printf( "  rectnums.....: %u\n", cnt_vector_like[1] );
  printf( "  ratnums......: %u\n", cnt_vector_like[2] );
  printf( "  symbols......: %u\n", cnt_vector_like[3] );
  printf( "  ports........: %u\n", cnt_vector_like[4] );
  printf( "Bytevector-like: %u\n", cnt_bytevector_total+scount );
  printf( "  bytevectors..: %u\n", cnt_bytevector_like[0]+scount );
  printf( "  strings......: %u (%u characters)\n", 
	  cnt_bytevector_like[1], cnt_string_chars );
  printf( "  flonums......: %u\n", cnt_bytevector_like[2] );
  printf( "  compnums.....: %u\n", cnt_bytevector_like[3] );
  printf( "  bignums......: %u\n", cnt_bytevector_like[4] );
  printf( "Procedures.....: %u\n", cnt_procedure );
  printf( "\n" );
  printf( "Immediates.....: %u\n", cnt_immediate+cnt_ptr );
  printf( "  fixnums......: %u\n", cnt_fixnum );
  printf( "  characters...: %u\n", cnt_char );
  printf( "  booleans.....: %u\n", cnt_boolean );
  printf( "  null.........: %u\n", cnt_null );
  printf( "  other imms...: %u (includes headers)\n", cnt_other_imm );
  printf( "\n" );
  printf( "Pointers.......: %u\n", cnt_ptr );
  printf( "\n" );
  printf( "Pad words......: %u\n\n", cnt_pad );
  printf( "All structure sizes include headers and padding.\n" );
  printf( "Immediates, pointers, and pad words exist within other objects,\n");
  printf( "and are therefore counted twice.  Pad words may look like any\n" );
  printf( "other immediate or pointer and will skew the data slightly.\n" );
}


/* Create linked list of addresses of bytevectors in tspace that
 * should be copied to sspace.
 *
 * Make three passes: 
 * - the first pass scans for procedures and moves the code vectors to
 *   the static area, leaving forwarding pointers. 
 * - the second pass looks for bytevector pointers and updates those 
 *   that point to forwarded structures.
 * - the third pass removes all forwarding pointers again.
 *
 * It should be possible to do this in two passes, but three is cleaner.
 *
 * Pass 2 could also undo the effect of the first pass if there's
 * a bytevector pointer to a code vector from a place other than a procedure.
 * Such things can exist in heaps which have had computation running in
 * them; whether such vectors should truly be considered static is an
 * interesting issue in itself.
 *
 * A forwarding pointer is a 2-word structure where the first word is FFFFFF7E
 * and the second word is a pointer to a forwarding structure.
 *
 * Returns: new static size (== its limit) in words.
 */
int reorg( tmem, tcount, scount )
word *tmem;
int tcount;  /* tenured size in words */
int scount;  /* static size in words */
{
  snode *new_slist();
  word *p, *limit;
  int i, j;
  int forwards = 0, fixups = 0, rootfwd = 0;

  scount *= 4;  /* to get bytes */

#define fixup( base, v ) (base+((word)ptrof( v ))/4)

  /* pass 1: forward code vectors */
  /* Make sequential pass looking for procedure headers.  If header is
     found, look at codevector slot.  Inspect code vector and move it
     if it has not already been moved; update codevector slot in process
     with new address.
   */
  p = tmem; limit = tmem+tcount;
  while (p < limit) {
    if (ishdr( *p ) && (*p & 0xFF) == PROC_HDR) {
      /* code is a raw pointer to the code vector */
      word *code = fixup( tmem, *(p+PROC_CODEPTR) );
      if (*code == FORWARD_PTR) {
	*(p+PROC_CODEPTR) = ((snode*)*(code+1))->newptr; 
      }
      else {
	word first, second, bytes, newptr;
	snode *q;

	first = *code;
	second = *(code+1);
	bytes = roundup8( sizefield( first ) + 4 );
	newptr = (word)tagptr( scount, BVEC_TAG ) | HIBIT;
	scount += bytes;
/*
	printf( "fwd: old %08x new %08x bytes %08x scount %08x\n", 
	       *(p+PROC_CODEPTR), newptr, bytes, scount );
*/
	*(p+PROC_CODEPTR) = newptr;
	q = new_slist();
	q->ptr = code;
	q->words = bytes/4;
	q->first = first;
	q->second = second;
	q->newptr = newptr;
	*code = FORWARD_PTR;
	*(code+1) = (word)q;
	forwards++;
      }
    }

    if (ishdr( *p )) {
      /* Clean up the pad word, if it's there and it's relevant */
      /* The hack with PROC_HDR masks a bug in the definition of PROC_HDR
	 which it is too much trouble to fix now; when it is fixed,
	 this code will still work.
       */
      int hdr = header(*p);
      if ((hdr == VEC_HDR || hdr == header(PROC_HDR)) &&
	  roundup8( sizefield( *p )+4 ) != sizefield(*p)+4 )
	*(p+(sizefield(*p)+4)/4) = 0;
      p += roundup8( sizefield( *p ) + 4 ) / 4;
    }
    else if (*p == FORWARD_PTR)
      p += ((snode*)*(p+1))->words;
    else
      p++;
  }

  /* pass 2: forward pointers to moved objects. */
  p = tmem;
  while (p < limit) {
    if (isptr( *p ) && tagof( *p ) == BVEC_TAG && (*p & HIBIT) == 0) {
      word *r = fixup( tmem, *p );
      if (*r == FORWARD_PTR) *p = ((snode*)*(r+1))->newptr;
    }

    if (*p == FORWARD_PTR)
      p += ((snode*)*(p+1))->words;
    else if (ishdr( *p ) && header( *p ) == BV_HDR)
      p += roundup8( sizefield( *p ) + 4 ) / 4;
    else
      p++;
  }

  /* also do the roots */
  for ( i = FIRST_ROOT, j=0 ; i <= LAST_ROOT ; i++, j++ ) {
    if (isptr( roots[j] ) && tagof( roots[j] ) == BVEC_TAG) {
      word *r = fixup( tmem, roots[j] );
      if (*r == FORWARD_PTR) {
	roots[j] = ((snode*)*(r+1))->newptr;
	rootfwd++;
      }
    }
  }

  /* pass 3: clean up */
  { snode *q = slist;
    while (q) {
      *(q->ptr) = q->first;
      *(q->ptr+1) = q->second;
      q = q->next;
      fixups++;
    }
  }

/*
  printf( "Statistics: %d forwards, %d fixups\n", forwards, fixups );
*/
  printf( "Reorganization\nMoved %d code vectors, fixed %d roots\n\n", 
	 forwards, rootfwd );
  return scount / 4; /* divide to get words */
}

/* Must be kept in order: will be written in order... */
snode *new_slist()
{
  snode *p = (snode*)malloc( sizeof( snode ) );
  if (p == 0) 
    panic( "Out of memory.\n" );
  p->next = 0;
  if (slast == 0)
    slist = p;
  else
    slast->next = p;
  slast = p;
  return p;
}

/***************************************************************************/
/* From heapio.c */

/* This procedure knows that the world is 32-bit and big-endian. */
word getword( fp )
FILE *fp;
{
  word a = getc( fp );
  word b = getc( fp );
  word c = getc( fp );
  word d = getc( fp );

  return (a << 24) | (b << 16) | (c << 8) | d;
}

/* Knows the world is 32-bit and big-endian. */
int putword( w, fp )
word w;
FILE *fp;
{
  if (putc( (w >> 24) & 0xFF, fp ) == EOF) return EOF;
  if (putc( (w >> 16) & 0xFF, fp ) == EOF) return EOF;
  if (putc( (w >> 8) & 0xFF, fp ) == EOF) return EOF;
  if (putc( w & 0xFF, fp ) == EOF) return EOF;
  return 0;
}

/* from larceny.c */

int panic( const char *fmt, ... )
{
  static int in_panic = 0;
  va_list args;

  va_start( args, fmt );
  fprintf( stderr, "Larceny Panic: " );
  vfprintf( stderr, fmt, args );
  va_end( args );
  fprintf( stderr, "\n" );

  exit( 1 );
  /* Never returns. Return type is 'int' to facilitate an idiom. */
}

/* eof */
