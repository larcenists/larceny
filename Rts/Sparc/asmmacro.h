/*
 * Some macros for portability.
 *
 * lth / December 5, 1994
 *  Created.
 */

/* The macro EXTNAME(x) produces an identifier which is a valid external
 * (i.e., C-type) name for the OS in question. 
 */

#ifdef SOLARIS
/* On solaris external names are not prefixed by _, for some
 * reason. Seems to me this breaks all the assembly code in
 * existence, but who am I to argue...
 */
#define EXTNAME(x)  x
#endif

#ifdef SUNOS
/* On Sunos all external names start with an underscore, and we have
 * to perform token pasting.
 */
#ifdef __STDC__
#define EXTNAME(x)  _##x
#else
#define EXTNAME(x)  _/**/x
#endif
#endif

/* eof */
