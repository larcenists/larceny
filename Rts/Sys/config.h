/* Pilfered from the Boehm-Weiser config.h */

#if !defined(INCLUDED_CONFIG_H)
#define INCLUDED_CONFIG_H

# if defined(sun) &&(defined(sparc) || defined(__sparc))
#  define SPARC
     /* Test for SunOS 5.x */
#    include <errno.h>
#    ifdef ECHRNG
#      define SUNOS5
#    else
#      define SUNOS4
#    endif
# endif

#endif
