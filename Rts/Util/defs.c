#include <stdio.h>
#include "../Sys/config.h"

main()
{
#if defined(SUNOS4)
  printf( "-DSUNOS4\n" );
#endif
#if defined(SUNOS5)
  printf( "-DSUNOS5\n" );
#endif
}
