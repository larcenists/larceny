#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>

main( argc, argv )
int argc;
char **argv;
{
  struct stat buf;
  struct tm *tm;

  if (stat( argv[1], &buf ) == -1) 
    return 1;

  tm = localtime( &buf.st_mtime );
  printf( "%ld\n%s", tm->tm_gmtoff, ctime( &buf.st_mtime ) );
  printf( "%d %d %d %d %d %d", tm->tm_year, tm->tm_mon, tm->tm_mday,
	 tm->tm_hour, tm->tm_min, tm->tm_sec );
  return 0;
}
