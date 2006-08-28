/* SRFI-22 stub for Larceny
   2004-01-03 / lth 

   $Id$

   Permission to copy this software, in whole or in part, to use this
   software for any lawful purpose, and to redistribute this software
   is granted.

   See the comment block at the beginning of srfi-22.sch for more
   information.
*/

#ifndef EXECUTABLE
# define EXECUTABLE "/home/lth/lib/larceny/petit"
#endif
#ifndef HEAPIMAGE
# define HEAPIMAGE  "/home/lth/lib/larceny/petit.heap"
#endif

#include <unistd.h>
#include <stdlib.h>
#include <string.h>

int str_end_eq(char *s1, char *s2)
{
    int l1, l2;
    l1 = strlen(s1);
    l2 = strlen(s2);
    if (l2 > l1)
	return 0;
    return strcmp( s1+l1-l2, s2 ) == 0;
}
    
int main(int argc, char **argv)
{
    char **args = malloc( sizeof(char*)*(argc-1+6) );
    int i;
    if (args == NULL)
	return -1;
    args[0] = EXECUTABLE;
    args[1] = HEAPIMAGE;
    args[2] = "-quiet";
    args[3] = "-args";
    if (str_end_eq( argv[0], "scheme-r4rs" ))
	args[4] = "r4rs";
    else if (str_end_eq( argv[0], "scheme-srfi-0" ))
	args[4] = "srfi-0";
    else if (str_end_eq( argv[0], "scheme-srfi-7" ))
	args[4] = "srfi-7";
    else if (str_end_eq( argv[0], "scheme-ieee-1178-1990" ))
	args[4] = "ieee";
    else
	args[4] = "r5rs";
    for ( i=1 ; i <= argc ; i++ )
	args[i+4] = argv[i];
    execv( EXECUTABLE, args );
    /*NOTREACHED*/
    return -1;
}

