#include "twobit.h"

int main( int argc, char **argv )
{ return larceny_main( argc, argv ); }


/* Loadable segments' code */

codeptr_t *twobit_load_table[] = { 
  0  /* The table may be empty; some compilers complain */
};
