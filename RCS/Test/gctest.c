/*
 * Test program for garbage collector.
 * Version 3.
 *
 * Usage:
 *   gctest <inheap> <outheap>
 *
 * Heaps are described using the Heap Description Language (HDL).
 * The syntax for a HDL "program" is as follows:
 *
 * program --> <stmt>* "END"
 * stmt    --> "ROOTS"
 *           | "EPHEMERAL"
 *           | "TENURED"
 *           | "ENTRYLIST"
 *           | "E-COLLECT"
 *           | "T-COLLECT"
 *           | "E-DUMP"
 *           | "T-DUMP"
 *           | "Y-DUMP"
 *           | "R-DUMP"
 *           | "POINTERS"
 *           | "EXEC" <code>
 *           | "DEF" <code>
 *           | <word>
 * word    --> ["E","T","C"]<hex>
 * hex     --> ["0"-"9","a"-"f","A"-"F"]+
 * code    --> <item>*
 * item    --> <0hex> | <string> | "E" | "T" | "R" | "!" | "@" | "n" | "$"
 *           | "t" | "f" | "+" | "-" | "`" | "=" | "<" | "C" | "V" | "L"
 *           | "X" | "&" | "_" | "?" | ":" | ";"
 * string  --> "'" <char>* "'"
 * 0hex    --> ["0"-"9"]["0"-"9","a"-"f","A"-"F"]*
 *
 * Lines cannot be blank; if a line does not start with a ';' then it
 * must have a meaningfull program segment. With the exception of EXEC and
 * DEF, anything but the first word on a line is ignored, so comments can
 * be added to a line, customarily preceded by ';'. All statements must be
 * flush left.
 *
 * The EXEC statement provides access to a simple stack-based language that
 * can be used to introduce pseudo-dynamic elements into the behavior in the
 * test system. (It is also a living example of why programmers should not be
 * allowed to write the specs -- we just add all sorts of fun stuff.)
 *
 * The meaning of the words in the EXEC statements:
 *   E - convert stacktop to ephemeral-area pointer
 *   T - convert stacktop to tenured-area pointer
 *   R - convert stacktop to root-area pointer (makes sense...)
 *   = - set location (second element) to value (first element)
 *   @ - fetch from address (first element); tagged or untagged ptr
 *   C - make a cons of the two top elements
 *   V - make vector of n elements (topmost, count on top)
 *   L - get length field of top element (assumed to have one)
 *   n - push a null pointer
 *   t - push true
 *   f - push false
 *   $ - print string to log file
 *   + - add 4 to a pointer
 *   - - subtract 4 from a pointer
 *   ` - convert stacktop to a variable address (0-99)
 *   ! - push loop address onto loopstack
 *   < - subtract 4 from stacktop and loop if nonzero, otherwise pop loopstack
 *       and valuestack
 *   & - dup
 *   X - swap
 *   _ - drop
 *   ? - if-equal: compare top elements, continue if equal, 
 *                                       skip to : or ; otherwise
 *   : - else: skip to ;
 *   ; - endif: no-op.
 *
 * No more than 10000 stack elements total.
 * No more than 10 nested loops per proc.
 * Conditionals cannot currently be nested.
 * Arbitrary whitespace is legal.
 *
 * DEF allows one to define new operators. The first nonblank after DEF
 * is the operator name (builtins cannot be overridden!) and the rest of the
 * line is the operator definition.
 */

#include <stdio.h>
#include <ctype.h>
#include "gcinterface.h"

#define MAXROOTS     20

word rootcnt;
word rootarray[ MAXROOTS ];
word *roots = &rootarray[0];
word vararray[ 100 ];

#define isptr( x )      ((word) (x) & 0x01)
#define ptrof( x )      ((word *) ((word) (x) & ~0x03))
#define sizefield( x )  ((word) (x) >> 8)

/* Our files */
FILE *ifp, *ofp;

int (*keyword())();        /* I love it */

char *strdup();
char *malloc();

/*
 * Here goes.
 */
main( argc, argv )
int argc;
char **argv;
{
  char s[ 200 ];
  int i;
  int (*f)();

  if (argc != 3)
    panic( "usage: %s <inheap> <outheap>\n", argv[ 0 ] );

  printf( "Initializing\n" );
  if (!init_collector( 1024*1024, 1024*1024, 1024*1024, 1024*512,
		       1024*8, 1024*6 ))
    panic( "Unable to initialize!" );

  printf( "Init done; running...\n" );

  if ((ifp = fopen( argv[ 1 ], "r" )) == NULL)
    panic( "Unable to open input file" );

  if ((ofp = fopen( argv[ 2 ], "w" )) == NULL)
    panic( "Unable to create output file" );

  while (getline( ifp, s )) {
    f = keyword( s );
    if (f != NULL)
      (*f)( s );
    else
      panic( "error in input" );
  }

  fclose( ifp );
  fclose( ofp );
}


load_ephemeral()
{
  load( e_base, &e_top );
}

load_tenured()
{
  load( t_base, &t_top );
}

/* This has to be loaded from higher toward lower addresses since we don't know
   how large it is before we've read it all. */
load_entrylist()
{
  char s[ 200 ];

  while(1) {
    if (!getline( ifp, s ))
      return;
    if (keyword( s ) != NULL) {
      ungetline( s );
      return;
    }
    *t_entries-- = getword( s );
  }
}

load_roots()
{
  char s[ 200 ];

  while (1) {
    if (!getline( ifp, s ))
      return;
    if (keyword( s ) != NULL) {
      ungetline( s );
      return;
    }
    rootarray[ rootcnt++ ] = getword( s );
  }
}

getword( s )
char *s;
{
  word tmp;

  if (sscanf( s+1, "%lx", &tmp ) != 1)
    panic( "Unable to get word" );
  if (*s == 'E')
    return tmp + (word) e_base;
  else if (*s == 'T')
    return tmp + (word) t_base;
  else if (*s == 'C')
    return tmp;
  else
    panic( "Illegal word spec" );
}

t_collect()
{
  fprintf( ofp, "; tenuring collection\n" );
  collect( 1 );
}

e_collect()
{
  fprintf( ofp, "; ephemeral collection\n" );
  collect( 0 );
}

e_dump()
{
  dump( "ephemeral area", e_base, e_top );
}

t_dump()
{
  dump( "tenured area", t_base, t_top );
}

r_dump()
{
  int i;

  fprintf( ofp, "; roots\n" );
  for (i = 0 ; i < rootcnt ; i++ )
    dumpword( rootarray[ i ] );
}

y_dump()
{
  word *p = t_limit;

  fprintf( ofp, "; entry list\n" );
  while (p > t_entries)
    dumpword( *p-- );
}

pointers()
{
#ifdef __STDC__
#define outit( x )  fprintf( ofp, "; %-20s %08lX\n", #x ":", (word) x )
#else
#define outit( x )  fprintf( ofp, "; %-20s %08lX\n", "x :", (word) x )
#endif
  outit( e_base );
  outit( e_top );
  outit( e_limit );
  outit( e_mark );
  outit( t_base );
  outit( t_top );
  outit( t_limit );
  outit( stack_base );
  outit( stack_limit );
  outit( stack_mark );
  outit( s_base );
  outit( s_limit );
  outit( s_top );
#undef outit
}


load( base, top )
word *base, **top;
{
  char s[ 200 ];

  while (getline( ifp, s )) {
    if (keyword( s ) != NULL) {
      ungetline( s );
      return;
    }
    *((*top)++) = getword( s );
  }
}


/* Doesn't take bytevectors into account, so some bytevectors have spurious
   'W' storage classes in them. This is not worth the bother to fix (yet). */

dump( name, base, top )
char *name;
word *base, *top;
{
  word *p = base;

  fprintf( ofp, "; %s\n", name );
  while (p < top)
    dumpword( *p++ );
}


/* Little interpreter */

char *optbl[ 100 ];
int opcnt = 0;
word stk[ 10000 ];
int sp = -1;

do_exec( s )
char *s;
{
  run( s + 4 );
}


do_define( s )
char *s;
{
  int i;

  if (opcnt == 100)
    panic( "too many operators!" );

  while (isspace( *s ))
    s++;

  if (!isprint( *s ))
    panic( "Cannot define unprintable char!" );

  for (i = 0 ; i < opcnt && *optbl[i] != *s ; i++ )
    ;

  optbl[ i ] = strdup( s );
  if (i == opcnt)
    opcnt++;
}


run( s )
char *s;
{
  word sum, tmp;
  int i, lsp = -1;
  char *lstk[ 10 ], sbuf[ 100 ], *q;

#define stkchk( n )  if (sp - n < -1) panic( "Stack underflow" )

  while (*s) {
    switch (*s) {
    case ' '  : break;
    case '\t' : break;
    case '\n' : break;
    case 'E'  : stkchk(1); stk[ sp ] = stk[ sp ] / 4 + (word) e_base; break;
    case 'T'  : stkchk(1); stk[ sp ] = stk[ sp ] / 4 + (word) t_base; break;
    case 'R'  : stkchk(1); stk[ sp ] = stk[ sp ] / 4 + (word) rootarray; break;
    case '='  : stkchk(2); *(word *)(stk[ sp-1 ]) = stk[ sp ]; sp -= 2; break;
    case '@'  : stkchk(1); stk[ sp ] = *(word *)(stk[ sp ]); break;
    case 'C'  : break;
    case 'V'  : break;
    case 'L'  : stkchk(1); stk[ sp ] = sizefield( stk[ sp ] ); break;
    case 'n'  : stk[ ++sp ] = 0x0000000A; break;
    case 't'  : stk[ ++sp ] = 0x00000006; break;
    case 'f'  : stk[ ++sp ] = 0x00000002; break;
    case '$'  : stkchk(1); fprintf( ofp, "%s\n", (char *) stk[ sp-- ] ); break;
    case '+'  : stkchk(1); stk[ sp ] += 4; break;
    case '-'  : stkchk(1); stk[ sp ] -= 4; break;
    case '`'  : stkchk(1); stk[ sp ] = stk[ sp ] / 4 + (word) vararray; break;
    case '!'  : lstk[ ++lsp ] = s+1; break;
    case '<'  : stkchk(1);
                if (stk[ sp ] -= 4) { 
                  s = lstk[ lsp ]; continue;
		}
		else {
		  lsp--; sp--;
		}
                break;
    case '\'' : s++; q = sbuf;
                while (*s != '\'' && *s) *q++ = *s++; *q = 0;
                stk[ ++sp ] = (word) strdup( sbuf );
                break;
    case '0'  :
    case '1'  :
    case '2'  :
    case '3'  :
    case '4'  :
    case '5'  :
    case '6'  :
    case '7'  :
    case '8'  :
    case '9'  : sum = 0; 
                while (isxdigit(*s)) {
                   sum = sum * 16 + (isdigit(*s) ? *s - '0' 
                                                 : tolower( *s ) - 'a' + 10);
		   s++;
		}
                stk[ ++sp ] = sum * 4;
                continue;
    case '&'  : stkchk(1); stk[ sp+1 ] = stk[ sp ]; sp++; break;
    case 'X'  : stkchk(2); 
                tmp = stk[ sp ]; stk[ sp ] = stk[ sp-1 ]; stk[ sp-1 ] = tmp;
                break;
    case '_'  : stkchk(1); sp -= 1; break;
    case '?'  : stkchk(2); sp -= 2; 
                if (stk[ sp+1 ] != stk[ sp+2 ])
		  while (*s && *s != ':' && *s != ';' ) s++;
                if (*s == ':')
		  s++;
                continue;
    case ':'  : while (*s && *s != ';') s++;
                break;
    case ';'  : break;
    default   : for (i = 0 ; i < opcnt && *optbl[i] != *s ; i++ ) ;
                if (i == opcnt)
		  panic( "Illegal word in EXEC" );
                else {
		  run( optbl[ i ]+1 );
		}
                break;
    }
    s++;
  }

#undef stkchk

}


panic( s )
char *s;
{
  fprintf( stderr, "%s\n", s );
  exit( 1 );
}


dumpword( x )
word x;
{
  word *p;
  
  if (isptr( x )) {
    p = (word *) ptrof( x );
    if (p >= e_base && p <= e_limit )
      fprintf( ofp, "E %08lX\n", x - (word) e_base );
    else if (p >= t_base && p <= t_limit)
      fprintf( ofp, "T %08lX\n", x - (word) t_base );
    else
      fprintf( ofp, "W %08lX\n", x );    /* "weird" */
  }
  else {
    fprintf( ofp, "C %08lX\n", x );
  }
}


/* Look for keyword, return its function pointer */

struct {
  char *s;
  int (*f)();
} ops[] = {  { "EPHEMERAL", load_ephemeral },
	     { "TENURED", load_tenured },
	     { "ENTRYLIST", load_entrylist },
	     { "ROOTS", load_roots },
	     { "E-COLLECT", e_collect },
	     { "T-COLLECT", t_collect },
	     { "E-DUMP", e_dump },
	     { "T-DUMP", t_dump },
	     { "Y-DUMP", y_dump },
	     { "R-DUMP", r_dump },
	     { "EXEC", do_exec },
	     { "DEF", do_define },
	     { "POINTERS", pointers } };

int (*keyword( s ))()
char *s;
{
  int i;

  for (i = 0 ; i < sizeof( ops ) / sizeof( ops[ 0 ] ) ; i++ )
    if (strncmp( s, ops[i].s, strlen( ops[i].s ) ) == 0)
      return ops[i].f;
  return NULL;
}


/* Input stuff */

int ungotten = 0;
char ungottenline[ 200 ];

getline( fp, s )
FILE *fp;
char *s;
{
  if (ungotten) {
    strcpy( s, ungottenline );
    ungotten = 0;
    return 1;
  }

  do {
    if (fgets( s, 200, fp ) == NULL)
      return 0;
    if (strncmp( s, "END", 3 ) == 0)
      return 0;
    if (*s == ';')
      continue;
    return 1;
  } while(1);
}


ungetline( s )
char *s;
{
  if (ungotten)
    panic( "ungetting again!" );
  ungotten = 1;
  strcpy( ungottenline, s );
}

char *strdup( s )
char *s;
{
  char *p;

  if ((p = malloc( strlen( s ) + 1)) == NULL)
    return NULL;
  strcpy( p, s );
  return p;
}
