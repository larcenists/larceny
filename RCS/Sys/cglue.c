/*
 * Some millicode.
 *
 */

word C_alloc();

/*
 * Millicode to deal with variable-length argument lists.
 *
 * There are four cases, depending on how many arguments are passed and
 * how many that are expected. When this procedure is entered it is
 * assumed that at least the minimum number of arguments are passed; i.e.
 * that check is performed outside this procedure.
 *
 * Let R = (the # of registers), r = (R - 1).
 * Let j = %RESULT (the actual number of arguments).
 * Let n = %ARGREG2 (the number of fixed arguments).
 *
 * We then have four cases.
 *
 * Case 0: n < R-2, j < r (i.e. all varargs are in registers).
 *   REGn+1 := (list REGn+1 ... REGj)
 *
 * Case 1: n < R-2, j >= r (i.e. all fixed args are in registers, but
 *   all varargs are not -- some are in a list in REGr).
 *   REGn+1 := (append! (list REGn+1 ... REGr-1) (copylist REGr))
 *
 * Case 2: n = R-2 (i.e. The varargs are exactly the ones in the list
 *   in REGr).
 *   REGr := (copylist REGr)
 *
 * Case 3: r <= n <= j (i.e. the varargs is a tail of the list in REGr).
 *   
 *
 */

C_scheme_varargs()
{
  word j = globals[ RESULT_OFFSET ] / 4;       /* Convert from fixnum */
  word n = globals[ ARGREG2_OFFSET ] / 4;      /* Ditto */
  word r = 31;                                 /* Highest register # */
  word R = 32;                                 /* # of registers */

  if (n < R-2 && j < r) {
    word *p, *q;
    word k;

    k = n + 1;
    q = p = (word *) C_alloc( 2*(j-k+1) );
    while ( k < j ) {
      *p = globals[ REG0_OFFSET + k ];
      *(p+1) = (word) p + 2 + PAIR_TAG;
      p += 2;
      k++;
    }
    *p = globals[ REG0_OFFSET + k ];
    *(p+1) = NULL_CONST;
    globals[ REG0_OFFSET + n + 1 ] = (word) q + PAIR_TAG;
  }
  else if (n < R-2 && j >= r) {
  }
  else if (n == R-2) {
  }
  else /* r <= n <= j */ {
  }
}


/*
 * Allocate a chunk of memory.
 *
 * `n' is a number of bytes, but must be divisible by 4.
 */
word C_alloc( n )
{
}
