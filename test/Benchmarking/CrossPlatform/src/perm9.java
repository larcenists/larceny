// PERM9 - Memory system benchmark using Zaks's permutation generator.

class Pair {

  //  A Java compiler ought to generate inline code for these.

  public static Pair cons (int n, Pair y) { return new Pair(n, y); }
  public static int  car (Pair x) { return x.hd; }
  public static Pair cdr (Pair x) { return x.tl; }

  //  If it doesn't, then we'll inline them by hand.
  //  That's why the following are public.
  //  (But Sun's Java 1.2 does the inlining ok.)

  public Pair (int n, Pair y) { hd = n; tl = y; }
  public int hd;
  public Pair tl;

  public static int length (Pair x) {
    int n = 0;
    while (x != null) {
      n = n + 1;
      x = Pair.cdr (x);
    }
    return n;
  }
}

class Perm {

  //  A Java compiler ought to generate inline code for these.

  public static Perm cons (Pair p, Perm y) { return new Perm(p, y); }
  public static Pair car (Perm x) { return x.hd; }
  public static Perm cdr (Perm x) { return x.tl; }

  public Perm (Pair n, Perm y) { hd = n; tl = y; }
  public Pair hd;
  public Perm tl;

}

class perm9 {

/******************************************************************************
*
*   permutations
*
******************************************************************************/

  private static Perm perms;    /* local to these functions */
  private static Pair x;        /* local to these functions */

  static Pair revloop( Pair x, int n, Pair y )
  {
    while (n-- != 0) {
      y = Pair.cons ( Pair.car(x), y );
      x = Pair.cdr(x);
    }
    return y;
  }

  static Pair tail( Pair l, int n )
  {
    while (n-- > 0) {
      l = Pair.cdr(l);
    }
    return l;
  }
    
  static void F( int n )
  {
    x = revloop( x, n, tail( x, n ) );
    perms = Perm.cons ( x, perms );
  }

  static void P( int n )
  {
    if (n > 1) {
      for ( int j = n-1; j != 0; --j ) {
        P( n-1 );
        F( n );
      }
      P( n-1 );
    }
  }

  static Perm permutations( Pair the_x )
  {
    x = the_x;
    perms = Perm.cons ( the_x, null );
    P( Pair.length( the_x ) );
    return perms;
  }

/******************************************************************************
*
*   sumperms
*
******************************************************************************/

  static long sumperms( Perm x ) {
    Pair y;
    long sum = 0;

    for (; x != null; x = Perm.cdr(x))
      for (y = Perm.car(x); y != null; y = Pair.cdr(y))
        sum = sum + Pair.car(y);

    return sum;
  }

/******************************************************************************
*
*   factorial
*
******************************************************************************/

  static long factorial( int n ) {
      int f = 1;
      while (n > 0) {
          f = n * f;
          n = n - 1;
      }
      return f;
  }

  public static void main( String args[] )
  {
    int n = 9;
    Perm m, m2;
    long sum;
    int k;

    /* Create the list [1, 2, ..., n]. */

    Pair one_to_n = null;
    int nn = n;
    while (nn > 0) {
      one_to_n = Pair.cons( nn--, one_to_n );
    }

    m = permutations( one_to_n );
    //printperms( m );

    for ( k = 5; k > 0; --k ) {
      m2 = permutations( one_to_n );
      m = m2;
    }

    sum = sumperms( m );
    if (sum != (n * (n + 1) * factorial (n)) / 2)
      System.out.println ("*** wrong result ***");
  }

/******************************************************************************
*
*   Miscellaneous.
*
******************************************************************************/

  /*  Prints a list for debugging.  */

  static void printints ( Pair l ) {
      System.out.print( "(" );
      while (l != null) {
        System.out.print( Pair.car(l) );
        l = Pair.cdr(l);
        if (l != null) System.out.print( " " );
      }
      System.out.println( ")" );
  }

  static void printperms( Perm perms ) {
    while (perms != null) {
      printints( Perm.car(perms) );
      perms = Perm.cdr(perms);
    }
    System.out.println( "" );
  }
}

/* eof */
