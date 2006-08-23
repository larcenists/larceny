// TAKL -- The TAKeuchi function using lists as counters.

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

}

class takl {

  static Pair listn (int n) {
    if (n != 0)
	return Pair.cons (n, listn (n - 1));
    else
        return null;
  }

  //  The boolean expression below comes from the original TAKL
  //  benchmark, and remains because it is fun to see which compilers
  //  can generate good code from it.  See NTAKL for a version in
  //  which this mess has been cleaned up.

  static boolean shorterp (Pair x, Pair y) {
    return (y != null) &&
           ((x == null) ||
            shorterp (Pair.cdr(x), Pair.cdr(y)));
  }

  static Pair mas (Pair x, Pair y, Pair z) {
    if (! shorterp (y, x))
	return z;
    else
        return mas( mas( Pair.cdr(x), y, z ),
                    mas( Pair.cdr(y), z, x ),
                    mas( Pair.cdr(z), x, y ) );
  }

  static Pair l18 = listn(18);
  static Pair l12 = listn(12);
  static Pair l6  = listn(6);

  static int result;

  static void test_takl() {
    result = Pair.car( mas( l18, l12, l6 ) );
  }

/*===========================================================================*/

    public static void main (String args[])
    {
      int i;

      for (i=0; i<200; i++)
        test_takl();

      if (result != 7)
        System.out.println ("*** wrong result ***");
    }

}
