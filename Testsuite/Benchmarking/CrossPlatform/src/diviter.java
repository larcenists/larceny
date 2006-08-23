// DIVITER -- Benchmark which divides by 2 using lists of n ()'s.
 
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

class diviter {
  static Pair ll;

  static Pair create_n (int n) {
    Pair result = null;
    while (n > 0) {
      n = n - 1;
      result = Pair.cons ( 0, result );
    }
    return result;
  }

  static Pair iterative_div2 (Pair l) {
    Pair a = null;
    for (l = l; l != null; l = Pair.cdr (Pair.cdr (l)))
      a = Pair.cons(Pair.car(l), a);
    return a;
  }

  static int list_length (Pair l) {
    int n = 0;
    while (l != null) {
      l = Pair.cdr(l);
      n++;
    }
    return n;
  }

/*===========================================================================*/

  public static void main (String args[])
  {
    int i;
    Pair result;
    Pair ll;
    ll = create_n (200);

    result = iterative_div2 (ll);
    for (i=1; i<400000; i++) {
      result = iterative_div2 (ll);
    }

    if (list_length (result) != 100)
      System.out.println ("*** wrong result ***");
  }

}
