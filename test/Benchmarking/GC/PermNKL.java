// PermNKL - Memory system benchmark using Zaks's permutation generator.
//
// Usage:
//
//     java PermNKL M N K L
//
// where
//
//     M is the number of iterations
//     N is the length of one permutation
//     K is the size of the queue of lists of permutations
//     L <= K is the number of lists to remove from the queue
//         after each iteration
//
// Translated by Will Clinger from the Scheme benchmark MpermNKL,
// based on an earlier translation of the perm9 benchmark (perm9.java).
//
// 110529 / wdc Renamed PermNKL with added parameters M, N, K, L.
//
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
// ; File:         perm9.sch
// ; Description:  memory system benchmark using Zaks's permutation generator
// ; Author:       Lars Hansen, Will Clinger, and Gene Luks
// ; Created:      18-Mar-94
// ; Language:     Scheme
// ; Status:       Public Domain
// ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//
// ; 940720 / lth Added some more benchmarks for the thesis paper.
// ; 970215 / wdc Increased problem size to 9; improved tenperm9-benchmark.
// ; 970531 / wdc Cleaned up for public release.
// ; 000820 / wdc Added the MpermNKL benchmark; revised for new run-benchmark.
//
// ; The perm9 benchmark generates a list of all 362880 permutations of
// ; the first 9 integers, allocating 1349288 pairs (typically 10,794,304
// ; bytes), all of which goes into the generated list.  (That is, the
// ; perm9 benchmark generates absolutely no garbage.)  This represents
// ; a savings of about 63% over the storage that would be required by
// ; an unshared list of permutations.  The generated permutations are
// ; in order of a Gray code that bears no obvious relationship to a
// ; lexicographic order.
//
// ; The 10perm9 benchmark repeats the perm9 benchmark 10 times, so it
// ; allocates and reclaims 13492880 pairs (typically 107,943,040 bytes).
// ; The live storage peaks at twice the storage that is allocated by the
// ; perm9 benchmark.  At the end of each iteration, the oldest half of
// ; the live storage becomes garbage.  Object lifetimes are distributed
// ; uniformly between 10.3 and 20.6 megabytes.
//
// ; The 10perm9 benchmark is the 10perm9:2:1 special case of the
// ; MpermNKL benchmark, which allocates a queue of size K and then
// ; performs M iterations of the following operation:  Fill the queue
// ; with individually computed copies of all permutations of a list of
// ; size N, and then remove the oldest L copies from the queue.  At the
// ; end of each iteration, the oldest L/K of the live storage becomes
// ; garbage, and object lifetimes are distributed uniformly between two
// ; volumes that depend upon N, K, and L.

// ; Date: Thu, 17 Mar 94 19:43:32 -0800
// ; From: luks@sisters.cs.uoregon.edu
// ; To: will
// ; Subject: Pancake flips
// ; 
// ; Procedure P_n generates a grey code of all perms of n elements
// ; on top of stack ending with reversal of starting sequence
// ; 
// ; F_n is flip of top n elements.
// ; 
// ; 
// ; procedure P_n
// ; 
// ;   if n>1 then
// ;     begin
// ;        repeat   P_{n-1},F_n   n-1 times;
// ;        P_{n-1}
// ;     end
// ; 

class PermNKL {

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

  static final int mDEFAULT = 200;
  static final int nDEFAULT = 9;
  static final int kDEFAULT = 10;
  static final int lDEFAULT = 1;

  public static void main( String args[] )
  {
      int m = mDEFAULT;
      int n = nDEFAULT;
      int k = kDEFAULT;
      int l = lDEFAULT;
      if (args.length > 0)
	  m = Integer.parseInt (args[0], 10);
      if (args.length > 1)
	  n = Integer.parseInt (args[1], 10);
      if (args.length > 2)
	  k = Integer.parseInt (args[2], 10);
      if (args.length > 3)
	  l = Integer.parseInt (args[3], 10);
      System.out.println ("m = " + m);
      System.out.println ("n = " + n);
      System.out.println ("k = " + k);
      System.out.println ("l = " + l);
      assert 0 <= m;
      assert 0 < n;
      assert 0 < k;
      assert 0 <= l;
      assert l <= k;

      Perm[] queue = new Perm[k];

      fillQueue( queue, n, 0, k - l);
      timeBenchmark(queue, m, n, k, l);
  }

  //  Returns the list [1, 2, ..., n].

  static Pair oneToN (int n) {
      Pair result = null;
      while (n > 0)
	  result = Pair.cons( n--, result );
      return result;
  }

  //  Fills queue positions [i, j).

  static void fillQueue (Perm[] queue, int n, int i, int j) {
      while (i <  j) {
	  queue[i] = permutations( oneToN( n ) );
	  i = i + 1;
      }
  }

  //  Removes l elements from queue.

  static void flushQueue (Perm[] queue, int k, int l) {
      for (int i = 0; i < k; i = i + 1) {
	  int j = i + l;
	  if (j < k)
	      queue[i] = queue[j];
	  else
	      queue[i] = null;
      }
  }

  static void timeBenchmark (Perm[] queue, int m, int n, int k, int l) {
      long tStart, tFinish;
      int result = 0;
      tStart = System.currentTimeMillis();
      for (int i = 0; i < m; i = i + 1) {
	  fillQueue( queue, n, k - l, k );
	  flushQueue( queue, k, l );
      }
      tFinish = System.currentTimeMillis();
      long elapsedTime = tFinish - tStart;
      System.out.println(result);
      System.out.println(elapsedTime + " msec");
      memoryReport();

      Perm q0 = queue[0];
      Perm qi = queue[Math.max( 0, k - l - 1 )];
      long sum0 = sumperms( q0 );
      long sumi = sumperms( qi );
      assert sum0 == sumi;
      assert sum0 == (n * (n + 1) * factorial( n ) / 2);
  }

  static void memoryReport () {
      long lFreeMemory = Runtime.getRuntime().freeMemory();
      long lTotalMemory = Runtime.getRuntime().totalMemory();

      System.out.print(" Total memory available="
		       + lTotalMemory + " bytes");
      System.out.println("  Free memory=" + lFreeMemory + " bytes");
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

// Instances of class Pair represent lists of int.
//
// Immutable lists are essential for this benchmark because
// much of the structure is shared, and that structure sharing
// can stress the garbage collector.

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

// Instances of class Perm represent lists of Pair
// (hence lists of permutations, so the name is confusing).
//
// Immutable lists are essential for this benchmark because
// much of the structure is shared, and that structure sharing
// can stress the garbage collector.

class Perm {

  //  A Java compiler ought to generate inline code for these.

  public static Perm cons (Pair p, Perm y) { return new Perm(p, y); }
  public static Pair car (Perm x) { return x.hd; }
  public static Perm cdr (Perm x) { return x.tl; }

  public Perm (Pair n, Perm y) { hd = n; tl = y; }
  public Pair hd;
  public Perm tl;
}

/* eof */
