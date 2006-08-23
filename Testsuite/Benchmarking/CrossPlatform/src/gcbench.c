/* This is adapted from a benchmark written by John Ellis and Pete Kovac
 * of Post Communications.
 * It was modified by Hans Boehm of Silicon Graphics.
 * Translated to C++ 30 May 1997 by William D Clinger of Northeastern Univ.
 * Translated to C and simplified for compatibility with the Gambit benchmark
 *   suite 1 July 1999 by William D Clinger of Northeastern Univ.
 *
 *      This is no substitute for real applications.  No actual application
 *      is likely to behave in exactly this way.  However, this benchmark was
 *      designed to be more representative of real applications than other
 *      Java GC benchmarks of which we are aware.
 *      It attempts to model those properties of allocation requests that
 *      are important to current GC techniques.
 *      It is designed to be used either to obtain a single overall performance
 *      number, or to give a more detailed estimate of how collector
 *      performance varies with object lifetimes.  It prints the time
 *      required to allocate and collect balanced binary trees of various
 *      sizes.  Smaller trees result in shorter object lifetimes.  Each cycle
 *      allocates roughly the same amount of memory.
 *      Two data structures are kept around during the entire process, so
 *      that the measured performance is representative of applications
 *      that maintain some live in-memory data.  One of these is a tree
 *      containing many pointers.  The other is a large array containing
 *      double precision floating point numbers.  Both should be of comparable
 *      size.
 *
 *      The results are only really meaningful together with a specification
 *      of how much memory was used.  It is possible to trade memory for
 *      better time performance.  This benchmark should be run in a 32 MB
 *      heap, though we don't currently know how to enforce that uniformly.
 *
 *      Unlike the original Ellis and Kovac benchmark, we do not attempt
 *      measure pause times.  This facility should eventually be added back
 *      in.  There are several reasons for omitting it for now.  The original
 *      implementation depended on assumptions about the thread scheduler
 *      that don't hold uniformly.  The results really measure both the
 *      scheduler and GC.  Pause time measurements tend to not fit well with
 *      current benchmark suites.  As far as we know, none of the current
 *      commercial Java implementations seriously attempt to minimize GC pause
 *      times.
 */

#include <stdio.h>

int kStretchTreeDepth    = 18;      /* about 16Mb */
int kLongLivedTreeDepth  = 16;      /* about 4Mb */
int kArraySize  = 500000;           /* about 4Mb */
int kMinTreeDepth = 4;
int kMaxTreeDepth = 16;

typedef struct Node0 *Node;

struct Node0 {
        Node left;
        Node right;
        int i, j;
};

static Node make_Node(Node l, Node r) {
    Node result = (Node) malloc(sizeof(struct Node0));
    result->left = l;
    result->right = r;
    return result;
}

static Node leaf_Node() {
    return make_Node(0, 0);
}

static void free_Node(Node x) {
    if (x->left)
        free_Node(x->left);
    if (x->right)
        free_Node(x->right);
    free(x);
}

/* Nodes used by a tree of a given size */
static int TreeSize(int i) {
        return ((1 << (i + 1)) - 1);
}

/* Number of iterations to use for a given tree depth */
static int NumIters(int i) {
        return 2 * TreeSize(kStretchTreeDepth) / TreeSize(i);
}

/* Build tree top down, assigning to older objects. */
static void Populate(int iDepth, Node thisNode) {
        if (iDepth<=0) {
                return;
        } else {
                iDepth--;
                thisNode->left  = leaf_Node();
                thisNode->right = leaf_Node();
                Populate (iDepth, thisNode->left);
                Populate (iDepth, thisNode->right);
        }
}

/* Build tree bottom-up */
static Node MakeTree(int iDepth) {
        if (iDepth<=0) {
                return leaf_Node();
        } else {
                return make_Node(MakeTree(iDepth-1),
                                 MakeTree(iDepth-1));
        }
}

static void PrintDiagnostics() {
#if 0
        long lFreeMemory = Runtime.getRuntime().freeMemory();
        long lTotalMemory = Runtime.getRuntime().totalMemory();

        System.out.print(" Total memory available="
                         + lTotalMemory + " bytes");
        System.out.println("  Free memory=" + lFreeMemory + " bytes");
#endif
}

static void TimeConstruction(int depth) {
        long    tStart, tFinish;
        int     iNumIters = NumIters(depth);
        Node    tempTree;
        int     i;

        printf ("Creating %d trees of depth %d\n", iNumIters, depth);
                
        for (i = 0; i < iNumIters; ++i) {
                tempTree = leaf_Node();
                Populate(depth, tempTree);
                free_Node(tempTree);
                tempTree = 0;
        }
        for (i = 0; i < iNumIters; ++i) {
                tempTree = MakeTree(depth);
                free_Node(tempTree);
                tempTree = 0;
        }
}

main() {
        Node    root;
        Node    longLivedTree;
        Node    tempTree;
        double *array;
        int     i;
        int     d;

        printf ("Garbage Collector Test\n");
        printf (" Live storage will peak at %d bytes.\n\n",
                2 * sizeof(struct Node0) * TreeSize(kLongLivedTreeDepth) +
                sizeof(double) * kArraySize);
        printf (" Stretching memory with a binary tree of depth %d\n",
                kStretchTreeDepth);
        PrintDiagnostics();
        
        /* Stretch the memory space quickly */
        tempTree = MakeTree(kStretchTreeDepth);
        free_Node(tempTree);
        tempTree = 0;

        /* Create a long lived object */
        printf (" Creating a long-lived binary tree of depth %d\n",
                kLongLivedTreeDepth);
        longLivedTree = leaf_Node();
        Populate(kLongLivedTreeDepth, longLivedTree);

        /* Create long-lived array, filling half of it */
        printf (" Creating a long-lived array of %d doubles\n",
                kArraySize);
        array = (double *) malloc(kArraySize*sizeof(double));
        for (i = 0; i < kArraySize/2; ++i) {
                array[i] = 1.0/i; /* sic */
        }
        PrintDiagnostics();

        for (d = kMinTreeDepth; d <= kMaxTreeDepth; d += 2) {
                TimeConstruction(d);
        }

        if (longLivedTree == 0 || array[1000] != 1.0/1000)
                printf ("Failed\n");
                /* Fake reference to LongLivedTree */
                /* and array */
                /* to keep them from being optimized away */

        PrintDiagnostics();
}
