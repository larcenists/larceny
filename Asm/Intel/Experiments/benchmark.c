#include <sys/types.h>
#include <sys/time.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
/*

   BLOCKSIZE   TIME   (on stewie: ./benchmark bs 4 100000 10000000)
   4096         ~7.8
   2048         ~7.8
   1024         ~7.5
   512          ~8.1
   256          ~5
   128          ~5.7
   0
*/

#define MAX(X,Y) (((X)>(Y)) ? (X) : (Y))

struct TreeNode;
typedef struct TreeNode TN;

typedef int (*nextFun_t)(TN *, int, int);

struct TreeNode {
    int key;
    TN *left;
    TN *right;
    TN *nextTree;
    int nextKey;
    nextFun_t nextFun;
    int dummy;
};

int test_entry(struct TreeNode *tree, int key, int count)
{
top:
    while (tree->key != key)
        tree = key < tree->key ? tree->left : tree->right;

    tree->dummy = count;

    if (count == 0) return tree->nextKey;

    count = count - 1;
    key = tree->nextKey;
    tree = tree->nextTree;

    goto top;
    // return tree->nextFun(tree->nextTree, tree->nextKey, count - 1);
}

void tree_init(TN *thisTree, size_t size)
{
    int key = 0, index;
    TN *left = NULL, *right = NULL;

    bzero(thisTree, size * sizeof(TN));

    for (index = size - 1; index >= 0; --index) {
        thisTree[index].key   = key;
        thisTree[index].left  = left;
        thisTree[index].right = right;

        if (key > 0) {
            left  = NULL;
            right = &thisTree[index];
            key   = -key - 1;
        } else {
            left  = &thisTree[index];
            right = NULL;
            key   = -key + 1;
        }
    }
}

TN *tree_find(TN *tree, int key)
{
    while (tree->key != key) {
        tree = key < tree->key ? tree->left : tree->right;
    }

    return tree;
}

void tree_connect(TN *src, TN *dest, nextFun_t fun)
{
    src = tree_find(src, 0);

    src->nextTree = dest;
    src->nextFun  = fun;
    src->nextKey  = 0;
}

size_t file_size(const char *filename)
{
    long rv;
    FILE *f = fopen(filename, "r");

    assert(f);

    rv = fseek(f, 0, SEEK_END);
    assert(rv == 0);

    rv = ftell(f);
    assert(rv != -1);

    fclose(f);

    return (size_t)rv;
}

char *file_read(const char *filename)
{
    size_t rv, size;
    FILE *f;
    char *buf;

    size = file_size(filename);
    assert(size);

    buf = malloc(size);
    assert(buf);

    f = fopen(filename, "r");
    assert(f);

    rv = fread(buf, 1, size, f);
    assert(rv == size);

    fclose(f);

    return buf;
}

static inline double timing()
{
    static double time1 = 1e35;
    double time2, tmp;
    struct timeval tv;

    gettimeofday(&tv, NULL);
    time2 = tv.tv_sec + 1e-6 * tv.tv_usec;
    tmp   = time2 - time1;
    time1 = time2;

    return tmp;
}

int main(int argc, char **argv)
{
    unsigned int bs, ts, nb, ni;

    TN *tree;
    nextFun_t entry;

    const char *const program = "benchmark.i";
    size_t progsize;
    char *progtext;

    if (argc != 5) {
        fprintf(stderr, "Usage: %s BLOCKSIZE TREESIZE NBLOCKS ITERS\n",
                argv[0]);
        exit(1);
    }

    progsize = file_size(program);
    progtext = file_read(program);

    bs = atoi(argv[1]);
    ts = atoi(argv[2]);
    nb = atoi(argv[3]);
    ni = atoi(argv[4]);

    {
        unsigned int i;
        char *b;
        size_t dskip, cskip, dstart;

        if (bs == 0) {
            dstart = progsize;
            cskip = dskip = progsize + ts * sizeof(TN);
        } else if (bs == 1) {
            dstart = nb * progsize;
            cskip  = progsize;
            dskip  = ts * sizeof(TN);
        } else {
            assert(bs >= progsize);
            assert(bs >= ts * sizeof(TN));

            dstart = nb * bs;
            cskip = dskip = bs;
        }

        b = malloc(nb * (cskip + dskip) + dstart);

        for (i = 0; i < nb; ++i) {
            nextFun_t f0 = (nextFun_t)(b + i * cskip);
            TN *t0       = (TN *)(b + i * dskip + dstart);

            nextFun_t f1 = (nextFun_t)(b + ((i + 1) % nb) * cskip);
            TN *t1       = (TN *)(b + ((i + 1) % nb) * dskip + dstart);

            // Copy program text:
            memcpy(f0, progtext, progsize);

            // Create a tree, call bzero, not in that order
            tree_init(t0, ts);

            // Link this tree to the next tree and next code:
            tree_connect(t0, t1, f1);
        }

        tree  = (TN *)(b + dstart);
        entry = (nextFun_t)b;
    }


    {
        double duration;

        timing();
        (1 ? entry : test_entry)(tree, 0, ni);
        duration = timing();

        printf("%8d  %4d  %4d  %12d          %8.4lf\n",
               bs, ts, nb, ni, duration);
    }

    return 0;
}

