#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>

const char *argv0;

void check(int cond)
{
    if (! cond) {
        perror(argv0);
        exit(1);
    }
}

int main(int argc, char **argv, char **env)
{
    size_t size = 1048576;
    int rv, result;
    FILE *f;
    int (*obj)(int, char **, char **, void *, void *);

    argv0 = argv[0];

    if (argc < 2) {
        fprintf(stderr, "Usage: %s OBJFILE ARGS...\n", argv0);
        return 1;
    }

    f = fopen(argv[1], "r");
    check(f != NULL);

    obj = malloc(size);
    check(obj != NULL);

    size = fread(obj, sizeof(char), size, f);
    check(size > 0);

    rv = fclose(f);
    check(rv == 0);

    result = obj(argc - 1, argv + 1, env, (void *)putchar, (void *)getchar);

    free(obj);

    return result;
}
