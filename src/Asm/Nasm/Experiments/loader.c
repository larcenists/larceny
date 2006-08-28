#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

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
    int rv, fd, result;
    struct stat sb;
    int (*obj)(int, char **, char **, void *, void *);

    argv0 = argv[0];

    if (argc < 2) {
        fprintf(stderr, "Usage: %s OBJFILE ARGS...\n", argv0);
        return 1;
    }

    fd = open(argv[1], O_RDONLY);
    check(fd >= 0);

    rv = fstat(fd, &sb);
    check(rv == 0);

    obj = mmap(NULL, (size_t)sb.st_size, PROT_EXEC|PROT_READ|PROT_WRITE,
               MAP_PRIVATE, fd, 0);
    check(obj != NULL);

    result = obj(argc - 1, argv + 1, env, (void *)putchar, (void *)getchar);

    rv = munmap(obj, (size_t)sb.st_size);
    check(rv == 0);

    return result;
}
