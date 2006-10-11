#!/bin/sh

# This script tries to find the Larceny binaries and to select the correct
# pair.  If LARCENY_ROOT is set, it looks there; otherwise, if the directory
# this script is in is named Scripts, it looks in the parent; otherwise, it
# looks in the same directory as itself.

# You can specify a particular LARCENY_ROOT here:
# LARCENY_ROOT=/usr/local/lib/larceny

if [ -z "$LARCENY_ROOT" ]; then
    # To guess LARCENY_ROOT, we start with the directory containing this
    # script.  If it's a relative path, we make it absolute.  Then, if it ends
    # in Scripts, we chop off that component.

    dir="`dirname "$0"`"
    dir="`( cd $dir; pwd )`"
    [ "`basename "$dir"`" = Scripts ] && dir="`dirname "$dir"`"

    # Now we have LARCENY_ROOT.
    LARCENY_ROOT="$dir"
fi

export LARCENY_ROOT

# Do not remove the #option comments following each case branch; they
# are used for generating help text.

case "`basename "$0"`" in
    petit)              #option
        heap=petit.heap
        cmd=petit.bin
    ;;

    larceny)            #option
	cmd=petit-larceny.bin
	heap=petit-larceny.heap
        if test -x $LARCENY_ROOT/$cmd; then
	    :
	else
	    cmd=larceny.bin
	    heap=larceny.heap
	fi
    ;;

    twobit)             #option
        heap=twobit.heap
        cmd=twobit.bin
        test -x $LARCENY_ROOT/$cmd || cmd=larceny.bin
    ;;

    larceny-r5rs)       #option
        heap=r5rs.heap
        cmd=larceny.bin
    ;;

    *)
        exec 1>&2
        echo "Usage:"
        sed '/)[[:space:]]*#option/!d;s/).*/ LARCENYOPTIONS/' $0
        exit 1
    ;;
esac

cmd="$LARCENY_ROOT/$cmd"
heap="$LARCENY_ROOT/$heap"

if [ ! -f "$cmd" ]; then
    echo "Not found: $cmd" >&2
    exit 1
elif [ ! -x "$cmd" ]; then
    echo "Not executable: $cmd" >&2
    exit 1
elif [ ! -f "$heap" ]; then
    echo "Not found: $heap" >&2
    exit 1
fi

exec "$cmd" -heap "$heap" "$@"

