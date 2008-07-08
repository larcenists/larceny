#!/bin/sh

# This script tries to find the Larceny binaries and to select the correct
# pair.  If LARCENY_ROOT is set, it looks there; otherwise, if the directory
# this script is in is named Scripts, it looks in the parent; otherwise, it
# looks in the same directory as itself.

# You can specify a particular LARCENY_ROOT here:
# LARCENY_ROOT=/usr/local/lib/larceny

# gdb variable controls whether we attempt to invoke larceny under a
# gdb debugger session.  (Set to true, or see gdb-larceny case below)
gdb=false
gdb_command_file=/tmp/gdb-larceny.gdb-commands

# rtopts variable is to control GC options via program name dispatch
# (see larceny-np case below)
rtopts=""

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

    gdb-larceny)        #option
	heap=larceny.heap
	cmd=larceny.bin
	gdb=true
    ;;

    larceny-np)         #option
	cmd=petit-larceny.bin
	heap=petit-larceny.heap
        if test -x $LARCENY_ROOT/$cmd; then
	    :
	else
	    cmd=larceny.bin
	    heap=larceny.heap
	fi
	rtopts="-np"
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

if $gdb ; then
    echo "r $rtopts -heap $heap $@" > $gdb_command_file
    exec "gdb" "$cmd" -command "$gdb_command_file"
else
    exec "$cmd" $rtopts -heap "$heap" "$@"
fi

