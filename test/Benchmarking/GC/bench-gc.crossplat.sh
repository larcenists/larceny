#! /usr/bin/env bash

# Larceny command check
if [ -z "$LARCENY" ] ; then
  echo "You must set the environment variable LARCENY to a command string."  >&2
  echo "The command string can include options to pass to the runtime."      >&2
  echo "The command string should not include the -- style of passing arguments to the repl." >&2
  exit
fi

DATE=`date +"%Y%b%d-at-%H-%M-%S"`

if [ -z $1 ] ; then
    OUTPUT=bench-crossplat-log.$DATE.log
    HUMAN_OUTPUT=bench-crossplat-std.$DATE.log
else 
    OUTPUT=$1
    if [ -z $2 ] ; then 
        HUMAN_OUTPUT=bench-crossplat-std.$DATE.log
    else
        HUMAN_OUTPUT=$2
    fi
fi

TMPOUTPUT=tmp.$OUTPUT

echo "\"running LARCENY=${LARCENY} and sending stat sexps to $OUTPUT\"" | tee $HUMAN_OUTPUT
echo "\"running LARCENY=${LARCENY} and sending stat sexps to $OUTPUT\"" > $OUTPUT
echo "\"$DATE\""                                                       >> $OUTPUT

function c () {
    FILE=$1
    CCMD="../../../larceny -stopcopy -nobanner -- overwrite-run-benchmark.sch prefix-larceny.scm num-iters.scm $FILE -e (dump-bmark-heap) -e (exit)"
    echo $CCMD
    $CCMD > /dev/null
}

function bm( ) {
    BENCHMARKNAME=$1
    BPATH="../../../test/Benchmarking/CrossPlatform/src/$BENCHMARKNAME.scm"
    c $BPATH

    echo "($BENCHMARKNAME (main)"  >> $OUTPUT
    echo "(begin (main) (values))" "(dump-stashed-and-current-stats \"$TMPOUTPUT\")" | \
        ${LARCENY} -heap bmark.heap | tee -a $HUMAN_OUTPUT
    cat $TMPOUTPUT >> $OUTPUT
    echo ") "      >> $OUTPUT
    echo           >> $OUTPUT
    rm $TMPOUTPUT
}

ALL_CP_BENCHMARKS="ack array1 boyer browse cat compiler conform cpstak ctak \
    dderiv deriv destruc diviter divrec dynamic earley fail fft fib fibc fibfp fpsum \
    graphs lattice matrix maze mazefun mbrot nbody nboyer nqueens ntakl nucleic \
    paraffins parsing perm9 peval pi pnpoly primes puzzle quicksort ray \
    sboyer scheme simplex slatex smlboyer string sum sum1 sumfp sumloop \
    tail tak takl tfib trav1 trav2 triangl wc"

SOME_CP_BENCHMARKS="ack array1 cat"

for bmark in $ALL_CP_BENCHMARKS ; do echo $bmark; bm $bmark ; done

$LARCENY -- parse-stats-output.sch            \
    -e "(process-and-print-log \"$OUTPUT\" )" \
    -e "(exit)"
