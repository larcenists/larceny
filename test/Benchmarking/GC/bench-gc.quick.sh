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
    OUTPUT=bench-quick-auto-log.$DATE.log
else 
    OUTPUT=$1
fi

TMPOUTPUT=tmp.$OUTPUT

# all fasls exist check
f( ) {
  if [ -z $1.fasl ] ; then
     echo "You need to build $1.fasl before running this script" >&2
     echo "One way is to run the Scheme program "                >&2
     echo "compile-files.sch in this directory."                 >&2
  fi
}

echo "\"running LARCENY=${LARCENY} and sending stat sexps to $OUTPUT\""
echo "\"running LARCENY=${LARCENY} and sending stat sexps to $OUTPUT\"" > $OUTPUT
echo "\"$DATE\""                                                       >> $OUTPUT

# The easiest thing to do is probably to keep using the existing sch
# files and their individual entry points.  The only problem there is
# that the standard run-benchmark prints the timing results, when we
# would prefer to extract those results and print them separately...
# 
# So to work around, I am replacing run-benchmark with one that
# stashes away the stats data for later output.
# (I would set! run-with-stats, but the environment of run-benchmark
# has been sealed off and so mutations to run-with-stats are not
# observed by run-benchmark)

# all fasls exist check
f( ) {
  if [ ! -e $1.fasl ] ; then
     echo "You need to build $1.fasl before running this script" >&2
     echo "One way is to run the Scheme program "                >&2
     echo "compile-files.sch in this directory."                 >&2
  fi
}
f overwrite-run-benchmark; 
f earley; f gcbench; f nboyer; f sboyer; f perm; f twobit; f gcold;

AFTER_BENCH="(call-with-output-file \"$TMPOUTPUT\"  \
               (lambda (p)                          \
                 (load \"parse-stats-output.sch\")  \
                 (let ((f (lambda (s x)             \
                            (display \"    \" p)    \
                            (write (list s x) p)    \
                            (newline p))))          \
                    (f (quote last-stashed-stats)   \
                       *last-stashed-stats*)        \
                    (f (quote stats-dump)           \
                       (stats-read)))))"

g( ) {
    echo "($2 $1"  >> $OUTPUT && \
    echo "(begin $1 (values))" ${AFTER_BENCH} | \
        ${LARCENY} -- overwrite-run-benchmark.fasl $2.fasl && \
    cat $TMPOUTPUT >> $OUTPUT && \
    echo ") "      >> $OUTPUT && \
    echo           >> $OUTPUT && \
    rm $TMPOUTPUT
}

# g '(sboyer-benchmark 6 5)'   sboyer
# g '(twobit-benchmark (quote long) 5)' twobit
# g '(pueue-benchmark 1000 1000000 50 50)' pueue3
# g '(earley-benchmark 10 20)' earley
# g '(earley-benchmark 13 20)' earley
# g '(nboyer-benchmark 5 5)'   nboyer
g '(MpermNKL-benchmark 200 9 10  1)' perm

$LARCENY -- parse-stats-output.sch            \
    -e "(process-and-print-log \"$OUTPUT\" )" \
    -e "(exit)"
