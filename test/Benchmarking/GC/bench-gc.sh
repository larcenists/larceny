#! /usr/bin/env bash
# This is a helper script for gathering GC statistics 
# To use it, you need to pass the Larceny invocation string
# via an environment variable, e.g. in the following manner:
#
# Default Collector:
# % LARCENY="../../../larceny" ./bench-gc.sh
#
# Garbage First, 20 regions; default options for mark bitmap construction frequency and wave-off parameters:
# % LARCENY="../../../larceny -regions 20" ./bench-gc.sh
#
# Garbage First, 20 regions; mark bitmap constructed every 50 collections, wave-off after 8000 SSB entries:
# % LARCENY="../../../larceny -regions 20 -mark_period 50 -summarize_limit 8000" ./bench-gc.sh
#

# Larceny command check
if [ -z "$LARCENY" ] ; then
  echo "You must set the environment variable LARCENY to a command string."  >&2
  echo "The command string can include options to pass to the runtime."      >&2
  echo "The command string should not include the -- style of passing arguments to the repl." >&2
  exit
fi

# all fasls exist check
f( ) {
  if [ -z $1.fasl ] ; then
     echo "You need to build $1.fasl before running this script" >&2
     echo "One way is to run the Scheme program "                >&2
     echo "compile-files.sch in this directory."                 >&2
  fi
}
f earley; f gcbench; f nboyer; f sboyer; f perm; f twobit; f gcold;

echo "running LARCENY=${LARCENY}"

# Will's list
# dynamic earley graphs perm9 nboyer sboyer gcbench gcold softscheme twobit

# ICFP '02 list
# ( stats gathered:                                                      )
# ( Allocation volume, Peak live (est.), Promo rate, Mut. time (s+c),    )
# (   Major GC (msec), Minor GC (msec), Ratio                            )
# ( and for {2GEN,3GEN,3ROF}: Mark/cons ratio, Major GCs, Rem. Set size  )
#                 
# 5earley:12 5earley:13 5earley:14
# gcbench:5 
# gcold:25,1,0 gcold:25,1,1000 gcold:25,10,100 gcold:100,1,0 gcold:100,1,1000
# nboyer:3 nboyer:4 nboyer:5 nboyer:6
# 5nboyer:3 5nboyer:4 5nboyer:5
# 5sboyer:4 5sboyer:5 sboyer:6
# perm:200,8,10,1 perm:25,8,10,8 perm:200,9,10,1 
# perm:25,9,10,8 perm:25,9,20,16 perm:400,9,20,1
# twobitlong
# twobitshort
# 5twobitshort

# ISMM 2008 list
# 20earley:13
# gcbench:5:20
# 5nboyer:5
# 5nboyer:6
# 5sboyer:6
# 200perm9:10:1
# 400perm9:20:1
# 5twobit:long
# gcold:100:0:1:0:800
# gcold:100:0:1:1000:800

AFTER_BENCH='(let ((m (memstats))) (newline) (display "{Max words, ") (for-each (lambda (nm+idx) (display (car nm+idx)) (display (vector-ref m (cadr nm+idx)))) (quote (("Mem: "  42) (" Heap: " 27) (" Remset: " 31) (" Rts: " 32) (" Waste: " 33)))) (display "}") (newline) (for-each (lambda (name+idxs) (let ((name (car name+idxs)) (idxs (cadr name+idxs))) (pretty-print (quasiquote ((unquote name) (unquote (map (lambda (idx) (vector-ref m idx)) idxs))))))) (quote (("MREFINE" (60 61 62 58 59)) ("MINORGC" (65 66 67 63 64)) ("MAJORGC" (70 71 72 68 69)) ("SUMRIZE" (75 76 77 73 74)) ("MINORS" (79)) ("MAJORS" (80)) ("SUMRIZES" (81)) ("MINORUN" (82))))))'
# AFTER_BENCH='(let ((m (memstats))) (for-each (lambda (nm+idx) (display (car nm+idx)) (display (vector-ref m (cadr nm+idx))) (newline)) (quote (("Max Mem: "  42) ("Max Heap: " 27) ("Max Remset: " 31) ("Max Rts: " 32) ("Max Waste: " 33)))) (for-each (lambda (idx) (pretty-print (vector-ref m idx))) (list 56 57 58)))'

# echo '(earley-benchmark 12 5)' ${AFTER_BENCH}     | ${LARCENY} -- earley.fasl
# echo '(earley-benchmark 13 5)' ${AFTER_BENCH}     | ${LARCENY} -- earley.fasl
# echo '(earley-benchmark 14 5)' ${AFTER_BENCH}     | ${LARCENY} -- earley.fasl
echo '(earley-benchmark 13 20)' ${AFTER_BENCH}     | ${LARCENY} -- earley.fasl

echo '(gc-benchmark 5 20)' ${AFTER_BENCH} | ${LARCENY} -- gcbench.fasl

# echo '(nboyer-benchmark 3)' ${AFTER_BENCH}      | ${LARCENY} -- nboyer.fasl
# echo '(nboyer-benchmark 4)' ${AFTER_BENCH}      | ${LARCENY} -- nboyer.fasl
# echo '(nboyer-benchmark 5)' ${AFTER_BENCH}      | ${LARCENY} -- nboyer.fasl
# echo '(nboyer-benchmark 6)' ${AFTER_BENCH}      | ${LARCENY} -- nboyer.fasl

# echo '(nboyer-benchmark 3 5)' ${AFTER_BENCH}    | ${LARCENY} -- nboyer.fasl
# echo '(nboyer-benchmark 4 5)' ${AFTER_BENCH}    | ${LARCENY} -- nboyer.fasl
echo '(nboyer-benchmark 5 5)' ${AFTER_BENCH}    | ${LARCENY} -- nboyer.fasl

# echo '(sboyer-benchmark 4 5)' ${AFTER_BENCH}    | ${LARCENY} -- sboyer.fasl
# echo '(sboyer-benchmark 5 5)' ${AFTER_BENCH}    | ${LARCENY} -- sboyer.fasl
echo '(sboyer-benchmark 6 5)' ${AFTER_BENCH}    | ${LARCENY} -- sboyer.fasl

# perm:M,N:K:L
# echo '(MpermNKL-benchmark 200 8 10  1)' ${AFTER_BENCH} | ${LARCENY} -- perm.fasl
# echo '(MpermNKL-benchmark 25  8 10  8)' ${AFTER_BENCH} | ${LARCENY} -- perm.fasl
echo '(MpermNKL-benchmark 200 9 10  1)' ${AFTER_BENCH} | ${LARCENY} -- perm.fasl
# echo '(MpermNKL-benchmark 25  9 10  8)' ${AFTER_BENCH} | ${LARCENY} -- perm.fasl
# echo '(MpermNKL-benchmark 25  9 20 16)' ${AFTER_BENCH} | ${LARCENY} -- perm.fasl
echo '(MpermNKL-benchmark 400 9 20  1)' ${AFTER_BENCH} | ${LARCENY} -- perm.fasl

# echo '(twobit-benchmark (quote long))'  ${AFTER_BENCH} | ${LARCENY} -- twobit.fasl
# echo '(twobit-benchmark (quote short))' ${AFTER_BENCH} | ${LARCENY} -- twobit.fasl
# echo '(twobit-benchmark (quote short) 5)' ${AFTER_BENCH} | ${LARCENY} -- twobit.fasl
echo '(twobit-benchmark (quote long) 5)' ${AFTER_BENCH} | ${LARCENY} -- twobit.fasl

# echo '(GCOld 25  0 1  0 200)'    ${AFTER_BENCH}  | ${LARCENY} -- gcold.fasl
# echo '(GCOld 25  0 1  1000 200)' ${AFTER_BENCH}  | ${LARCENY} -- gcold.fasl
# echo '(GCOld 25  0 10 1000 200)' ${AFTER_BENCH}  | ${LARCENY} -- gcold.fasl
echo '(GCOld 100 0 1  0    800)' ${AFTER_BENCH}  | ${LARCENY} -- gcold.fasl
echo '(GCOld 100 0 1  1000 800)' ${AFTER_BENCH}  | ${LARCENY} -- gcold.fasl

