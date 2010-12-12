for f in $@; do
    ../../../larceny -- parse-stats-output.sch \
        -e "(process-and-print-log \"$f\")"    \
        -e "(exit)"
done
