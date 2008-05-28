pushd ~/larcenytest
ls | egrep ^larceny-.*\(2[5-9]\|3[0-1]\)$ | xargs rm -rvf
popd
