pushd ~/larcenytest
ls | egrep \(^larceny-.*1[6-9]\|2[0-9]\|3[0-1]\)$ | xargs rm -rvf
popd
