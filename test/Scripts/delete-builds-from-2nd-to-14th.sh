pushd ~/larcenytest
ls | egrep \(^larceny-.*0[2-9]\|1[1-4]\)$ | xargs rm -rvf
popd
