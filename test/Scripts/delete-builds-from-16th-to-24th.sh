pushd ~/larcenytest
ls | egrep ^larceny-.*\(1[6-9]\|2[0-4]\)$ | xargs rm -rvf
popd
