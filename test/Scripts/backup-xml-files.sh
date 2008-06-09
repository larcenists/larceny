#! /usr/sh

for f in */*.xml ; do 
    echo $f; 
    mkdir `dirname dart-xml-logs/$f`; 
    cp -v $f dart-xml-logs/$f; 
done
