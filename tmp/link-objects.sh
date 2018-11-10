#!/bin/bash
cd bazel-obj;
for prog in `ls *.o  | sed -e 's/.o$//'`;
do
  ld "${prog}.o" -o "${prog}.exe" && \
  strip "${prog}.exe" -N "${prog}.o" && \
  xxd "${prog}.exe" >! "${prog}.xxd";
done;
