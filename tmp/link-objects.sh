#!/bin/bash
cd bazel-obj;
for prog in `ls *.o  | sed -e 's/.o$//'`;
do
  temp=$(mktemp)
  ld.lld-6.0 "${prog}.o" -o "${temp}" --gc-sections && \
  mv "${temp}" "${prog}.exe" && \
  strip -D "${prog}.exe" -N "${prog}.o" && \
  xxd "${prog}.exe" >! "${prog}.xxd";
done;
