#!/usr/bin/env zsh
for prog in `ls prog*.s | sed -e 's/.s//'`;
do
  as "${prog}.s" -o "${prog}.o";
  xxd "${prog}.o" >! "${prog}.xxd";
done;
