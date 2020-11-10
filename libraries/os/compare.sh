#!/bin/zsh

xxd -a $1 >! disk1.xxd
xxd -a $2 >! disk2.xxd
vimdiff disk1.xxd disk2.xxd
