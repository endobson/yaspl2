#!/bin/zsh

xxd $1 >! tmp/disk1.xxd
xxd $2 >! tmp/disk2.xxd
vimdiff tmp/disk1.xxd tmp/disk2.xxd
