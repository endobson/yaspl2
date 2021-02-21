#!/bin/zsh

tail -c "+$3" $1 | head -c $4 | xxd -a >! tmp/disk1.xxd
tail -c "+$3" $2 | head -c $4 | xxd -a >! tmp/disk2.xxd
vimdiff tmp/disk1.xxd tmp/disk2.xxd
