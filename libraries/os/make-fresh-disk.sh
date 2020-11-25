#!/bin/bash

set -e
set -x

rm -f tmp/fresh-disk.img
mkfile -n 1g tmp/fresh-disk.img
sgdisk -g \
       -U "80ba0fdb-e075-47c5-9325-d20fd0175d9d" \
       \
       -n 1:1M:+128M \
       -t 1:C12A7328-F81F-11D2-BA4B-00A0C93EC93B \
       -u 1:a38d6cd6-6958-4259-a816-cac2eb3e7058 \
       -c 1:"EFI System Name" \
       \
       -n 2:0:+256M \
       -t 2:44479540-F297-41B2-9AF7-D131D5F0458A \
       -u 2:0c3f4560-8c6d-44dd-8c5a-e014506cea45 \
       -c 2:"Linux Name" \
       \
       tmp/fresh-disk.img

DISKS="$(hdiutil attach tmp/fresh-disk.img -nomount)"
EXPECTED_DISKS="\
/dev/disk2          	GUID_partition_scheme          	
/dev/disk2s1        	EFI                            	
/dev/disk2s2        	44479540-F297-41B2-9AF7-D131D5F	"

if [ "$DISKS" != "$EXPECTED_DISKS" ]
then
  echo "DISKS are not as expected"
  exit 1
fi;

newfs_msdos -F 32 -I "0xDEADABBA" /dev/disk2s1

mkdir -p tmp/mount
mount_msdos /dev/disk2s1 tmp/mount
mkdir -p tmp/mount/.fseventsd
# TODO make this lower case (no_log)
touch tmp/mount/.fseventsd/NO_LOG
mkdir -p tmp/mount/EFI/BOOT
cp tmp/prog.efi tmp/mount/EFI/BOOT/BOOTX64.EFI


touch -t 202001021234.56 tmp/mount/EFI/BOOT/BOOTX64.EFI
SetFile -d "1/2/2020 12:34:56 PM" tmp/mount/EFI/BOOT/BOOTX64.EFI
touch -t 202001021234.56 tmp/mount/EFI/BOOT
SetFile -d "1/2/2020 12:34:56 PM" tmp/mount/EFI/BOOT
touch -t 202001021234.56 tmp/mount/EFI
SetFile -d "1/2/2020 12:34:56 PM" tmp/mount/EFI
touch -t 202001021234.56 tmp/mount/.fseventsd/NO_LOG
SetFile -d "1/2/2020 12:34:56 PM" tmp/mount/.fseventsd/NO_LOG
touch -t 202001021234.56 tmp/mount/.fseventsd
SetFile -d "1/2/2020 12:34:56 PM" tmp/mount/.fseventsd

umount tmp/mount

# Overwrite timestamps that I cannot figure out how to update
# /FSEVEN~1
echo -n -e '\x00\x5c\x64\x22\x50\x22\x50' | \
  dd bs=1 count=7 conv=notrunc seek="$((0x2fc42d))" if=/dev/stdin of=tmp/fresh-disk.img
echo -n -e '\x5c\x64\x22\x50' | \
  dd bs=1 count=4 conv=notrunc seek="$((0x2fc436))" if=/dev/stdin of=tmp/fresh-disk.img
# /EFI
echo -n -e '\x00\x5c\x64\x22\x50\x22\x50' | \
  dd bs=1 count=7 conv=notrunc seek="$((0x2fc44d))" if=/dev/stdin of=tmp/fresh-disk.img
echo -n -e '\x5c\x64\x22\x50' | \
  dd bs=1 count=4 conv=notrunc seek="$((0x2fc456))" if=/dev/stdin of=tmp/fresh-disk.img

# /.fseventsd/.
echo -n -e '\x00' | \
  dd bs=1 count=1 conv=notrunc seek="$((0x2fc60d))" if=/dev/stdin of=tmp/fresh-disk.img
# /.fseventsd/..
echo -n -e '\x00\x5c\x64\x22\x50\x22\x50' | \
  dd bs=1 count=7 conv=notrunc seek="$((0x2fc62d))" if=/dev/stdin of=tmp/fresh-disk.img
echo -n -e '\x5c\x64\x22\x50' | \
  dd bs=1 count=4 conv=notrunc seek="$((0x2fc636))" if=/dev/stdin of=tmp/fresh-disk.img
# /.fseventsd/NO_LOG
echo -n -e '\x00\x5c\x64\x22\x50\x22\x50' | \
  dd bs=1 count=7 conv=notrunc seek="$((0x2fc64d))" if=/dev/stdin of=tmp/fresh-disk.img
echo -n -e '\x5c\x64\x22\x50' | \
  dd bs=1 count=4 conv=notrunc seek="$((0x2fc656))" if=/dev/stdin of=tmp/fresh-disk.img


# /EFI/.
echo -n -e '\x00' | \
  dd bs=1 count=1 conv=notrunc seek="$((0x2fc80d))" if=/dev/stdin of=tmp/fresh-disk.img
# /EFI/..
echo -n -e '\x00\x5c\x64\x22\x50\x22\x50' | \
  dd bs=1 count=7 conv=notrunc seek="$((0x2fc82d))" if=/dev/stdin of=tmp/fresh-disk.img
echo -n -e '\x5c\x64\x22\x50' | \
  dd bs=1 count=4 conv=notrunc seek="$((0x2fc836))" if=/dev/stdin of=tmp/fresh-disk.img
# /EFI/BOOT
echo -n -e '\x00\x5c\x64\x22\x50\x22\x50' | \
  dd bs=1 count=7 conv=notrunc seek="$((0x2fc84d))" if=/dev/stdin of=tmp/fresh-disk.img
echo -n -e '\x5c\x64\x22\x50' | \
  dd bs=1 count=4 conv=notrunc seek="$((0x2fc856))" if=/dev/stdin of=tmp/fresh-disk.img

# /EFI/BOOT/.
echo -n -e '\x00' | \
  dd bs=1 count=1 conv=notrunc seek="$((0x2fca0d))" if=/dev/stdin of=tmp/fresh-disk.img
# /EFI/BOOT/..
echo -n -e '\x00\x5c\x64\x22\x50\x22\x50' | \
  dd bs=1 count=7 conv=notrunc seek="$((0x2fca2d))" if=/dev/stdin of=tmp/fresh-disk.img
echo -n -e '\x5c\x64\x22\x50' | \
  dd bs=1 count=4 conv=notrunc seek="$((0x2fca36))" if=/dev/stdin of=tmp/fresh-disk.img
# /EFI/BOOT/BOOTX64.EFI
echo -n -e '\x00' | \
  dd bs=1 count=1 conv=notrunc seek="$((0x2fca4d))" if=/dev/stdin of=tmp/fresh-disk.img


hdiutil detach /dev/disk2
