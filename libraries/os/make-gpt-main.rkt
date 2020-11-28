#lang racket/base

(require
  racket/file
  racket/match
  racket/port
  "util.rkt"
  "fat32.rkt"
  "gpt.rkt")

(define f32
  (fat32-add-file
  (fat32-add-directory
  (fat32-add-directory
  (fat32-add-file
  (fat32-add-directory (make-fat32)
    (list #".fseventsd"))
    (list #".fseventsd" #"NO_LOG") #"")
    (list #"EFI"))
    (list #"EFI" #"BOOT"))
    (list #"EFI" #"BOOT" #"BOOTX64.EFI") (file->bytes "tmp/prog.efi")))

(define disk-guid #"\xdb\x0f\xba\x80\x75\xe0\xc5\x47\x93\x25\xd2\x0f\xd0\x17\x5d\x9d")
(define efi-partition-guid #"\xd6\x6c\x8d\xa3\x58\x69\x59\x42\xa8\x16\xca\xc2\xeb\x3e\x70\x58")
(define linux-guid #"\x60\x45\x3f\x0c\x6d\x8c\xdd\x44\x8c\x5a\xe0\x14\x50\x6c\xea\x45")

(define efi-partition
  (make-efi-system-partition "EFI System Name" efi-partition-guid (mebibytes 1) (mebibytes 128) f32))

(define linux-partition
  (make-x86-root-partition "Linux Name" linux-guid (mebibytes 129) (mebibytes 256) #f))

(define gpt-disk
  (gpt-disk-add-partition
  (gpt-disk-add-partition
    (make-gpt-disk disk-guid (mebibytes 1024))
    efi-partition)
    linux-partition))

(match-define
  (vector output-path)
  (current-command-line-arguments))

(call-with-output-file output-path 
  (lambda (out)
    (write-gpt-disk gpt-disk out)))
