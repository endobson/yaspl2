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

(define partition1
  (make-efi-system-partition
    "EFI System Name" 
    #"\xd6\x6c\x8d\xa3\x58\x69\x59\x42\xa8\x16\xca\xc2\xeb\x3e\x70\x58"
    (mebibytes 1)
    (mebibytes 128)
    f32))

(define partition2
  (make-x86-root-partition
    "Linux Name" 
    #"\x60\x45\x3f\x0c\x6d\x8c\xdd\x44\x8c\x5a\xe0\x14\x50\x6c\xea\x45"
    (mebibytes 129)
    (mebibytes 256)
    #f))

(define gpt-disk
  (gpt-disk-add-partition
  (gpt-disk-add-partition
    (make-gpt-disk (mebibytes 1028))
    partition1)
    partition2))


(match-define
  (vector output-path)
  (current-command-line-arguments))

(call-with-output-file output-path 
  (lambda (out)
    (write-gpt-disk gpt-disk out)))
