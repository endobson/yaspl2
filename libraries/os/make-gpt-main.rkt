#lang racket/base

(require
  racket/bytes
  racket/dict
  racket/file
  racket/hash
  racket/list
  racket/match
  racket/port
  racket/set
  (for-syntax
    racket/base
    syntax/parse)
  "util.rkt"
  "fat32.rkt")



(define-syntax (define-sector stx)
  (syntax-parse stx
    [(_ name:id bodies:expr ...)
     #'(define-section name #:size 512 bodies ...)]))

(define-syntax (define-partition-entry stx)
  (syntax-parse stx
    [(_ name:id bodies:expr ...)
     #'(define-section name #:size 128 bodies ...)]))


(define-sector mbr-sector
  (define partition-entry
    (bytes-append
      #"\x00"             ; Status
      #"\x00\x02\x00"     ; CHS Start
      #"\xee"             ; Partition type (GPT protective MBR)
      #"\x8a\x08\x82"     ; CHS end
      #"\x01\x00\x00\x00" ; LBA start
      #"\xff\xff\x1f\x00" ; Number of sectors
      ))

  (bytes-copy! mbr-sector #x1be partition-entry)
  (bytes-copy! mbr-sector #x1fe #"\x55\xaa"))


(define (crc32 data)
  (bitwise-xor
   (for/fold ([accum #xFFFFFFFF])
     ([byte  (in-bytes data)])
     (for/fold ([accum (bitwise-xor accum byte)])
               ([num (in-range 0 8)])
       (bitwise-xor (quotient accum 2)
                    (* #xEDB88320 (bitwise-and accum 1)))))
   #xFFFFFFFF))

(define-partition-entry partition-entry1
  ; EFI System partition
  (bytes-copy! partition-entry1 0  #"\x28\x73\x2a\xc1\x1f\xf8\xd2\x11\xba\x4b\x00\xa0\xc9\x3e\xc9\x3b")
  ; Unique GUID for partition
  (bytes-copy! partition-entry1 16 #"\xd6\x6c\x8d\xa3\x58\x69\x59\x42\xa8\x16\xca\xc2\xeb\x3e\x70\x58")
  ;; Starting LBA
  (integer->integer-bytes #x0800 8 #f #f partition-entry1 32)
  ;; Ending LBA
  (integer->integer-bytes #x0407ff 8 #f #f partition-entry1 40)
  ;; Attributes
  (bytes-copy! partition-entry1 48 #"\x00\x00\x00\x00")
  ;; Name
  (define name #"EFI System Name")
  (for ([i (bytes-length name)])
    (bytes-set! partition-entry1 (+ 56 (* 2 i)) (bytes-ref name i))))

(define-partition-entry partition-entry2
  ; x86 Root partition
  (bytes-copy! partition-entry2 0  #"\x40\x95\x47\x44\x97\xf2\xb2\x41\x9a\xf7\xd1\x31\xd5\xf0\x45\x8a")
  ; Unique GUID for partition
  (bytes-copy! partition-entry2 16 #"\x60\x45\x3f\x0c\x6d\x8c\xdd\x44\x8c\x5a\xe0\x14\x50\x6c\xea\x45")
  ;; Starting LBA
  (integer->integer-bytes #x040800 8 #f #f partition-entry2 32)
  ;; Ending LBA
  (integer->integer-bytes #x0c07ff 8 #f #f partition-entry2 40)
  ;; Attributes
  (bytes-copy! partition-entry2 48 #"\x00\x00\x00\x00")
  (define name #"Linux Name")
  (for ([i (bytes-length name)])
    (bytes-set! partition-entry2 (+ 56 (* 2 i)) (bytes-ref name i)))
  partition-entry2)


(define-partition-entry empty-partition-entry)

;; Partition entries take up 32 sectors
(define partition-entries
  (bytes-append*
    partition-entry1
    partition-entry2
    (make-list 126 empty-partition-entry)))

(define-sector blank-sector)

(define-sector gpt-sector
  (bytes-copy! gpt-sector 0  #"EFI PART")                         ; Signature
  (bytes-copy! gpt-sector 8  #"\x00\x00\x01\x00")                 ; Revision
  (bytes-copy! gpt-sector 12 #"\x5c\x00\x00\x00")                 ; Header Size (92)
                                                                  ; CRC (4 bytes)
                                                                  ; Reserved (4 bytes)
  (bytes-copy! gpt-sector 24 #"\x01\x00\x00\x00\x00\x00\x00\x00") ; Current LBA
  (bytes-copy! gpt-sector 32 #"\xff\xff\x1f\x00\x00\x00\x00\x00") ; Backup LBA
  (bytes-copy! gpt-sector 40 #"\x22\x00\x00\x00\x00\x00\x00\x00") ; First usable LBA
  (bytes-copy! gpt-sector 48 #"\xde\xff\x1f\x00\x00\x00\x00\x00") ; Last usable LBA
  (bytes-copy! gpt-sector 56                                      ; Disk GUID
    #"\xdb\x0f\xba\x80\x75\xe0\xc5\x47\x93\x25\xd2\x0f\xd0\x17\x5d\x9d")
  (bytes-copy! gpt-sector 72 #"\x02\x00\x00\x00\x00\x00\x00\x00") ; Start LBA of partition entries
  (bytes-copy! gpt-sector 80 #"\x80\x00\x00\x00")                 ; Number of partition entries
  (bytes-copy! gpt-sector 84 #"\x80\x00\x00\x00")                 ; Size of partition entries
  (integer->integer-bytes (crc32 partition-entries) 4 #f #f gpt-sector 88) ; CRC of partition entries

  ; Compute the CRC and put it in the right place
  (integer->integer-bytes (crc32 (subbytes gpt-sector 0 92)) 4 #f #f gpt-sector 16))

(define-sector gpt-sector2
  ;; Start with the original gpt sector
  (bytes-copy! gpt-sector2 0 gpt-sector)

  ; Flipped current and backup LBA
  (bytes-copy! gpt-sector2 24 #"\xff\xff\x1f\x00\x00\x00\x00\x00")
  (bytes-copy! gpt-sector2 32 #"\x01\x00\x00\x00\x00\x00\x00\x00")

  ; Start LBA of partition entries
  (bytes-copy! gpt-sector2 72 #"\xdf\xff\x1f\x00\x00\x00\x00\x00")

  ; Zero the CRC, and then recompute it.
  (integer->integer-bytes 0 4 #f #f gpt-sector2 16)
  (integer->integer-bytes (crc32 (subbytes gpt-sector2 0 92)) 4 #f #f gpt-sector2 16))

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

(match-define
  (vector output-path)
  (current-command-line-arguments))

(call-with-output-file output-path
  (lambda (out)
    (define (mebibytes n) (* n #x100000))
    (define gpt-header
      (bytes-append
        mbr-sector
        gpt-sector
        partition-entries))
    (define gpt-footer
      (bytes-append
        partition-entries
        gpt-sector2))


    (write-all-bytes gpt-header out)
    (write-all-bytes (make-bytes (- (mebibytes 1) (bytes-length gpt-header))) out)
    ;; 1MiB (2048 sectors)

    (write-all-bytes (serialize-fat32 f32 #:volume-size (mebibytes 128)) out)

    (write-all-bytes (make-bytes (- (mebibytes (- 1024 129)) (bytes-length gpt-footer))) out)
    (write-all-bytes gpt-footer out)))

