#lang racket/base

(require
  racket/bytes
  racket/list
  racket/match
  "fat32.rkt"
  "util.rkt")

(provide
  crc32
  make-gpt-disk
  gpt-disk-add-partition
  write-gpt-disk

  make-efi-system-partition
  make-x86-root-partition)

(define (crc32 data)
  (bitwise-xor
   (for/fold ([accum #xFFFFFFFF])
     ([byte  (in-bytes data)])
     (for/fold ([accum (bitwise-xor accum byte)])
               ([num (in-range 0 8)])
       (bitwise-xor (quotient accum 2)
                    (* #xEDB88320 (bitwise-and accum 1)))))
   #xFFFFFFFF))

; Standard partition types
(define efi-system-partition-guid #"\x28\x73\x2a\xc1\x1f\xf8\xd2\x11\xba\x4b\x00\xa0\xc9\x3e\xc9\x3b")
(define x86-root-partition-guid #"\x40\x95\x47\x44\x97\xf2\xb2\x41\x9a\xf7\xd1\x31\xd5\xf0\x45\x8a")

;; Structured gpt disks
(struct gpt-disk (size guid partitions))

(struct partition (name guid type-guid start size contents))

(define (make-gpt-disk guid size)
  (gpt-disk size guid empty))

(define (gpt-disk-add-partition disk p)
  (match disk
    [(gpt-disk size guid partitions)
     (gpt-disk size guid (append partitions (list p)))]))

(define (make-efi-system-partition name guid start size contents)
  (partition name guid efi-system-partition-guid start size contents))
(define (make-x86-root-partition name guid start size contents)
  (partition name guid x86-root-partition-guid start size contents))

; Serialize partition entries
(define (make-partition-entry name guid type-guid start size)
  (make-section partition-entry #:size 128
    (bytes-copy!       partition-entry 0 type-guid)               ; GUID for partition type
    (bytes-copy!       partition-entry 16 guid)                   ; Unique GUID for partition
    (bytes-set!/u64-le partition-entry 32 start)                  ; Starting LBA
    (bytes-set!/u64-le partition-entry 40 (sub1 (+ start size)))  ; Ending LBA
    (bytes-set!/u64-le partition-entry 48 0)                      ; Attributes
    (bytes-copy!       partition-entry 56 (ascii->utf-16 name)))) ; Partition name

(define (partition->partition-entry p)
  (match-define (partition name guid type-guid start size _) p)
  (define lba-start (quotient start 512))
  (define lba-size (quotient size 512))
  (make-partition-entry name guid type-guid lba-start lba-size))


;; GPT header/footer sections
(define (gpt-disk->header/footer g)

  ;; Standard MBR sector
  (define-section mbr-sector #:size 512
    ;; TODO figure out how to change CHS for different disk sizes
    (define partition-entry
      (bytes-append
        #"\x00"             ; Status
        #"\x00\x02\x00"     ; CHS Start
        #"\xee"             ; Partition type (GPT protective MBR)
        #"\x8a\x08\x82"     ; CHS end
        ))

    (bytes-copy!       mbr-sector #x1be partition-entry)
    (bytes-set!/u32-le mbr-sector #x1c6 1)                                ; LBA start
    (bytes-set!/u32-le mbr-sector #x1ca (sub1 (/ (gpt-disk-size g) 512))) ; Number of sectors

    (bytes-copy!       mbr-sector #x1fe #"\x55\xaa"))


  ; Number of possible partition
  (define max-number-of-partitions 128)
  (define partition-entry-size 128)

  (define partitions (gpt-disk-partitions g))
  (define partition-entries
    (bytes-append*
      (append (map partition->partition-entry partitions)
              (list (make-bytes (* (- max-number-of-partitions (length partitions)) 128))))))

  (define primary-header-lba 1)
  (define backup-header-lba (sub1 (/ (gpt-disk-size g) 512)))
  (define num-partition-sectors (/ (* max-number-of-partitions partition-entry-size) 512))

  ;; TODO fix sizes
  (define-section gpt-sector #:size 512
    (bytes-copy!       gpt-sector 0  #"EFI PART")                         ; Signature
    (bytes-copy!       gpt-sector 8  #"\x00\x00\x01\x00")                 ; Revision
    (bytes-set!/u32-le gpt-sector 12 92)                                  ; Header Size (92)
                                                                          ; CRC (4 bytes)
                                                                          ; Reserved (4 bytes)
    (bytes-set!/u64-le gpt-sector 24 primary-header-lba)                  ; Current LBA of header
    (bytes-set!/u64-le gpt-sector 32 backup-header-lba)                   ; Backup LBA of header
    (bytes-set!/u64-le gpt-sector 40 (add1 (+ primary-header-lba          ; First usable LBA
                                              num-partition-sectors)))
    (bytes-set!/u64-le gpt-sector 48 (sub1 (- backup-header-lba           ; Last usable LBA
                                              num-partition-sectors)))
    (bytes-copy!       gpt-sector 56 (gpt-disk-guid g))                   ; Disk GUID
    (bytes-set!/u64-le gpt-sector 72 (add1 primary-header-lba))           ; Start LBA of partition entries
    (bytes-set!/u64-le gpt-sector 80 max-number-of-partitions)            ; Number of partition entries
    (bytes-set!/u64-le gpt-sector 84 partition-entry-size)                ; Size of partition entries
    (bytes-set!/u32-le gpt-sector 88 (crc32 partition-entries))           ; CRC of partition entries

    ; Compute the CRC and put it in the right place
    (bytes-set!/u32-le gpt-sector 16 (crc32 (subbytes gpt-sector 0 92))))

  (define-section gpt-sector2 #:size 512
    ;; Start with the original gpt sector
    (bytes-copy! gpt-sector2 0 gpt-sector)

    ; Flipped current and backup LBA for header
    (bytes-set!/u64-le gpt-sector2 24 backup-header-lba)
    (bytes-set!/u64-le gpt-sector2 32 primary-header-lba)

    ; Start LBA of partition entries
    (bytes-set!/u64-le gpt-sector2 72 #x1fffdf)

    ; Zero the CRC, and then recompute it.
    (bytes-set!/u32-le gpt-sector2 16 0)
    (bytes-set!/u32-le gpt-sector2 16 (crc32 (subbytes gpt-sector2 0 92))))

  (values
    (bytes-append
      mbr-sector
      gpt-sector
      partition-entries)
    (bytes-append
      partition-entries
      gpt-sector2)))


(define (serialize-partition p)
  (match-define (partition name guid type-guid start size contents) p)
  (match contents
    [(? fat32? p)
     (serialize-fat32 p #:volume-size size)]
    [#f
     (make-bytes size)]))

(define (write-gpt-disk g out)
  (define amount-output 0)
  (define (output b)
    (write-all-bytes b out)
    (set! amount-output (+ amount-output (bytes-length b))))
  (define (pad-to n)
    (output (make-bytes (- n amount-output))))

  (define-values (gpt-header gpt-footer) (gpt-disk->header/footer g))

  (output gpt-header)
  ;; Currently requires that partitions be in order
  (for ([p (gpt-disk-partitions g)])
    (pad-to (partition-start p))
    (output (serialize-partition p)))
  (pad-to (- (gpt-disk-size g) (bytes-length gpt-footer)))
  (output gpt-footer))
