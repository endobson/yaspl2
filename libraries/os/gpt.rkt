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
(struct gpt-disk (size partitions))

(struct partition (name guid type-guid start size contents))

(define (make-gpt-disk size)
  (gpt-disk size empty))

(define (gpt-disk-add-partition disk p)
  (match disk
    [(gpt-disk size partitions)
     (gpt-disk size (append partitions (list p)))]))

(define (make-efi-system-partition name guid start size contents)
  (partition name guid efi-system-partition-guid start size contents))
(define (make-x86-root-partition name guid start size contents)
  (partition name guid x86-root-partition-guid start size contents))


;; Standard MBR sector
;; TODO fix size
(define-section mbr-sector #:size 512
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

;; GPT header/footer sectors
(define (gpt-sectors g)
  (define partition-entries (gpt-disk->partition-entries g))

  ;; TODO fix sizes
  (define-section gpt-sector #:size 512
    (bytes-copy!       gpt-sector 0  #"EFI PART")                         ; Signature
    (bytes-copy!       gpt-sector 8  #"\x00\x00\x01\x00")                 ; Revision
    (bytes-set!/u32-le gpt-sector 12 92)                                  ; Header Size (92)
                                                                          ; CRC (4 bytes)
                                                                          ; Reserved (4 bytes)
    (bytes-set!/u64-le gpt-sector 24 1)                                   ; Current LBA of header
    (bytes-set!/u64-le gpt-sector 32 #x1fffff)                            ; Backup LBA of header
    (bytes-set!/u64-le gpt-sector 40 34)                                  ; First usable LBA
    (bytes-set!/u64-le gpt-sector 48 #x1fffde)                            ; Last usable LBA
    (bytes-copy!       gpt-sector 56                                      ; Disk GUID
      #"\xdb\x0f\xba\x80\x75\xe0\xc5\x47\x93\x25\xd2\x0f\xd0\x17\x5d\x9d")
    (bytes-set!/u64-le gpt-sector 72 2)                                   ; Start LBA of partition entries
    (bytes-set!/u64-le gpt-sector 80 128)                                 ; Number of partition entries
    (bytes-set!/u64-le gpt-sector 84 128)                                 ; Size of partition entries
    (bytes-set!/u32-le gpt-sector 88 (crc32 partition-entries))           ; CRC of partition entries
  
    ; Compute the CRC and put it in the right place
    (bytes-set!/u32-le gpt-sector 16 (crc32 (subbytes gpt-sector 0 92))))
  
  (define-section gpt-sector2 #:size 512
    ;; Start with the original gpt sector
    (bytes-copy! gpt-sector2 0 gpt-sector)
  
    ; Flipped current and backup LBA for header
    (bytes-set!/u64-le gpt-sector2 24 #x1fffff)
    (bytes-set!/u64-le gpt-sector2 32 1)
  
    ; Start LBA of partition entries
    (bytes-set!/u64-le gpt-sector2 72 #x1fffdf)
  
    ; Zero the CRC, and then recompute it.
    (bytes-set!/u32-le gpt-sector2 16 0)
    (bytes-set!/u32-le gpt-sector2 16 (crc32 (subbytes gpt-sector2 0 92))))

  (values gpt-sector gpt-sector2 partition-entries))


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

(define (gpt-disk->partition-entries g)
  (bytes-append*
    (append (map partition->partition-entry (gpt-disk-partitions g))
            (make-list 126 (make-bytes 128)))))

;;
(define (gpt-disk->header/footer g)
  (define-values (gpt-sector gpt-sector2 partitions) (gpt-sectors g))
  (values
    (bytes-append
      mbr-sector
      gpt-sector
      partitions)
    (bytes-append
      partitions
      gpt-sector2)))


(define (serialize-partition p)
  (match (partition-contents p)
    [(? fat32? p) 
     (serialize-fat32 p #:volume-size (mebibytes 128))]
    [#f
     (make-bytes 0)]))

(define (write-gpt-disk g out)
  (define-values (gpt-header gpt-footer) (gpt-disk->header/footer g))

  (write-all-bytes gpt-header out)
  (write-all-bytes (make-bytes (- (mebibytes 1) (bytes-length gpt-header))) out)
  ;; 1MiB (2048 sectors)

  (for ([p (gpt-disk-partitions g)])
    (write-all-bytes (serialize-partition p) out))

  (write-all-bytes (make-bytes (- (mebibytes (- 1024 129)) (bytes-length gpt-footer))) out)
  (write-all-bytes gpt-footer out))


