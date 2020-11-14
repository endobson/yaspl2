#lang racket/base

(require
  racket/bytes
  racket/list
  racket/match
  racket/port)



(define mbr-sector
  (let ([mbr-sector (make-bytes 512)])
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
    (bytes-copy! mbr-sector #x1fe #"\x55\xaa")
    mbr-sector))

(define (crc32 data)
  (bitwise-xor
   (for/fold ([accum #xFFFFFFFF])
     ([byte  (in-bytes data)])
     (for/fold ([accum (bitwise-xor accum byte)])
               ([num (in-range 0 8)])
       (bitwise-xor (quotient accum 2)
                    (* #xEDB88320 (bitwise-and accum 1)))))
   #xFFFFFFFF))

(define partition-entry1
  (let ([partition-entry1 (make-bytes 128)])
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
      (bytes-set! partition-entry1 (+ 56 (* 2 i)) (bytes-ref name i)))
    partition-entry1))

(define partition-entry2
  (let ([partition-entry2 (make-bytes 128)])
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
    partition-entry2))

(define empty-partition-entry (make-bytes 128))
(define partition-entries
  (bytes-append*
    partition-entry1
    partition-entry2
    (make-list 126 empty-partition-entry)))

(define blank-sector (make-bytes 512))
 

(define gpt-sector
  (let ([gpt-sector (make-bytes 512)])

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
    (integer->integer-bytes (crc32 (subbytes gpt-sector 0 92)) 4 #f #f gpt-sector 16)


    


    gpt-sector))

(define gpt-sector2
  (let ([gpt-sector2 (bytes-copy gpt-sector)])
    ; Flipped current and backup LBA
    (bytes-copy! gpt-sector2 24 #"\xff\xff\x1f\x00\x00\x00\x00\x00")
    (bytes-copy! gpt-sector2 32 #"\x01\x00\x00\x00\x00\x00\x00\x00")

    ; Start LBA of partition entries
    (bytes-copy! gpt-sector2 72 #"\xdf\xff\x1f\x00\x00\x00\x00\x00")

    ; Zero the CRC, and then recompute it.
    (integer->integer-bytes 0 4 #f #f gpt-sector2 16)
    (integer->integer-bytes (crc32 (subbytes gpt-sector2 0 92)) 4 #f #f gpt-sector2 16)

    gpt-sector2))


(define fat-volume-id #"\xba\xab\xad\xde")

(define fat-bpb-sector
  (let ([fat-bpb-sector (make-bytes 512)])
    (bytes-copy! fat-bpb-sector 0  #"\xeb\x58\x90")      ; Jump instruction
    (bytes-copy! fat-bpb-sector 3  #"BSD  4.4\x00")      ; OS NAME
    (bytes-copy! fat-bpb-sector 11 #"\x00\x02")          ; Bytes per sector (512)
    (bytes-copy! fat-bpb-sector 13 #"\x01")              ; Log2(Sectors) per Cluster
    (bytes-copy! fat-bpb-sector 14 #"\x20\x00")          ; Number of reserved sectors
    (bytes-copy! fat-bpb-sector 16 #"\x02")              ; Number of FATs
    (bytes-copy! fat-bpb-sector 17 #"\x00\x00")          ; Reserved 0 for FAT32
    (bytes-copy! fat-bpb-sector 19 #"\x00\x00")          ; Reserved 0 for FAT32
    (bytes-copy! fat-bpb-sector 21 #"\xF8")              ; Media Type (Fixed)
    (bytes-copy! fat-bpb-sector 22 #"\x00\x00")          ; Reserved 0 for FAT32
    (bytes-copy! fat-bpb-sector 24 #"\x20\x00")          ; Sectors per Track
    (bytes-copy! fat-bpb-sector 26 #"\x20\x00")          ; Number of Heads
    (bytes-copy! fat-bpb-sector 28 #"\x00\x08\x00\x00")  ; Number of Hidden Sectors
    (bytes-copy! fat-bpb-sector 32 #"\x00\x00\x04\x00")  ; Total number of sectors
    (bytes-copy! fat-bpb-sector 36 #"\xe1\x07\x00\x00")  ; FAT size
    (bytes-copy! fat-bpb-sector 40 #"\x00\x00")          ; EXT Flags (Mirroring on)
    (bytes-copy! fat-bpb-sector 42 #"\x00\x00")          ; Version (0.0)
    (bytes-copy! fat-bpb-sector 44 #"\x02\x00\x00\x00")  ; Root Cluster Number
    (bytes-copy! fat-bpb-sector 48 #"\x01\x00")          ; FSInfo Sector Number
    (bytes-copy! fat-bpb-sector 50 #"\x06\x00")          ; Backup Boot Sector number
                                                     ; Reserved (12 Bytes)
    (bytes-copy! fat-bpb-sector 64 #"\x80")          ; Drive Number
    (bytes-copy! fat-bpb-sector 65 #"\x00")          ; Reserved1
    (bytes-copy! fat-bpb-sector 66 #"\x29")          ; Boot Sig
    (bytes-copy! fat-bpb-sector 67 fat-volume-id) ; VolId
    (bytes-copy! fat-bpb-sector 71 #"NO NAME    ") ; VolLab
    (bytes-copy! fat-bpb-sector 82 #"FAT32   ") ; Format Name
    ;; Boot code
    (bytes-copy! fat-bpb-sector 90 
      (bytes-append
        #"\xfa\x31\xc0\x8e\xd0\xbc"
        #"\x00\x7c\xfb\x8e\xd8\xe8\x00\x00\x5e\x83\xc6\x19\xbb\x07\x00\xfc"
        #"\xac\x84\xc0\x74\x06\xb4\x0e\xcd\x10\xeb\xf5\x30\xe4\xcd\x16\xcd"
        #"\x19\x0d\x0a\x4e\x6f\x6e\x2d\x73\x79\x73\x74\x65\x6d\x20\x64\x69"
        #"\x73\x6b\x0d\x0a\x50\x72\x65\x73\x73\x20\x61\x6e\x79\x20\x6b\x65"
        #"\x79\x20\x74\x6f\x20\x72\x65\x62\x6f\x6f\x74\x0d\x0a"))

                                

    (bytes-copy! fat-bpb-sector #x1fe #"\x55\xaa")
    fat-bpb-sector))

(define fat-fsinfo-sector
  (let ([fat-fsinfo-sector (make-bytes 512)])
    (bytes-copy! fat-fsinfo-sector 0   #"RRaA")             ; Signature
                                                            ; Reserved
    (bytes-copy! fat-fsinfo-sector 484 #"rrAa")             ; Signature part 2
    (bytes-copy! fat-fsinfo-sector 488 #"\x1d\xf0\x03\x00") ; Number of free clusters
    (bytes-copy! fat-fsinfo-sector 492 #"\x03\x00\x00\x00") ; Last allocated cluster
                                                            ; Reserved
    (bytes-copy! fat-fsinfo-sector 508 #"\x00\x00\x55\xAA") ; Signature part 3

    fat-fsinfo-sector))

(define fat-fsinfo-sector2
  (let ([fat-fsinfo-sector2 (make-bytes 512)])
    (bytes-copy! fat-fsinfo-sector2 0   #"RRaA")             ; Signature
                                                             ; Reserved
    (bytes-copy! fat-fsinfo-sector2 484 #"rrAa")             ; Signature part 2
    (bytes-copy! fat-fsinfo-sector2 488 #"\x1d\xf0\x03\x00") ; Number of free clusters
    (bytes-copy! fat-fsinfo-sector2 492 #"\x03\x00\x00\x00") ; Last allocated cluster
                                                             ; Reserved
    (bytes-copy! fat-fsinfo-sector2 508 #"\x00\x00\x55\xAA") ; Signature part 3

    fat-fsinfo-sector2))


(define fat-file-allocation-table
  (let ([fat-file-allocation-table (make-bytes 512)])
    (bytes-copy! fat-file-allocation-table 0   #"\xf8\xff\xff")
    (bytes-copy! fat-file-allocation-table 3   #"\x0f\xff\xff\xff")
    (bytes-copy! fat-file-allocation-table 7   #"\x0f\xff\xff\xff")
    (bytes-copy! fat-file-allocation-table 11  #"\x0f")

    fat-file-allocation-table))




(define (write-all-bytes b p)
  (let loop ([offset 0])
    (when (< offset (bytes-length b))
      (define written (write-bytes b p offset))
      (loop (+ offset written)))))

(match-define
  (vector output-path)
  (current-command-line-arguments))

(call-with-output-file output-path
  (lambda (out)
    (write-all-bytes mbr-sector out)
    (write-all-bytes gpt-sector out)
    (write-all-bytes partition-entries out)
    (for ([i (- 2048 34)])
      (write-all-bytes blank-sector out))

    (write-all-bytes fat-bpb-sector out)
    (write-all-bytes fat-fsinfo-sector out)

    (for ([i 4])
      (write-all-bytes blank-sector out))

    (write-all-bytes fat-bpb-sector out)
    (write-all-bytes fat-fsinfo-sector2 out)
    (for ([i 24])
      (write-all-bytes blank-sector out))

    (write-all-bytes fat-file-allocation-table out)

    (for ([i (- 2048 32)])
      (write-all-bytes blank-sector out))

    (write-all-bytes fat-file-allocation-table out)

    (for ([i (- 2048 1)])
      (write-all-bytes blank-sector out))

    (for ([i (- (* 2048 16 64) 2048 2048 2048 34)])
      (write-all-bytes blank-sector out))


    (write-all-bytes partition-entries out)
    (write-all-bytes gpt-sector2 out)

    ))

