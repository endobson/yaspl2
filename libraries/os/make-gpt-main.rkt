#lang racket/base

(require
  racket/bytes
  racket/list
  racket/match
  racket/port
  racket/file
  (for-syntax
    racket/base
    syntax/parse))


(define-syntax (make-section stx)
  (syntax-parse stx
    [(_ name:id #:size size:exact-positive-integer bodies:expr ...)
     #'(let ([name (make-bytes size)])
         bodies ...
         (bytes->immutable-bytes name))]))


(define-syntax (define-section stx)
  (syntax-parse stx
    [(_ name:id #:size size:exact-positive-integer bodies:expr ...)
     #'(define name (make-section name #:size size bodies ...))]))


(define-syntax (define-sector stx)
  (syntax-parse stx
    [(_ name:id bodies:expr ...)
     #'(define-section name #:size 512 bodies ...)]))

(define-syntax (define-partition-entry stx)
  (syntax-parse stx
    [(_ name:id bodies:expr ...)
     #'(define-section name #:size 128 bodies ...)]))

(define (bytes-set!/u16-le bytes offset v)
  (integer->integer-bytes v 2 #f #f bytes offset))
(define (bytes-set!/u32-le bytes offset v)
  (integer->integer-bytes v 4 #f #f bytes offset))
(define (bytes-set!/u64-le bytes offset v)
  (integer->integer-bytes v 8 #f #f bytes offset))



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


(define fat-volume-id #"\xba\xab\xad\xde")

(define-sector fat-bpb-sector
  (bytes-copy! fat-bpb-sector 0  #"\xeb\x58\x90")      ; Jump instruction
  (bytes-copy! fat-bpb-sector 3  #"BSD  4.4\x00")      ; OS NAME
  (bytes-copy! fat-bpb-sector 11 #"\x00\x02")          ; Bytes per sector (512)
  (bytes-copy! fat-bpb-sector 13 #"\x01")              ; Sectors per Cluster
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

  (bytes-copy! fat-bpb-sector #x1fe #"\x55\xaa"))



(define blank-cluster blank-sector)
(define file-contents (file->bytes "tmp/prog.efi"))

(define fs-entities
  (hash #"EFI"
        (dir-entity 3
           (hash #"BOOT"
                 (dir-entity 4
                   (hash #"BOOTX64.EFI"
                         (file-entity 5 file-contents)))))))

(define (file-clusters contents)
  (for/list ([i (in-range 0 (bytes-length contents) 512)])
    (define sector (make-bytes 512)) 
    (bytes-copy! sector 0 file-contents i (min (bytes-length file-contents) (+ i 512)))
    (bytes->immutable-bytes sector)))

(define file-allocation-table
  (let ()
    (define file-sizes (list 1 1 1 1 (length (file-clusters file-contents))))
    (define (next-clusters file-sizes current-cluster)
      (match file-sizes
        [(list) (list)]
        [(cons size file-sizes)
         (append (build-list (- size 1) (lambda (i) (add1 (+ current-cluster i))))
                 (cons #xfffffff
                       (next-clusters file-sizes (+ current-cluster size))))]))
    (cons #x0ffffff8 (next-clusters file-sizes 1))))

(define-sector fat-file-allocation-table
  (for ([i (in-naturals)] [entry file-allocation-table])
    (integer->integer-bytes entry 4 #f #f fat-file-allocation-table (* i 4))))


(define-sector fat-fsinfo-sector
  (bytes-copy! fat-fsinfo-sector 0   #"RRaA")                     ; Signature
                                                                  ; Reserved
  (bytes-copy! fat-fsinfo-sector 484 #"rrAa")                     ; Signature part 2
  (integer->integer-bytes #x03f014 4 #f #f fat-fsinfo-sector 488) ; Number of free clusters
  (bytes-copy! fat-fsinfo-sector 492 #"\x05\x00\x00\x00")         ; Last allocated cluster
                                                                  ; Reserved
  (bytes-copy! fat-fsinfo-sector 508 #"\x00\x00\x55\xAA"))        ; Signature part 3

(define-sector fat-fsinfo-sector2
  (bytes-copy! fat-fsinfo-sector2 0   #"RRaA")              ; Signature
                                                            ; Reserved
  (bytes-copy! fat-fsinfo-sector2 484 #"rrAa")              ; Signature part 2
  (bytes-copy! fat-fsinfo-sector2 488 #"\x1d\xf0\x03\x00")  ; Number of free clusters
  (bytes-copy! fat-fsinfo-sector2 492 #"\x03\x00\x00\x00")  ; Last allocated cluster
                                                            ; Reserved
  (bytes-copy! fat-fsinfo-sector2 508 #"\x00\x00\x55\xAA")) ; Signature part 3


(define-struct dir-entry (filename attributes first-cluster size))

(define-struct date (year month day))

(define (date->bytes d)
  (match-define (date year month day) d)
  (integer->integer-bytes (+ (* (- year 1980) 512) (* month 32) day) 2 #f #f))

(define-struct time-of-day (hour minute second))
(define (time-of-day->bytes t)
  (match-define (time-of-day hours minutes seconds) t)
  (integer->integer-bytes (+ (* hours 2048) (* minutes 32) (quotient seconds 2)) 2 #f #f))

(define fixed-time-of-day (time-of-day 12 34 56))
(define fixed-date (date 2020 1 2))

(define (dir-entry-attribute->byte attribute)
  (match attribute
    ['subdirectory #x10]
    ['archived #x20]))

(define (dir-entry-split-filename filename)
  (match filename
    [#"."  (values #".       " #"   ")]
    [#".." (values #"..      " #"   ")]
    [(regexp #px#"([A-Z0-9]{1,8})(?:\\.([A-Z0-9]{1,3}))?"
             (list _ orig-filename orig-extension))
     ; Pad with spaces (#x20) to amount
     (define (pad b amount)
       (bytes-append b (make-bytes (- amount (bytes-length b)) #x20)))
     (values
       (pad orig-filename 8)
       (pad (or orig-extension #"") 3))]))

(define (write-dir-entry d bytes offset)
  (define-values (filename-base extension) (dir-entry-split-filename (dir-entry-filename d)))
  (bytes-copy!       bytes (+ offset 0) filename-base)
  (bytes-copy!       bytes (+ offset 8) extension)
  (bytes-set!        bytes (+ offset 11) 
    (apply bitwise-ior (map dir-entry-attribute->byte (dir-entry-attributes d))))
  (bytes-set!        bytes (+ offset 12) 0) ; Reserved
  (bytes-set!        bytes (+ offset 13) 0) ; Creation ms
  (bytes-copy!       bytes (+ offset 14) (time-of-day->bytes fixed-time-of-day)) ; Creation time
  (bytes-copy!       bytes (+ offset 16) (date->bytes fixed-date))               ; Creation date
  (bytes-copy!       bytes (+ offset 18) (date->bytes fixed-date))               ; Last access date
  (bytes-set!/u16-le bytes (+ offset 20) (quotient (dir-entry-first-cluster d) #x100))
  (bytes-copy!       bytes (+ offset 22) (time-of-day->bytes fixed-time-of-day)) ; Modification time
  (bytes-copy!       bytes (+ offset 24) (date->bytes fixed-date))               ; Modification date
  (bytes-set!/u16-le bytes (+ offset 26) (remainder (dir-entry-first-cluster d) #x100))
  (bytes-set!/u32-le bytes (+ offset 28) (dir-entry-size d)))

(define (dir-cluster entries)
  (make-section cluster #:size 512
    (for ([i (in-naturals)] [entry entries])
      (write-dir-entry entry cluster (* i 32)))))
     

(define files
  (hash (list #"EFI" #"BOOT" #"BOOTX64.EFI") file-contents))

(define file-tree
  (let ()
    (define (add-file filename contents dir-hash)
      (match filename
        [(list filename)
         (hash-set! dir-hash filename contents)]
        [(cons dir filename)
         (define subdir-hash (hash-ref dir-hash filename (make-hash)))
         (unless (hash? subdir-hash)
            (error 'file-tree "Cannot have file and director with same name"))
         (add-file filename contents subdir-hash)]))

    (define root-dir-hash (make-hash))

    (for ([(filename contents) files])
      (add-file filename contents root-dir-hash))
    (define (freeze fs-item)
      (match fs-item
        [(? hash?)
         (for/hash ([(name sub-item) fs-item])
           (values name (freeze sub-item)))]
        [(? bytes?)
         fs-item]))
    (freeze root-dir-hash)))

(define-struct fs-entity (first-cluster))
;; Contents is a hash of bytes to fs-entity
(define-struct (dir-entity fs-entity) (contents))
;; Contents is a bytes
(define-struct (file-entity fs-entity) (contents))

      
(define (dot-dir-entry self-first-cluster)
  (dir-entry #"." '(subdirectory archived) self-first-cluster 0))
(define (dot-dot-dir-entry parent-first-cluster)
  (dir-entry #".." '(subdirectory) parent-first-cluster 0))
(define (subdir-dir-entry name dir-first-cluster)
  (dir-entry name '(subdirectory) dir-first-cluster 0))
(define (file-dir-entry name file-first-cluster contents)
  (dir-entry name '(archived) file-first-cluster (bytes-length contents)))

(define (fs-entity->dir-entry name e)
  (match e
    [(dir-entity cluster _)
     (subdir-dir-entry name cluster)]
    [(file-entity cluster contents)
     (file-dir-entry name cluster contents)]))

(define (dir-contents->dir-entries contents)
  (for/list ([(name e) contents])
    (fs-entity->dir-entry name e)))

(define root-dir-entries
  (dir-contents->dir-entries fs-entities))

(define (subdir->dir-entries entity parent-first-cluster)
  (list* (dot-dir-entry (fs-entity-first-cluster entity))
         (dot-dot-dir-entry parent-first-cluster)
         (dir-contents->dir-entries (dir-entity-contents entity))))


(define root-cluster-number 2)
(define non-zero-cluster-hash
  (let ([clusters (make-hash)])
    ;; Root directory
    (hash-set! clusters root-cluster-number
      (dir-cluster (dir-contents->dir-entries fs-entities)))

    ;; Rest of the filesystem
    (define (handle name entity parent-dir-cluster)
      (match entity
        [(dir-entity cluster-number items)
         (hash-set! clusters cluster-number
                    (dir-cluster (subdir->dir-entries entity parent-dir-cluster)))
         (for ([(name subentity) items])
           (handle name subentity cluster-number))]
        [(file-entity cluster-number contents)
         (for ([cluster (file-clusters contents)] [i (in-naturals)])
           (hash-set! clusters (+ i cluster-number) cluster))]))
    (for ([(name entity) fs-entities])
      (handle name entity 0))

    ;; Make an immutable hash
    (for/hash ([(k v) clusters])
      (values k v))))

(define initialized-clusters
  (let ()
    (define max-cluster (apply max (hash-keys non-zero-cluster-hash)))
    (for/list ([i (in-range 2 (add1 max-cluster))])
      (hash-ref non-zero-cluster-hash i blank-cluster))))


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
    ;; 1 * 2048 sectors

    (write-all-bytes fat-bpb-sector out)
    (write-all-bytes fat-fsinfo-sector out)
    (for ([i 4])
      (write-all-bytes blank-sector out))
    (write-all-bytes fat-bpb-sector out)
    (write-all-bytes fat-fsinfo-sector2 out)
    (for ([i 24])
      (write-all-bytes blank-sector out))
    (write-all-bytes fat-file-allocation-table out)

    (for ([i (- 2048 33)])
      (write-all-bytes blank-sector out))
    ;; 2 * 2048 sectors

    (write-all-bytes blank-sector out)
    (write-all-bytes fat-file-allocation-table out)
    (for ([i (- 2048 32)])
      (write-all-bytes blank-sector out))

    ;; Start of clusters (First cluster is #2)
    (for ([cluster initialized-clusters])
      (write-all-bytes cluster out))
    (for ([i (- 30 (length initialized-clusters))])
      (write-all-bytes blank-sector out))
    ;; 3 * 2048 sectors

    (for ([i (* (- (* 16 64) 4) 2048)])
      (write-all-bytes blank-sector out))
    ;; 1023 * 2048 sectors

    (for ([i (- 2048 33)])
      (write-all-bytes blank-sector out))
    (write-all-bytes partition-entries out)
    (write-all-bytes gpt-sector2 out)
    ;; 1024 * 2048 sectors
    ))

