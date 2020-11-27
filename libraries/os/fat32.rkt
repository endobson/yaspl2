#lang racket

(require
  racket/dict
  racket/hash
  "util.rkt")

(provide
  make-fat32
  fat32?
  fat32-add-directory
  fat32-add-file

  serialize-fat32)

(struct fat32 (next-free-cluster cluster-chains file-system))

(struct fs-entity (first-cluster))
;; Contents is a hash of bytes to fs-entity
;; Currently assume that we don't need more than one cluster for a directory
(struct dir-entity fs-entity (contents))
;; Contents is a bytes
(struct file-entity fs-entity (length clusters))


(define (make-fat32)
  (define initial-cluster-chains
    (hash 0 #x0ffffff8
          1 #x0fffffff
          2 #x0fffffff))
  (fat32 3 initial-cluster-chains (list)))

(define (fat32-add-directory f32 path)
  (match f32
    [(fat32 next-free-cluster cluster-chains fs)
     (fat32 (add1 next-free-cluster)
            (dict-set cluster-chains next-free-cluster #x0fffffff)
            (filesystem-add-entity fs path (dir-entity next-free-cluster (list))))]))

(define (file->clusters contents)
  (for/list ([i (ceiling (/ (bytes-length contents) 512))])
    (define offset (* i 512))
    (make-section cluster #:size 512
      (bytes-copy! cluster 0 contents offset (min (bytes-length contents) (+ offset 512))))))

(define (fat32-add-file f32 path contents)
  (match f32
    [(fat32 next-free-cluster cluster-chains fs)
     (define file-clusters (file->clusters contents))
     (define size (bytes-length contents))
     (define e (file-entity (if (zero? size) 0 next-free-cluster) size
                            (for/hash ([c file-clusters] [i (in-naturals)])
                              (values (+ i next-free-cluster) c))))
     (define new-chains
       (for/hash ([i (length file-clusters)])
         (values (+ i next-free-cluster)
                 (if (= (add1 i) (length file-clusters))
                     #x0fffffff
                     (add1 (+ i next-free-cluster))))))

     (fat32 (+ next-free-cluster (length file-clusters))
            (hash-union cluster-chains new-chains)
            (filesystem-add-entity fs path e))]))

(define (filesystem-add-entity fs path e)
  (match path
    [(list (? bytes? name)) (dict-set fs name e)]
    [(cons (? bytes? name) path)
     (dict-update fs name
       (lambda (sub-dir)
         (match sub-dir
           [(dir-entity cluster contents)
            (dir-entity cluster (filesystem-add-entity contents path e))])))]))


;; Constants
(define fat-volume-id #"\xba\xab\xad\xde")
;; Number of sectors per cluster
(define sectors-per-cluster 1)
;; Number of file allocation tables
(define fat-reserved-sectors 32)
;; Number of file allocation tables
(define number-of-fats 2)
;; Number of sectors in the actual file allocation table
(define fat-size 2017)
;; Number of sectors in the FAT32 volume
(define fat-total-size #x40000)
;; Cluster number of root directory
(define root-cluster-number 2)

;; The BPB sector
(define-section fat-bpb-sector #:size 512
  (bytes-copy!       fat-bpb-sector 0  #"\xeb\x58\x90")      ; Jump instruction
  (bytes-copy!       fat-bpb-sector 3  #"BSD  4.4\x00")      ; OS NAME
  (bytes-copy!       fat-bpb-sector 11 #"\x00\x02")          ; Bytes per sector (512)
  (bytes-set!        fat-bpb-sector 13 sectors-per-cluster)  ; Sectors per Cluster
  (bytes-set!/u16-le fat-bpb-sector 14 fat-reserved-sectors) ; Number of reserved sectors
  (bytes-set!        fat-bpb-sector 16 number-of-fats)       ; Number of FATs
  (bytes-copy!       fat-bpb-sector 17 #"\x00\x00")          ; Reserved 0 for FAT32
  (bytes-copy!       fat-bpb-sector 19 #"\x00\x00")          ; Reserved 0 for FAT32
  (bytes-copy!       fat-bpb-sector 21 #"\xF8")              ; Media Type (Fixed)
  (bytes-copy!       fat-bpb-sector 22 #"\x00\x00")          ; Reserved 0 for FAT32
  (bytes-copy!       fat-bpb-sector 24 #"\x20\x00")          ; Sectors per Track
  (bytes-copy!       fat-bpb-sector 26 #"\x20\x00")          ; Number of Heads
  (bytes-copy!       fat-bpb-sector 28 #"\x00\x08\x00\x00")  ; Number of Hidden Sectors
  (bytes-set!/u32-le fat-bpb-sector 32 fat-total-size)       ; Total number of sectors
  (bytes-set!/u32-le fat-bpb-sector 36 fat-size)             ; FAT size
  (bytes-copy!       fat-bpb-sector 40 #"\x00\x00")          ; EXT Flags (Mirroring on)
  (bytes-copy!       fat-bpb-sector 42 #"\x00\x00")          ; Version (0.0)
  (bytes-set!/u32-le fat-bpb-sector 44 root-cluster-number)  ; Root Cluster Number
  (bytes-copy!       fat-bpb-sector 48 #"\x01\x00")          ; FSInfo Sector Number
  (bytes-copy!       fat-bpb-sector 50 #"\x06\x00")          ; Backup Boot Sector number
                                                             ; Reserved (12 Bytes)
  (bytes-copy!       fat-bpb-sector 64 #"\x80")        ; Drive Number
  (bytes-copy!       fat-bpb-sector 65 #"\x00")        ; Reserved1
  (bytes-copy!       fat-bpb-sector 66 #"\x29")        ; Boot Sig
  (bytes-copy!       fat-bpb-sector 67 fat-volume-id)  ; VolId
  (bytes-copy!       fat-bpb-sector 71 #"NO NAME    ") ; VolLab
  (bytes-copy!       fat-bpb-sector 82 #"FAT32   ")    ; Format Name
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

;; FS Info sector
(define (make-fat-fs-info-sector f32)
  (define next-clusters (fat32-cluster-chains f32))
  (define max-cluster (apply max (hash-keys next-clusters)))
  (define max-start-cluster
    (apply max
      (set->list
        (set-subtract
          (list->set (hash-keys next-clusters))
          (list->set (hash-values next-clusters))))))
  (make-fat-fs-info-sector* max-cluster max-start-cluster))

(define (make-fat-fs-info-sector* max-used-cluster last-allocated-cluster)
  (make-section fat-fsinfo-sector #:size 512
    (bytes-copy!       fat-fsinfo-sector 0   #"RRaA")                  ; Signature
                                                                       ; Reserved
    (bytes-copy!       fat-fsinfo-sector 484 #"rrAa")                  ; Signature part 2
    (bytes-set!/u32-le fat-fsinfo-sector 488                           ; Number of free clusters
                       (- fat-total-size (* fat-size number-of-fats)
                          fat-reserved-sectors (* (sub1 max-used-cluster) sectors-per-cluster)))
    (bytes-set!/u32-le fat-fsinfo-sector 492 last-allocated-cluster)   ; Last allocated cluster
                                                                       ; Reserved (12 bytes)
    (bytes-copy!       fat-fsinfo-sector 508 #"\x00\x00\x55\xAA")))    ; Signature part 3

;; The File Allocation Table
(define (make-fat-file-allocation-table f32)
  (make-section fat-file-allocation-table #:size (* fat-size 512)
    (for ([(cluster next) (fat32-cluster-chains f32)])
      (bytes-set!/u32-le fat-file-allocation-table (* cluster 4) next))))

;; Dir Entries and filesystem flattening
(define-struct dir-entry (filename attributes first-cluster size))
(define-struct long-file-name-dir-entry (last-logical sequence-number checksum bytes))

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
    ['hidden #x02]
    ['visible #x00] ;; Just used so that there is a not-hidden
    ['archived #x20]))

(define (dir-entry-split-filename filename)
  (match filename
    [#"."  (values #".       " #"   ")]
    [#".." (values #"..      " #"   ")]
    [(regexp #px#"^([A-Z0-9_~]{1,8})(?:\\.([A-Z0-9]{1,3}))?$"
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

(define (write-long-file-name-dir-entry d bytes offset)
  (define ucs-2-bytes
    (let ([u (ascii-bytes->ucs-2 (long-file-name-dir-entry-bytes d))])
      (if (= (bytes-length u) 26)
          u
          (bytes-append u #"\x00\x00" (make-bytes (- 24 (bytes-length u)) #xff)))))
  (define checksum (long-file-name-dir-entry-checksum d))

  (bytes-set!  bytes (+ offset 0)
               (bitwise-ior
                 (if (long-file-name-dir-entry-last-logical d) #x40 0)
                 (long-file-name-dir-entry-sequence-number d)))
  (bytes-copy! bytes (+ offset 1) ucs-2-bytes 0 10)    ; 5 characters of name
  (bytes-set!  bytes (+ offset 11) #x0f)               ; Attributes (hidden, volume, system, read-only)
  (bytes-set!  bytes (+ offset 12) #x00)               ; Reserved
  (bytes-set!  bytes (+ offset 13) checksum)          ; Checksum of 8.3 name
  (bytes-copy! bytes (+ offset 14) ucs-2-bytes 10 22)  ; 6 characters of name
  (bytes-set!  bytes (+ offset 26) #x00)               ; First cluster
  (bytes-set!  bytes (+ offset 27) #x00)               ; First cluster continued
  (bytes-copy! bytes (+ offset 28) ucs-2-bytes 22 26)) ; 2 characters of name


(define (dir-cluster entries)
  (make-section cluster #:size 512
    (for ([i (in-naturals)] [entry entries])
      (cond
        [(dir-entry? entry)
         (write-dir-entry entry cluster (* i 32))]
        [(long-file-name-dir-entry? entry)
         (write-long-file-name-dir-entry entry cluster (* i 32))]))))

(define (name->visibility name)
  (if (equal? (bytes-ref name 0) 46) ;; 46 = .
      'hidden
      'visible))

(define (dir-contents->dir-entries contents)
  (append*
    (for/list ([(name e) (in-dict contents)])
      (define-values (sanitized-name long-file-name-entries)
        (case name
          [(#".fseventsd")
           (values
             #"FSEVEN~1"
             (list
              (long-file-name-dir-entry #t 1 #xda #".fseventsd")))]
          [else (values name empty)]))

      (define visibility (name->visibility name))

      (define real-entry
        (match e
          [(dir-entity cluster _)
           (dir-entry sanitized-name `(,visibility subdirectory) cluster 0)]
          [(file-entity cluster length _)
           (dir-entry sanitized-name `(,visibility archived) cluster length)]))

      (append
        long-file-name-entries
        (list real-entry)))))

(define (subdir->dir-entries name entity parent-first-cluster)
  (list* (dir-entry #"." `(,(name->visibility name) subdirectory archived)
                    (fs-entity-first-cluster entity) 0)
         (dir-entry #".." '(subdirectory) parent-first-cluster 0)
         (dir-contents->dir-entries (dir-entity-contents entity))))

(define (fat32->initialized-clusters f32)
  ;; sub directories
  (define (handle name entity parent-dir-cluster)
    (match entity
      [(dir-entity cluster-number items)
       (apply hash-union
         (hash cluster-number
               (dir-cluster (subdir->dir-entries name entity parent-dir-cluster)))
         (for/list ([(name subentity) (in-dict items)])
           (handle name subentity cluster-number)))]
      [(file-entity _ _ file-clusters)
       file-clusters]))

  (define root-dir (fat32-file-system f32))

  (define clusters
    (apply hash-union
      (hash root-cluster-number (dir-cluster (dir-contents->dir-entries root-dir)))
      (for/list ([(name entity) (in-dict root-dir)])
        (handle name entity 0))))
  (define max-cluster (apply max (hash-keys clusters)))

  ;; Currently assume that all clusters are still reachable from live filesystem
  (for/list ([i (in-range 2 (add1 max-cluster))])
    (hash-ref clusters i)))

;; Put the disk together
(define (blank-sectors n)
  (make-bytes (* 512 n)))

(define (serialize-fat32 f32 #:volume-size volume-size #:disk-offset [disk-offset #f])

  (unless (= volume-size (* 128 2048 512))
    (error 'serialize-fat32
           "Only fixed sized FAT32 volumes currently supported. Expected: ~a, Got: ~a"
           (* 128 2048 512)
           volume-size))

  (define fat-file-allocation-table (make-fat-file-allocation-table f32))
  (define fat-fsinfo-sector (make-fat-fs-info-sector f32))
  ;; This is stale to support byte for byte matching with external implementation
  (define fat-fsinfo-sector2 (make-fat-fs-info-sector* 2 3))
  (define initialized-clusters (fat32->initialized-clusters f32))
  (define number-uninitialized-clusters
    (+ (- (length initialized-clusters))
       30
       (* 126 2048)))


  (bytes-append*
    (flatten
      (list
        ;; The reserved sectors (volume headers)
        fat-bpb-sector
        fat-fsinfo-sector
        (blank-sectors 4)
        fat-bpb-sector
        fat-fsinfo-sector2
        (blank-sectors 24)
        ;; Sector 32

        ;; Each fat is 2017 sectors
        fat-file-allocation-table
        fat-file-allocation-table
        ;; Sector 4066

        ;; Start of clusters (First cluster is #2)
        initialized-clusters
        (blank-sectors number-uninitialized-clusters)))))

