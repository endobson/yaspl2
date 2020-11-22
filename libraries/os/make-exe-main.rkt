#lang racket/base

(require
  racket/bytes
  racket/list
  racket/match
  racket/port
  (for-syntax
    racket/base
    syntax/parse))


(define-syntax (define-section stx)
  (syntax-parse stx
    [(_ name:id #:size size:exact-positive-integer bodies:expr ...)
     #'(define name
         (let ([name (make-bytes size)])
           bodies ...
           (bytes->immutable-bytes name)))]))


(define-syntax (define-section* stx)
  (syntax-parse stx
    [(_ name:id bodies:expr ...)
     #'(define-section name #:size 512 bodies ...)]))

(define (ascii->utf-16 str)
  (define b (make-bytes (* (string-length str) 2)))
  (for ([i (string-length str)])
    (define code-point (char->integer (string-ref str i)))
    (bytes-set! b (* 2 i) code-point))
  (bytes->immutable-bytes b))

(define (bytes-set!/u16-le bytes offset v)
  (integer->integer-bytes v 2 #f #f bytes offset))
(define (bytes-set!/u32-le bytes offset v)
  (integer->integer-bytes v 4 #f #f bytes offset))
(define (bytes-set!/u64-le bytes offset v)
  (integer->integer-bytes v 8 #f #f bytes offset))

(define-section coff-header #:size 20
  (bytes-set!/u16-le coff-header 0  #x8664)  ; Machine
  (bytes-set!/u16-le coff-header 2  5)       ; Number of sections
  (bytes-set!/u32-le coff-header 4  #x3039)  ; Timestamp
  (bytes-set!/u32-le coff-header 8  0)       ; Pointer to symbol table
  (bytes-set!/u32-le coff-header 12 0)       ; Number of symbols
  (bytes-set!/u16-le coff-header 16 #xf0)    ; Size of optional header
  (bytes-set!/u16-le coff-header 18          ; Characteristics
                     (bitwise-ior
                       #x0002    ; Executable
                       #x0020    ; Large address aware
                       #x2000))) ; DLL

(define-section pe-header #:size 240
  (bytes-set!/u16-le pe-header 0  #x20b)  ; PE32+ Magic Number
  (bytes-set!        pe-header 2  14)     ; Linker major version
  (bytes-set!        pe-header 3  0)      ; Linker minor version
  (bytes-set!/u32-le pe-header 4  #x200)  ; Size of code
  (bytes-set!/u32-le pe-header 8  #x800)  ; Size of initialized data
  (bytes-set!/u32-le pe-header 12 0)      ; Size of uninitialized data
  (bytes-set!/u32-le pe-header 16 #x1000) ; Address of entry point
  (bytes-set!/u32-le pe-header 20 #x1000) ; Base of code
  (bytes-set!/u64-le pe-header 24 #x180000000) ; Image base
  (bytes-set!/u32-le pe-header 32 #x1000) ; Section alignment
  (bytes-set!/u32-le pe-header 36 #x0200) ; File alignment
  (bytes-set!/u16-le pe-header 40 6)      ; Major operating system version
  (bytes-set!/u16-le pe-header 42 0)      ; Minor operating system version
  (bytes-set!/u16-le pe-header 44 0)      ; Major image version
  (bytes-set!/u16-le pe-header 46 0)      ; Minor image version
  (bytes-set!/u16-le pe-header 48 6)      ; Major subsystem version
  (bytes-set!/u16-le pe-header 50 0)      ; Minor subsystem version
  (bytes-set!/u32-le pe-header 52 0)      ; Win32 version value
  (bytes-set!/u32-le pe-header 56 #x6000) ; Size of image
  (bytes-set!/u32-le pe-header 60 #x400)  ; Size of headers
  (bytes-set!/u32-le pe-header 64 0)      ; Checksum
  (bytes-set!/u16-le pe-header 68 10)     ; Subsystem (EFI)
  (bytes-set!/u16-le pe-header 70 #x160)  ; Dll characteristics
                                          ; (high entropy, dynamic base, NX)
  (bytes-set!/u64-le pe-header 72 #x100000) ; Size of stack reserve
  (bytes-set!/u64-le pe-header 80 #x1000)   ; Size of stack commit
  (bytes-set!/u64-le pe-header 88 #x100000) ; Size of heap reserve
  (bytes-set!/u64-le pe-header 96 #x1000)   ; Size of heap commit
  (bytes-set!/u32-le pe-header 104 0)       ; Loader flags
  (bytes-set!/u32-le pe-header 108 16)      ; Number of rva and sizes

  (bytes-set!/u32-le pe-header 112 0)      ; Export table
  (bytes-set!/u32-le pe-header 120 0)      ; Import table
  (bytes-set!/u32-le pe-header 128 0)      ; Resource table
  (bytes-set!/u32-le pe-header 136 #x4000) ; Exception table
  (bytes-set!/u32-le pe-header 140 #x0c)   ; Exception table (part 2)
  (bytes-set!/u32-le pe-header 144 0)      ; Certificate table
  (bytes-set!/u32-le pe-header 152 #x5000) ; Base relocation table
  (bytes-set!/u32-le pe-header 156 #x0c)   ; Base relocation table (part 2)
  (bytes-set!/u32-le pe-header 160 0)      ; Debug
  (bytes-set!/u32-le pe-header 168 0)      ; Architecture
  (bytes-set!/u32-le pe-header 176 0)      ; Global ptr
  (bytes-set!/u32-le pe-header 184 0)      ; TLS table
  (bytes-set!/u32-le pe-header 192 0)      ; Load config table
  (bytes-set!/u32-le pe-header 200 0)      ; Bound import
  (bytes-set!/u32-le pe-header 208 0)      ; IAT
  (bytes-set!/u32-le pe-header 216 0)      ; Delay import descriptor
  (bytes-set!/u32-le pe-header 224 0)      ; CLR runtime header
  (bytes-set!/u32-le pe-header 232 0)      ; Reserved
  )

(define-section text-section-header #:size 40
  (bytes-copy!       text-section-header 0 #".text\x00\x00\x00") ; Name
  (bytes-set!/u32-le text-section-header 8  #x108)      ; VirtualSize
  (bytes-set!/u32-le text-section-header 12 #x1000)     ; VirtualAddress
  (bytes-set!/u32-le text-section-header 16 #x200)      ; SizeOfRawData
  (bytes-set!/u32-le text-section-header 20 #x400)      ; PointerToRawData
  (bytes-set!/u32-le text-section-header 24 0)          ; PointerToRelocations
  (bytes-set!/u32-le text-section-header 28 0)          ; PointerToLinenumbers
  (bytes-set!/u16-le text-section-header 32 0)          ; NumberOfRelocations
  (bytes-set!/u16-le text-section-header 34 0)          ; NumberOfLinenumbers
  (bytes-set!/u32-le text-section-header 36 #x60000020) ; Characteristics
  )


(define-section rdata-section-header #:size 40
  (bytes-copy!       rdata-section-header 0 #".rdata\x00\x00") ; Name
  (bytes-set!/u32-le rdata-section-header 8  #x48)      ; VirtualSize
  (bytes-set!/u32-le rdata-section-header 12 #x2000)    ; VirtualAddress
  (bytes-set!/u32-le rdata-section-header 16 #x200)     ; SizeOfRawData
  (bytes-set!/u32-le rdata-section-header 20 #x600)     ; PointerToRawData
  (bytes-set!/u32-le rdata-section-header 24 0)         ; PointerToRelocations
  (bytes-set!/u32-le rdata-section-header 28 0)         ; PointerToLinenumbers
  (bytes-set!/u16-le rdata-section-header 32 0)         ; NumberOfRelocations
  (bytes-set!/u16-le rdata-section-header 34 0)         ; NumberOfLinenumbers
  (bytes-set!/u32-le rdata-section-header 36 #x40000040) ; Characteristics
  )

(define-section data-section-header #:size 40
  (bytes-copy!       data-section-header 0 #".data\x00\x00\x00") ; Name
  (bytes-set!/u32-le data-section-header 8  #x20)       ; VirtualSize
  (bytes-set!/u32-le data-section-header 12 #x3000)     ; VirtualAddress
  (bytes-set!/u32-le data-section-header 16 #x200)      ; SizeOfRawData
  (bytes-set!/u32-le data-section-header 20 #x800)      ; PointerToRawData
  (bytes-set!/u32-le data-section-header 24 0)          ; PointerToRelocations
  (bytes-set!/u32-le data-section-header 28 0)          ; PointerToLinenumbers
  (bytes-set!/u16-le data-section-header 32 0)          ; NumberOfRelocations
  (bytes-set!/u16-le data-section-header 34 0)          ; NumberOfLinenumbers
  (bytes-set!/u32-le data-section-header 36 #xc0000040) ; Characteristics
  )
(define-section pdata-section-header #:size 40
  (bytes-copy!       pdata-section-header 0 #".pdata\x00\x00") ; Name
  (bytes-set!/u32-le pdata-section-header 8  #x0c)   ; VirtualSize
  (bytes-set!/u32-le pdata-section-header 12 #x4000) ; VirtualAddress
  (bytes-set!/u32-le pdata-section-header 16 #x200)  ; SizeOfRawData
  (bytes-set!/u32-le pdata-section-header 20 #xa00)  ; PointerToRawData
  (bytes-set!/u32-le pdata-section-header 24 0) ; PointerToRelocations
  (bytes-set!/u32-le pdata-section-header 28 0) ; PointerToLinenumbers
  (bytes-set!/u16-le pdata-section-header 32 0) ; NumberOfRelocations
  (bytes-set!/u16-le pdata-section-header 34 0) ; NumberOfLinenumbers
  (bytes-set!/u32-le pdata-section-header 36 #x40000040) ; Characteristics
  )
(define-section reloc-section-header #:size 40
  (bytes-copy!       reloc-section-header 0 #".reloc\x00\x00") ; Name
  (bytes-set!/u32-le reloc-section-header 8  #x0c)       ; VirtualSize
  (bytes-set!/u32-le reloc-section-header 12 #x5000)     ; VirtualAddress
  (bytes-set!/u32-le reloc-section-header 16 #x200)      ; SizeOfRawData
  (bytes-set!/u32-le reloc-section-header 20 #xc00)      ; PointerToRawData
  (bytes-set!/u32-le reloc-section-header 24 0)          ; PointerToRelocations
  (bytes-set!/u32-le reloc-section-header 28 0)          ; PointerToLinenumbers
  (bytes-set!/u16-le reloc-section-header 32 0)          ; NumberOfRelocations
  (bytes-set!/u16-le reloc-section-header 34 0)          ; NumberOfLinenumbers
  (bytes-set!/u32-le reloc-section-header 36 #x42000040) ; Characteristics
  )



(define-section first-section #:size 1024
  (define pe-offset #x78)

  ;; The DOS Header
  (bytes-copy!       first-section 0  #"MZ")     ; Signature
  (bytes-set!/u16-le first-section 2  #x78)      ; Bytes on last page
  (bytes-set!/u16-le first-section 4  1)         ; Number of pages
  (bytes-set!/u16-le first-section 6  0)         ; Relocations
  (bytes-set!/u16-le first-section 8  4)         ; Size of header in paragraphs
  (bytes-set!/u16-le first-section 10 0)         ; Minimum extra paragraphs needed
  (bytes-set!/u16-le first-section 12 0)         ; Maximum extra paragraphs needed
  (bytes-set!/u16-le first-section 14 0)         ; Initial (relative) SS value
  (bytes-set!/u16-le first-section 16 0)         ; Initial SP value
  (bytes-set!/u16-le first-section 18 0)         ; Checksum
  (bytes-set!/u16-le first-section 20 0)         ; Initial IP value
  (bytes-set!/u16-le first-section 22 0)         ; Initial (relative) CS value
  (bytes-set!/u16-le first-section 24 #x40)      ; File address of relocation table
  (bytes-set!/u16-le first-section 26 0)         ; Overlay number
                                                 ; 8 bytes Reserved
  (bytes-set!/u16-le first-section 36 0)         ; OEM identifier
  (bytes-set!/u16-le first-section 38 0)         ; OEM information
                                                 ; 20 bytes Reserved
  (bytes-set!/u16-le first-section 60 pe-offset) ; Offset for PE

  ;; The DOS Program
  (bytes-copy! first-section 64
    (bytes-append
      #"\x0e"         ; push   cs
      #"\x1f"         ; pop    ds
      #"\xba\x0e\x00" ; mov    #x0e, dx (Message is at offset 14 [0x0e])
      #"\xb4\x09"     ; mov    #x09, ah
      #"\xcd\x21"     ; int    0x21
      #"\xb8\x01\x4c" ; mov    0x4c01, ax
      #"\xcd\x21"     ; int    0x21
      #"This program cannot be run in DOS mode" ; Message
      #"\x2e\x24"     ; Unknown
      ))

  (bytes-copy! first-section pe-offset 
    (bytes-append
       #"PE\x00\x00" ; Signature
       coff-header
       pe-header
       text-section-header
       rdata-section-header
       data-section-header
       pdata-section-header
       reloc-section-header
       ))


  )

(define-section* text-section
  (bytes-copy! text-section 0 (make-bytes #x200 #xcc))

  (bytes-copy! text-section 0 
    (bytes-append
      #"\x48\x83\xec\x58\x48\x89\x54\x24\x50\x48\x89\x4c\x24\x48\x48\x8b"
      #"\x44\x24\x48\x48\x89\x05\xf6\x1f\x00\x00\x48\x8b\x44\x24\x50\x48" 
      #"\x89\x05\xf2\x1f\x00\x00\x66\xc7\x44\x24\x42\x00\x00\x66\xc7\x44" 
      #"\x24\x44\x00\x00\x66\xc7\x44\x24\x46\x00\x00\xc7\x44\x24\x3c\x01" 
      #"\x00\x00\x00\x83\x7c\x24\x3c\x1a\x0f\x8f\xb5\x00\x00\x00\x8b\x44" 
      #"\x24\x3c\x83\xc0\x40\x66\x89\x44\x24\x42\xc7\x44\x24\x38\x01\x00" 
      #"\x00\x00\x83\x7c\x24\x38\x1a\x0f\x8f\x81\x00\x00\x00\x48\x8d\x54" 
      #"\x24\x42\x8b\x44\x24\x38\x83\xc0\x40\x66\x89\x44\x24\x44\x48\x8b" 
      #"\x0d\x93\x1f\x00\x00\x48\x8b\x49\x40\x48\x8b\x49\x08\x4c\x8b\x05" 
      #"\x84\x1f\x00\x00\x4d\x8b\x40\x40\x48\x89\x4c\x24\x30\x4c\x89\xc1" 
      #"\x4c\x8b\x44\x24\x30\x41\xff\xd0\x48\x8b\x0d\x69\x1f\x00\x00\x48" 
      #"\x8b\x49\x40\x48\x8b\x49\x08\x48\x8b\x15\x4a\x1f\x00\x00\x4c\x8b" 
      #"\x05\x53\x1f\x00\x00\x4d\x8b\x40\x40\x48\x89\x4c\x24\x28\x4c\x89" 
      #"\xc1\x4c\x8b\x44\x24\x28\x48\x89\x44\x24\x20\x41\xff\xd0\x8b\x44" 
      #"\x24\x38\x83\xc0\x01\x89\x44\x24\x38\xe9\x74\xff\xff\xff\xe9\x00" 
      #"\x00\x00\x00\x8b\x44\x24\x3c\x83\xc0\x01\x89\x44\x24\x3c\xe9\x40" 
      #"\xff\xff\xff\xe9\x33\xff\xff\xff\xcc\xcc\xcc\xcc\xcc\xcc\xcc\xcc"))
  )

(define-section* fourth-section
  (bytes-copy! fourth-section 0 (ascii->utf-16 "Hello, you slab of warm meat!\r\n"))
  (bytes-copy! fourth-section #x40 #"\x01\x04\x01\x00\x04\xa2")
  )
(define-section* fifth-section
  (bytes-copy! fifth-section 0 #"\x00\x20\x00\x80\x01\x00\x00\x00\x3a\x20\x00\x80\x01\x00")
  )
(define-section* sixth-section
  (bytes-copy! sixth-section 0 #"\x00\x10\x00\x00\x08\x11\x00\x00\x40\x20")
  )
(define-section* seventh-section
  (bytes-copy! seventh-section 0 #"\x00\x30\x00\x00\x0c\x00\x00\x00\x00\xa0\x08\xa0")
  )

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
    (write-all-bytes first-section out)

    (write-all-bytes text-section out)
    (write-all-bytes fourth-section out)
    (write-all-bytes fifth-section out)
    (write-all-bytes sixth-section out)
    (write-all-bytes seventh-section out)
    ))

