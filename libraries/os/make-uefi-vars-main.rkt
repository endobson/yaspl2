#lang racket/base

(require
  racket/match
  racket/port
  openssl/sha1
  "util.rkt")

(struct efi-variable (guid name state attributes contents))
(define (write-efi-variable v out)
  (match-define (efi-variable guid name state attributes contents) v)
  (define encoded-name (ascii->utf-16 (string-append name "\0")))

  (write-all-bytes #"\xaa\x55" out)
  (write-all-bytes state out)
  (write-all-bytes attributes out)
  (write-all-bytes (make-bytes 30) out)
  (write-all-bytes (integer->integer-bytes (bytes-length encoded-name) 4 #f #f) out)
  (write-all-bytes (integer->integer-bytes (bytes-length contents) 4 #f #f) out)
  (write-all-bytes guid out)
  (write-all-bytes encoded-name out)
  (write-all-bytes contents out)
  (write-all-bytes (make-bytes (- 3 (modulo (sub1 (+ (bytes-length encoded-name)
                                                     (bytes-length contents))) 
                                            4))
                               #xff) out)
  )

(struct device-path-node ())
(struct firmware-volume-device-path-node device-path-node (guid))
(struct firmware-file-device-path-node device-path-node (guid))
(struct acpi-device-path-node device-path-node (hid uid))
(struct acpi-adr-device-path-node device-path-node (adr-value))
(struct pci-device-path-node device-path-node (function-num device-num))
(struct uart-device-path-node device-path-node (baud-rate data-bits parity stop-bits))
(struct vendor-defined-device-path-node device-path-node ())
(struct end-device-path-node device-path-node ())
(struct vendor-device-path-node device-path-node (guid))
(struct usb-device-path-node device-path-node ())
(struct atapi-device-path-node device-path-node (secondary))


(define (write-device-path-node p out)
  (match p
    [(firmware-volume-device-path-node guid)
     (write-all-bytes #"\x04" out)
     (write-all-bytes #"\x07" out)
     (write-all-bytes #"\x14\x00" out)
     (write-all-bytes guid out)]
    [(firmware-file-device-path-node guid)
     (write-all-bytes #"\x04" out)
     (write-all-bytes #"\x06" out)
     (write-all-bytes #"\x14\x00" out)
     (write-all-bytes guid out)]

    [(acpi-device-path-node hid uid)
     (write-all-bytes #"\x02" out)
     (write-all-bytes #"\x01" out)
     (write-all-bytes #"\x0c\x00" out)
     (write-all-bytes (integer->integer-bytes hid 4 #f #f) out)
     (write-all-bytes (integer->integer-bytes uid 4 #f #f) out)]
    [(acpi-adr-device-path-node adr-value)
     (write-all-bytes #"\x02" out)
     (write-all-bytes #"\x03" out)
     (write-all-bytes #"\x08\x00" out)
     (write-all-bytes adr-value out)]

    [(pci-device-path-node function-num device-num)
     (write-all-bytes #"\x01" out)
     (write-all-bytes #"\x01" out)
     (write-all-bytes #"\x06\x00" out)
     (write-byte function-num out)
     (write-byte device-num out)]
    [(vendor-device-path-node guid)
     (write-all-bytes #"\x01" out)
     (write-all-bytes #"\x04" out)
     (write-all-bytes #"\x14\x00" out)
     (write-all-bytes guid out)]

    [(uart-device-path-node baud-rate data-bits parity stop-bits)
     (write-all-bytes #"\x03" out)
     (write-all-bytes #"\x0e" out)
     (write-all-bytes #"\x13\x00" out)
     (write-all-bytes #"\x00\x00\x00\x00" out)
     (write-all-bytes baud-rate out)
     (write-all-bytes data-bits out)
     (write-all-bytes parity out)
     (write-all-bytes stop-bits out)]
    [(vendor-defined-device-path-node)
     (write-all-bytes #"\x03" out)
     (write-all-bytes #"\x0a" out)
     (write-all-bytes #"\x14\x00" out)
     ;; PC-ANSI GUID
     (write-all-bytes (hex-string->bytes "5347c1e0bef9d2119a0c0090273fc14d") out)]
    [(usb-device-path-node)
     (write-all-bytes #"\x03" out)
     (write-all-bytes #"\x0f" out)
     (write-all-bytes #"\x0b\x00" out)
     (write-all-bytes #"\xff\xff" out)
     (write-all-bytes #"\xff\xff" out)
     (write-all-bytes #"\x03" out)
     (write-all-bytes #"\x01" out)
     (write-all-bytes #"\x01" out)]
    [(atapi-device-path-node secondary)
     (write-all-bytes #"\x03" out)
     (write-all-bytes #"\x01" out)
     (write-all-bytes #"\x08\x00" out)
     (write-all-bytes (if secondary #"\x01" #"\x00") out)      ; Primary/Secondary
     (write-all-bytes #"\x00" out)      ; Master/Slave
     (write-all-bytes #"\x00\x00" out)] ; Unit Number

    [(end-device-path-node)
     (write-all-bytes #"\x7f\x01\x04\x00" out)]
    ))

(define uart-device-path-node1
  (uart-device-path-node
    #"\x00\xc2\x01\x00\x00\x00\x00\x00" ; Baud Rate
    #"\x08"                             ; Data Bits
    #"\x01"                             ; Parity
    #"\x01"                             ; Stop Bits
    ))

(define uart-device-path-node2
  (uart-device-path-node
    #"\x00\x00\x00\x00\x00\x00\x00\x00"
    #"\x00"                             ; Data Bits
    #"\x00"                             ; Parity
    #"\x00"                             ; Stop Bits
    ))

(define acpi-adr-device-path-node1
  (acpi-adr-device-path-node #"\x00\x01\x01\x80"))

(define acpi-adr-device-path-node2
  (acpi-adr-device-path-node #"\x00\x03\x01\x80"))


(define vendor-device-path-node1
  (vendor-device-path-node
    (hex-string->bytes "9b5a5a865db84c47845565d1be844be2")))

(define vendor-device-path-node2
  (vendor-device-path-node
    (hex-string->bytes "a1237455ab636c40be7e91cdbc08c457")))

(define device-A
  (list
    (acpi-device-path-node #x0a0341d0 0)
    (pci-device-path-node 0 1)
    (acpi-device-path-node #x050141d0 0)
    uart-device-path-node1
    (vendor-defined-device-path-node)))
(define device-B
  (list
    (acpi-device-path-node #x0a0341d0 0)
    (pci-device-path-node 0 1)
    (acpi-device-path-node #x050141d0 1)
    uart-device-path-node1
    (vendor-defined-device-path-node)))
(define device-C
  (list
    vendor-device-path-node1
    uart-device-path-node2
    (vendor-defined-device-path-node)))
(define device-D
  (list
    (acpi-device-path-node #x0a0341d0 0)
    (pci-device-path-node 0 1)
    (acpi-device-path-node #x030341d0 0)))
(define device-E
  (list
    (acpi-device-path-node #x0a0341d0 0)
    (pci-device-path-node 0 2)
    acpi-adr-device-path-node1))
;; Unclear on what this device is
(define device-E*
  (list
    (acpi-device-path-node #x0a0341d0 #xaa000000)
    (pci-device-path-node 0 2)
    acpi-adr-device-path-node1))
(define device-F
  (list
    (usb-device-path-node)))
(define device-G
  (list
    vendor-device-path-node2
    acpi-adr-device-path-node2))

(define (write-end-device-path-node out)
  ; Type    : 0x7f
  ; Subtype : 0xff
  ; Length  : 4 (0x0004)
  (write-all-bytes #"\x7f\xff\x04\x00" out))

(define (write-device-path p out)
  (for ([node (in-list p)])
    (write-device-path-node node out)))

(define (device-path->bytes p)
  (call-with-output-bytes
    (lambda (out)
      (write-device-path p out)
      (write-end-device-path-node out))))

(define (device-paths->bytes paths)
  (call-with-output-bytes
    (lambda (out)
      (match-define (cons p1 ps) paths)
      (write-device-path p1 out)
      (for ([p ps])
        (write-device-path (cons (end-device-path-node) p) out))
      (write-end-device-path-node out))))



(struct load-option (attributes device-path name data))

(define (write-load-option o out)
  (match-define (load-option attributes path name data) o)
  (define encoded-path (device-path->bytes path))
  (write-all-bytes attributes out)
  (write-all-bytes (integer->integer-bytes (bytes-length encoded-path) 2 #f #f) out)
  (write-all-bytes (ascii->utf-16 (string-append name "\0")) out)
  (write-all-bytes encoded-path out)
  (write-all-bytes data out))

(define (load-option->bytes o)
  (call-with-output-bytes (lambda (p) (write-load-option o p))))




(match-define
  (vector output-path)
  (current-command-line-arguments))

(define v1
  (efi-variable
    #"\x0c\xec\x76\xc0\x28\x70\x99\x43\xa0\x72\x71\xee\x5c\x44\x8b\x9f"
    "CustomMode" ; Name
    #"\x3f\x00"  ; State
    #"\x03\x00"  ; Attributes
    #"\x00"      ; Contents
    ))
(define v2
  (efi-variable
    #"\x6e\xe5\xbe\xd9\xdc\x75\xd9\x49\xb4\xd7\xb5\x34\x21\x0f\x63\x7a"
    "certdb"            ; Name
    #"\x3f\x00"         ; State
    #"\x27\x00"         ; Attributes
    #"\x04\x00\x00\x00" ; Contents
    ))
(define v3
  (efi-variable
    (hex-string->bytes "e0e47390ec606e4b99034c223c260f3c")
    "VendorKeysNv" ; Name
    #"\x3f\x00"    ; State
    #"\x23\x00"    ; Attributes
    #"\x01"        ; Contents
    ))
(define v4
  (efi-variable
    (hex-string->bytes "114070eb0214d3118e7700a0c969723b")
    "MTC"               ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    #"\x01\x00\x00\x00" ; Contents
    ))
(define v5
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "BootOrder" ; Name
    #"\x3c\x00" ; State
    #"\x07\x00" ; Attributes
    #"\x00\x00" ; Contents
    ))
(define v6
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "Boot0000"          ; Name
    #"\x3f\x00"         ; State
    #"\x07\x00"         ; Attributes
    (load-option->bytes ; Contents
      (load-option
        #"\x09\x01\x00\x00"
        (list (firmware-volume-device-path-node
                (hex-string->bytes "c9bdb87cebf8344faaea3ee4af6516a1"))
              (firmware-file-device-path-node
                (hex-string->bytes "21aa2c4614760345836e8ab6f4662331")))
        "UiApp"
        #"")) 
    ))
(define v7
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "Timeout"   ; Name
    #"\x3f\x00" ; State
    #"\x07\x00" ; Attributes
    #"\x00\x00" ; Contents
    ))
(define v8
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "PlatformLang"   ; Name
    #"\x3f\x00" ; State
    #"\x07\x00" ; Attributes
    #"en\0" ; Contents
    ))
(define v9
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "Lang"   ; Name
    #"\x3f\x00" ; State
    #"\x07\x00" ; Attributes
    #"eng\0" ; Contents
    ))
(define v10
  (efi-variable
    (hex-string->bytes "e87fb304aef60b48bdd537d98c5e89aa")
    "VarErrorFlag"   ; Name
    #"\x3f\x00" ; State
    #"\x07\x00" ; Attributes
    #"\xff" ; Contents
    ))
(define v11
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D))
    ))
(define v12
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A))
    ))
(define v13
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        ))
    ))
(define v14
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        ))
    ))
(define v15
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        ))
    ))
(define v16
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-B
        ))
    ))
(define v17
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        ))
    ))
(define v18
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        device-E
        ))
    ))
(define v19
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-B
        device-C
        ))
    ))

(define v20
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        device-E
        device-C
        ))
    ))
(define v21
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        device-C
        ))
    ))
(define v22
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-B
        device-C
        device-F
        ))
    ))
(define v23
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        device-E
        device-C
        device-G))
    ))

(define v24
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "Key0000"           ; Name
    #"\x3f\x00"         ; State
    #"\x07\x00"         ; Attributes
    (bytes-append       ; Contents
      #"\x00\x00\x00\x40" ; EFI_BOOT_KEY_DATA
      #"\x51\xd7\x97\x9f" ; CRC
      #"\x00\x00"         ; Boot Option
      #"\x0c\x00\x00\x00" ; Keys
      )
    ))

(define v25
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "Key0001"           ; Name
    #"\x3f\x00"         ; State
    #"\x07\x00"         ; Attributes
    (bytes-append       ; Contents
      #"\x00\x00\x00\x40" ; EFI_BOOT_KEY_DATA
      #"\x51\xd7\x97\x9f" ; CRC
      #"\x00\x00"         ; Boot Option
      #"\x17\x00\x00\x00" ; Keys
      )
    ))

(define v26
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-C
        device-G
        ))
    ))

(define v27
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-G
        ))
    ))
(define v28
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        ))
    ))
(define v29
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-C
        device-F
        ))
    ))
(define v30
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        ))
    ))
(define v31
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-C
        ))
    ))
(define v32
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        ))
    ))
(define v33
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "BootOrder"         ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    #"\x00\x00\x01\x00" ; Contents
    ))
(define v34
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "Boot0001"          ; Name
    #"\x3d\x00"         ; State
    #"\x07\x00"         ; Attributes
    (load-option->bytes ; Contents
      (load-option
        #"\x01\x00\x00\x00"
        (list
          (acpi-device-path-node #x0a0341d0 0)
          (pci-device-path-node 1 1)
          (atapi-device-path-node #t))
        "UEFI QEMU DVD-ROM QM00003 "
        (hex-string->bytes "4eac0881119f594d850ee21a522c59b2")) 
    )))
(define v35
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "BootOrder"                 ; Name
    #"\x3c\x00"                 ; State
    #"\x07\x00"                 ; Attributes
    #"\x00\x00\x01\x00\x02\x00" ; Contents
    ))
(define v36
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "Boot0002"          ; Name
    #"\x3d\x00"         ; State
    #"\x07\x00"         ; Attributes
    (load-option->bytes ; Contents
      (load-option
        #"\x01\x00\x00\x00"
        (list
          (acpi-device-path-node #x0a0341d0 0)
          (pci-device-path-node 1 1)
          (atapi-device-path-node #f))
        "UEFI QEMU HARDDISK QM00001 "
        (hex-string->bytes "4eac0881119f594d850ee21a522c59b2")) 
    )))
(define v37
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "BootOrder"                         ; Name
    #"\x3c\x00"                         ; State
    #"\x07\x00"                         ; Attributes
    #"\x00\x00\x01\x00\x02\x00\x03\x00" ; Contents
    ))
(define v38
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "Boot0003"          ; Name
    #"\x3f\x00"         ; State
    #"\x07\x00"         ; Attributes
    (load-option->bytes ; Contents
      (load-option
        #"\x01\x00\x00\x00"
        (list (firmware-volume-device-path-node
                (hex-string->bytes "c9bdb87cebf8344faaea3ee4af6516a1"))
              (firmware-file-device-path-node
                (hex-string->bytes "83a5047c3e9e1c4fad65e05268d0b4d1")))
        "EFI Internal Shell"
        #""))
    ))
(define v39
  (efi-variable
    (hex-string->bytes "9f04194c3741d34d9c108b97a83ffdfa")
    "MemoryTypeInformation" ; Name
    #"\x3f\x00"             ; State
    #"\x03\x00"             ; Attributes
    (bytes-append
      #"\x0a\x00\x00\x00" #"\x2a\x00\x00\x00"
      #"\x09\x00\x00\x00" #"\x0b\x00\x00\x00"
      #"\x00\x00\x00\x00" #"\x20\x00\x00\x00"
      #"\x06\x00\x00\x00" #"\xac\x01\x00\x00"
      #"\x05\x00\x00\x00" #"\xa8\x00\x00\x00"
      #"\x03\x00\x00\x00" #"\x32\x02\x00\x00"
      #"\x04\x00\x00\x00" #"\x00\x0f\x00\x00"
      #"\x0f\x00\x00\x00" #"\x00\x00\x00\x00")
    ))
(define v40
  (efi-variable
    (hex-string->bytes "114070eb0214d3118e7700a0c969723b")
    "MTC"               ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    #"\x02\x00\x00\x00" ; Contents
    ))
(define v41
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        ))
    ))
(define v42
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-B))
    ))
(define v43
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        ))
    ))

(define v44
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-B
        device-C
        ))
    ))
(define v45
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E*
        device-B
        device-C
        ))
    ))
(define v46
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        device-C
        ))
    ))
(define v47
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        device-C
        device-G
        ))
    ))
(define v48
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-C
        device-G
        ))
    ))
(define v49
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-G
        ))
    ))
(define v50
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        ))
    ))
(define v51
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-C
        ))
    ))
(define v52
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        ))
    ))
(define v53
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-C
        ))
    ))
(define v54
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        ))
    ))
(define v55
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "BootOrder"                 ; Name
    #"\x3c\x00"                 ; State
    #"\x07\x00"                 ; Attributes
    #"\x00\x00\x01\x00\x03\x00" ; Contents
    ))
(define v56
  (efi-variable
    (hex-string->bytes "114070eb0214d3118e7700a0c969723b")
    "MTC"               ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    #"\x03\x00\x00\x00" ; Contents
    ))
(define v57
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        ))
    ))

(define v58
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-B
        ))
    ))
(define v59
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        ))
    ))
(define v60
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-B
        device-C
        ))
    ))
(define v61
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        device-C
        ))
    ))
(define v62
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        device-C
        ))
    ))
(define v63
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        device-C
        device-G
        ))
    ))
(define v64
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-C
        device-G
        ))
    ))
(define v65
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-G
        ))
    ))
(define v66
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        ))
    ))
(define v67
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-C
        ))
    ))
(define v68
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        ))
    ))
(define v69
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-C
        ))
    ))
(define v70
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        ))
    ))
(define v71
  (efi-variable
    (hex-string->bytes "114070eb0214d3118e7700a0c969723b")
    "MTC"               ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    #"\x04\x00\x00\x00" ; Contents
    ))
(define v72
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        ))
    ))
(define v73
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-B
        ))
    ))
(define v74
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        ))
    ))
(define v75
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-B
        device-C
        ))
    ))
(define v76
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        device-C
        ))
    ))
(define v77
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        device-C
        ))
    ))
(define v78
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        device-C
        device-G
        ))
    ))
(define v79
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-C
        device-G
        ))
    ))
(define v80
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-G
        ))
    ))
(define v81
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        ))
    ))
(define v82
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-C
        ))
    ))
(define v83
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        ))
    ))
(define v84
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-C
        ))
    ))
(define v85
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        ))
    ))
(define v86
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "BootOrder"         ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    #"\x00\x00\x03\x00" ; Contents
    ))
(define v87
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "BootOrder"         ; Name
    #"\x3f\x00"         ; State
    #"\x07\x00"         ; Attributes
    #"\x00\x00\x03\x00\x01\x00" ; Contents
    ))
(define v88
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "Boot0001"          ; Name
    #"\x3f\x00"         ; State
    #"\x07\x00"         ; Attributes
    (load-option->bytes ; Contents
      (load-option
        #"\x01\x00\x00\x00"
        (list
          (acpi-device-path-node #x0a0341d0 0)
          (pci-device-path-node 1 1)
          (atapi-device-path-node #t))
        "UEFI QEMU DVD-ROM QM00003 "
        (hex-string->bytes "4eac0881119f594d850ee21a522c59b2")) 
    )))
(define v89
  (efi-variable
    (hex-string->bytes "114070eb0214d3118e7700a0c969723b")
    "MTC"               ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    #"\x05\x00\x00\x00" ; Contents
    ))
(define v90
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        ))
    ))
(define v91
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-B
        ))
    ))
(define v92
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        ))
    ))
(define v93
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-B
        device-C
        ))
    ))
(define v94
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        device-C
        ))
    ))
(define v95
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        device-C
        ))
    ))
(define v96
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        device-C
        device-G
        ))
    ))
(define v97
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-C
        device-G
        ))
    ))
(define v98
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-G
        ))
    ))
(define v99
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        ))
    ))
(define v100
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-C
        ))
    ))
(define v101
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        ))
    ))
(define v102
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-C
        ))
    ))
(define v103
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        ))
    ))
(define v104
  (efi-variable
    (hex-string->bytes "114070eb0214d3118e7700a0c969723b")
    "MTC"               ; Name
    #"\x3f\x00"         ; State
    #"\x07\x00"         ; Attributes
    #"\x06\x00\x00\x00" ; Contents
    ))
(define v105
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        ))
    ))
(define v106
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-B
        ))
    ))
(define v107
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        ))
    ))
(define v108
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-B
        device-C
        ))
    ))
(define v109
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        device-C
        ))
    ))
(define v110
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-B
        device-C
        ))
    ))
(define v111
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-B
        device-C
        device-G
        ))
    ))
(define v112
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-C
        device-G
        ))
    ))
(define v113
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        device-G
        ))
    ))
(define v114
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3f\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-E
        ))
    ))
(define v115
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        device-C
        ))
    ))
(define v116
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3f\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-D
        device-A
        device-F
        ))
    ))
(define v117
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        device-C
        ))
    ))
(define v118
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3f\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-paths->bytes ; Contents
      (list
        device-A
        ))
    ))

(call-with-output-file output-path
  (lambda (out)
    (write-all-bytes (make-bytes #x10 #x00) out)
    ;; GUID
    (write-all-bytes #"\x8d\x2b\xf1\xff\x96\x76\x8b\x4c\xa9\x85\x27\x47\x07\x5b\x4f\x50" out)
    ;; Length of FileSystem
    (write-all-bytes #"\x00\x40\x08\x00\x00\x00\x00\x00" out)
    ;; Signature
    (write-all-bytes #"_FVH" out)
    ;; Attributes
    (write-all-bytes #"\xff\xfe\x04\x00" out)

    ;; Header Length
    (write-all-bytes #"\x48\x00" out)
    ;; Checksum
    (write-all-bytes #"\xaf\xb8" out)

    ;; Extended header offset
    (write-all-bytes #"\x00\x00" out)
    ;; Reserved
    (write-all-bytes #"\x00" out)
    ;; Revision
    (write-all-bytes #"\x02" out)
    
    ;; Number of blocks /  Block size
    (write-all-bytes #"\x84\x00\x00\x00" out)
    (write-all-bytes #"\x00\x10\x00\x00" out)
    ;; End Block Map
    (write-all-bytes #"\x00\x00\x00\x00\x00\x00\x00\x00" out)
    ;; GUID
    (write-all-bytes #"\x78\x2c\xf3\xaa\x7b\x94\x9a\x43" out)
    (write-all-bytes #"\xa1\x80\x2e\x14\x4e\xc3\x77\x92" out)
    ;; Some offset for variable dispatch
    (write-all-bytes #"\xb8\xff\x03\x00" out)
    ;; Formated/Healthy
    (write-all-bytes #"\x5a\xfe" out)
    ;; Reserved
    (write-all-bytes (make-bytes 2) out)
    ;; Reserved
    (write-all-bytes (make-bytes 4) out)

    (write-efi-variable v1 out)
    (write-efi-variable v2 out)
    (write-efi-variable v3 out)
    (write-efi-variable v4 out)
    (write-efi-variable v5 out)
    (write-efi-variable v6 out)
    (write-efi-variable v7 out)
    (write-efi-variable v8 out)
    (write-efi-variable v9 out)
    (write-efi-variable v10 out)
    (write-efi-variable v11 out)
    (write-efi-variable v12 out)
    (write-efi-variable v13 out)
    (write-efi-variable v14 out)
    (write-efi-variable v15 out)
    (write-efi-variable v16 out)
    (write-efi-variable v17 out)
    (write-efi-variable v18 out)
    (write-efi-variable v19 out)
    (write-efi-variable v20 out)
    (write-efi-variable v21 out)
    (write-efi-variable v22 out)
    (write-efi-variable v23 out)
    (write-efi-variable v24 out)
    (write-efi-variable v25 out)
    (write-efi-variable v26 out)
    (write-efi-variable v27 out)
    (write-efi-variable v28 out)
    (write-efi-variable v29 out)
    (write-efi-variable v30 out)
    (write-efi-variable v31 out)
    (write-efi-variable v32 out)
    (write-efi-variable v33 out)
    (write-efi-variable v34 out)
    (write-efi-variable v35 out)
    (write-efi-variable v36 out)
    (write-efi-variable v37 out)
    (write-efi-variable v38 out)
    (write-efi-variable v39 out)
    (write-efi-variable v40 out)
    (write-efi-variable v41 out)
    (write-efi-variable v42 out)
    (write-efi-variable v43 out)
    (write-efi-variable v44 out)
    (write-efi-variable v45 out)
    (write-efi-variable v46 out)
    (write-efi-variable v47 out)
    (write-efi-variable v48 out)
    (write-efi-variable v49 out)
    (write-efi-variable v50 out)
    (write-efi-variable v51 out)
    (write-efi-variable v52 out)
    (write-efi-variable v53 out)
    (write-efi-variable v54 out)
    (write-efi-variable v55 out)
    (write-efi-variable v56 out)
    (write-efi-variable v57 out)
    (write-efi-variable v58 out)
    (write-efi-variable v59 out)
    (write-efi-variable v60 out)
    (write-efi-variable v61 out)
    (write-efi-variable v62 out)
    (write-efi-variable v63 out)
    (write-efi-variable v64 out)
    (write-efi-variable v65 out)
    (write-efi-variable v66 out)
    (write-efi-variable v67 out)
    (write-efi-variable v68 out)
    (write-efi-variable v69 out)
    (write-efi-variable v70 out)
    (write-efi-variable v71 out)
    (write-efi-variable v72 out)
    (write-efi-variable v73 out)
    (write-efi-variable v74 out)
    (write-efi-variable v75 out)
    (write-efi-variable v76 out)
    (write-efi-variable v77 out)
    (write-efi-variable v78 out)
    (write-efi-variable v79 out)
    (write-efi-variable v80 out)
    (write-efi-variable v81 out)
    (write-efi-variable v82 out)
    (write-efi-variable v83 out)
    (write-efi-variable v84 out)
    (write-efi-variable v85 out)
    (write-efi-variable v86 out)
    (write-efi-variable v87 out)
    (write-efi-variable v88 out)
    (write-efi-variable v89 out)
    (write-efi-variable v90 out)
    (write-efi-variable v91 out)
    (write-efi-variable v92 out)
    (write-efi-variable v93 out)
    (write-efi-variable v94 out)
    (write-efi-variable v95 out)
    (write-efi-variable v96 out)
    (write-efi-variable v97 out)
    (write-efi-variable v98 out)
    (write-efi-variable v99 out)
    (write-efi-variable v100 out)
    (write-efi-variable v101 out)
    (write-efi-variable v102 out)
    (write-efi-variable v103 out)
    (write-efi-variable v104 out)
    (write-efi-variable v105 out)
    (write-efi-variable v106 out)
    (write-efi-variable v107 out)
    (write-efi-variable v108 out)
    (write-efi-variable v109 out)
    (write-efi-variable v110 out)
    (write-efi-variable v111 out)
    (write-efi-variable v112 out)
    (write-efi-variable v113 out)
    (write-efi-variable v114 out)
    (write-efi-variable v115 out)
    (write-efi-variable v116 out)
    (write-efi-variable v117 out)
    (write-efi-variable v118 out)
    (write-all-bytes (make-bytes #x3ae2c #xff) out)
    ;; Working Block
    ;; GUID
    (write-all-bytes #"\x2b\x29\x58\x9e\x68\x7c\x7d\x49\xa0\xce\x65\x00\xfd\x9f\x1b\x95" out)
    ;; CRC
    (write-all-bytes #"\x2c\xaf\x2c\x64" out) 
    ;; Invalid/Valid and reserved
    (write-all-bytes #"\xfe\xff\xff\xff" out)
    ;; Write Queue Size
    (write-all-bytes #"\xe0\x0f\x00\x00\x00\x00\x00\x00" out)
    (write-all-bytes (make-bytes #x42fe0 #xff) out)
    ))
