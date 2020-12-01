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

(define (write-end-device-path-node out)
  ; Type    : 0x7f
  ; Subtype : 0xff
  ; Length  : 4 (0x0004)
  (write-all-bytes #"\x7f\xff\x04\x00" out))

(define (write-device-path p out)
  (for ([node (in-list p)])
    (write-device-path-node node out))
  (write-end-device-path-node out))

(define (device-path->bytes p)
  (call-with-output-bytes (lambda (out) (write-device-path p out))))

(struct load-option ())

(define (write-load-option o out)
  (define path 
    (list (firmware-volume-device-path-node
            (hex-string->bytes "c9bdb87cebf8344faaea3ee4af6516a1"))
          (firmware-file-device-path-node
            (hex-string->bytes "21aa2c4614760345836e8ab6f4662331"))))
  (define contents (device-path->bytes path))
  (write-all-bytes #"\x09\x01\x00\x00" out)
  (write-all-bytes (integer->integer-bytes (bytes-length contents) 2 #f #f) out)
  (write-all-bytes (ascii->utf-16 "UiApp\0") out)
  (write-all-bytes contents out))

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
      (load-option)) 
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
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x030341d0 0)))
    ))
(define v12
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)))
    ))
(define v13
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x030341d0 0)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        ))
    ))
(define v14
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        ))
    ))
(define v15
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 1)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        ))
    ))
(define v16
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x030341d0 0)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 1)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        ))
    ))
(define v17
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 1)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        ))
    ))
(define v18
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 1)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 2)
        acpi-adr-device-path-node1
        ))
    ))
(define v19
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x030341d0 0)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 1)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        vendor-device-path-node1
        uart-device-path-node2
        (vendor-defined-device-path-node)
        ))
    ))

(define v20
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 1)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 2)
        acpi-adr-device-path-node1
        (end-device-path-node)
        vendor-device-path-node1
        uart-device-path-node2
        (vendor-defined-device-path-node)
        ))
    ))
(define v21
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 1)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        vendor-device-path-node1
        uart-device-path-node2
        (vendor-defined-device-path-node)
        ))
    ))
(define v22
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x030341d0 0)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 1)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        vendor-device-path-node1
        uart-device-path-node2
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (usb-device-path-node)
        ))
    ))
(define v23
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 1)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 2)
        acpi-adr-device-path-node1
        (end-device-path-node)
        vendor-device-path-node1
        uart-device-path-node2
        (vendor-defined-device-path-node)
        (end-device-path-node)
        vendor-device-path-node2
        acpi-adr-device-path-node2
        ))
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
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 2)
        acpi-adr-device-path-node1
        (end-device-path-node)
        vendor-device-path-node1
        uart-device-path-node2
        (vendor-defined-device-path-node)
        (end-device-path-node)
        vendor-device-path-node2
        acpi-adr-device-path-node2
        ))
    ))

(define v27
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 2)
        acpi-adr-device-path-node1
        (end-device-path-node)
        vendor-device-path-node2
        acpi-adr-device-path-node2
        ))
    ))
(define v28
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConOut"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 2)
        acpi-adr-device-path-node1
        ))
    ))
(define v29
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x030341d0 0)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        vendor-device-path-node1
        uart-device-path-node2
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (usb-device-path-node)
        ))
    ))
(define v30
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ConIn"             ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x030341d0 0)
        (end-device-path-node)
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        (usb-device-path-node)
        ))
    ))
(define v31
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
        (end-device-path-node)
        vendor-device-path-node1
        uart-device-path-node2
        (vendor-defined-device-path-node)
        ))
    ))
(define v32
  (efi-variable
    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
    "ErrOut"            ; Name
    #"\x3c\x00"         ; State
    #"\x07\x00"         ; Attributes
    (device-path->bytes ; Contents
      (list
        (acpi-device-path-node #x0a0341d0 0)
        (pci-device-path-node 0 1)
        (acpi-device-path-node #x050141d0 0)
        uart-device-path-node1
        (vendor-defined-device-path-node)
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
;(define v34
;  (efi-variable
;    (hex-string->bytes "61dfe48bca93d211aa0d00e098032b8c")
;    "Boot0001"          ; Name
;    #"\x3d\x00"         ; State
;    #"\x07\x00"         ; Attributes
;    (load-option->bytes ; Contents
;      (load-option)) 
;    ))

(call-with-output-file output-path
  (lambda (out)
    (write-all-bytes (make-bytes #x60 #xff) out)
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
    ;(write-efi-variable v34 out)
    ))
