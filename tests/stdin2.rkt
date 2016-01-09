(module stdin2
  (import
    (numbers decimal-bytes->integer)
    (io read-all-bytes))
  (export main)
  (types)

  (define (main [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (decimal-bytes->integer (read-all-bytes stdin))))
#:test-cases
(#:module-name stdin2 #:stdin #"123" #:exit-code 123)
