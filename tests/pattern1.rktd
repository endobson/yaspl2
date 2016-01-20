(module pattern1
  (import
    (prim +)
    (io read-all-bytes write-all-bytes))
  (export main)
  (types
    (define-type Foo
      (foo-a)
      (foo-b [b1 Byte])
      (foo-c [c1 Bytes] [c2 Foo] [c3 Foo])))
  (define (main [stdin : InputPort] [stdout : OutputPort] [stderr : OutputPort]) : Byte
    (let ([v (foo-c #"b1" (foo-b 3) (foo-c #"b2" (foo-a) (foo-b 5)))])
      (case (read-all-bytes stdin)
        [#"1"
         (case v
           [foo 1])]
        [#"2"
         (case v
           [(foo-c b1 _ (foo-c b2 _ _))
            (begin
              (write-all-bytes b1 stdout)
              (write-all-bytes b2 stdout)
              0)])]
        [#"3"
         (case v
           [(foo-c b1 (foo-b x) (foo-c b2 (foo-b y) (foo-b z)))
            1]
           [(foo-c b1 (foo-b x) (foo-c b2 (foo-a) (foo-b z)))
            (+ x z)])]))))

#:test-cases
(#:module-name pattern1 #:stdin #"1" #:exit-code 1)
(#:module-name pattern1 #:stdin #"2" #:stdout #"b1b2")
(#:module-name pattern1 #:stdin #"3" #:exit-code 8)
