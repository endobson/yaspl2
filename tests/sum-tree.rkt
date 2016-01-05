(module sum-tree
  (import (prim +))
  (export main)
  (types
    (define-type Tree
      (node [v Byte] [left Tree] [right Tree])
      (leaf)))

  (define (sum-tree t)
    (case t
      [(node v left right) (+ v (+ (sum-tree left) (sum-tree right)))]
      [(leaf) 0]))

  (define (main stdin stdout stderr)
    (sum-tree (node 4
                    (node 3 (node 1 (leaf) (node 2 (leaf) (leaf))) (leaf))
                    (node 5 (leaf) (leaf))))))
#:test-cases
(#:module-name sum-tree #:exit-code 15)
