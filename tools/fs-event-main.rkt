#lang racket/base

(require racket/match)

(struct node (path box))
(struct inner-node (raw-evt evt))
(struct directory-inner-node inner-node (children))


(define (create-recursive-filesystem-change-node path)
  (define a-box (box #f))
  (define n (node path a-box))
  (initialize-recursive-filesystem-change-node n)
  n)

(define (initialize-recursive-filesystem-change-node n)
  (define a-box (node-box n))
  (define path (node-path n))
  (define raw-evt (filesystem-change-evt path))
  (define wrapped-evt (wrap-evt raw-evt (lambda (x) n)))
  (when (unbox a-box)
    (cancel-inner-node (unbox a-box)))
  (set-box! a-box
    (if (directory-exists? path)
        (directory-inner-node
          raw-evt
          wrapped-evt
          (map create-recursive-filesystem-change-node (directory-list path #:build? #t)))
        (inner-node raw-evt wrapped-evt)))
  n)

(define (cancel-inner-node n)
  (filesystem-change-evt-cancel (inner-node-raw-evt n))
  (when (directory-inner-node? n)
    (for ([child (directory-inner-node-children n)])
      (cancel-inner-node (unbox (node-box child))))))

(define (recursive-filesystem-change-node->evt n)
  (define inner (unbox (node-box n)))
  (if (directory-inner-node? inner)
      (apply choice-evt
             (inner-node-evt inner)
             (map recursive-filesystem-change-node->evt (directory-inner-node-children inner)))
      (inner-node-evt inner)))



(define root
  (match (current-command-line-arguments)
    [(vector v)
     (create-recursive-filesystem-change-node v)]))

(let loop ()
  (define n (sync (recursive-filesystem-change-node->evt root)))
  (displayln (node-path n))
  (initialize-recursive-filesystem-change-node n)
  (loop))
