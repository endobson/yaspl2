#lang racket

(require
  racket/dict
  racket/hash
  "util.rkt")

(provide
  make-fat32
  fat32-add-directory
  fat32-add-file
  fat32-cluster-chains
  fat32-file-system
  (struct-out fs-entity)
  (struct-out dir-entity)
  (struct-out file-entity))

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
