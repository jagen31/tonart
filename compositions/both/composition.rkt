#lang racket/base

(require tonart/core tonart/lib
         (prefix-in canon: tonart/compositions/canon/composition)
         (prefix-in pan: tonart/compositions/pan/composition))

(define-music loop1
  (merge (do (loop 2 (do pan:pattern1 (rename-hole b a)))
             canon:line1)
         (do (loop 2 (do pan:pattern1 (rename-hole b a)))
             canon:line2 (note->midi))
         pan:loop1))

#;(play (perform (do ([0 8] loop1 (note->tone)))))
