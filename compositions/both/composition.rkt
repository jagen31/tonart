#lang racket/base

(require tonart/core tonart/lib
         (prefix-in canon: tonart/compositions/canon/composition)
         (prefix-in pan: tonart/compositions/pan/composition))

(define-music loop1
  (@ (loop (@ pan:pattern1 (rename-hole b a)))
     canon:line1)
  (@ (loop (@ pan:pattern1 (rename-hole b a)))
     canon:line2
     (note->midi))
  (@ (loop pan:a-full)))

#;(music
 (in [0 8] loop1 (note->tone) (perform-midi) (pan:perform-pans) (perform-tone) (play)))
