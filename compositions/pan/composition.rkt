#lang racket/base

(require rsound tonart/core tonart/lib (for-syntax racket/base syntax/parse))
(provide pattern1 loop1)

;; https://freesound.org/people/ryancacophony/sounds/202009/
(define pan1 (rs-scale .25 (rs-read  "pan1.wav")))
;; https://freesound.org/people/mrThomas2000/sounds/542936/
(define pan2 (rs-scale .25 (rs-read "pan2.wav")))

(define-object pan [n]
 #:perform _
 (cond [(= n 1) pan1] [(= n 2) pan2]))

(declare-transformer swap-pans)
(define-transformer-instance swap-pans
 {=> [pans : pan] -> _}
 (for/comp c [p pans]
  (hash c (list (syntax-parse #'p [(pan 1) #'(pan 2)] [(pan 2) #'(pan 1)])))))

(define-music pattern1
 (do ([0 .5] (put (hole a)))
     ([.5 1] (put (hole b)))
     ([1 1.5] (put (hole a)))
     ([1.25 1.75] (put (hole a)))
     ([1.5 2] (put (hole b)))))

(define-music loop1
  (loop 2
   (merge (do pattern1 (fill-hole a (pan 1)) (fill-hole b (pan 2)))
          (do ([0 .5] (put (note a 0 4)))
              ([.5 1.5] (put (note c 0 4)))
              ([1.5 2] (put (note c 0 5)))))))

#;(play (perform (do ([0 8] loop1) ([4 8] (swap-pans)) (note->tone))))
