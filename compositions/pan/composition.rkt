#lang racket/base

(require tonart/core tonart/lib (for-syntax rsound racket/base syntax/parse))
(provide pattern1 a-full perform-pans)

(define-art-macro (swap-pans comp c stx)
 (for/comp c [p comp]
  (hash c
   (list
    (syntax-parse #'p
     [({~literal pan} 1) #'(pan 2)]
     [({~literal pan} 2) #'(pan 1)]
     [e #'e])))))

(define-art-macro (perform-pans comp c stx)
 ;; https://freesound.org/people/ryancacophony/sounds/202009/
 (define pan1 (rs-scale .25 (rs-read "pan1.wav")))
 ;; https://freesound.org/people/mrThomas2000/sounds/542936/
 (define pan2 (rs-scale .25 (rs-read "pan2.wav")))
 (for/comp c [e comp]
  (syntax-parse #'e
   [({~literal pan} n)
    (hash c (list (syntax-property #'my-rsound 'sound (if (= (syntax->datum #'n) 1) pan1 pan2))))]
   [e (hash c (list #'e))])))

(define-music pattern1
 (in [0 .5] (hole a))
 (in [.5 1] (hole b))
 (in [1 1.5] (hole a))
 (in [1.25 1.75] (hole a))
 (in [1.5 2] (hole b)))

(define-music a-full 
 (@ pattern1 (fill-hole a (pan 1)) (fill-hole b (pan 2)))
 (in [0 .5] (note a 0 4))
 (in [.5 1.5] (note c 0 4))
 (in [1.5 2] (note c 0 5)))

(define-music composition
 (bind s1 (in [0 8] (loop a-full)))
 (in+ [(/ (end s1) 2) (end s1)] (swap-pans)))

#;(music
 composition
 (note->tone)
 (perform-pans)
 (perform-tone)
 (play))
