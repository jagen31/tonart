#lang racket/base

(require
 tonart/core tonart/lib
 (for-syntax racket/base racket/match racket/function racket/generator racket/dict syntax/parse))

(provide fill-holes rhythm line1 line2)

(declare-transformer (fill-holes name vals))
(define-transformer-instance (fill-holes name vals*)
 {=> [holes : hole] -> _}
 (define val/g (generator ()
  (for ([val (in-cycle (syntax->list vals*))]) (yield val))))
 (define hole/g (generator ()
  (for* ([(k v) (in-dict (sort (hash->list holes)
                      (λ(c1 c2) (match* (c1 c2) [((coord s _) (coord s* _)) (< s s*)]))
                       #:key car))]
         [e v]
   #:when (syntax-parse e [(hole name*) (free-identifier=? #'name* name)]))
   (yield k))))
 (for/fold ([acc (hash)])
           ([val (in-producer val/g (void))] [k (in-producer hole/g (void))])
   (hash-update acc k (curry cons val) '())))

(define-music rhythm
 (do ([0 2] (put (hole a))) ([2 2.5] (put (hole a))) ([2.5 2.75] (put (hole a)))
     ([2.75 3] (put (hole a))) ([3 5] (put (hole a))) ([5 6] (put (hole a)))
     ([6 6.25] (put (hole a))) ([6.25 7.25] (put (hole a))) ([7.25 8] (put (hole a)))
     ([8 8.1] (put (hole a))) ([8.1 8.2] (put (hole a))) ([8.2 8.3] (put (hole a)))
     ([8.3 8.4] (put (hole a))) ([8.4 8.5] (put (hole a))) ([8.5 10] (put (hole a)))))

(define-music line1
 (fill-holes a ((note a 0 4) (note b 0 5) (note c 1 4))))

(define-music line2
 (fill-holes a ((note c 0 4) (note f 1 3) (note g 0 3) (note b -1 3) (note a 0 3))))

#;(play (perform (merge (do rhythm line1 (note->midi)) (do rhythm line2 (note->tone)))))
