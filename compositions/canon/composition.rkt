#lang racket/base

(require
 tonart/core tonart/lib
 (for-syntax racket/base racket/match racket/function racket/generator racket/dict syntax/parse))

(provide fill-holes rhythm line1 line2)

(define-art-macro (fill-holes comp c stx)
 (syntax-parse stx
   [(_ name (vals* ...))
    (define holes
     (for/comp c [e comp]
      (syntax-parse #'e
       [({~literal hole} _) (hash c (list #'e))]
       [_ (hash)])))
    (define val/g (generator ()
     (for ([val (in-cycle (syntax->list #'(vals* ...)))]) (yield val))))
    (define hole/g (generator ()
     (for* ([(k v) (in-dict (sort (hash->list holes)
                             (λ(c1 c2) (match* (c1 c2) [((coord s _) (coord s* _)) (< s s*)]))
                             #:key car))]
            [e v]
      #:when (syntax-parse e [(hole name*) (free-identifier=? #'name* #'name)]))
      (yield k))))
    (for/fold ([acc (hash)])
              ([val (in-producer val/g (void))] [k (in-producer hole/g (void))])
      (hash-update acc k (curry cons val) '()))]))

(define-music rhythm
 (in [0 2] (hole a))
 (in [2 2.5] (hole a))
 (in [2.5 2.75] (hole a))
 (in [2.75 3] (hole a))
 (in [3 5] (hole a))
 (in [5 6] (hole a))
 (in [6 6.25] (hole a))
 (in [6.25 7.25] (hole a))
 (in [7.25 8] (hole a))
 (in [8 8.1] (hole a))
 (in [8.1 8.2] (hole a))
 (in [8.2 8.3] (hole a))
 (in [8.3 8.4] (hole a))
 (in [8.4 8.5] (hole a))
 (in [8.5 10] (hole a)))

(define-music line1
 (fill-holes a ((note a 0 4) (note b 0 5) (note c 1 4))))

(define-music line2
 (fill-holes a ((note c 0 4) (note f 1 3) (note g 0 3) (note b -1 3) (note a 0 3))))

#;(music_
 (@ rhythm line1 (note->midi)
 (@ rhythm line2 (note->tone))
 (perform-midi)
 (perform-tone)
 (play)))
