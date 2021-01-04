#lang racket/base

(require tonart/core
         (for-syntax rsound rsound/piano-tones rsound/envelope
                     racket/base racket/dict syntax/parse
                     racket/hash racket/match racket/format
                     racket/function))

(provide
  @ loop
  fill-hole rename-hole
  note->midi note->tone
  perform-tone
  perform-midi
  play show
  (for-syntax comp-dim))

(define-art-macro (@ comp c stx)
 (syntax-parse stx
  [(_ exprs ...)  (hash-union comp (expand-seq (hash) c (syntax-e #'(exprs ...)))
                   #:combine append)]))

(define-for-syntax (comp-dim comp)
 (for/fold ([start +inf.0] [end 0] #:result (coord start end))
           ([(k _) comp])
  (match k [(coord start* end*) (values (min start* start) (max end* end))])))

(define-art-macro (loop comp c stx)
 (syntax-parse stx
  [(_ expr ...)
   (match c
    [(coord start end)
     (for/fold ([acc comp] [i 0] #:result acc)
               ([_ (in-naturals)])
               #:break (>= i end)
      (define expanded (expand-seq comp (coord i end) (syntax-e #'(expr ...))))
      (match-define (coord _ end*) (comp-dim expanded))
      (values (hash-union expanded acc #:combine append) end*))])]))

(define-art-macro (note->midi comp c stx)
 (for/comp c [e comp]
  (syntax-parse #'e
   [({~literal note} p a o)
    (hash c
     (list
     #`(midi #,(+ (match (syntax->datum #'p)
                   ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11])
                   (syntax->datum #'a) (* 12 (syntax->datum #'o)) 12))))]
   [e (hash c (list #'e))])))

(define-art-macro (note->tone comp c stx)
 (for/comp c [e comp]
  (syntax-parse #'e
   [({~literal note} p a o)
    (define semitones
     (+ (syntax-parse #'p
         #:datum-literals [c d e f g a b]
         [c 0] [d 2] [e 4] [f 5] [g 7] [a 9] [b 11])
         (syntax->datum #'a) (* 12 (syntax->datum #'o)) 12))
    (hash c (list #`(tone #,(* 440 (expt (expt 2 1/12) (- semitones 69))))))]
   [_ (hash c (list #'e))])))

(define-art-macro (perform-tone comp c stx)
 (for/comp (and c (coord (app ->frames start) (app ->frames end))) [e comp]
  (syntax-parse #'e
   [({~literal tone} hz)
    (define len (- end start))
    (define sound (rs-mult (make-tone (syntax->datum #'hz) .2 len) ((adsr 2 1.0 2 1.0 (round (* 1/4 len))) len)))
    (hash c (list (syntax-property #'my-rsound 'sound sound)))]
   [e (hash c (list #'e))])))

(define-for-syntax FPS 44100)
(define-for-syntax ->frames (compose round (curry * FPS)))

(define-art-macro (perform-midi comp c stx)
 (for/comp (and c (coord (app ->frames start) (app ->frames end))) [e comp]
  (syntax-parse #'e
   [({~literal midi} tone)
    (define len (- end start))
    (define sound (rs-mult (piano-tone (syntax->datum #'tone)) ((adsr 2 1.0 2 1.0 (round (* 1/4 len))) len)))
    (hash c (list (syntax-property #'my-rsound 'sound sound)))]
   [e (hash c (list #'e))])))

(define-art-macro (play comp c stx)
 (define my-pstream (make-pstream))
 (for/comp (and c (coord (app ->frames start) (app ->frames end))) [e comp]
  (syntax-parse #'e
   [{~literal my-rsound}
    (pstream-queue my-pstream (syntax-property #'e 'sound) start)
    (hash c (list #'e))]
   [e (hash c (list #'e))])))

(define-art-macro (show comp c stx) (println comp) comp)

(define-art-macro (fill-hole comp c expr)
 (syntax-parse expr
  [(_ n expr)
   (for/comp c [h comp]
    (hash c
     (list
      (syntax-parse #'h
       [({~literal hole} n*) #:when (free-identifier=? #'n* #'n) #'expr]
       [_ #'h]))))]))

(define-art-macro (rename-hole comp c stx)
 (syntax-parse stx
  [(_ n n*)
   (for/comp c [h comp]
    (hash c
     (list
      (syntax-parse #'h
       [({~literal hole} n**) #:when (free-identifier=? #'n** #'n) #'(hole n*)]
       [_ #'h]))))]))
