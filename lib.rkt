#lang racket/base

(require tonart/core rsound rsound/piano-tones rsound/envelope
         (for-syntax racket/base racket/dict syntax/parse
                     racket/hash racket/match racket/format))

(provide
  (for-syntax translate)
  loop
  hole hole-object fill-hole rename-hole
  note note->midi note->tone
  midi t/midi
  tone
  quote-comp)

(define-for-syntax (translate comp t)
  (for/hash ([(k v) (in-dict comp)])
    (match k [(coord start end) (values (coord (+ start t) (+ end t)) v)])))

;; this can be a transformer if I can get my act together re patterns
(define-syntax quote-comp
 (syntax-parser
  [(_ c)
   (comp->comp*
    (syntax-parse (local-expand #'c 'expression (list #'comp*))
     [c*:~comp (for/comp k [e (attribute c*.comp-value)]
                (hash k (list #''e)))]))]))

(declare-transformer (loop length expr))
(define-transformer-instance (loop length expr)
 {=> all -> _}
 (define s (coord-start (current-coord)))
 (define len (syntax->datum length))
 (define comp* (local-expand expr 'expression (list #'comp*)))
 (syntax-parse comp*
   [c:~comp
    (define comp (attribute c.comp-value))
    (match-define (coord start end) (current-coord))
    (for/fold ([acc (hash)]
               [prev (translate comp (- len))]
               #:result acc)
              ([i (in-range (floor (/ (- end start) len)))])
      (define new (translate prev len))
      (values (hash-union new acc #:combine append) new))]))

(define-object hole [name])

(declare-transformer (fill-hole n expr))
(define-transformer-instance (fill-hole n expr)
 {=> [holes : hole] -> _}
 (for/comp c [h holes]
   (hash c
     (list
      (syntax-parse #'h
        [(_ n*) #:when (free-identifier=? #'n* n) expr]
        [_ #'h])))))

(declare-transformer (rename-hole n n*))
(define-transformer-instance (rename-hole n n*)
 {=> [holes : hole] -> _}
 (for/comp c [h holes]
   (hash c
     (list
      (syntax-parse #'h
        [(_ n**) #:when (free-identifier=? #'n** n) #`(hole #,n*)]
        [_ #'h])))))

(define-object note [pitch accidental octave])
(define-object midi [pitch]
 #:perform (coord start end)
 (define len (- end start))
 (rs-mult
  (piano-tone pitch)
  ((adsr 2 1.0 2 1.0 (round (* 1/4 len))) len)))
(define-object tone [freq]
 #:perform (coord start end)
 (define len (- end start))
  (rs-mult
   (make-tone freq .2 len)
   ((adsr 2 1.0 2 1.0 (round (* 1/4 len))) len)))

(declare-transformer note->midi)
(define-transformer-instance note->midi
 {=> [notes : note] -> midi}
 (for/comp c [e notes]
  (syntax-parse #'e
   [(_ p a o)
    (hash c
      (list
       #`(midi #,(+ (syntax-parse #'p
                     #:datum-literals [c d e f g a b]
                     [c 0] [d 2] [e 4] [f 5] [g 7] [a 9] [b 11])
                    (syntax->datum #'a) (* 12 (syntax->datum #'o)) 12))))])))

(declare-transformer note->tone)
(define-transformer-instance note->tone
 {=> [notes : note] -> midi}
 (for/comp c [e notes]
  (syntax-parse #'e
   [(_ p a o)
    (define semitones
     (+ (syntax-parse #'p
         #:datum-literals [c d e f g a b]
         [c 0] [d 2] [e 4] [f 5] [g 7] [a 9] [b 11])
         (syntax->datum #'a) (* 12 (syntax->datum #'o)) 12))
    (hash c (list #`(tone #,(* 440 (expt (expt 2 1/12) (- semitones 69))))))])))

(declare-transformer (t/midi n))
(define-transformer-instance (t/midi n)
 {=> [midis : midi] -> midi}
 (for/comp c [e midis]
  (syntax-parse #'e
   [(_ tone) (hash c (list #`(midi #,(+ (syntax->datum #'tone) (syntax->datum n)))))])))
