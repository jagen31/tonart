#lang racket

(require ee-lib/define ee-lib/errors syntax/parse
         (for-syntax ee-lib syntax/parse racket/match racket/dict racket/function racket/generic racket/hash
                     rsound rsound/piano-tones rsound/envelope
                     match-plus)
         (for-meta 2 racket/base syntax/parse))

(provide (for-syntax coord within? for/comp expand-seq)
         define-art-macro define-music music music_ in)

(define-literal-forms tonart-literals "tonart forms may not be used outside music" [in])

(struct coord [start end] #:transparent)

(begin-for-syntax

(struct coord [start end] #:transparent)

(define/match* (within? (coord s1 e1) (coord s2 e2))
  (and (>= s1 s2) (<= e1 e2) (not (= e1 s2))))

(struct art-binding [exprs] #:transparent)

(define-generics art-macro
  (art-macro-transform art-macro comp loc stx))
(struct art-macro/s [transformer]
  #:methods gen:art-macro
  [(define (art-macro-transform transform comp loc stx)
     ((art-macro/s-transformer transform) comp loc stx))])

(define (fold-expand comp)
  (for*/fold ([acc (hash)]) ([(k v) (in-dict comp)] [e v]) (expand-art acc k e)))
(define (expand-seq comp c li)
  (for*/fold ([acc comp]) ([e li]) (expand-art acc c e)))

(define (partition-env comp coord)
 (for/fold ([in (hash)] [out (hash)])
           ([(k v) (in-dict comp)])
  (cond [(within? k coord) (values (hash-update in k (curry append v) '()) out)]
        [else (values in (hash-update out k (curry append v) '()))])))

(define/hygienic (expand-art comp c stx) #:expression
 (syntax-parse stx
  #:literal-sets (tonart-literals)
  [(in [start:number end:number] expr ...)
   (match c
    [(coord start* end*)
     (expand-seq
      comp
      (coord (+ (syntax->datum #'start) start*) (+ (syntax->datum #'end) start*))
      (syntax-e #'(expr ...)))])]
  [(head:id . rest)
   #:do [(define binding (lookup #'head art-macro?))]
   #:when binding
   (define-values (in out) (partition-env comp c))
   (hash-union (fold-expand (art-macro-transform binding in c stx)) out)]
  [e:id
   #:do [(define val (lookup #'e art-binding?))]
   #:when val
   (expand-seq comp c (art-binding-exprs val))]
  [e (dict-update comp c (curry cons #'e) '())]))

(define (render c)
 (define clauses
  (for/list ([(k v) (in-dict c)])
   (match k
    [(coord start end) #`(cons (coord #,start #,end) (list #,@v))])))
 #`(list #,@clauses))

(define-syntax for/comp
 (syntax-parser
  [(_ coord [bind comp] body ...)
   #'(for/fold ([acc (hash)])
               ([(k v) (in-dict comp)])
      (hash-union acc
       (match k
        [coord (for/fold ([acc (hash)])
                         ([blah v])
                 (with-syntax ([bind blah])
                  (hash-union acc (let () body ...) #:combine append)))])
       #:combine append))]))
)

(define-syntax define-music
 (syntax-parser
  [(_ name e ...)
   #'(define-syntax name (art-binding (list #'e ...)))]))

(define-syntax music
 (syntax-parser
  [(_ e ...)
   (render (expand-seq (hash) (coord 0 +inf.0) (syntax-e #'(e ...))))]))

(define-syntax music_
 (syntax-parser
  [(_ e ...)
   (expand-seq (hash) (coord 0 +inf.0) (syntax-e #'(e ...)))
   #'(void)]))

(define-for-syntax (translate comp t)
 (for/hash ([(k v) (in-dict comp)])
  (match k [(coord start end) (values (coord (+ start t) (+ end t)) v)])))

(define-syntax define-art-macro
  (syntax-parser
    [(_ (n:id comp:id c:id stx:id) body ...)
     #'(define-syntax n (art-macro/s (λ (comp c stx) body ...)))]))
