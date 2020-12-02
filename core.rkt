#lang racket/base

(require match-plus racket/generic rsound racket/function racket/dict
 (for-syntax racket/base match-plus syntax/parse racket/dict racket/match racket/set
             racket/hash racket/syntax racket/function racket/list syntax/id-set data/gvector)
 (for-meta 2 racket/base syntax/parse))

(provide coord define-object declare-transformer define-transformer-instance define-music
         perform do merge in put comp*
         play rs-write
 (for-syntax coord current-env current-coord objects ~comp coord-start for/comp comp->comp*))

(struct coord [start end] #:transparent)

(begin-for-syntax

(define current-env (make-parameter (hash)))
(define objects (mutable-free-id-set))

(struct coord [start end] #:transparent)
(define current-coord (make-parameter (coord 0 +inf.0)))

(define-syntax-class bind
  (pattern type:id #:attr id #f)
  (pattern [id:id {~datum :} type:id]))

(define-syntax-class ~comp
 (pattern (_ {~and c ([start end] expr ...)} ...)
   #:attr comp-value
   (for/fold ([acc (hash)])
             ([clause (syntax->list #'(c ...))])
    (syntax-parse clause
      [([start end] exprs ...)
       (hash-update acc
        (coord (syntax->datum #'start) (syntax->datum #'end))
        (curry append (syntax->list #'(exprs ...)))
        '())]))))

(struct transformer-instance [typ f] #:transparent)

(define (comp->comp* comp)
 #`(comp* #,@(for/list ([(k v) (in-dict comp)])
    (match k
     [(coord start end)
      #`([#,start #,end] #,@v)]))))

(define (render c)
  (define clauses
    (for/list ([(k v) (in-dict c)])
      (match k
        [(coord start end) #`(cons (coord #,start #,end) (list #,@v))])))
  #`(list #,@clauses))

(define (mobject-ref stx) (syntax-parse stx [(id:id args ...) #'id]))

(define/match* (within? (coord s1 e1) (coord s2 e2))
  (and (>= s1 s2) (<= e1 e2) (not (= e1 s2))))

;; https://lexi-lambda.github.io/blog/2016/02/18/simple-safe-multimethods-in-racket/
(struct transformer (arity clauses-name clauses)
  #:transparent
  #:property prop:procedure
  (λ (method stx) (syntax-parse stx [(id arg ...) #'(convert id arg ...)])))

(struct transform-type [env in out] #:transparent)
(define/match* (print-type (transform-type env in out))
  (format "~s => ~s -> ~s" (free-id-set->list env) (free-id-set->list in) (free-id-set->list out)))

(define (comp-type comp)
 (for*/fold ([acc (set)])
            ([(k v) (in-dict comp)] [e v])
    (set-add acc (mobject-ref e))))

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

(struct mobject-info [related name] #:transparent
  #:property prop:procedure
  (λ (method stx) (syntax-parse stx [(id arg ...) #'(convert2 id arg ...)]))))

(define-syntax convert
 (syntax-parser
  [(_ id arg ...)
   (comp->comp* (apply-transformer #'id (current-coord) (current-env) (syntax->list #'(arg ...))))]))

(define-syntax convert2
 (syntax-parser
  [(_ id arg ...)
   #:do [(match-define (mobject-info _ name) (syntax-local-value #'id))]
   #:with name* name
   #'(name* arg ...)]))

(define-syntax with
 (syntax-parser
  [(_ bound expr)
   (syntax-parse (local-expand #'bound 'expression (append (list #'comp*) (free-id-set->list objects)))
     [c:~comp
      (parameterize ([current-env (attribute c.comp-value)])
       (local-expand #'expr 'expression (append (list #'comp*) (free-id-set->list objects))))])]))

(define-syntax merge
 (syntax-parser
  [(_ expr ...)
   (comp->comp*
    (for/fold ([acc (hash)])
              ([e (syntax->list #'(expr ...))])
      (syntax-parse (local-expand e 'expression (append (list #'comp*) (free-id-set->list objects)))
        [c:~comp (hash-union acc (attribute c.comp-value) #:combine append)])))]))

(define-syntax in
 (syntax-parser
  [(_ [start* end*] expr)
   (match-define (coord start end) (current-coord))
   (parameterize ([current-coord
                   (coord (+ start (syntax->datum #'start*)) (+ start (syntax->datum #'end*)))])
     (local-expand #'expr 'expression (append (list #'comp*) (free-id-set->list objects))))]))

(define-syntax (comp* stx)
 (syntax-parse stx
  [c:~comp (render (attribute c.comp-value))]))

(define-syntax define-object
 (syntax-parser
  [(_ name [fields ...] {~optional {~seq {~and can-perform #:perform} cpat exprs ...}})
   #:with oname (format-id #'name "~a~a" (syntax->datum #'name) "-object")
   (free-id-set-add! objects #'name)
   (define st
    (if (attribute can-perform)
     #'(struct oname [fields ...] #:transparent
        #:methods gen:performable
        [(define/match* (gen-perform (oname fields ...) cpat) exprs ...)])
     #'(struct oname [fields ...] #:transparent)))
   #`(begin
      #,st
      (define-syntax name (mobject-info (mutable-free-id-set) #'oname)))]))

(define-syntax define-music
 (syntax-parser
  [(_ name expr)
   #'(define-syntax name
      (syntax-parser
       [_:id (local-expand #'expr 'expression (append (list #'comp*) (free-id-set->list objects)))]))]))

(define-for-syntax (extract-env-value comp typ coord)
 (for*/first ([(k v) (in-dict comp)] #:when (within? coord k)
              [e v] #:when (free-identifier=? (mobject-ref e) typ))
  e))

(define-for-syntax (partition-env-by-value comp typ coord)
 (for/fold ([in (hash)] [out (hash)])
           ([(k v) (in-dict comp)])
  (cond [(within? k coord)
         (define-values (in* out*)
          (partition (λ(e) (free-identifier=? typ (mobject-ref e))) v))
          (values (hash-update in k (curry append in*) '())
                  (hash-update out k (curry append out*) '()))]
        [else (values in (hash-update out k (curry append v) '()))])))

(define-syntax declare-transformer
 (syntax-parser
  [(_ name:id) #'(declare-transformer (name))]
  [(_ (name args:id ...))
   #:with instances* (format-id #'name "~a~a" (syntax->datum #'name) "-instances" #:source #'name)
   #`(begin
      (define-for-syntax instances* (make-gvector))
      (define-syntax name (transformer #,(length (syntax-e #'(args ...))) #'instances* instances*)))]))

;; TODO: break into more primitive concepts
(define-syntax define-transformer-instance
 (syntax-parser
  [(_ name:id expr ...) #'(define-transformer-instance (name) expr ...)]
  [(_ (name:id arg ...)
      {env:bind ... {~datum =>} in:bind ... {~datum ->} out:id ...}
      body ...)
   #:do [(define-values (env-typs env-clauses)
          (for/fold ([typs '()] [clauses '()])
                    ([e (syntax->list #'(env ...))])
           (syntax-parse e
            [e:bind
             (define id (attribute e.id))
             (define typ (attribute e.type))
             (values
              (cons typ typs)
              (cons #`((#,(or id (gensym))) (extract-env-value comp #'#,typ coord)) clauses))])))
         (define-values (in-typs in-clauses)
          (for/fold ([typs '()] [clauses '()])
                    ([e (syntax->list #'(in ...))])
           (syntax-parse e
            [e:bind
             (define id (attribute e.id))
             (define typ (attribute e.type))
             (values
              (cons typ typs)
              (cons #`((#,(or id (gensym)) comp) (partition-env-by-value comp #'#,typ coord)) clauses))])))
         (match-define (transformer arity instances _) (syntax-local-value #'name))]
   #:with [(env-typ* ...) (in-typ* ...) (clause ...)]
    #`[(#,@env-typs) (#,@in-typs) (#,@env-clauses #,@in-clauses)]
   #:with fun*
    #`(λ(arg ... coord comp)
        (let*-values (clause ...) (hash-union comp (let () body ...) #:combine append)))
   #:with fun-name (gensym)
   #:with instances* instances
   #'(begin-for-syntax
      (gvector-add!
       instances*
       (transformer-instance
        (transform-type
         (immutable-free-id-set (set #'env-typ* ...))
         (immutable-free-id-set (set #'in-typ* ...))
         (immutable-free-id-set (set #'out ...)))
        fun*)))]))

(define-for-syntax (apply-transformer id coord comp args)
 (match-define (transformer arity _ clauses) (syntax-local-value id))
 (define comp-info (comp-type comp))
 (define comp-type-set (immutable-free-id-set comp-info))
 (define comp-relateds (set-map comp-info (compose mobject-info-related syntax-local-value)))
 (define-values (transform-inputs transforms)
  (for/fold ([transform-inputs (immutable-free-id-set)] [transforms '()])
            ([clause clauses])
   (match clause
    [(transformer-instance (transform-type reqs inputs _) f)
     (if (and (for/and ([req reqs]) (extract-env-value comp req coord))
              (or free-id-set-member? #'all (not (free-id-set-empty? (free-id-set-intersect inputs comp-type-set)))))
         (values (free-id-set-union transform-inputs inputs)
                 (cons f transforms))
         (values transform-inputs transforms))])))
 (for ([type (in-free-id-set comp-type-set)])
  (when (and (not (free-id-set-member? transform-inputs type))
             (ormap (λ(related) (free-id-set-member? related type)) comp-relateds))
   (error 'apply-transformer
    "found related type ~s not transformed by transformer ~s with instances: ~s"
    type id (map (compose print-type transformer-instance-typ) (gvector->list clauses)))))
 (for/fold ([acc comp])
           ([f transforms])
  (apply f (append args (list coord acc)))))

;; separate complication guarantee
(define-syntax provide-transformer
 (syntax-parser
  [(_ id:id)
   #:do [(match-define (transformer arity instances _) (syntax-local-value #'id))]
   #:with instances* instances
   #`(provide id (for-syntax instances*))]))

(define-generics performable (gen-perform performable coord))

(define FPS 44100)
(define ->frames (compose round (curry * FPS)))

;; perform one expression using the defined performers.
(define/match* (perform1 (coord (app ->frames start) (app ->frames end)) expr sound)
  (cond [(performable? expr)
         (define sound* (gen-perform expr (coord start end)))
         (rs-overlay ((if (> start 0) (curry rs-append (silence start)) identity) sound*)
                     sound)]
        [else (error 'perform "cannot perform ~s" expr)]))

(define (perform comp)
  (for*/fold ([acc (silence 1)])
             ([(k v) (in-dict comp)] [e v])
    (perform1 k e acc)))

(declare-transformer (put val))
(define-transformer-instance (put val)
 {=> all -> _}
 (hash (current-coord) (list val)))

(provide-transformer put)

(define-syntax do
  (syntax-parser
    [(_ ([start:number end:number] expr exprs ...) next more ...)
     #'(with (in [start end] (do expr exprs ...)) (do next more ...))]
    [(_ ([start:number end:number] expr exprs ...)) #'(in [start end] (do expr exprs ...))]
    [(_ expr next more ...) #'(with expr (do next more ...))]
    [(_ expr) #'expr]))
