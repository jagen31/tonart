#lang racket/base
(require racket/match racket/list racket/function syntax/parse lens
         rsound rsound/envelope)

(struct space/s [class start end id parent value] #:transparent)
(struct transform/s [body] #:transparent)
(define-struct-lenses space/s)

(define (within? s s* env)
  (match* (s s*)
    [((space/s _ s e _ _ _) (space/s _ s* e* _ _ _))
     (and (>= s s*) (<= e e*) (not (= s e*)))]))

(define (subspace? s s* all)
  (match* (s s*)
    [((space/s _ _ _ _ #f _) _) #f]
    [((space/s _ _ _ _ parent _) (space/s _ _ _ id _ _))
     (or (eq? parent id)
         (subspace? (space-by-id parent all) s* all))]))

(define (direct-subspace? s s* all)
  (match* (s s*)
    [((space/s _ _ _ _ parent _) (space/s _ _ _ id _ _))
     (eq? parent id)]
    [(_ _) #f]))

(define (space-subspaces space all)
  (filter (λ(s) (subspace? s space all)) all))

(define (space-direct-subspaces space all)
  (filter (λ(s) (direct-subspace? s space all)) all))

(define (space-offset space offset)
  (lens-transform space/s-end-lens
                  (lens-transform space/s-start-lens space (curry + offset))
                  (curry + offset)))

(define (partition-subspaces space all)
  (partition (λ(s) (subspace? s space all)) all))

(define (space-by-id id spaces)
  (match spaces
    ['() #f]
    [(cons (and space (space/s _ _ _ id* _ _)) more) #:when (eq? id id*)
     space]
    [(cons _ more)
     (space-by-id id more)]))

(define undefined (gensym))

;; Copy a space and all subspaces.  This creates a fresh id for the copy,
;; and sets the subspace parent to the fresh id.  Optionally, set a new
;; parent for the copy.  This copies it into the env, returning a new env
;; of all spaces
(define (space-copy space all #:parent* [parent* undefined])
  (match space
    [(space/s class start end _ parent value)
     (define id* (gensym))
     (define subspaces (space-direct-subspaces space all))
     (define all*
       (cons
        (lens-set space/s-id-lens
                  (lens-set space/s-parent-lens
                            space
                            (if (eq? parent* undefined) parent parent*))
                  id*)
        all))
     (foldr (λ(s acc) (space-copy s acc #:parent* id*))
            all*
            subspaces)]))

;; Remove a space and all subspaces
(define (space-remove space all)
  (match space
    [(space/s _ _ _ id _ _)
     (define subspaces (space-direct-subspaces space all))
     (define all*
       (foldr (λ(s acc) (space-remove s acc))
              all
              subspaces))
     (define all** (filter (λ(s) (not (eq? (space/s-id s) id))) all*))
     all**]))

(define (bounds spaces)
  (for/fold ([start +inf.0] [end -inf.0])
            ([space spaces])
    (match space
      [(space/s _ start* end* _ _ _)
       (values (min start start*) (max end end*))])))

(define (try-expand-loop space all)
  (match space
    [(space/s 'loop start end id parent _)
     (define subspaces (space-direct-subspaces space all))
     (define-values (min-start max-end) (bounds subspaces))
     (define len (- end start))
     (define loop-len (- max-end min-start))
     (define copies (floor (/ len loop-len)))
     (define all*
       (for/fold ([acc all])
                 ([space subspaces])
         (match space
           [(space/s class start* end* id _ _)
            (println copies)
            (for/fold ([acc acc] [offset 0] #:result acc)
                      ([i (in-range copies)])
              ;; new parent is former parent of loop
              (values (space-copy (space-offset space offset) acc #:parent* parent)
                      (+ offset loop-len)))])))
     (space-remove space all*)]
    [_ #f]))

(define (try-note->tone space all)
  (match space
    [(space/s 'note start end id parent _)
     (match (space-subspaces space all)
       [(list-no-order (space/s 'pitch _ _ _ _ p)
                       (space/s 'accidental _ _ _ _ a)
                       (space/s 'octave _ _ _ _ o))
     (define semitones
       (+ (match p ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11])
          a (* 12 o) 12))
     (cons (space/s 'tone start end id parent
                    (* 440 (expt (expt 2 1/12) (- semitones 69))))
           (space-remove space all))])]
    [_ #f]))

(define (make-note p a o s e [parent #f])
  (define note-id (gensym))
  (define the-note (space/s 'note s e note-id parent #f))
  (define the-pitch (space/s 'pitch s e (gensym) note-id p))
  (define the-accidental (space/s 'accidental s e (gensym) note-id a))
  (define the-octave (space/s 'octave s e (gensym) note-id o))
  (list the-note the-pitch the-accidental the-octave))

(define (try-expand-theme1 space all)
  (match space
    [(space/s 'theme1 start end id parent _)
     (append (make-note 'a 0 4 start (+ start 0.5))
             (make-note 'b 0 4 (+ start 0.5) (+ start 1))
             (make-note 'c 0 5 (+ start 1) (+ start 1.5))
             (make-note 'd 0 5 (+ start 1.5) (+ start 2))
             (make-note 'e 0 5 (+ start 2) (+ start 4))
             (space-remove space all))]
    [_ #f]))

(define FPS 44100)
(define ->frames (compose round (curry * FPS)))

(define (tone->sound tone)
  (match tone
    [(space/s 'tone (app ->frames start) (app ->frames end) id parent
              value)
     (define len (- end start))
     (define sound (rs-mult (make-tone value .2 len)
                            ((adsr 2 1.0 2 1.0 (round (* 1/4 len))) len)))
     (cons start sound)]))

(define (apply transform spaces)
  (for/fold ([acc spaces])
            ([space spaces])
    (or ((transform/s-body transform) space acc) acc)))

(define (play comp)
  (define the-sounds (map tone->sound comp))
  (define my-pstream (make-pstream))
  (for ([start+sound the-sounds])
    (match start+sound
      [(cons start sound) (pstream-queue my-pstream sound start)])))

(define loop-id (gensym))

(define the-theme1 (space/s 'theme1 0 8 (gensym) #f #f))
(define the-loop (space/s 'loop 0 4 loop-id #f #f))
(define the-note1s (make-note 'a 0 3 0 1 loop-id))
(define the-note2s (make-note 'e 0 4 1 2 loop-id))

(define all
  (append (list the-theme1 the-loop)
          the-note1s the-note2s))

(define expand-loop-transform (transform/s try-expand-loop))
(define expand-theme1-transform (transform/s try-expand-theme1))
(define note->tone-transform (transform/s try-note->tone))

(define expanded-loop (apply expand-loop-transform all))
expanded-loop
(define expanded-theme (apply expand-theme1-transform expanded-loop))
expanded-theme
(define tones (apply note->tone-transform expanded-theme))
tones

(set-output-device! 1)
(play tones)
