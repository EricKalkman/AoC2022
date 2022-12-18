#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define expect-input (at-least-one (expv-set '(#\> #\<))))

(define test-inp (parse-file expect-input "inputs/day17.test"))
(define test-inp-2 (parse-file expect-input "inputs/day17.test2"))
(define real-inp (parse-file expect-input "inputs/day17.inp"))

; alist of x,y coordinates relative to bottom left
; coords are sorted by x
(define SHAPES
  (hasheqv '- '((0 . 0) (1 . 0) (2 . 0) (3 . 0))
         '+ '((0 . 1) (1 . 0) (1 . 1) (1 . 2) (2 . 1))
         'I '((0 . 0) (0 . 1) (0 . 2) (0 . 3))
         'L '((0 . 0) (1 . 0) (2 . 0) (2 . 1) (2 . 2))
         'Q '((0 . 0) (0 . 1) (1 . 0) (1 . 1)))) ; Q for cube

(define (translate-coords coords dx)
  (map (lambda (coord) (cons (+ (car coord) (car dx)) (+ (cdr coord) (cdr dx)))) coords))
(define (translate-shape shape to)
  (translate-coords (hash-ref SHAPES shape) to))

; -- BOILERPLATE --
; points representing the top or bottom side of the given shape at pos
; side can be 'top or 'bottom
(define (vertical-contour shape shape-pos side)
  (let ([shape-coords (if (eqv? side 'bottom)
                        (hash-ref SHAPES shape)
                        (-> (hash-ref SHAPES shape)
                            (sort (lambda (a b) (if (= (car a) (car b))
                                                  (> (cdr a) (cdr b))
                                                  (< (car a) (car b)))))))])
    (let loop ([coords shape-coords]
               [bottom '()])
      (if (null? coords)
        (translate-coords bottom shape-pos)
        (loop (dropf coords (lambda (x) (= (car x) (caar coords))))
              (cons (car coords) bottom))))))

; points representing the left or right edge of a shape at pos
; side can be 'left or 'right
(define (side-contour shape shape-pos side)
  (let ([shape-coords (-> (hash-ref SHAPES shape)
                          (sort (lambda (a b) (if (= (cdr a) (cdr b))
                                                ((if (eqv? side 'left) < >) (car a) (car b))
                                                (< (cdr a) (cdr b))))))])
    (let loop ([coords shape-coords]
               [side '()])
      (if (null? coords)
        (translate-coords side shape-pos)
        (loop (dropf coords (lambda (x) (= (cdr x) (cdar coords))))
              (cons (car coords) side))))))
(define (left-contour shape shape-pos) (side-contour shape shape-pos 'left))
(define (right-contour shape shape-pos) (side-contour shape shape-pos 'right))
(define (bottom-contour shape shape-pos) (vertical-contour shape shape-pos 'bottom))
(define (top-contour shape shape-pos) (vertical-contour shape shape-pos 'top))

; cached values for faster lookup for shapes at 0,0
(define LEFT-CONTOURS (hash-map/copy SHAPES (lambda (s cs) (values s (left-contour s '(0 . 0))))))
(define RIGHT-CONTOURS (hash-map/copy SHAPES (lambda (s cs) (values s (right-contour s '(0 . 0))))))
(define BOTTOM-CONTOURS (hash-map/copy SHAPES (lambda (s cs) (values s (bottom-contour s '(0 . 0))))))
(define TOP-CONTOURS (hash-map/copy SHAPES (lambda (s cs) (values s (top-contour s '(0 . 0))))))

; most extreme point of a shape in a given direction, with shape at pos
; bottommost and leftmost are represented by pos itself
(define (rightmost* shape pos)
  (let ([c (argmax car (hash-ref RIGHT-CONTOURS shape))])
    (cons (+ (car pos) (car c)) (+ (cdr pos) (cdr c)))))
(define (topmost* shape pos)
  (let ([c (argmax cdr (hash-ref TOP-CONTOURS shape))])
    (cons (+ (car pos) (car c)) (+ (cdr pos) (cdr c)))))

; cached versions of points of shapes at 0,0
(define RIGHTMOSTS (hash-map/copy SHAPES (lambda (s cs) (values s (rightmost* s '(0 . 0))))))
(define TOPMOSTS (hash-map/copy SHAPES (lambda (s cs) (values s (topmost* s '(0 . 0))))))

(define (rightmost shape pos)
  (let ([c (hash-ref RIGHTMOSTS shape)])
    (cons (+ (car pos) (car c)) (+ (cdr pos) (cdr c)))))
(define (topmost shape pos)
  (let ([c (hash-ref TOPMOSTS shape)])
    (cons (+ (car pos) (car c)) (+ (cdr pos) (cdr c)))))

; infinite sequence generators
(struct repeat-gen (vs idx))
(define (next-gen g)
  (match g
    [(repeat-gen vs idx)
     (values
       (vector-ref vs idx)
       (struct-copy repeat-gen g
                    [idx (modulo (add1 idx)
                                 (vector-length vs))]))]))
(define GSG (repeat-gen (vector '- '+ 'L 'I 'Q) 0)) ; Global Shape Gen
(define GJG (repeat-gen test-inp 0)) ; Global Jet Gen

; --- FUNCTIONS FOR FIRST-ATTEMPT CODE ---
; calculate the staring position based landed shapes and their positions
(define (shape-starting-position other-shapes other-shape-poses)
  (cons 3
        (+ 4 (foldl (lambda (other-shape pos y)
                      (max
                        y
                        (cdr (topmost other-shape pos))))
                    0
                    other-shapes
                    other-shape-poses))))

; attempt to blow a shape left or right
; shape-pos is unmodified if the shape collided with something
(define (blow-shape shape shape-pos dir occ-poses)
  (let ([new-pos (cond
                   [(eqv? dir #\<) (if (= 1 (car shape-pos))
                                     shape-pos
                                     (cons (sub1 (car shape-pos)) (cdr shape-pos)))]
                   [(eqv? dir #\>) (if (= 7 (car (rightmost shape shape-pos)))
                                     shape-pos
                                     (cons (add1 (car shape-pos)) (cdr shape-pos)))]
                   [else (error "nope")])])
    (if (any (lambda (coord) (set-member? occ-poses coord))
             (translate-coords (if (eqv? dir #\<)
                                 (hash-ref LEFT-CONTOURS shape)
                                 (hash-ref RIGHT-CONTOURS shape))
                               new-pos))
      shape-pos
      new-pos)))

; attempt to let a shape move down one block; returns false if the the shape
; cannot fall (i.e., it landed)
(define (fall-shape shape shape-pos occ-poses)
  (if (= 1 (cdr shape-pos))
    #f
    (let ([new-pos (cons (car shape-pos) (sub1 (cdr shape-pos)))])
      ;(pretty-print occ-poses)
      ;(pretty-print (set-member? occ-poses '(1 . 3)))
      (if (any (lambda (coord) (set-member? occ-poses coord))
               (translate-coords (hash-ref BOTTOM-CONTOURS shape) new-pos))
        #f
        new-pos))))


; walls are x = 0, 8; y = 0
; occ-poses is a list
; spawn a rock and let it be blown and fall until it lands
(define (do-round shape-gen jet-gen shapes shape-poses occ-poses)
  (let-values ([(next-shape new-shape-gen) (next-gen shape-gen)])
    (let loop ([cur-shape-pos (shape-starting-position shapes shape-poses)]
               [jet-gen jet-gen])
      (let-values ([(jet-dir jet-gen) (next-gen jet-gen)])
        (let* ([blown-pos (blow-shape next-shape cur-shape-pos jet-dir occ-poses)]
               [fallen-pos (fall-shape next-shape blown-pos occ-poses)])
          (if fallen-pos
            (loop fallen-pos jet-gen)
            (values next-shape blown-pos new-shape-gen jet-gen)))))))

; make n shapes fall
(define (dump-rocks inp n)
  (let ([jet-gen  (repeat-gen (list->vector inp) 0)])
    (for/fold ([shape-gen GSG]
               [jet-gen jet-gen]
               [shapes '()]
               [shape-poses '()]
               [occs (set)])
              ([rnd (in-range n)])
    (if (zero? (modulo rnd 1000))
      (pretty-print rnd)
      (void))
    (let-values ([(shape
                    shape-pos
                    shape-gen
                    jet-gen)
                  (do-round shape-gen jet-gen shapes shape-poses occs)])
      (values shape-gen
              jet-gen
              (cons shape shapes)
              (cons shape-pos shape-poses)
              (foldl (lambda (c occ) (set-add occ c))
                     occs
                     (translate-shape shape shape-pos)))))))

; drop N blocks and then calculate the height
(define (height-at inp N)
  (let-values ([(shape-gen
                 jet-gen
                 shapes
                 shape-poses
                 occs)
                (dump-rocks inp N)])
    (-> (append-map (lambda (s p) (translate-shape s p)) shapes shape-poses)
        (=>> map cdr)
        (=>> foldl max 0))))

; ~ 300 ms
(define (part-1 inp)
  (height-at inp 2022))

; --- asymptotically improved performance by removing invisible shapes ---
; also, no longer considers shapes that have landed as their own entities; instead,
; keeps track of a master set of occupied spaces (occ-poses)

(define (shape-starting-position* occ-poses)
  (cons 3
        (+ 4 (for/fold ([mx 0]) ([c (in-set occ-poses)])
               (max mx (cdr c))))))

; uses DFS to figure out which coordinates are exposed from above, and
; prunes the ones that are not
(define (exposed-coords all-coords from)
  (call-with-values
    (lambda ()
      (let dfs ([pos from]
                [seen-poses (set)]
                [exposed-poses (set)])
        (let ([seen-poses (set-add seen-poses pos)])
          (if (set-member? all-coords pos)
            (values seen-poses
                    (set-add exposed-poses pos))
            (for/fold ([seen-poses seen-poses]
                       [exposed-poses exposed-poses])
                      ([neigh (filter (lambda (c)
                                        (and (<= 1 (car c) 7)
                                             (<= 1 (cdr c))
                                             (not (set-member? seen-poses c))))
                                      (list (cons (add1 (car pos)) (cdr pos))
                                            (cons (sub1 (car pos)) (cdr pos))
                                            (cons (car pos) (sub1 (cdr pos)))))])
              (dfs neigh seen-poses exposed-poses))))))
    (lambda (seen-poses exposed-poses) exposed-poses)))

; same as do-round, but prunes occupied spaces that are unreachable
(define (do-round-improved shape-gen jet-gen occ-poses)
  (let-values ([(next-shape new-shape-gen) (next-gen shape-gen)])
    (let* ([cur-shape-pos (shape-starting-position* occ-poses)]
           ; prune
           [occ-poses (exposed-coords occ-poses
                                      (cons (car cur-shape-pos)
                                            (- (cdr cur-shape-pos) 3)))])
    (let loop ([cur-shape-pos cur-shape-pos]
               [jet-gen jet-gen])
      (let-values ([(jet-dir jet-gen) (next-gen jet-gen)])
        (let* ([blown-pos (blow-shape next-shape cur-shape-pos jet-dir occ-poses)]
               [fallen-pos (fall-shape next-shape blown-pos occ-poses)])
          (if fallen-pos
            (loop fallen-pos jet-gen)
            (values next-shape blown-pos occ-poses new-shape-gen jet-gen))))))))
(define (dump-rocks-improved inp n)
  (let ([jet-gen (repeat-gen (list->vector inp) 0)])
    (for/fold ([shape-gen GSG]
               [jet-gen jet-gen]
               [occ-poses (set)])
              ([rnd (in-range n)])
    (if (zero? (modulo rnd 1000))
      (pretty-print rnd)
      (void))
    (let-values ([(shape
                    shape-pos
                    occ-poses
                    shape-gen
                    jet-gen)
                  (do-round-improved shape-gen jet-gen occ-poses)])
      (values shape-gen
              jet-gen
              ; update occupied coordinates with the just-placed shape
              (foldl (lambda (c occ-poses) (set-add occ-poses c))
                     occ-poses
                     (translate-shape shape shape-pos)))))))

(define (height-at-improved inp N)
  (let-values ([(shape-gen
                 jet-gen
                 occ-poses)
                (dump-rocks-improved inp N)])
    (for/fold ([mx 0]) ([c (in-set occ-poses)])
      (max mx (cdr c)))))

; 355 ms
(define (part-1-improved-1 inp)
  (height-at-improved inp 2022))

; caches the round state (state = shape index, jet index, and a sorted
; list of exposed block coordinates relative to the top of the stack).
; If a previous round had a matching state, that means we've hit a cycle,
; and we can predict what the height will be an arbitrary number of rounds
; into the future using some very off-by-one-prone math
;
; Note that this algorithm will fail if the floor is never fully covered
(define (height-after inp n)
  (let ([jet-gen  (repeat-gen (list->vector inp) 0)])
    (let loop ([shape-gen GSG]
               [jet-gen jet-gen]
               [occ-poses (set)]
               [rnd 1]
               [seen-states (hash)]
               [heights (hash)])
    (if (zero? (modulo rnd 1000))
      (pretty-print rnd)
      (void))
    (let ([shape-idx (repeat-gen-idx shape-gen)]
          [jet-idx (repeat-gen-idx jet-gen)])
      (let-values ([(shape
                      shape-pos
                      occ-poses
                      shape-gen
                      jet-gen)
                    (do-round-improved shape-gen jet-gen occ-poses)])
        (let* ([new-occs (foldl (lambda (c occ-poses) (set-add occ-poses c))
                                occ-poses
                                (translate-shape shape shape-pos))]
               [height (for/fold ([mx 0]) ([c (in-set new-occs)]) (max mx (cdr c)))]
               [state (list shape-idx jet-idx
                            (sort (translate-coords (set->list new-occs)
                                                    (cons 0 (- height)))
                                  (lambda (a b) (if (= (car a) (car b))
                                                (< (cdr a) (cdr b))
                                                (< (car a) (car b))))))]
               [prev-seen-rnd (hash-ref seen-states state #f)])
          (if prev-seen-rnd
            (let* ([height-diff-over-cyc (- height (hash-ref heights prev-seen-rnd))]
                   [cyc-len (- rnd prev-seen-rnd)]
                   [rounds-remaining (- n rnd)]
                   [cycs-remaining (floor (/ rounds-remaining cyc-len))]
                   [extra-rounds (modulo rounds-remaining cyc-len)]
                   [extra-height (- (hash-ref heights (+ prev-seen-rnd extra-rounds))
                                    (hash-ref heights prev-seen-rnd))])
              (display "Cycle length: ")
              (pretty-print cyc-len)
              (display "Cycle start: ")
              (pretty-print prev-seen-rnd)
              (values rnd
                      (+ height
                         (* cycs-remaining height-diff-over-cyc)
                         extra-height)))
            (loop shape-gen
                  jet-gen
                  new-occs
                  (add1 rnd)
                  (hash-set seen-states state rnd)
                  (hash-set heights rnd height)))))))))

; 370 ms
(define (part-1-improved-2 inp)
  (height-after inp 2022))
; 400 ms
(define (part-2 inp)
  (let* ([N 1000000000000])
    (height-after inp N)))
