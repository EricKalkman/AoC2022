#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define expect-path
  (expect-list
    (group-output
      (and-then
        expect-int
        (skip (expect #\,))
        expect-int))
    (expect-string " -> ")))
(define expect-input
  (at-least-one
    (and-then
      expect-path
      skip-eol-or-eof)))

(define test-inp (parse-file expect-input "inputs/day14.test"))
(define real-inp (parse-file expect-input "inputs/day14.inp"))

(define (draw-segment cave seg)
  (call-with-values
    (lambda ()
      ; fill in all coords between start and end in the cave
      ; Note that on each iteration, the previous end becomes the current iteration's start
      (for/fold ([cave cave]
                 [start (car seg)])
                ([end (cdr seg)])
        (values
          (foldl (lambda (coord cave) (hash-set cave coord 'wall))
                 cave
                 ; determine if we need to fill vertically or horizontally
                 (if (= (first start) (first end))
                   (map (lambda (y) (list (first start) y))  ; vertical fill
                        (range (min (second start) (second end))
                               (add1 (max (second start) (second end)))))
                   (map (lambda (x) (list x (second start)))  ; horizontal fill
                        (range (min (first start) (first end))
                               (add1 (max (first start) (first end)))))))
          end)))
    (lambda (a b) a)))
(define (paint-cave cave segs)
  (foldl (lambda (seg cave) (draw-segment cave seg))
         cave
         segs))

; first value is #f if the stop condition was met
(define (drop-sand cave from-x from-y stopcond)
    (cond
      [(stopcond cave from-x from-y) (values #f cave)]
      ; move down if we can
      [(not (hash-ref cave (list from-x from-y) #f)) (drop-sand cave from-x (add1 from-y) stopcond)]
      ; else, move left if we can
      [(not (hash-ref cave (list (sub1 from-x) from-y) #f)) (drop-sand cave (sub1 from-x) from-y stopcond)]
      ; else, move right if we can
      [(not (hash-ref cave (list (add1 from-x) from-y) #f)) (drop-sand cave (add1 from-x) from-y stopcond)]
      ; else, place sand above
      [else 
        (values #t (hash-set cave (list from-x (sub1 from-y)) 'sand))]));)

(define (feed-all-the-sand cave stopcond)
  (let-values ([(continue? new-cave) (drop-sand cave 500 0 stopcond)])
    (if continue?
      (feed-all-the-sand new-cave stopcond)
      new-cave)))

(define (part-1 inp)
  (let* ([cave (paint-cave (hash) inp)]
         [killfloor (foldl max 0 (map second (hash-keys cave)))])
    (any-> cave cave
           (feed-all-the-sand cave (lambda (cave cur-x cur-y) (> cur-y killfloor)))
           (count (lambda (x) (eqv? 'sand x))
                  (hash-values cave)))))

(define (part-2 inp)
  (let* ([cave (paint-cave (hash) inp)]
         [floorlevel (+ 2 (foldl max 0 (map second (hash-keys cave))))])
    (any-> cave cave
           ; place floor as wide as the pyramid of sand that we expect
           ; rows of sand are 2n-1 wide, extending out one tile in either direction
           ; for each level down
           ; So, at level 11, we expect 21 wide. Pad with 1 more on either side so that
           ; we don't fall into void
           (foldl (lambda (x cave) (hash-set cave (list x floorlevel) 'wall))
                  cave
                  (range (- 500 floorlevel 1) (+ 500 floorlevel 2)))
           (feed-all-the-sand cave (lambda (cave cur-x cur-y) (hash-ref cave '(500 0) #f)))
           (count (lambda (x) (eqv? 'sand x))
                  (hash-values cave)))))
