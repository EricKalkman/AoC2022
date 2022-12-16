#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define expect-sensor
  (group-output
    (and-then
      (skip-string "Sensor at x=")
      (group-output
        (and-then
          expect-int
          (skip-string ", y=")
          expect-int))
      (skip-string ": closest beacon is at x=")
      (group-output
        (and-then
          expect-int
          (skip-string ", y=")
          expect-int))
      skip-eol-or-eof)))

(define expect-input
  (at-least-one expect-sensor))

(define test-inp (parse-file expect-input "inputs/day15.test"))
; pre-sorting the input helps a bit in the runtime for merging ranges
; (O(N^2) worst case becomes O(N) average)
(define real-inp ;(parse-file expect-input "inputs/day15.inp"))
  (->
    (parse-file expect-input "inputs/day15.inp")
    (sort (lambda (a b)
            (let ([sensor-a (first a)]
                  [sensor-b (first b)])
              (if (= (first sensor-a) (first sensor-b))
                (> (second sensor-a) (second sensor-b))
                (> (first sensor-a) (first sensor-b))))))))

(define (manh-dist c1 c2)
  (+ (abs (- (first c1) (first c2)))
     (abs (- (second c1) (second c2)))))

(define (edge-of-invalids sensor radius beacon y)
  (let* ([sensor-x (first sensor)]
         [sensor-y (second sensor)]
         [x-dist (- radius (abs (- y sensor-y)))]
         [l (- sensor-x x-dist)]
         [r (+ sensor-x x-dist)])
    (if (< x-dist 0)
      #f
      (list (if (equal? l beacon) (add1 l) l)
            (if (equal? r beacon) (sub1 r) r)))))

; mix of insertion sort and range merging if they overlap
(define (merge-range rng rngs)
  (cond
    [(null? rngs) (list rng)]
    ; if the range to merge contains the first range in rngs
    [(and (<= (first rng) (first (car rngs)) (second (car rngs)) (second rng)))
     (merge-range rng (cdr rngs))]
    ; if we don't overlap and are before the next range, insert into list
    [(< (second rng) (sub1 (first (car rngs)))) (cons rng rngs)]
    ; if rng overlaps with the next range in rngs
    [(or (<= (sub1 (first (car rngs))) (first rng) (add1 (second (car rngs))))
         (<= (sub1 (first (car rngs))) (second rng) (add1 (second (car rngs)))))
     (merge-range (list (min (first rng) (first (car rngs)))
                        (max (second rng) (second (car rngs))))
                  (cdr rngs))]
    ; rng is beyond the first rng in rngs, continue
    [else (cons (car rngs) (merge-range rng (cdr rngs)))]))

; assumes ranges are merged and sorted
(define (split-beacon beacon-x rngs)
  (cond
    [(null? rngs) rngs]
    ; if beacon as at the start or end of a range, just shave that off the current
    ; range without splitting ranges in half
    [(= beacon-x (first (car rngs)))
     (cons (list (add1 (first (car rngs))) (second (car rngs))) (cdr rngs))]
    [(= beacon-x (second (car rngs)))
     (cons (list (first (car rngs)) (sub1 (second (car rngs)))) (cdr rngs))]
    ; beacon is in the middle of a range; split the range
    [(< (first (car rngs)) beacon-x (second (car rngs)))
     `(,(list (first (car rngs)) (sub1 beacon-x))
       ,(list (add1 beacon-x) (second (car rngs)))
       ,@(cdr rngs))]
    ; beacon is not in the current range, move on to the next
    [else (cons (car rngs) (split-beacon beacon-x (cdr rngs)))]))

; get the ranges of squares in which there can be no beacons
; note that this function does not remove from consideration squares
; which already contain beacons; handle that yourself
(define (ranges-in-row sensors beacons radii y)
  (->> (filter-map (lambda (sensor radius beacon)
                       (edge-of-invalids sensor radius beacon y))
                     sensors radii beacons)
       (foldl merge-range '())))

(define (part-1 inp y)
  (let ([sensors (map first inp)]
        [beacons (map second inp)]
        [radii (map (lambda (sensor-beacon) (apply manh-dist sensor-beacon)) inp)])
    (any-> (ranges-in-row sensors beacons radii y) x
           ; remove beacons from consideration because a beacon can always be where a
           ; beacon is :)
           (foldl (lambda (beacon rngs) (split-beacon (first beacon) rngs))
                  x
                  (filter (lambda (beacon) (= y (second beacon))) beacons))
           ; calculate the number of impossible beacon squares represented by
           ; these ranges
           (map (lambda (rng) (add1 (- (second rng) (first rng)))) x)
           (sum x))))

; takes ranges of impossible beacon squares and figures out the possible
; beacon spots (i.e., returns the gaps between ranges)
(define (get-possible-beacon-spots rngs)
  (if (null? (cdr rngs))
    '()
    (append (range (add1 (second (first rngs)))
                   (first (second rngs)))
            (get-possible-beacon-spots (cdr rngs)))))

(define (part-2 inp max-coord)
  (let* ([sensors (map first inp)]
         [beacons (map second inp)]
         [radii (map (lambda (sensor-beacon) (apply manh-dist sensor-beacon)) inp)]
         [max-x max-coord]
         [max-y max-coord]
         [coord
           (for/fold ([coords '()])
                     ([y (in-range max-y)])
             (if (zero? (modulo y 10000))
               ; it's a bad sign when you have to check whether your code is running
               ; properly or recursing to infinity
               (pretty-print y)
               (void))
             ; calculate ranges for the current row
             (let* ([rngs (ranges-in-row sensors beacons radii y)]
                    [beacon-spots (get-possible-beacon-spots rngs)])
               (if (null? beacon-spots)
                 coords
                 (append (filter-map (lambda (x)
                                       (if (<= 0 x max-x)
                                         (list x y)
                                         #f))
                                     beacon-spots)
                         coords))))])
    (+ (* (caar coord) 4000000) (cadar coord))))
