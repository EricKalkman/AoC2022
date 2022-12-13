#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define expect-line
  (and-then
    (group-output
      (at-least-one (expv-set (append '(#\E #\S) LOCASE))))
    skip-eol-or-eof
    (mod-top list->vector)))
(define expect-input
  (and-then
    (group-output (at-least-one expect-line))
    (mod-top list->vector)))

(define test-inp (car (parse-file expect-input "inputs/day12.test")))
(define real-inp (car (parse-file expect-input "inputs/day12.inp")))

; shorthand for indexing a vector of vectors
(define (idx2d v i j)
  (vector-ref (vector-ref v i) j))

(define (find-all-2d height-map st)
  (for/fold ([coords '()])
            ([idx (in-range (vector-length height-map))])
    (append
      (for/fold ([cols '()])
                ([jdx (in-range (vector-length (vector-ref height-map idx)))])
        (if (memv (idx2d height-map idx jdx) st)
          (cons (cons idx jdx) cols)
          cols))
      coords)))

(define (find-start height-map) (first (find-all-2d height-map '(#\S))))
(define (find-end height-map) (first (find-all-2d height-map '(#\E))))

(define (height-at height-map coord)
  (let* ([row (car coord)]
         [col (cdr coord)]
         [height (idx2d height-map row col)])
    (case height
      [(#\S) 0]
      [(#\E) 25]
      [else (- (char->integer height) (char->integer #\a))])))

(define (neighbors height-map coord neighbor-criterion)
  (let ([row (car coord)] [col (cdr coord)]
        [cur-height (height-at height-map coord)])
    (->> (list `(,(add1 row) . ,col)
               `(,(sub1 row) . ,col)
               `(,row . ,(add1 col))
               `(,row . ,(sub1 col)))
         (filter (lambda (coord)
                   (and
                     (< -1 (car coord) (vector-length height-map))
                     (< -1 (cdr coord) (vector-length (vector-ref height-map 0)))
                     (let ([coord-height (height-at height-map coord)])
                       (neighbor-criterion cur-height coord-height))))))))
                       ;(<= coord-height (add1 cur-height)))))))))

; fully explores the entire map starting from ,from
; returns a hash table that maps each tile to the previous
; tile in the path leading from the start to that tile
(define (bfs height-map from neighbor-criterion)
  (let loop ([q (list from)]
             [prevs (hash)])
    (if (null? q)
      prevs
      ; get all neighbors that we haven't countered before
      (let ([neighs (filter (lambda (n) (not (hash-ref prevs n #f)))
                            (neighbors height-map (car q) neighbor-criterion))])
        (loop
          (append (cdr q) neighs)
          (apply hash-set* prevs
                 (append-map (lambda (n) (list n (car q)))
                             neighs)))))))

; trace that path along the path table from end back to start,
; returning a list representing the path in the forwards direction
(define (trace-path prevs start end)
  (let loop ([acc (list end)])
    (if (equal? (car acc) start)
      acc
      (loop (cons (hash-ref prevs (car acc)) acc)))))

(define (part-1 inp)
  (let* ([start (find-start inp)]
         [end (find-end inp)])
    (-> inp
        (bfs start
             ; neighbor criterion: can go down any amount, but only up 1
             (lambda (cur-height neighbor-height)
               (<= neighbor-height (add1 cur-height))))
        (trace-path start end)
        length
        sub1)))

; the faster way to do part 2 is to invert the neighbors criterion (invert the
; edges) and BFS the entire board from #\E, then look up the paths from each
; of the #\a or #\S coordinates in the map
; I know, my star/tend naming is confusing here
(define (part-2 inp)
  (let* ([start (find-end inp)]  ; the tallest point
         [ends (find-all-2d inp '(#\S #\a))])  ; all possible lowest points
    (-> inp
        (bfs start
             ; neighbor criterion: can go up any amount, but only down one
             (lambda (cur-height neighbor-height)
               (<= cur-height (add1 neighbor-height))))
        ((lambda (prevs)
           (filter-map (lambda (end)
                         ; oh-ho! a trick! some ends are unreachable! filter
                         ; those out
                         (if (hash-ref prevs end #f)
                           (-> (trace-path prevs start end)
                               length
                               sub1)
                           #f))
                ends)))
        (sort <)
        first)))
