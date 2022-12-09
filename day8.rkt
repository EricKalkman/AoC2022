#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define expect-input
  (and-then
    (at-least-one
      (and-then
        (group-output (at-least-one expect-digit))
        skip-eol-or-eof
        (mod-top (lambda (lst) (list->vector (map
                                               (compose
                                                 list
                                                 string->number
                                                 string) lst))))))))

(define test-inp (load-file-as-list "inputs/day8.test"))
(define real-inp (load-file-as-list "inputs/day8.inp"))

(define (parse-input inp)
  (let-values ([(_ parsed) (run-parser expect-input inp)])
    (list->vector parsed)))

; pin-drop algorithm; from is either 'left or 'right
; upon encountering a taller tree, marks that tree as "seen"
(define (expose-row row from)
  ((if (eqv? from 'left) vector-foldl vector-foldr)
   (lambda (tree max-height)
     (let ([idx (car tree)]
           [tree (cdr tree)])
       (if (> (first tree) max-height)
         (begin
           (if (null? (cdr tree))
             (vector-set! row idx (list (first tree) 'seen))
             '())
           (first tree))
         max-height)))
   -1
   (vector-map cons (list->vector (range (vector-length row))) row))
  row)

; same as above, but looks either from either 'top or 'bottom, and considers
; the entire forest at once
(define (expose-vertical forest from)
  ((if (eqv? from 'top) vector-foldl vector-foldr)
   (lambda (row heights)
     (for/vector ([i (range (vector-length row))]
                  [max-height heights]
                  [tree row])
       (if (> (first tree) max-height)
         (begin
           (if (null? (cdr tree))
             (vector-set! row i (list (first tree) 'seen))
             '())
           (first tree))
         max-height)))
   (make-vector (vector-length (vector-ref forest 0)) -1)
   forest)
  forest)

(define (part-1 inp)
  (-> (parse-input inp)
      ; mark all the visible trees
      (expose-vertical 'top)
      (expose-vertical 'bottom)
      (=>> vector-map! (lambda (row) (expose-row row 'left)))
      (=>> vector-map! (lambda (row) (expose-row row 'right)))
      ; count them
      (=>> vector-map! (lambda (row)
                         (vector-foldl (lambda (tree acc)
                                         (if (not (null? (cdr tree)))
                                           (+ acc 1)
                                           acc))
                                       0
                                       row)))
      (=>> vector-foldl + 0)))

; coords are `(,row . ,col) pairs
; counts the number of trees that are shorter than that at ,src along the path defined
; by ,coords, stopping either at the edge or a same-height-or-taller tree. Also counts
; that last tallest tree, if applicable
(define (scenic-trace forest src coords)
  (let ([src-height (first (vector-ref (vector-ref forest (car src)) (cdr src)))])
    (let-values ([(shorter remaining) (splitf-at coords
                                                 (lambda (coord)
                                                   (let ([cur-height (first (vector-ref
                                                                              (vector-ref forest (car coord))
                                                                              (cdr coord)))])
                                                     (< cur-height src-height))))])
      (+ (length shorter)
         (if (null? remaining) 0 1))))) ; properly seeing past the edge

; Trace the cardinal directions around each tree to determine its scenic score
(define (scenic-score forest coord)
  (let ([height (vector-length forest)]
        [width (vector-length (vector-ref forest 0))]
        [src-row (car coord)]
        [src-col (cdr coord)])
    (->> (list
           ; hooray, index hacking
           (map cons (make-list src-col src-row) (range (sub1 src-col) -1 -1)) ; scan left
           (map cons (make-list (- width src-col 1) src-row) (range (add1 src-col) width)) ; scan right
           (map cons (range (sub1 src-row) -1 -1) (make-list src-row src-col)) ; scan up
           (map cons (range (add1 src-row) height) (make-list (- height src-row 1) src-col))) ; scan down
         (map (lambda (coords) (scenic-trace forest coord coords)))
         (foldl * 1))))

; no part of this code is efficient
(define (part-2 inp)
  (let* ([forest (parse-input inp)]
         [height (vector-length forest)]
         [width (vector-length (vector-ref forest 0))])
    (->> (map (lambda (coord) (scenic-score forest coord))
              (cart-product (range height) (range width)))
         (foldl max 0))))
