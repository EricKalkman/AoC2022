#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define (expect-packet*)
  ; items are either lists or integers
  (let ([expect-packet-item
          (or-else expect-int
                   ; need lazy evaluation for recursion
                   (lambda (inp stack) ((expect-packet*) inp stack)))])
    (group-output
      (and-then
        (skip (expect #\[))
        ; comma-separated list of valid items (can be empty)
        (maybe expect-packet-item)
        (at-least-none (and-then
                         (skip (expect #\,))
                         expect-packet-item))
        (skip (expect #\]))))))
(define expect-packet (expect-packet*))

(define expect-packet-pair
  (group-output
    (expect-n (and-then expect-packet skip-eol-or-eof) 2)))

(define expect-input
  (at-least-one
    (and-then
      expect-packet-pair
      skip-whitespace)))

(define test-inp (parse-file expect-input "inputs/day13.test"))
(define real-inp (parse-file expect-input "inputs/day13.inp"))

(define (packets-right-order? a b)
  (cond
    ; handle integers
    [(and (integer? a) (integer? b))
      (cond
        [(< a b) 'yes]
        [(> a b) 'no]
        [else 'maybe])]
    ; handle lists
    [(and (list? a) (list? b))
     (cond
       [(and (null? a) (null? b)) 'maybe]
       [(null? a) 'yes]
       [(null? b) 'no]
       [else
         (case (packets-right-order? (car a) (car b))
           [(yes) 'yes]
           [(no) 'no]
           ; only continue checking in ambiguous cases
           [(maybe) (packets-right-order? (cdr a) (cdr b))])])]
    ; one list one int
    [else
      (packets-right-order? (if (integer? a) (list a) a)
                            (if (integer? b) (list b) b))]))

(define (part-1 inp)
  (->> inp
       ; find all keys in the right order and return their indices
       (filter-map (lambda (idx pairs)
                     (let ([right-order? (apply packets-right-order? pairs)])
                       (if (eqv? right-order? 'yes)
                         idx
                         #f)))
                   (range 1 (add1 (length inp))))
       sum))

(define (part-2 inp)
  (->> inp
       ; ungroup packets into a single list
       (apply append)
       ; add the divider packets
       (append (list '((2)) '((6))))
       ; sort the packets by packets-right-order?
       (<<= sort (lambda (a b) (eqv? (packets-right-order? a b) 'yes)))
       ; find the indices of the divider packets
       (filter-map (lambda (idx packet)
                     (if (member packet '(((2)) ((6))))
                       idx
                       #f))
                   (range 1 (+ 3 (* 2 (length inp)))))
       ; get the decoder key
       (foldl * 1)))
