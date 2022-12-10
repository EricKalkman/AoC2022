#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define expect-int-range
  (group-output
    (and-then
      expect-natural
      (skip (expect #\-))
      expect-natural)))
(define pair-parser
  (group-output
    (and-then
      expect-int-range
      (skip (expect #\,))
      expect-int-range
      skip-eol-or-eof)))
(define input-parser
  (at-least-one pair-parser))

(define test-inp (load-file-as-list "inputs/day04.test"))
(define real-inp (load-file-as-list "inputs/day04.inp"))

(define (is-subrange? rng1 rng2)
  (let-values ([(a b) (apply values rng1)]
               [(c d) (apply values rng2)])
    (and (>= a c) (<= b d))))

(define (part-1 input)
  (let-values ([(_ parsed) (run-parser input-parser input)])
    ; parsed is a list of elf pairs
    ; an elf pair is a list of ranges
    ; a range is a list (length 2) of integers representing the low and high
    ; end of the ranges
    (count
      (lambda (pair)
        (or (apply is-subrange? pair)
            (apply is-subrange? (reverse pair))))
      parsed)))


(define (ranges-overlap? rng1 rng2)
  (let-values ([(a b) (apply values rng1)]
               [(c d) (apply values rng2)])
    ; sort ranges by low end
    (let ([sorted (if (< a c)
                    `(,rng1 ,rng2)
                    `(,rng2 ,rng1))])
      ; the first range is guaranteed to be starting at or before the
      ; second range
      ; thus, for the ranges to intersect, the end of the first range
      ; must be at or past the start of the second range
      (>= (cadar sorted) (caadr sorted)))))

(define (part-2 input)
  (let-values ([(_ parsed) (run-parser input-parser input)])
    (count
      (lambda (pair) (apply ranges-overlap? pair))
      parsed)))
