#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define test-inp (string->list (load-file "day1.test")))
(define real-inp (string->list (load-file "day1.inp")))

(define input-parser
  (at-least-one (at-least-one
                  (and-then
                    push-guard
                    (and-then
                      (at-least-one
                        (and-then
                          expect-int
                          (skip (expv #\newline))))
                      (or-else
                        (skip (expv #\newline))
                        expect-empty))
                    (mod-guarded-stack reverse)))))

(define (part-1 input)
  (let-values ([(_ parsed) (run-parser input-parser input)])
    (foldl max 0 (map (lambda (elf) (foldl + 0 elf)) parsed))))

(define (part-2 input)
  (let-values ([(_ parsed) (run-parser input-parser input)])
    (any-> parsed x
           (map (lambda (elf) (foldl + 0 elf)) x)
           (sort x >)
           (take x 3)
           (foldl + 0 x))))
