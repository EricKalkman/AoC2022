#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define test-inp (string->list (load-file "day1.test")))
(define real-inp (string->list (load-file "day1.inp")))

(define input-parser
  (at-least-one (at-least-one
                  (and-then  ; parse elf
                    push-guard
                    (and-then
                      (at-least-one  ; series of int separated by newlines
                        (and-then
                          expect-int
                          (skip (expv #\newline))))
                      (or-else ; end each elf's inventory with blank line or EOF
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

; rethreaded using newe =>> operator, indicating to pass the threaded argument
; to the form as the last argument instead of the first
(define (part-2-rethreaded input)
  (let-values ([(_ parsed) (run-parser input-parser input)])
    (-> parsed
        (=>> map (lambda (elf) (foldl + 0 elf)))
        (sort >)
        (take 3)
        (=>> foldl + 0))))
