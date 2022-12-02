#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define test-inp (load-file-as-list "day2.test"))
(define real-inp (load-file-as-list "day2.inp"))

(define translate (hash #\A 'rock #\B 'paper #\C 'scissors #\X 'rock #\Y 'paper #\Z 'scissors))

(define expect-op (and-then (expv-set (string->list "ABC"))))
(define expect-you (and-then (expv-set (string->list "XYZ"))))

(define parse-input
  (at-least-one
    (and-then
      expect-op
      skip-whitespace
      expect-you
      ; note: this puts your play at the front
      (mod-stack (lambda (stack)
                   (call-with-values (lambda () (split-at stack 2)) cons)))
      skip-whitespace)))

(define beaten-by (hash 'rock 'paper 'paper 'scissors 'scissors 'rock))
(define (what-beats x) (hash-ref beaten-by x))

(define shape-scores (hash 'rock 1 'paper 2 'scissors 3))
(define (score-play you op)
  (cond
    [(eqv? op you) 3]
    [(eqv? (what-beats op) you) 6]
    [else 0]))
(define (score-round you op)
  (+ (hash-ref shape-scores you)
     (score-play you op)))

(define (part-1 inp)
  (let-values ([(_ result) (run-parser parse-input inp)])
    (sum
      (map (lambda (rnd)
             (->> rnd
                  (map (lambda (play) (hash-ref translate play)))
                  (apply score-round)))
           result))))

(define player-strats
  (hash #\X (compose what-beats what-beats) ; lose
        #\Y (lambda (op-move) op-move) ; tie
        #\Z what-beats)) ; win

(define (part-2 inp)
  (let-values ([(_ result) (run-parser parse-input inp)])
    (sum
      (map (lambda (rnd)
             (let ([player-strat (hash-ref player-strats (car rnd))]
                   [op-move (hash-ref translate (cadr rnd))])
               (score-round (player-strat op-move) op-move)))
           result))))
