#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define (snafu-digit->number d)
  (case d
    [(#\0) 0]
    [(#\1) 1]
    [(#\2) 2]
    [(#\-) -1]
    [(#\=) -2]
    [else (error "Unexpected digit")]))
(define  (digit->snafu-digit d)
  (case d
    [(-2) #\=]
    [(-1) #\-]
    [(0) #\0]
    [(1) #\1]
    [(2) #\2]
    [else "Unexpected digit"]))

; gives a SNAFU number as a list of integers in big-endian notation
(define expect-snafu
  (and-then
    (group-output (at-least-one (expv-set '(#\0 #\1 #\2 #\- #\=))))
    (mod-top (lambda (x) (reverse (map snafu-digit->number x))))))

(define expect-input
  (at-least-one (and-then expect-snafu skip-eol-or-eof)))

(define test-inp (parse-file expect-input "inputs/day25.test"))
(define real-inp (parse-file expect-input "inputs/day25.inp"))

(define (snafu->number n)
  (let loop ([result 0]
             [n n]
             [ex 1])
    (if (null? n)
      result
      (loop (+ result (* ex (car n)))
            (cdr n)
            (* ex 5)))))

; big endian repr of a snafu digit
(define (number->snafu* n)
  (if (zero? n)
    '()
    (let ([place (modulo n 5)]
          [quot (floor (/ n 5))])
      (if (> place 2)
        ; convert an invalid digit (> 2) into a valid one by subtracting 5 and carrying
        ; it (as a 1) to the next digit's place (e.g., 3 = -2 + 1x5)
        (cons (digit->snafu-digit (- place 5))
              (number->snafu* (add1 quot)))
        (cons (digit->snafu-digit place)
              (number->snafu* quot))))))
(define number->snafu (compose list->string reverse number->snafu*))

(define (part-1 inp)
  (->> inp
       (map snafu->number)
       sum
       number->snafu))
