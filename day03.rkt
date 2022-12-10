#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define parse-rucksack
  (and-then
    push-guard
    (at-least-one expect-letter)
    (mod-guarded-stack (lambda (x) x))
    skip-eol-or-eof))
(define parse-input
  (at-least-one parse-rucksack))

(define test-inp (load-file-as-list "inputs/day03.test"))
(define real-inp (load-file-as-list "inputs/day03.inp"))

(define (shared-items rucksack)
  ; so apparently lists work as sets in racket
  (set-intersect (car rucksack) (cdr rucksack)))

; index hacking
(define (item-priority item)
  (- (char->integer item)
     (if (memv item LOCASE)
       (- (char->integer #\a) 1)
       (- (char->integer #\A) 27))))

(define (part-1 inp)
  (let-values ([(_ parsed) (run-parser parse-input inp)])
    (->> parsed
         (map (compose             ; could have done this with a threading macro as well
                item-priority
                car                ; shared-items should return a list with one element; unwrap
                shared-items       ; shared items between two compartments
                (lambda (rucksack) ; split each rucksack into compartments
                  (call-with-values
                    (lambda () (split-at rucksack (/ (length rucksack) 2)))
                    cons))))
         sum)))

(define (part-2 inp)
  (let-values ([(_ parsed) (run-parser parse-input inp)])
    (->> parsed
         (chunkify 3)  ; break elves into groups of 3 in order
         (map (lambda (group)
                (->> (foldl (compose shared-items cons)
                           (car group)
                           (cdr group))
                     car  ; there should only be one common item among all three elves
                     item-priority)))
         sum)))
