#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

; a crate position can can be [X] (X is a letter) or three spaces (blank)
(define expect-crate
  (and-then
    (skip (expect #\[))
    expect-uppercase
    (skip (expect #\]))))
(define expect-blank-crate
  (and-then
    (skip (expect-n (expect #\space) 3))
    (push-stack 'blank)))

; reads a particular level of the stack across all stacks
; returns a list containing this slice across all stacks
; crates are represented by the letter that was in the brackets, while
; empty space is represented by the symbol 'blank
; The input is expected to be full padded out with spaces, i.e.,
;
;
; [A]
;  1   2   3
; 
; is represented as (. = space)
;
; [A]........
;  1   2   3
;
(define expect-crate-row
  (and-then
    (at-least-one
      (and-then
        (or-else
          expect-crate
          expect-blank-crate)
        (skip (maybe (expect #\space)))))
    (skip (expect #\newline))))


; at the end of the crate positional input, there is a list of
; labels for each column; we actually ignore these values in the
; rest of the program
(define expect-column-labels
  (group-output
    (and-then
      (at-least-one
        (and-then
          (skip (expect #\space))
          expect-natural
          (skip (expect #\space))
          (skip (maybe (expect #\space)))))
      (skip (expect #\newline)))))


; expects the full table of crates in each stack
(define expect-table
  (group-output
    (and-then
      (at-least-one expect-crate-row)
      expect-column-labels)))

; One line of movement; outputs onto the parseer stack a list containing
; the number of items to move, the source stack, and the destination stack
; as the first, second, and third elements of a list, respectively
(define expect-move
  (group-output
    (and-then
      (skip (expect-string "move "))
      expect-natural
      (skip (expect-string " from "))
      expect-natural
      (skip (expect-string " to "))
      expect-natural
      (skip (expect #\newline)))))

(define expect-move-list
  (group-output (at-least-one expect-move)))

(define expect-input
  (and-then
    expect-table
    (skip (expect #\newline))
    expect-move-list))

; generates list representations of each stack, returning them in a vector
(define (gen-stacks table-inp)
  (let-values ([(table ids) (split-at-right table-inp 1)])
    (->> table
         (chunkify (length (car ids)))  ; group each row of the stacks into lists
         (apply map list)  ; lovely Scheme hack for list transposition
         (map (lambda (col) (dropf col (lambda (x) (eqv? x 'blank))))) ; trim off blank spaces on top
         list->vector)))

(define test-inp (load-file-as-list "inputs/day05.test"))
(define real-inp (load-file-as-list "inputs/day05.inp"))

(define (do-movements inp part)
  (let-values ([(_ parsed) (run-parser expect-input inp)])
    (let* ([stacks (gen-stacks (car parsed))]
           [moves (cadr parsed)])
      (->> moves
           ; apply each move in sequence
           (foldl (lambda (move stacks)
                    ; destructure the move information
                    (let* ([number (first move)]
                           [from (- (second move) 1)]
                           [to (- (third move) 1)])
                      ; take the top ,number items off the source stack
                      (let-values ([(top new-from) (split-at (vector-ref stacks from) number)])
                        (vector-set! stacks from new-from) ; update the source stack
                        (vector-set! stacks to (append     ; update the destination stack
                                                 (if (eqv? part 'part1)
                                                   (reverse top)  ; one box at a time
                                                   top)           ; vs all at once
                                                 (vector-ref stacks to)))
                        stacks)))
                  stacks)
           (vector-map car)  ; get the top item in each stack
           vector->list      ; make pretty for printing
           list->string))))

(define (part-1 inp) (do-movements inp 'part1))
(define (part-2 inp) (do-movements inp 'part2))
