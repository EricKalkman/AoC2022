#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")


(define expect-noop
  (and-then (expect-string "noop") (mod-top string->symbol)))
(define expect-add
  (group-output (and-then
                  (expect-string "addx") (mod-top string->symbol)
                  skip-whitespace
                  expect-int)))
(define parse-input
  (at-least-one (and-then
                  (or-else
                    expect-noop
                    expect-add)
                  skip-eol-or-eof)))

(define test-inp (parse-file parse-input "inputs/day10.test"))
(define real-inp (parse-file parse-input "inputs/day10.inp"))

; replaces all instances of `(... (addx ,v) ...) with `(... noop (addx ,v) ...)
; to account for the 2-cycle duration of add
(define (expand-instructions insts)
  (append-map (lambda (inst) (if (list? inst) (list 'noop inst) (list inst)))
              insts))

; runs instructions keeping track of a user-defined state
; Each cycle, calls (cycle-hook cycnum regx cycle-state) to update
; the state. cycle-hook is called before commiting the changes in the current
; cyele to the state.
; returns the pair `(,regx . final-state)
(define (run-insts expanded-insts init-state cycle-hook)
  (foldl (lambda (cycnum inst state)
           (let ([regx (car state)]
                 [rest-state (cdr state)])
             (cons
               (if (list? inst)
                 (+ regx (second inst))
                 regx)
               (cycle-hook cycnum regx rest-state))))
         (cons 1 init-state)
         (range 1 (+ 1 (length expanded-insts)))
         expanded-insts))

(define (part-1 parsed-inp)
  (let ([expanded-insts (expand-instructions parsed-inp)])
    (-> expanded-insts
        ; state = list of signals
        (run-insts '()
                   (lambda (cycnum regx signals)
                     (if (zero? (modulo (- cycnum 20) 40))
                       (cons (* cycnum regx) signals)
                       signals)))
        cdr
        sum)))

(define (sprite-visible? sprite-pos scan-pos)
  (<= (sub1 sprite-pos) scan-pos (add1 sprite-pos)))

(define (part-2 parsed-inp)
  (let ([expanded-insts (expand-instructions parsed-inp)])
    (-> expanded-insts
        ; No state necessary here because we're printing to console
        (run-insts '()
                   (lambda (cycnum regx signals)
                     (begin
                       (let ([scan-pos (modulo (sub1 cycnum) 40)])
                         (if (zero? scan-pos) (newline) '()) ; move to new line after we reach column 39
                         ; dots were too messy for me, I like empty space better
                         (display (if (sprite-visible? regx scan-pos) #\# #\space)))
                       '()))))
    (newline)))
