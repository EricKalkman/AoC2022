#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

; eschewing monkey ID because they will be treated in order in a vec
(struct monkey (items operation throw-to mod))

(define (monkey->vector m)
  (match m
    [(monkey xs op th mod) `#(monkey ,xs ,op ,th ,mod)]))

(define test-inp (load-file-as-list "inputs/day11.test"))
(define real-inp (load-file-as-list "inputs/day11.inp"))

; just validation; doesn't actually put any output onto the stack
(define expect-monkey-header
  (mod-err
    (skip
      (and-then
        (expect-string "Monkey ")
        expect-natural
        (skip (expect-string ":\n"))))
    "Error in monkey header"))
; parses a comma-separated list of worry values
(define expect-monkey-items
  (mod-err
    (group-output
      (and-then
        skip-whitespace
        (skip (expect-string "Starting items: "))
        (maybe expect-natural)
        (and-then
          (at-least-none
            (and-then
              (skip (expect-string ", "))
              expect-natural))
          (skip (expect #\newline)))))
    "Error in monkey item list"))
; Parses the arithmetic update to the worry operation
(define expect-arithmetic-expression
  (group-output
    (and-then
      (or-else
        (and-then (expect-string "old ") (mod-top (lambda (x) 'old)))
        expect-natural)
      (expv-set '(#\+ #\*))
      skip-whitespace
      (or-else
        (and-then (expect-string "old") (mod-top (lambda (x) 'old)))
        expect-natural))))
; parses the whole worry operation, returning a lambda
(define expect-operation
  (mod-err
    (and-then
      skip-whitespace
      (skip (expect-string "Operation: new = "))
      expect-arithmetic-expression
      (skip (expect #\newline))
      (mod-top (lambda (op)
                 (lambda (old)
                   ((case (second op)
                      [(#\+) +]
                      [(#\*) *])
                    (if (eqv? 'old (first op)) old (first op))
                    (if (eqv? 'old (third op)) old (third op)))))))
    "Error expecting operation"))
; parses the throw target logic, returning a pair containing the
; divisibility criterion (i.e., the modulus) and a lambda for the
; throw logic
(define expect-throw-logic
  (mod-err
    (and-then
      (group-output
        (and-then
          skip-whitespace
          (skip (expect-string "Test: divisible by "))
          expect-natural
          skip-whitespace
          (skip (expect-string "If true: throw to monkey "))
          expect-natural
          skip-whitespace
          ;(pdebug inp stack (begin (pretty-print (list->string inp)) (pretty-print stack)))
          (skip (expect-string "If false: throw to monkey "))
          expect-natural
          skip-eol-or-eof))
      (mod-top (lambda (throw-targets)
                 (cons (first throw-targets)
                   (lambda (worry)
                     (if (zero? (modulo worry (first throw-targets)))
                       (second throw-targets)
                       (third throw-targets)))))))
    "Error expecting throw logic"))

; parses a monkey
(define expect-monkey
  (and-then
    (group-output
      (and-then
        expect-monkey-header
        expect-monkey-items
        expect-operation
        expect-throw-logic))
    (mod-top (lambda (monkey-data)
               (monkey
                 (first monkey-data)
                 (second monkey-data)
                 (cdr (third monkey-data))
                 (car (third monkey-data)))))))

; parse all the monkeys
(define expect-input
  (at-least-one
    (and-then skip-whitespace expect-monkey)))

; run one round of monkey volleys
(define (do-round mnks invs)
  ; invs contains each monkey's inventory
  ; inspections is the number of inspections that the monkey has done this round
  (for/fold ([invs invs]
             [inspections (make-vector (vector-length mnks) 0)])
            ([idx (in-range (vector-length mnks))])
    (let ([current-mnk (vector-ref mnks idx)]
          [current-inv (vector-ref invs idx)])
      ; for each item in the current monkey's inventory
      (for/list ([item (in-list current-inv)])
        (let* ([new-worry ((monkey-operation current-mnk) item)]
               [throw-to ((monkey-throw-to current-mnk) new-worry)])
          ; add the thrown item to the target monkey's inventory
          (vector-set! invs throw-to
                       (cons new-worry
                             (vector-ref invs throw-to)))))
      ; keep track of the 
      (vector-set! inspections idx (length current-inv))
      ; clear the current monkey's inventory
      (vector-set! invs idx '())
      (values invs inspections))))

(define (do-part monkeys part)
  ; convert to vectors for easier indexing
  (let ([monkey-invs (list->vector (map monkey-items monkeys))]
        [monkeys (list->vector monkeys)])
    (->
      (let-values ([(_ inspecs)
                    ; iterate rounds
                    (for/fold ([monkey-invs monkey-invs]
                               [inspections (make-vector (vector-length monkeys) 0)])
                              ([rnd (in-range (if (eqv? part 'part1)
                                                20
                                                10000))])
                      (let-values ([(new-invs n-inspecs) (do-round monkeys monkey-invs)])
                        (values new-invs
                                (vector-map + inspections n-inspecs))))])
        inspecs)
      ; sort by monkey business
      (vector-sort >)
      (vector-take 2)
      ; multiply the top 2
      (=>> vector-foldl * 1))))

(define (part-1 inp)
  (let-values ([(_ monkeys) (run-parser expect-input inp)])
    ; modify the monkey's worry operation function to divide by 3
    (-> (map (lambda (mnk)
               (let ([op (monkey-operation mnk)])
                 (struct-copy monkey mnk
                              [operation (lambda (x) (floor (/ (op x) 3)))])))
             monkeys)
        (do-part 'part1))))

(define (part-2 inp)
  (let-values ([(_ monkeys) (run-parser expect-input inp)])
    (let ([ash-nazg-durbatuluuk (foldl * 1 (map monkey-mod monkeys))])
      ; Ints mod n form a ring :)
      ;
      ; The worry levels quickly blow up, making the computation slow even with big integers
      ; You could imagine fixing this by doing operations for a given monkey, modulo that
      ; monkey's divisor. However, when the monkey passes to another with a different divisor,
      ; the congruence falls apart (e.g., for monkeys with divisors 3 and 7, the worry value 25
      ; would be passed as 25 mod 3 = 1. Then, when Monkey 2 does its own passing logic, it
      ; decides whom to throw to based on 1 mod 7 = 1. In contrast, If Monkey 1 had
      ; not truncated the worry value with its modulo operation, the value 25 would have been passed
      ; to Monkey 2, whose divisibility test would be 25 mod 7 = 4, potentially leading to a different
      ; target).
      ;
      ; To keep the modular arithmetic congruent across all divisors, we need a common divisor.
      ; To this end, we can simply multiply all monkeys' divisors together (e.g., in the above
      ; example, 3 * 7 = 21). When we do this, the worry value of 25 becomes 25 mod 21 = 4. Then,
      ; when the monkeys do their individual modulo operations when deciding whom to throw to, we
      ; get:
      ;
      ; Monkey 1: (25 mod 21) mod 3 = 4 mod 3 = 1 mod 3; passes 25 mod 21 = 4 mod 21 to Monkey 2
      ; Monkey 2: <operation on 4, suppose it also gives 4 mod 21>: 4 mod 7 = (25 mod 21) mod 7
      ; 
      ; the following code addes this modulo operation to the operation function for each monkey,
      ; then runs as usual
      (-> (map (lambda (mnk)
                 (let ([op (monkey-operation mnk)])
                   (struct-copy monkey mnk
                                [operation (lambda (x) (modulo (op x)
                                                               ash-nazg-durbatuluuk))])))
               monkeys)
          (do-part 'part2)))))
