#lang racket

(require "misc.rkt")
(require "parsecomb.rkt")

(define expect-monkey-name (and-then
                             (group-output (at-least-one expect-lowercase))
                             (mod-top (compose string->symbol list->string))))
; parses an s-expression relating constants and monkeys
(define expect-math
  (and-then
    (group-output
      (and-then expect-monkey-name
                (skip (expect #\space))
                (expv-set '(#\+ #\- #\* #\/))
                (mod-top (compose string->symbol string))
                (skip (expect #\space))
                expect-monkey-name))
    (mod-top (lambda (x) (list (second x) (first x) (third x))))))
(define expect-monkey
  (group-output
    (and-then
      expect-monkey-name
      (skip-string ": ")
      (or-else
        expect-int
        expect-math)
      skip-eol-or-eof)))

(define expect-input (at-least-one expect-monkey))

(define test-inp (parse-file expect-input "inputs/day21.test"))
(define real-inp (parse-file expect-input "inputs/day21.inp"))

; not useful for part 2 so much, but still fun :)
; Builds a series of s-expressions that define functions with the
; monkeys' names that evaluate according to the input, then evaluates
; the function called root
(define (part-1 inp)
  ; terms are `(,monkey-name ,s-expr), where s-expr is either a constant
  ; or a proper scheme form for a math operation
  (let ([terms (-> (map (lambda (term)
                          (let ([name (first term)]
                                [fn (second term)])
                            (list 'define (list name) 
                                  (if (list? fn)
                                    (list (car fn) (list (second fn)) (list (third fn)))
                                    fn))))
                        inp)
                   (append '((root))))])
    (-> (cons 'begin terms)
        eval)))

(define (eval-form op l r)
  ((case op [(+) +] [(-) -] [(*) *] [(/) /])
   l
   r))

; folds all constants together that can be folded
(define (reduce-tree tree node)
  (cond
    [(integer? (hash-ref tree node)) tree]
    [(eqv? node 'humn) tree]
    [else
      (let* ([expr (hash-ref tree node)]
             [tree (-> tree
                       (reduce-tree (second expr))
                       (reduce-tree (third expr)))]
             [left-val (hash-ref tree (second expr))]
             [right-val (hash-ref tree (third expr))])
        (cond
          [(and (integer? left-val) (integer? right-val))
           (hash-set tree node (eval-form (first expr) left-val right-val))]
          [(integer? left-val) (hash-set tree node (list (first expr) left-val (third expr)))]
          [else (hash-set tree node (list (first expr) (second expr) right-val))]))]))

; calculates the inverse of a particular arithmetic operation of the form
; result = a `op` b
; ,solving-for indicates whether we are solving for a ('left) or b ('right) in
; order to properly handle non-commutative operations
(define (inverse-op op result x solving-for)
  (case op
    [(+) (- result x)]
    [(*) (/ result x)]
    [(-) (if (eqv? solving-for 'left)
           (+ result x)
           (- x result))]
    [(/) (if (eqv? solving-for 'left)
           (* result x)
           (/ x result))]))

; figures out what 'humn must be equal to
; takes a the constant-folded tree, the node in question, and
; what we need to get the node to equal (,val)
; expects node to refer to a node that is half-defined, i.e., one argument
; depends on 'humn, while the other is a constant
(define (solve-equal tree node val)
  ; we need to set 'humn to ,val, so...
  (if (eqv? node 'humn)
    val
    (let* ([expr (hash-ref tree node)]
           [op (first expr)]
           [left (second expr)]
           [right (third expr)])
      ; if the right argument depends on human
      (if (integer? left) ; right must be the thing we solve
        ; solve val = a `op` b for b
        (let ([right-val (inverse-op op val left 'right)])
          ; figure out how to make the right node equal to b
          (solve-equal tree (third expr) right-val))
        ; solve val = a `op` b for a
        (let ([left-val (inverse-op op val right 'left)])
          ; figure out how to make the left node equal to a
          (solve-equal tree (second expr) left-val))))))

(define (part-2 inp)
  (let ([tree (-> (for/hash ([x inp]) (values (first x) (second x)))
                  ; change the root operation
                  (hash-update 'root (lambda (x) (list '= (second x) (third x))))
                  ; change the human operation; this doesn't matter, it just assures
                  ; that I error out if I try to evaluate because node values are either
                  ; constants or lists, never symbols
                  (hash-set 'humn 'humn))])
    (let* ([tree (reduce-tree tree 'root)]
           [root-val (hash-ref tree 'root)]
           [left-val (second root-val)]
           [right-val (third root-val)])
      ; if right-val is undefined
      (if (integer? left-val)
        ; figure out how to get right-val equal to left val
        (solve-equal tree right-val left-val)
        ; figure out how to get left-val equal to right-val
        (solve-equal tree left-val right-val)))))
