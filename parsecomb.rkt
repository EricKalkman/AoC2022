#lang racket 

(require "misc.rkt")

(provide
  define-parser
  define-nonempty-parser
  pdebug
  nop
  push-stack
  push-guard
  mod-top
  mod-stack
  mod-stack-with-guard
  mod-guarded-stack
  mod-err
  and-then
  or-else
  maybe
  group-output
  at-least-none
  at-least-one
  skip
  expect
  expv
  expq
  expect-n
  expect-empty
  expect-remaining
  expect-fn
  expv-set
  expv-not-set
  expect-digit
  expect-uppercase
  expect-lowercase
  expect-letter
  expect-whitespace
  expect-line
  skip-eol-or-eof
  skip-whitespace
  expect-seq
  expect-string
  skip-string
  expect-natural
  expect-int
  expect-list
  run-parser
  parse-file

  ; sets
  DIGITS
  UPCASE
  LOCASE
  LETTERS
  WHITESPACE)

; defines a parser, taking symbols for input and stack bindings (see below) as well as any additional
; parameters
;
; If the parser is unparametrized, then "name" is a binding for the parser itself, taking 2 arguments
; (the remaining input (a list), and the current state of the parser ("stack")) and returning
; the remaining input and the updated stack as multiple values.
;
; Alternatively if the parser fails, returns #f as the remaining input, and a string error message
; as the second value
;
; If the parser is parametrized, then "name" is a binding for a function that returns such a parser
;
(define-syntax define-parser
  (syntax-rules ()
    [(_ (name inp-var stack-var) body ...)
     (begin
       (define (name inp-var stack-var)
         body ...)
       (provide name))]
    [(_ (name inp-var stack-var args ...) body ...)
     (begin
       (define (name args ...)
         (lambda (inp-var stack-var)
           body ...))
       (provide name))]))

; Defines a parser-type function (same rules as define-parser if the definition is parametrized or not)
; that checks to make sure that there is input remaining (i.e., not null input) before
(define-syntax define-nonempty-parser
  (syntax-rules ()
    [(_ (name inp-var stack-var) body ...)
      (begin
        (define (name inp-var stack-var)
          (if (null? inp-var)
            (values #f (format "End of input in ~a" name))
            (begin
              body ...)))
        (provide name))]
    [(_ (name inp-var stack-var args ...) body ...)
      (begin
        (define (name args ...)
          (lambda (inp-var stack-var)
            (if (null? inp-var)
              (values #f (format "End of input in ~a" name))
              (begin
                body ...))))
        (provide name))]))


(define-syntax-rule (pdebug inp stack body ...)
  (lambda (inp stack)
    (begin
      body ...
      (values inp stack))))

; this parser does not do anything; useful for some combinators
; always succeeds
(define-parser (nop inp stack) (values inp stack))

; defines a parser that pushes x onto the parser stack without modifying the input
; always succeeds
(define-parser (push-stack inp stack x) (values inp (cons x stack)))

; sentinal value used to mark a point in the parser stack for future reference
(define LOCAL-GUARD (gensym))

; pushes a sentinal value onto the stack
(define-parser (push-guard inp stack) (values inp (cons LOCAL-GUARD stack)))

; modifies the top value of the stack with the supplied fn
(define-parser (mod-top inp stack fn) (values inp (cons (fn (car stack)) (cdr stack))))

; modifies the entire stack with the supplied fn
(define-parser (mod-stack inp stack fn) (values inp (fn stack)))

; assumes that the sentinal value "guard" has been pushed to the stack previously
; Removes the top of the stack all the way down but excluding "guard", and passes these
; removed values to fn. The result of fn is pushed back onto the stack after removing the guard
(define-parser (mod-stack-with-guard inp stack guard fn)
               (let-values ([(head tail) (splitf-at stack (lambda (x) (not (eqv? x guard))))])
                 (values inp (cons (fn head) (cdr tail))))) ; cdr to drop guard

; assumes that a guard has been pusehd to the stack using push-guard
(define (mod-guarded-stack fn) (mod-stack-with-guard LOCAL-GUARD fn))

; Returns a new parser that runs p, and if it returns an error, replaces
; the error message with msg
(define-parser (mod-err inp stack p msg)
               (let-values ([(i2 s2) (p inp stack)])
                 (if i2
                   (values i2 s2)
                   (values i2 msg))))

; returns a new parser that runs p1, and if it succeeds, passes its output to p2
; otherwise, returns p1's error
(define-parser (and-then-2 inp stack p1 p2)
               (let-values ([(i2 s2) (p1 inp stack)])
                 (if i2
                   (p2 i2 s2)
                   (values i2 s2))))

; Returns a new parser that runs the provided sequence of parsers, passing one's output
; along as the input of the next. Returns at the first error encountered.
(define (and-then p1 . ps)
  (if (null? ps)
    p1
    (and-then-2 p1 (apply and-then ps))))

; Returns a new parser that runs p1. If p1 succeeds, returns p1's result. If p1 fails,
; returns the result of p2 using the original input.
(define-parser (or-else-2 inp stack p1 p2)
               (let-values ([(i2 s2) (p1 inp stack)])
                 (if i2
                   (values i2 s2)
                   (p2 inp stack))))

(define (or-else p1 . ps)
  (if (null? ps)
    p1
    (or-else-2 p1 (apply or-else ps))))

; Returns a new parser that runs p1, but returns unmodified input and stack if p1 fail
(define (maybe p1)
  (or-else p1 nop))

; Instead of putting the output of p directly onto the stack, groups the output
; into a list and pushes that list onto the stack
(define (group-output p)
  (and-then
    push-guard
    p
    (mod-guarded-stack reverse)))

; Returns a new parser that repeated evalutes p as many times as possible (minimum: 0 times).
; Cannot fail
(define-parser (at-least-none inp stack p)
               (let loop ([inp inp] [stack stack])
                 (let-values ([(i2 s2) (p inp stack)])
                   (if i2
                     (loop i2 s2)
                     (values inp stack)))))

; same as above, but p must succeed at least once
(define (at-least-one p)
  (and-then p (at-least-none p)))

; runs a parser that consumes input but returns an unmodified stack
(define-parser (skip inp stack p)
               (let-values ([(i2 s2) (p inp stack)])
                 (if i2
                   (values i2 stack)
                   (values i2 s2))))

(define (expect-n p n)
  (if (<= n 0)
    nop
    (and-then-2 p (expect-n p (- n 1)))))

; expects end of file (empty list as inp)
(define-parser (expect-empty inp stack)
               (if (null? inp)
                 (values inp stack)
                 (values #f "Expected no remaining input")))

; expects a non-empty input (i.e., not EOF)
(define-parser (expect-remaining inp stack)
               (if (null? inp)
                 (values #f "Empty input!")
                 (values inp stack)))

; Returns a parser that checks if x is the first element of the input list using the
; equality function eqfn. If so, consumes that element and places it on the stack
(define-nonempty-parser (expect-val inp stack x eqfn)
                        (if (eqfn x (car inp))
                          (values (cdr inp) (cons x stack))
                          (values #f (format "Expected ~a, found ~a" x (car inp)))))

; expect-val using equal?, eqv?, or eq?, respectively, as equality operators
(define (expect x) (expect-val x equal?))
(define (expv x) (expect-val x eqv?))
(define (expq x) (expect-val x eq?))

; Returns a parser that checks if the next input element satisfies the predicate pred. If so,
; consumes this element and places it on the stack.
(define-nonempty-parser (expect-fn inp stack pred)
               (if (pred (car inp))
                 (values (cdr inp) (cons (car inp) stack))
                 (values #f (format "Predicate failed: ~a" pred))))

; character sets
(define DIGITS (string->list "0123456789"))
(define UPCASE (string->list "ABCDEFGHIJKLMNOPQRSTVUWXYZ"))
(define LOCASE (string->list "abcdefghijklmnopqrstuvwxyz"))
(define LETTERS (append UPCASE LOCASE))
; sorry Windows
(define WHITESPACE (string->list " \t\n"))

; Checks that the next element in the input is present in the list st. If so,
; consumes this input and places it on the stack.
; Uses eqv? as the equality operator
(define (expv-set st)
  (expect-fn (lambda (x) (memv x st))))
(define (expv-not-set st)
  (expect-fn (lambda (x) (not (memv x st)))))
(define expect-digit
  (mod-err (expv-set DIGITS) "Expected a digit"))
(define expect-uppercase
  (mod-err (expv-set UPCASE) "Expected an uppercase letter"))
(define expect-lowercase
  (mod-err (expv-set LOCASE) "Expected a lowercase letter"))
(define expect-letter
  (mod-err (expv-set LETTERS) "Expected a letter"))
(define expect-whitespace
  (mod-err (expv-set WHITESPACE) "Expected a whitespace character"))
(define skip-whitespace (skip (at-least-none expect-whitespace)))
(define skip-eol-or-eof
  (mod-err
    (or-else (skip (expv #\newline)) expect-empty)
    "Expected newline or EOF"))

(define expect-line
  (and-then expect-remaining
            push-guard
            (at-least-none (expv-not-set '(#\newline)))
            skip-eol-or-eof
            (mod-guarded-stack reverse)))

(define (expect-seq lst)
  (mod-err
    (apply and-then (map expect lst))
    (format "Error when expecting ~a" lst)))
(define (expect-string str)
  (mod-err
    (and-then
      (skip (expect-seq (string->list str)))
      (push-stack str))
    (format "Error when expecting ~a" str)))
(define (skip-string str)
  (skip (expect-string str)))

; parses [0-9]+, and if it succeeds, pushes the parsed integer onto the stack
(define expect-natural
  (let [(guard (gensym))]
    (and-then
      (push-stack guard)
      (at-least-one expect-digit)
      (mod-stack-with-guard guard (lambda (slice) (-> slice reverse list->string string->number))))))

; like expect-natural, but allows for negative numbers
(define expect-int
  (and-then
    (maybe (expect #\-))
    expect-natural
    (mod-stack (lambda (stack)
                 (if (and (not (null? (cdr stack))) (eqv? (cadr stack) #\-))
                   `(,(- (car stack)) ,@(cddr stack))
                   stack)))))

(define (expect-list item-parser sep-parser)
  (group-output
    (and-then
      item-parser
      (at-least-none
        (and-then
          (skip sep-parser)
          item-parser)))))

; maybe more error handling here at some point, IDK
(define (run-parser p inp)
  (let-values ([(rem stack) (p inp '())])
    (if rem
      (values rem (reverse stack))
      (values rem stack))))

(define (parse-file parser fname)
  (let-values ([(_ parsed) (run-parser parser (load-file-as-list fname))])
    parsed))
