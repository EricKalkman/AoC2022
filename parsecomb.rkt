#lang racket 

(require "misc.rkt")

(provide
  define-parser
  define-nonempty-parser
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
  at-least-none
  at-least-one
  skip
  expect
  expv
  expq
  expect-empty
  expect-remaining
  expect-fn
  expect-digit
  expect-whitespace
  skip-whitespace
  expect-int
  run-parser)

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

(define-parser (nop inp stack) (values inp stack))

(define-parser (push-stack inp stack x) (values inp (cons x stack)))

; note: now needs single-threaded input stream
(define LOCAL-GUARD (gensym))

(define-parser (push-guard inp stack) (values inp (cons LOCAL-GUARD stack)))

(define-parser (mod-top inp stack fn) (values inp (cons (fn (car stack)) (cdr stack))))

(define-parser (mod-stack inp stack fn) (values inp (fn stack)))

(define-parser (mod-stack-with-guard inp stack guard fn)
               (let-values ([(head tail) (splitf-at stack (lambda (x) (not (eqv? x guard))))])
                 (values inp (cons (fn head) (cdr tail))))) ; cdr to drop guard

(define (mod-guarded-stack fn) (mod-stack-with-guard LOCAL-GUARD fn))

(define-parser (and-then-2 inp stack p1 p2)
               (let-values ([(i2 s2) (p1 inp stack)])
                 (if i2
                   (p2 i2 s2)
                   (values i2 s2))))

(define-parser (mod-err inp stack p msg)
               (let-values ([(i2 s2) (p inp stack)])
                 (if i2
                   (values i2 s2)
                   (values i2 msg))))

(define (and-then p1 . ps)
  (if (null? ps)
    p1
    (and-then-2 p1 (apply and-then ps))))

(define-parser (or-else-2 inp stack p1 p2)
               (let-values ([(i2 s2) (p1 inp stack)])
                 (if i2
                   (values i2 s2)
                   (p2 inp stack))))

(define (or-else p1 . ps)
  (if (null? ps)
    p1
    (or-else-2 p1 (apply or-else ps))))

(define (maybe p1)
  (or-else p1 nop))

(define-parser (at-least-none inp stack p)
               (let loop ([inp inp] [stack stack])
                 (let-values ([(i2 s2) (p inp stack)])
                   (if i2
                     (loop i2 s2)
                     (values inp stack)))))

(define (at-least-one p)
  (and-then p (at-least-none p)))

;; runs a parser but returns an unmodified stack
(define-parser (skip inp stack p)
               (let-values ([(i2 s2) (p inp stack)])
                 (values i2 stack)))

(define-parser (expect-empty inp stack)
               (if (null? inp)
                 (values inp stack)
                 (values #f "Expected no remaining input")))

(define-parser (expect-remaining inp stack)
               (if (null? inp)
                 (values #f "Empty input!")
                 (values inp stack)))

(define-nonempty-parser (expect-val inp stack x eqfn)
               (if (eqfn x (car inp))
                 (values (cdr inp) (cons x stack))
                 (values #f (format "Expected ~a, found ~a" x (car inp)))))

(define (expect x) (expect-val x equal?))
(define (expv x) (expect-val x eqv?))
(define (expq x) (expect-val x eq?))

(define-nonempty-parser (expect-fn inp stack fn)
               (if (fn (car inp))
                 (values (cdr inp) (cons (car inp) stack))
                 (values #f (format "Predicate failed: ~a" fn))))

(define DIGITS (string->list "0123456789"))
(define UPCASE (string->list "ABCDEFGHIJKLMNOPQRSTVUWXYZ"))
(define LOCASE (string->list "abcdefghijklmnopqrstuvwxyz"))
(define LETTERS (append UPCASE LOCASE))
(define WHITESPACE (string->list " \t\n"))

(define (expv-set st)
  (expect-fn (lambda (x) (memv x st))))
(define expect-digit
  (mod-err (expv-set DIGITS) "Expected a digit"))
(define expect-whitespace
  (mod-err (expv-set WHITESPACE) "Expected a whitespace character"))
(define skip-whitespace (skip (at-least-none expect-whitespace)))

(define expect-int
  (let [(guard (gensym))]
    (and-then
      (push-stack guard)
      (maybe (expv #\-))
      (at-least-one expect-digit)
      (mod-stack-with-guard guard (lambda (slice) (-> slice reverse list->string string->number))))))

(define (run-parser p inp)
  (let-values ([(rem stack) (p inp '())])
    (if rem
      (values rem (reverse stack))
      (values rem stack))))
