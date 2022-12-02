#lang racket


(provide
  load-file
  ->
  ->>
  any->)

(define (load-file fname)
  (port->string (open-input-file fname) #:close? #t))

(define-syntax ->
  (syntax-rules ()
    [(-> val) val]
    [(-> val (fn args ...) others ...) (-> (fn val args ...) others ...)]
    [(-> val fn others ...) (-> (fn val) others ...)]))

(define-syntax ->>
  (syntax-rules ()
    [(->> val) val]
    [(->> val (fn args ...) others ...) (->> (fn args ... val) others ...)]
    [(->> val fn others ...) (->> (fn val) others ...)]))

(define-syntax (any-> stx)
  (syntax-case stx ()
    [(_ value it) #'value]
    [(_ value it (fn ...) others ...)
      #'(let [(it value)]
          (any-> (fn ...) it others ...))]))
