#lang racket


(provide
  load-file
  ->
  ->>
  any->)

; just gulp a file
(define (load-file fname)
  (port->string (open-input-file fname) #:close? #t))

; threading macro; passes the result of one function as the first argument
; of the next
(define-syntax ->
  (syntax-rules ()
    [(-> val) val]
    [(-> val (fn args ...) others ...) (-> (fn val args ...) others ...)]
    [(-> val fn others ...) (-> (fn val) others ...)]))

; same as ->, but passes the result as the last argument of the next function
(define-syntax ->>
  (syntax-rules ()
    [(->> val) val]
    [(->> val (fn args ...) others ...) (->> (fn args ... val) others ...)]
    [(->> val fn others ...) (->> (fn val) others ...)]))

; threading macro that allows one to specify a placeholder varialbe marking the
; position into which a result will be passed into the next function, i.e.,
; (any-> '(a b c) lst
;   (append lst '(d))
;   (map (lambda (x) (cons x 42)) lst))
; gives
; '((a . 42) (b . 42) (c . 42) (d . 42))
(define-syntax (any-> stx)
  (syntax-case stx ()
    [(_ value it) #'value]
    [(_ value it (fn ...) others ...)
      #'(let [(it value)]
          (any-> (fn ...) it others ...))]))
